(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: closure.ml,v 1.27 2006/10/16 12:45:55 montela Exp $ *)

(* Introduction of closures, uncurrying, recognition of direct calls *)

open Misc
open Asttypes
open Primitive
open Lambda
open Typedlambda
open Switch
open Clambda
open Ctypedlambda
open Types

let typemap_ident (id,typ) = (id,get_accurate_typeinfo typ)

(* Auxiliaries for compiling functions *)

let rec split_list n l =
  if n <= 0 then ([], l) else begin
    match l with
      [] -> fatal_error "Closure.split_list"
    | a::l -> let (l1, l2) = split_list (n-1) l in (a::l1, l2)
  end

let rec build_closure_env env_param pos = function
    [] -> Tbl.empty
  | (id,typ) :: rem ->
      Tbl.add id (build_uterm (TypUprim(Pfield pos, [build_uvarterm env_param])) typ)
              (build_closure_env env_param (pos+1) rem)

(* Check if a variable occurs in a [clambda] term. *)

let occurs_var_untyped var u =
  let rec occurs u = match u with
      Uvar v -> v = var
    | Uconst cst -> false
    | Udirect_apply(lbl, args) -> List.exists occurs args
    | Ugeneric_apply(funct, args) -> occurs funct || List.exists occurs args
    | Uclosure(fundecls, clos) -> List.exists occurs clos
    | Uoffset(u, ofs) -> occurs u
    | Ulet(id, def, body) -> occurs def || occurs body
    | Uletrec(decls, body) ->
        List.exists (fun (id, u) -> occurs u) decls || occurs body
    | Uprim(p, args) -> List.exists occurs args
    | Uswitch(arg, s) ->
        occurs arg ||
        occurs_array_untyped s.us_actions_consts || occurs_array_untyped s.us_actions_blocks
    | Ustaticfail (_, args) -> List.exists occurs args
    | Ucatch(_, _, body, hdlr) -> occurs body || occurs hdlr
    | Utrywith(body, exn, hdlr) -> occurs body || occurs hdlr
    | Uifthenelse(cond, ifso, ifnot) ->
        occurs cond || occurs ifso || occurs ifnot
    | Usequence(u1, u2) -> occurs u1 || occurs u2
    | Uwhile(cond, body) -> occurs cond || occurs body
    | Ufor(id, lo, hi, dir, body) -> occurs lo || occurs hi || occurs body
    | Uassign(id, u) -> id = var || occurs u
    | Usend(met, obj, args) -> 
        occurs met || occurs obj || List.exists occurs args
  and occurs_array_untyped a =
    try
      for i = 0 to Array.length a - 1 do
        if occurs a.(i) then raise Exit
      done;
      false
    with Exit ->
      true
  in occurs u


let occurs_var var u =
  let rec occurs u = match u.utlterm with
      TypUvar v -> v = var
    | TypUconst cst -> false
    | TypUdirect_apply(lbl, args) -> List.exists occurs args
    | TypUgeneric_apply(funct, args) -> occurs funct || List.exists occurs args
    | TypUclosure(fundecls, clos) -> List.exists occurs clos
    | TypUoffset(u, ofs) -> occurs u
    | TypUlet(id, def, body) -> occurs def || occurs body
    | TypUletrec(decls, body) ->
        List.exists (fun (id, u) -> occurs u) decls || occurs body
    | TypUprim(p, args) -> List.exists occurs args
    | TypUswitch(arg, s) ->
        occurs arg ||
        occurs_array s.tus_actions_consts || occurs_array s.tus_actions_blocks
    | TypUstaticfail (_, args) -> List.exists occurs args
    | TypUcatch(_, _, body, hdlr) -> occurs body || occurs hdlr
    | TypUtrywith(body, exn, hdlr) -> occurs body || occurs hdlr
    | TypUifthenelse(cond, ifso, ifnot) ->
        occurs cond || occurs ifso || occurs ifnot
    | TypUsequence(u1, u2) -> occurs u1 || occurs u2
    | TypUwhile(cond, body) -> occurs cond || occurs body
    | TypUfor(id, lo, hi, dir, body) -> occurs lo || occurs hi || occurs body
    | TypUassign((id,_), u) -> id = var || occurs u
    | TypUsend(met, obj, args) -> 
        occurs met || occurs obj || List.exists occurs args
  and occurs_array a =
    try
      for i = 0 to Array.length a - 1 do
        if occurs a.(i) then raise Exit
      done;
      false
    with Exit ->
      true
  in occurs u

(* Determine whether the estimated size of a clambda term is below
   some threshold *)

let prim_size prim args =
  match prim with
    Pidentity -> 0
  | Pgetglobal id -> 1
  | Psetglobal id -> 1
  | Pmakeblock(tag, mut) -> 5 + List.length args
  | Pfield f -> 1
  | Pfldtag _ -> 1
  | Psetfield(f, isptr) -> if isptr then 4 else 1
  | Pfloatfield f -> 1
  | Psetfloatfield f -> 1
  | Pccall p -> (if p.prim_alloc then 10 else 4) + List.length args
  | Praise -> 4
  | Pstringlength -> 5
  | Pstringrefs | Pstringsets -> 6
  | Pmakearray kind -> 5 + List.length args
  | Parraylength kind -> if kind = Pgenarray then 6 else 2
  | Parrayrefu kind -> if kind = Pgenarray then 12 else 2
  | Parraysetu kind -> if kind = Pgenarray then 16 else 4
  | Parrayrefs kind -> if kind = Pgenarray then 18 else 8
  | Parraysets kind -> if kind = Pgenarray then 22 else 10
  | Pbittest -> 3
  | Pbigarrayref(ndims, _, _) -> 4 + ndims * 6
  | Pbigarrayset(ndims, _, _) -> 4 + ndims * 6
  | _ -> 2 (* arithmetic and comparisons *)

(* Very raw approximation of switch cost *)
  
let lambda_smaller lam threshold =
  let size = ref 0 in
  let rec lambda_size lam =
    if !size > threshold then raise Exit;
    match lam.utlterm with
      TypUvar v -> ()
    | TypUconst(TypTConst_base(Const_int _ | Const_char _ | Const_float _) |
             TypTConst_pointer _) -> incr size
    | TypUconst _ ->
        raise Exit (* avoid duplication of structured constants *)
    | TypUdirect_apply(fn, args) ->
        size := !size + 4; lambda_list_size args
    | TypUgeneric_apply(fn, args) ->
        size := !size + 6; lambda_size fn; lambda_list_size args
    | TypUclosure(defs, vars) ->
        raise Exit (* inlining would duplicate function definitions *)
    | TypUoffset(lam, ofs) ->
        incr size; lambda_size lam
    | TypUlet(id, lam, body) ->
        lambda_size lam; lambda_size body
    | TypUletrec(bindings, body) ->
        raise Exit (* usually too large *)
    | TypUprim(prim, args) ->
        size := !size + prim_size prim args;
        lambda_list_size args
    | TypUswitch(lam, cases) ->
        if Array.length cases.tus_actions_consts > 1 then size := !size + 5 ;
        if Array.length cases.tus_actions_blocks > 1 then size := !size + 5 ;
        lambda_size lam;
        lambda_array_size cases.tus_actions_consts ;
        lambda_array_size cases.tus_actions_blocks
    | TypUstaticfail (_,args) -> lambda_list_size args
    | TypUcatch(_, _, body, handler) ->
        incr size; lambda_size body; lambda_size handler
    | TypUtrywith(body, id, handler) ->
        size := !size + 8; lambda_size body; lambda_size handler
    | TypUifthenelse(cond, ifso, ifnot) ->
        size := !size + 2;
        lambda_size cond; lambda_size ifso; lambda_size ifnot
    | TypUsequence(lam1, lam2) ->
        lambda_size lam1; lambda_size lam2
    | TypUwhile(cond, body) ->
        size := !size + 2; lambda_size cond; lambda_size body
    | TypUfor(id, low, high, dir, body) ->
        size := !size + 4; lambda_size low; lambda_size high; lambda_size body
    | TypUassign(id, lam) ->
        incr size;  lambda_size lam
    | TypUsend(met, obj, args) ->
        size := !size + 8;
        lambda_size met; lambda_size obj; lambda_list_size args
  and lambda_list_size l = List.iter lambda_size l
  and lambda_array_size a = Array.iter lambda_size a in
  try
    lambda_size lam; !size <= threshold
  with Exit ->
    false

(* Check if a clambda term is pure'',
   that is without side-effects *and* not containing function definitions *)

let rec is_pure_clambda tu = match tu.utlterm with
    TypUvar v -> true
  | TypUconst cst -> true
  | TypUprim((Psetglobal _ | Psetfield _ | Psetfloatfield _ |
           Pccall _ | Praise | Poffsetref _ | Pstringsetu | Pstringsets |
           Parraysetu _ | Parraysets _ | Pbigarrayset _), _) -> false
  | TypUprim(p, args) -> List.for_all is_pure_clambda args
  | _ -> false

(* Simplify primitive operations on integers *)

let make_const_int n = (build_uterm (TypUconst(TypTConst_base(Const_int n))) TIint, Value_integer n) (* !! IS OKAY only if Value_approx_int is right about integers ! *)
let make_const_ptr n typ = (build_uterm (TypUconst(TypTConst_pointer n)) typ, Value_constptr n)
let make_const_bool b = 
  let n = (if b then 1 else 0) in
    (build_uterm (TypUconst(TypTConst_pointer n)) TIbool , Value_constptr n)

let simplif_prim_pure p (args, approxs) typ =
  match approxs with
    [Value_integer x] ->
      begin match p with
        Pidentity -> make_const_int x
      | Pnegint -> make_const_int (-x)
      | Poffsetint y -> make_const_int (x + y)
      | _ -> (build_uprimterm (p, args) typ, Value_unknown)
      end
  | [Value_integer x; Value_integer y] ->
      begin match p with
        Paddint -> make_const_int(x + y)
      | Psubint -> make_const_int(x - y)
      | Pmulint -> make_const_int(x * y)
      | Pdivint when y <> 0 -> make_const_int(x / y)
      | Pmodint when y <> 0 -> make_const_int(x mod y)
      | Pandint -> make_const_int(x land y)
      | Porint -> make_const_int(x lor y)
      | Pxorint -> make_const_int(x lxor y)
      | Plslint -> make_const_int(x lsl y)
      | Plsrint -> make_const_int(x lsr y)
      | Pasrint -> make_const_int(x asr y)
      | Pintcomp cmp ->
          let result = match cmp with
              Ceq -> x = y
            | Cneq -> x <> y
            | Clt -> x < y
            | Cgt -> x > y
            | Cle -> x <= y
            | Cge -> x >= y in
          make_const_bool result
      | _ -> (build_uprimterm(p,args) typ, Value_unknown)
      end
  | [Value_constptr x] ->
      begin match p with
        Pidentity -> make_const_ptr x typ
      | Pnot -> make_const_bool(x = 0)
      | Pisint -> make_const_bool true
      | _ -> (build_uprimterm(p,args) typ, Value_unknown)
      end
  | [Value_constptr x; Value_constptr y] ->
      begin match p with
        Psequand -> make_const_bool(x <> 0 && y <> 0)
      | Psequor  -> make_const_bool(x <> 0 || y <> 0)
      | _ -> (build_uprimterm(p,args) typ, Value_unknown)
      end
  | _ ->
      (build_uprimterm(p,args) typ, Value_unknown)

let simplif_prim p (args, approxs as args_approxs) typ =
  if List.for_all is_pure_clambda args
  then simplif_prim_pure p args_approxs typ
  else (build_uprimterm(p,args) typ, Value_unknown)

(* Substitute variables in a [ulambda] term (a body of an inlined function)
   and perform some more simplifications on integer primitives.
   Also perform alpha-conversion on let-bound identifiers to avoid
   clashes with locally-generated identifiers.
   The variables must not be assigned in the term.
   This is used to substitute "trivial" arguments for parameters
   during inline expansion. *)

let approx_ulam = function
    {utlterm=TypUconst(TypTConst_base(Const_int n))} -> Value_integer n
(*  | {utlterm=TypUconst(TypTConst_base(Const_char c))} -> Value_integer(Char.code c)  *) (* NOT correct ! should add Value_char !!! *)
  | {utlterm=TypUconst(TypTConst_pointer n)} -> Value_constptr n
  | _ -> Value_unknown

let rec substitute sb ulam =
  match ulam.utlterm with
    TypUvar v ->
      begin try Tbl.find v sb with Not_found -> ulam end
  | TypUconst cst -> ulam
  | TypUdirect_apply(lbl, args) ->
      build_uterm (TypUdirect_apply(lbl, List.map (substitute sb) args)) ulam.utltype
  | TypUgeneric_apply(fn, args) ->
       build_uterm (TypUgeneric_apply(substitute sb fn, List.map (substitute sb) args)) ulam.utltype
  | TypUclosure(defs, env) ->
      (* never present in an inlined function body; painful to get right *)
      assert false
  | TypUoffset(u, ofs) ->  build_uterm (TypUoffset(substitute sb u, ofs)) ulam.utltype
  | TypUlet((id,typ), u1, u2) ->
      let id' = Ident.rename id in
      build_uletterm ((id',typ), substitute sb u1, substitute (Tbl.add id (build_uvarterm (id',typ)) sb) u2) ulam.utltype
  | TypUletrec(bindings, body) ->
      (* never present in an inlined function body; painful to get right *)
      assert false
  | TypUprim(p, args) ->
      let sargs = List.map (substitute sb) args in
      let (res, _) = simplif_prim p (sargs, List.map approx_ulam sargs) ulam.utltype in
      res
  | TypUswitch(arg, sw) ->
      build_uswitchterm (substitute sb arg,
              { sw with
                tus_actions_consts =
                  Array.map (substitute sb) sw.tus_actions_consts;
                tus_actions_blocks =
                  Array.map (substitute sb) sw.tus_actions_blocks;
               }) ulam.utltype
  | TypUstaticfail (nfail, args) ->
      build_uterm (TypUstaticfail (nfail, List.map (substitute sb) args)) ulam.utltype
  | TypUcatch(nfail, ids, u1, u2) ->
      build_ucatchterm (nfail, ids, substitute sb u1, substitute sb u2) ulam.utltype
  | TypUtrywith(u1, (id,typ), u2) ->
      let id' = Ident.rename id in
      build_uterm (TypUtrywith(substitute sb u1, (id',typ), substitute (Tbl.add id (build_uvarterm (id',typ)) sb) u2)) ulam.utltype
  | TypUifthenelse(u1, u2, u3) ->
      begin match substitute sb u1 with
        {utlterm=TypUconst(TypTConst_pointer n)} ->
          if n <> 0 then substitute sb u2 else substitute sb u3
      | su1 ->
          build_uifthenelseterm (su1, substitute sb u2, substitute sb u3) ulam.utltype
      end
  | TypUsequence(u1, u2) -> build_usequenceterm((substitute sb u1, substitute sb u2)) ulam.utltype
  | TypUwhile(u1, u2) ->  build_uterm (TypUwhile(substitute sb u1, substitute sb u2)) ulam.utltype
  | TypUfor((id,typ), u1, u2, dir, u3) ->
      let id' = Ident.rename id in
       build_uterm (TypUfor((id',typ), substitute sb u1, substitute sb u2, dir,
           substitute (Tbl.add id (build_uvarterm (id',typ)) sb) u3)) ulam.utltype
  | TypUassign((id,typ), u) ->
      let id' =
        try
          match Tbl.find id sb with {utlterm=TypUvar i} -> i | _ -> assert false
        with Not_found ->
          id in
       build_uterm (TypUassign((id',typ), substitute sb u)) ulam.utltype
  | TypUsend(u1, u2, ul) ->
       build_uterm (TypUsend(substitute sb u1, substitute sb u2, List.map (substitute sb) ul)) ulam.utltype

(* Perform an inline expansion *)

let is_simple_argument ut = match ut.utlterm with
    TypUvar _ -> true
  | TypUconst(TypTConst_base(Const_int _ | Const_char _ | Const_float _)) -> true
  | TypUconst(TypTConst_pointer _) -> true
  | _ -> false

let no_effects ut = match ut.utlterm with
    TypUclosure _ -> true
  | TypUconst(TypTConst_base(Const_string _)) -> true
  | _ -> is_simple_argument ut

let rec bind_params subst params args body =
  match (params, args) with
    ([], []) -> substitute subst body
  | ((p1,typ) :: pl, a1 :: al) ->
      if is_simple_argument a1 then
        bind_params (Tbl.add p1 a1 subst) pl al body
      else begin
        let p1' = Ident.rename p1 in
        let body' = bind_params (Tbl.add p1 (build_uvarterm (p1',typ)) subst) pl al body in
        if occurs_var p1 body then build_uletterm ((p1',typ), a1, body') body'.utltype
        else if no_effects a1 then body'
        else build_usequenceterm((a1, body')) body'.utltype
      end
  | (_, _) -> assert false

(* Check if a lambda term is pure'',
   that is without side-effects *and* not containing function definitions *)

let rec is_pure ut = match ut.tlterm with
    TypLvar v -> true
  | TypLconst cst -> true
  | TypLprim((Psetglobal _ | Psetfield _ | Psetfloatfield _ |
           Pccall _ | Praise | Poffsetref _ | Pstringsetu | Pstringsets |
           Parraysetu _ | Parraysets _), _) -> false
  | TypLprim(p, args) -> List.for_all is_pure args
  | _ -> false

(* Generate a direct application *)

let direct_apply fundesc funct ufunct uargs apptype (* extra argument for Ocamil type propagation *) =
  let app_args =
    if fundesc.fun_closed then uargs else uargs @ [ufunct] in
  let app = (* TEMPO ? *) build_uterm (TypUdirect_apply(fundesc.fun_label, app_args)) apptype in
(* DE-ACTIVATED !!! MOD RAF !!! 
    match fundesc.fun_inline with
      None -> build_uterm (TypUdirect_apply(fundesc.fun_label, app_args)) apptype
    | Some(params, body) -> bind_params Tbl.empty params app_args body in *)
  (* If ufunct can contain side-effects or function definitions,
     we must make sure that it is evaluated exactly once.
     If the function is not closed, we evaluate ufunct as part of the
     arguments.
     If the function is closed, we force the evaluation of ufunct first. *)
  if not fundesc.fun_closed || is_pure funct
  then app
  else build_usequenceterm((ufunct, app)) app.utltype


(* Add [Value_integer] or [Value_constptr] info to the approximation
   of an application *)

let strengthen_approx appl approx =
  match approx_ulam appl with
    (Value_integer _ | Value_constptr _) as intapprox -> intapprox
  | _ -> approx

(* If a term has approximation Value_integer or Value_constptr and is pure,
   replace it by an integer constant *)

let check_constant_result lam ulam approx =
  match approx with
    Value_integer n when is_pure lam -> make_const_int n
  | Value_constptr n when is_pure lam -> make_const_ptr n ulam.utltype
  | _ -> (ulam, approx)

(* Evaluate an expression with known value for its side effects only,
   or discard it if it's pure *)

let sequence_constant_expr lam ulam1 (ulam2, approx2 as res2) =
  if is_pure lam then res2 else (build_usequenceterm((ulam1, ulam2)) ulam2.utltype, approx2)

(* Maintain the approximation of the global structure being defined *)

let global_approx = ref([||] : value_approximation array)

(* Maintain the nesting depth for functions *)

let function_nesting_depth = ref 0
let excessive_function_nesting_depth = 5

(* Uncurry an expression and explicitate closures.
   Also return the approximation of the expression.
   The approximation environment [fenv] maps idents to approximations.
   Idents not bound in [fenv] approximate to [Value_unknown].
   The closure environment [cenv] maps idents to [ulambda] terms.
   It is used to substitute environment accesses for free identifiers. *)

let close_approx_var fenv cenv (id,typ) =
  let approx = try Tbl.find id fenv with Not_found -> Value_unknown in
  match approx with
    Value_integer n ->
      make_const_int n
  | Value_constptr n ->
      make_const_ptr n typ
  | approx ->
      let subst = try Tbl.find id cenv with Not_found -> build_uvarterm (id,typ) in
      (subst, approx)

let close_var fenv cenv id =
  let (ulam, app) = close_approx_var fenv cenv id in ulam

exception Found of int

let rec close fenv cenv tl = match tl.tlterm with
    TypLvar id ->
      close_approx_var fenv cenv (id,get_accurate_typeinfo tl.tltype)
  | TypLconst cst ->
      let ucst = build_uterm (TypUconst (const_accurate_typeinfo cst)) (get_accurate_typeinfo tl.tltype) in
      begin match cst with
        TConst_base(Const_int n) -> (ucst, Value_integer n)
(*      | TConst_base(Const_char c) -> (ucst, Value_integer(Char.code c)) *) (* NOT Correct ! should add Value_char !! *)
      | TConst_pointer n -> (ucst, Value_constptr n)
      | _ -> (ucst, Value_unknown)
      end
  | TypLfunction(kind, params, body)  ->
      close_one_function fenv cenv ((Ident.create "fun"),tl.tltype) tl
  | TypLapply(funct, args) ->
      let nargs = List.length args in
      begin match (close fenv cenv funct, close_list fenv cenv args) with
        ((ufunct, Value_closure(fundesc, approx_res)),
         [{utlterm=TypUprim(Pmakeblock(_, _), uargs)}])
        when List.length uargs = - fundesc.fun_arity ->
          let app = direct_apply fundesc funct ufunct uargs (get_accurate_typeinfo tl.tltype) in
          (app, strengthen_approx app approx_res)
      | ((ufunct, Value_closure(fundesc, approx_res)), uargs)
        when nargs = fundesc.fun_arity ->
          let app = direct_apply fundesc funct ufunct uargs (get_accurate_typeinfo tl.tltype) in
          (app, strengthen_approx app approx_res)
      | ((ufunct, Value_closure(fundesc, approx_res)), uargs)
        when fundesc.fun_arity > 0 && nargs > fundesc.fun_arity -> (* sur-application *)
          let (first_args, rem_args) = split_list fundesc.fun_arity uargs in
	  (* TODO !! pb: typer l'application interne *)

	    (*
	  let inner_type = match fundesc.fun_label.funtype with 
	     TIarrow _ as typ -> Ctypedlambda.arrow_apply typ fundesc.fun_arity
	    | _ -> print_string "Failed to retrieve inner type of overapp\n";TIgenclosure
	    *)

	  (* dans ce cas de figure on s'attend tjrs a une fermeture *)
	  (* parfois le type inféré peut porter à confusion, comme dans le cas de Printf *)
	  let inner_type = TIgenclosure 

	  in
          (build_uterm (TypUgeneric_apply(direct_apply fundesc funct ufunct first_args inner_type, 
                          rem_args)) (get_accurate_typeinfo tl.tltype),
           Value_unknown)
      | ((ufunct, _), uargs) ->
          (build_uterm (TypUgeneric_apply(ufunct, uargs)) (get_accurate_typeinfo tl.tltype), Value_unknown)
      end
  | TypLsend(met, obj, args) ->
      let (umet, _) = close fenv cenv met in
      let (uobj, _) = close fenv cenv obj in
      (build_uterm (TypUsend(umet, uobj, close_list fenv cenv args)) (get_accurate_typeinfo tl.tltype), Value_unknown)
  | TypLlet(str, tid, lam, body) ->
      let (ulam, alam) = close_named fenv cenv tid lam in
      begin match (str, alam) with
        (Variable, _) ->
          let (ubody, abody) = close fenv cenv body in
          (build_uletterm (typemap_ident tid, ulam, ubody) (get_accurate_typeinfo tl.tltype), abody) 
      | (_, (Value_integer _ | Value_constptr _))
        when str = Alias || is_pure lam ->
          close (Tbl.add (fst tid) alam fenv) cenv body
      | (_, _) ->
          let (ubody, abody) = close (Tbl.add (fst tid) alam fenv) cenv body in
          (build_uletterm (typemap_ident tid, ulam, ubody) (get_accurate_typeinfo tl.tltype), abody)
      end
  | TypLletrec(defs, body) ->
      if List.for_all
           (function (id, {tlterm=TypLfunction(_, _, _)}) -> true | _ -> false)
           defs
      then begin
        (* Simple case: only function definitions *)
        let (clos, infos) = close_functions fenv cenv defs in
        let clos_ident = Ident.create "clos",clos.utltype in 
        let fenv_body =
          List.fold_right
            (fun (tid, pos, approx) fenv -> Tbl.add (fst tid) approx fenv)
            infos fenv in
        let (ubody, approx) = close fenv_body cenv body in
        (build_uletterm (clos_ident, clos,
              List.fold_right
                (fun (id, pos, approx) body ->
                    build_uletterm (id, build_uterm (TypUoffset(build_uvarterm clos_ident, pos)) (snd id), body) body.utltype)
                infos ubody) TIdontknow, (* rem : build_uletterm will replace TIdontknow *)
         approx)
      end else begin
        (* General case: recursive definition of values *)
        let rec clos_defs = function
          [] -> ([], fenv)
        | (tid, lam) :: rem ->
            let (udefs, fenv_body) = clos_defs rem in
            let (ulam, approx) = close fenv cenv lam in
            ((typemap_ident tid, ulam) :: udefs, Tbl.add (fst tid) approx fenv_body) in
        let (udefs, fenv_body) = clos_defs defs in
        let (ubody, approx) = close fenv_body cenv body in
        (build_uterm (TypUletrec(udefs, ubody)) ubody.utltype, approx)
      end
  | TypLprim(Pgetglobal id, []) ->
      check_constant_result tl
          (build_uprimterm (Pgetglobal id, []) (TIarray TIobject)) (Compilenv.global_approx id) 
  | TypLprim(Pmakeblock(tag, mut) as prim, lams) ->
      let (ulams, approxs) = List.split (List.map (close fenv cenv) lams) in
      (build_uprimterm (prim, ulams) (get_accurate_typeinfo tl.tltype), (* !!prim *)
       begin match mut with
           Immutable -> Value_tuple(Array.of_list approxs)
         | Mutable -> Value_unknown
       end)
  | TypLprim(Pfield n, [lam]) ->
      let (ulam, approx) = close fenv cenv lam in
      let fieldapprox =
        match approx with
          Value_tuple a when n < Array.length a -> a.(n)
        | _ -> Value_unknown in
      check_constant_result lam (build_uterm (TypUprim(Pfield n, [ulam])) (get_accurate_typeinfo tl.tltype)) fieldapprox (* !!prim *)
  | TypLprim(Pfldtag (n,tag), [lam]) ->
      let (ulam, approx) = close fenv cenv lam in
      let fieldapprox =
        match approx with
          Value_tuple a when n < Array.length a -> a.(n)
        | _ -> Value_unknown in
      check_constant_result lam (build_uterm (TypUprim(Pfldtag (n,tag), [ulam])) (get_accurate_typeinfo tl.tltype)) fieldapprox (* !!prim *)
  | TypLprim(Psetfield(n, _), [{tlterm=TypLprim(Pgetglobal id, [])} as arg1; lam]) ->
      let (ulam, approx) = close fenv cenv lam in
      (!global_approx).(n) <- approx;
	(build_uprimterm (Psetfield(n, false), [build_uprimterm (Pgetglobal id, []) (TIarray TIobject); ulam]) (get_accurate_typeinfo tl.tltype),
       Value_unknown)
  | TypLprim(p, args) ->
      simplif_prim p (close_list_approx fenv cenv args) (get_accurate_typeinfo tl.tltype)
  | TypLswitch(arg, sw) as l -> 
(* NB: failaction might get copied, thus it should be some Lstaticraise *)
      let (uarg, _) = close fenv cenv arg in
      let const_index, const_actions =
        close_switch fenv cenv sw.tsw_consts sw.tsw_numconsts sw.tsw_failaction
      and block_index, block_actions =
        close_switch fenv cenv sw.tsw_blocks sw.tsw_numblocks sw.tsw_failaction in
      (build_uswitchterm (uarg, 
               {tus_index_consts = const_index;
                tus_actions_consts = const_actions;
                tus_index_blocks = block_index;
                tus_actions_blocks = block_actions}) (get_accurate_typeinfo tl.tltype),
       Value_unknown)
  | TypLstaticraise (i, args) ->
      (build_uterm (TypUstaticfail (i, close_list fenv cenv args)) (get_accurate_typeinfo tl.tltype), Value_unknown)
  | TypLstaticcatch(body, (i, vars), handler) ->
      let (ubody, _) = close fenv cenv body in
      let (uhandler, _) = close fenv cenv handler in
      (build_ucatchterm (i, (List.map typemap_ident vars), ubody, uhandler) (get_accurate_typeinfo tl.tltype), Value_unknown)
  | TypLtrywith(body, id, handler) ->
      let (ubody, _) = close fenv cenv body in
      let (uhandler, _) = close fenv cenv handler in
      (build_uterm (TypUtrywith(ubody, typemap_ident id, uhandler)) (get_accurate_typeinfo tl.tltype), Value_unknown)
  | TypLifthenelse(arg, ifso, ifnot) ->
      begin match close fenv cenv arg with
        (uarg, Value_constptr n) ->
          sequence_constant_expr arg uarg
            (close fenv cenv (if n = 0 then ifnot else ifso))
      | (uarg, _ ) ->    
          let (uifso, _) = close fenv cenv ifso in
          let (uifnot, _) = close fenv cenv ifnot in
          (build_uifthenelseterm (uarg, uifso, uifnot) (get_accurate_typeinfo tl.tltype), Value_unknown)
      end
  | TypLsequence(lam1, lam2) ->
      let (ulam1, _) = close fenv cenv lam1 in
      let (ulam2, approx) = close fenv cenv lam2 in
      (build_usequenceterm((ulam1, ulam2)) (get_accurate_typeinfo tl.tltype), approx)
  | TypLwhile(cond, body) ->
      let (ucond, _) = close fenv cenv cond in
      let (ubody, _) = close fenv cenv body in
      (build_uterm (TypUwhile(ucond, ubody)) (get_accurate_typeinfo tl.tltype), Value_unknown)
  | TypLfor(id, lo, hi, dir, body) ->
      let (ulo, _) = close fenv cenv lo in
      let (uhi, _) = close fenv cenv hi in
      let (ubody, _) = close fenv cenv body in
      (build_uterm (TypUfor(typemap_ident id, ulo, uhi, dir, ubody)) (get_accurate_typeinfo tl.tltype), Value_unknown)
  | TypLassign(id, lam) ->
      let (ulam, _) = close fenv cenv lam in
      (build_uterm (TypUassign(typemap_ident id, ulam)) (get_accurate_typeinfo tl.tltype), Value_unknown)
  | TypLevent _ | TypLifused _ -> assert false 


and close_list fenv cenv = function
    [] -> []
  | lam :: rem ->
      let (ulam, _) = close fenv cenv lam in
      ulam :: close_list fenv cenv rem

and close_list_approx fenv cenv = function
    [] -> ([], [])
  | lam :: rem ->
      let (ulam, approx) = close fenv cenv lam in
      let (ulams, approxs) = close_list_approx fenv cenv rem in
      (ulam :: ulams, approx :: approxs)

and close_named fenv cenv id tl = match tl.tlterm with
    TypLfunction(kind, params, body) ->
      close_one_function fenv cenv id tl
  | _ ->
      close fenv cenv tl

(* Build a shared closure for a set of mutually recursive functions *)

and close_functions fenv cenv fun_defs =
  (* Update and check nesting depth *)
  incr function_nesting_depth;
  let initially_closed =
    !function_nesting_depth < excessive_function_nesting_depth in
  (* Determine the free variables of the functions *)
  let fv_with_types =
    List.map typemap_ident (IdentSet.elements (free_variables (build_term (TypLletrec(fun_defs, lambda_unit)) None (* aucune importance ici*) ))) in
  let fv = List.map fst fv_with_types in
  (* Build the function descriptors for the functions.
     Initially all functions are assumed not to need their environment
     parameter. *)
  let uncurried_defs =
    List.map
      (function
          ((id,typ), ({tlterm=TypLfunction(kind, params, body)} as def)) ->
	    let typ2 = get_accurate_typeinfo typ in
            let label =
	      let uname = 
		(* pour le toplevel, utilisation du caractere special '^$' *)
		(* pour des identificateurs voués à être uniques *)
		let tempname = Ident.unique_name id in
		if tempname.[0] != '$' then tempname else Ident.name id in
              Compilenv.current_unit_name() ^ "__" ^ uname in
            let arity = List.length params in
            let fundesc =
              {fun_label =  { opt=label; ilinfo=None;  funtype= typ2  }; 
               fun_arity = (if kind = Tupled then -arity else arity);
               fun_closed = initially_closed;
               (*fun_inline = None TEMPO!!RAF *) } in
              ((id,typ), List.map typemap_ident params, body, fundesc)
        | (_, _) -> fatal_error "Closure.close_functions")
      fun_defs in
  (* Build an approximate fenv for compiling the functions *)
  let fenv_rec =
    List.fold_right
      (fun ((id,_), params, body, fundesc) fenv ->
        Tbl.add id (Value_closure(fundesc, Value_unknown)) fenv)
      uncurried_defs fenv in
  (* Determine the offsets of each function's closure in the shared block *)
  let env_pos = ref (-1) in
  let clos_offsets =
    List.map
      (fun ((id,_), params, body, fundesc) ->
        let pos = !env_pos + 1 in
        env_pos := !env_pos + 1 + (if fundesc.fun_arity <> 1 then 3 else 2);
        pos)
      uncurried_defs in
  let fv_pos = !env_pos in
  (* This reference will be set to false if the hypothesis that a function
     does not use its environment parameter is invalidated. *)
  let useless_env = ref initially_closed in
  (* Translate each function definition *)
  let clos_fundef ((id,typ), params, body, fundesc) env_pos =
    let typ2 = get_accurate_typeinfo typ in
    let env_param = Ident.create "env",TIgenclosure in (* preciser fermeture ? *)
    let cenv_fv =
      build_closure_env env_param (fv_pos - env_pos) fv_with_types in
    let cenv_body =
      List.fold_right2
        (fun ((id,ftyp), params, arity, body) pos env ->
	   let ftyp2 = get_accurate_typeinfo ftyp in
          Tbl.add id (build_uterm (TypUoffset(build_uvarterm env_param, pos - env_pos)) ftyp2) env)
        uncurried_defs clos_offsets cenv_fv in
    let body' = if (get_accurate_typeinfo body.tltype) <> TIdontknow then body 
    else {body with tltype = arrow_apply_camltypes typ fundesc.fun_arity} in
      (* ^^ permet de retrouver le type du corps à partir du type de la fonction ^^ *)
      (* par exemple le corps de "fun () -> assert false" a un type propagé inconnu jusqu'à présent *)
    let (ubody, approx) = close fenv_rec cenv_body body' in
    if !useless_env && occurs_var (fst env_param) ubody then useless_env := false;
    let fun_params = if !useless_env then params else params @ [env_param] in
    ((fundesc.fun_label, fundesc.fun_arity, fun_params, ubody),
     ((id,typ2), env_pos, Value_closure(fundesc, approx))) in
  (* Translate all function definitions. *)
  let clos_info_list =
    if initially_closed then begin
      let cl = List.map2 clos_fundef uncurried_defs clos_offsets in
      (* If the hypothesis that the environment parameters are useless has been
         invalidated, then set [fun_closed] to false in all descriptions and
         recompile *)
      if !useless_env then cl else begin
        List.iter
          (fun (id, params, body, fundesc) -> fundesc.fun_closed <- false)
          uncurried_defs;
        List.map2 clos_fundef uncurried_defs clos_offsets
      end
    end else
      (* Excessive closure nesting: assume environment parameter is used *)
        List.map2 clos_fundef uncurried_defs clos_offsets
    in
  (* Update nesting depth *)
  decr function_nesting_depth;
  (* Return the Uclosure node and the list of all identifiers defined,
     with offsets and approximations. *)
  let (clos, infos) = List.split clos_info_list in

  (build_uterm (TypUclosure(clos, List.map (close_var fenv cenv) fv_with_types)) (if (List.length clos) >1 then TIsharedclosure else TIgenclosure), infos) 

(* Same, for one non-recursive function *)

and close_one_function fenv cenv id funct =
  match close_functions fenv cenv [id, funct] with
      (({utlterm=TypUclosure([_, _, params, body], _)} as clos),
       [_, _, (Value_closure(fundesc, _) as approx)]) ->
        (* See if the function can be inlined *)
(* !! MOD RAPH TEMPO : interdire l'inlining *)
(*        if lambda_smaller body (!Clflags.inline_threshold + List.length params)
        then fundesc.fun_inline <- Some(params, body); *)
        (clos, approx)
    | _ -> fatal_error "Closure.close_one_function"

(* Close a switch *)

and close_switch fenv cenv cases num_keys default =
  let index = Array.create num_keys 0
  and store = mk_store Pervasives.(=) in

  (* First default case *)
  begin match default with
  | Some def when List.length cases < num_keys ->
      ignore (store.act_store def)
  | _ -> ()
  end ;
  (* Then all other cases *)
  List.iter
    (fun (key,lam) ->
     index.(key) <- store.act_store lam)
    cases ;
  (* Compile action *)
  let actions =
    Array.map
      (fun lam ->
        let ulam,_ = close fenv cenv lam in
        ulam)
      (store.act_get ()) in
  match actions with
  | [| |] -> [| |], [| |] (* May happen when default is None *)
  | _     -> index, actions


(* The entry point *)

let intro size lam =
  function_nesting_depth := 0;
  global_approx := Array.create size Value_unknown;
  Compilenv.set_global_approx(Value_tuple !global_approx);
  let (ulam, approx) = close Tbl.empty Tbl.empty lam in
  global_approx := [||];
  ulam

