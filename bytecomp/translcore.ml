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

(* $Id: translcore.ml,v 1.38 2007/03/25 09:45:59 montela Exp $ *)

(* Translation from typed abstract syntax to lambda terms,
   for the core language *)

(* ATTENTION !!!!!!!!!!!!!!!!!!!!!! *)
(* modifs des primitives afin d'eviter le plus possible ceq *)
(* a cause des entiers BoxInt *)



(* Etat d'incorporation des annotations de type *)
(* event laissé de côté *)



open Misc
open Asttypes
open Primitive
open Path
open Types
open Typedtree
open Typeopt
open Lambda
open Typedlambda

type error =
    Illegal_letrec_pat
  | Illegal_letrec_expr
  | Free_super_var

exception Error of Location.t * error

(* Forward declaration -- to be filled in by Translmod.transl_module *)
let transl_module =
  ref((fun cc rootpath modl -> assert false) :
      module_coercion -> Path.t option -> module_expr -> typedlambda)

(* Translation of primitives *)

open Il

let comparisons_table =
  let sysobj = Tobject in
  let sysstr = Ilpredef.builder_type in 
  let compare = Ilpredef.comp_ref in
  create_hashtable 11 [
  "%eq", (* this is == *)
      (Pccall{prim_name = "eq"; prim_arity = 2; prim_alloc = true;
              prim_native_name = ""; prim_native_float = false;
              prim_IL=Some
                { ilprim_class=compare; ilprim_name="eq"; 
                  ilprim_rt=Il.Tbool ; ilprim_virt=false;
                  ilprim_sig=[sysobj,None;sysobj,None] }},
       Pintcomp Ceq, 
       Pfloatcomp Ceq, 
       Pil "eq",
       Pbintcomp(Pnativeint, Ceq),
       Pbintcomp(Pint32, Ceq),
       Pbintcomp(Pint64, Ceq));
  "%noteq",
   (Pccall{prim_name = "noteq"; prim_arity = 2; prim_alloc = true;
              prim_native_name = ""; prim_native_float = false; 
              prim_IL=Some 
                { ilprim_class=compare; ilprim_name="noteq"; 
                  ilprim_rt=Il.Tbool ; ilprim_virt=false;
                  ilprim_sig=[sysobj,None;sysobj,None] }},
       Pintcomp Cneq,
       Pfloatcomp Cneq, 
       Pil "neq",
       Pbintcomp(Pnativeint, Cneq),
       Pbintcomp(Pint32, Cneq),
       Pbintcomp(Pint64, Cneq)) ; 
  "%equal", (* this is = *)
      (Pccall{prim_name = "equal"; prim_arity = 2; prim_alloc = true;
              prim_native_name = ""; prim_native_float = false;
              prim_IL=Some
                { ilprim_class=compare; ilprim_name="equal"; 
                  ilprim_rt=Il.Tbool ; ilprim_virt=false;
                  ilprim_sig=[sysobj,None;sysobj,None] }},
       Pintcomp Ceq,
       Pfloatcomp Ceq,
       Pil "str_equal",
       Pbintcomp(Pnativeint, Ceq),
       Pbintcomp(Pint32, Ceq),
       Pbintcomp(Pint64, Ceq));
  "%notequal",
      (Pccall{prim_name = "notequal"; prim_arity = 2; prim_alloc = true;
              prim_native_name = ""; prim_native_float = false; 
              prim_IL=Some 
                { ilprim_class=compare; ilprim_name="notequal"; 
                  ilprim_rt=Il.Tbool ; ilprim_virt=false;
                  ilprim_sig=[sysobj,None;sysobj,None] }},
       Pintcomp Cneq,
       Pfloatcomp Cneq,
       Pil "str_nequal",
       Pbintcomp(Pnativeint, Cneq),
       Pbintcomp(Pint32, Cneq),
       Pbintcomp(Pint64, Cneq)) ;
  "%lessthan",
      (Pccall{prim_name = "lessthan"; prim_arity = 2; prim_alloc = true;
              prim_native_name = ""; prim_native_float = false; 
              prim_IL=Some 
                { ilprim_class=compare; ilprim_name="lessthan"; 
                  ilprim_rt=Il.Tbool ; ilprim_virt=false;
                  ilprim_sig=[sysobj,None;sysobj,None] }},
       Pintcomp Clt,
       Pfloatcomp Clt,
       Pil "str_lessthan",
       Pbintcomp(Pnativeint, Clt),
       Pbintcomp(Pint32, Clt),
       Pbintcomp(Pint64, Clt));
  "%greaterthan",
      (Pccall{prim_name = "greaterthan"; prim_arity = 2; prim_alloc = true;
              prim_native_name = ""; prim_native_float = false; 
              prim_IL=Some 
                { ilprim_class=compare; ilprim_name="greaterthan"; 
                  ilprim_rt=Il.Tbool ; ilprim_virt=false;
                  ilprim_sig=[sysobj,None;sysobj,None] }},
       Pintcomp Cgt,
       Pfloatcomp Cgt,
       Pil "str_greaterthan",
       Pbintcomp(Pnativeint, Cgt),
       Pbintcomp(Pint32, Cgt),
       Pbintcomp(Pint64, Cgt));
  "%lessequal",
      (Pccall{prim_name = "lessequal"; prim_arity = 2; prim_alloc = true;
              prim_native_name = ""; prim_native_float = false; 
              prim_IL=Some 
                { ilprim_class=compare; ilprim_name="lessequal"; 
                  ilprim_rt=Il.Tbool ; ilprim_virt=false;
                  ilprim_sig=[sysobj,None;sysobj,None] }},
       Pintcomp Cle,
       Pfloatcomp Cle,
       Pil "str_lessequal",
       Pbintcomp(Pnativeint, Cle),
       Pbintcomp(Pint32, Cle),
       Pbintcomp(Pint64, Cle));
  "%greaterequal",
      (Pccall{prim_name = "greaterequal"; prim_arity = 2; prim_alloc = true;
              prim_native_name = ""; prim_native_float = false; 
              prim_IL=Some 
                { ilprim_class=compare; ilprim_name="greaterequal"; 
                  ilprim_rt=Il.Tbool ; ilprim_virt=false;
                  ilprim_sig=[sysobj,None;sysobj,None] }},
       Pintcomp Cge,
       Pfloatcomp Cge,
       Pil "str_greaterequal",
       Pbintcomp(Pnativeint, Cge),
       Pbintcomp(Pint32, Cge),
       Pbintcomp(Pint64, Cge))
  ] 

let primitives_table = 
  let sysobj = Tobject in
  let sysstr = Ilpredef.builder_type in 
  let compare = Ilpredef.comp_ref in
    create_hashtable 57 [
  "%identity", Pidentity;
  "%ignore", Pignore;
  "%field0", Pfield 0;
  "%field1", Pfield 1;
  "%setfield0", Psetfield(0, true);
  "%makeblock", Pmakeblock(0, Immutable);
  "%makemutable", Pmakeblock(0, Mutable);
  "%raise", Praise;
  "%sequand", Psequand;
  "%sequor", Psequor;
  "%boolnot", Pnot;
  "%negint", Pnegint;
  "%succint", Poffsetint 1;
  "%predint", Poffsetint(-1);
  "%addint", Paddint;
  "%subint", Psubint;
  "%mulint", Pmulint;
  "%divint", Pdivint;
  "%modint", Pmodint;
  "%andint", Pandint;
  "%orint", Porint;
  "%xorint", Pxorint;
  "%lslint", Plslint;
  "%lsrint", Plsrint;
  "%asrint", Pasrint;
  "%ltint", Pintcomp Clt;
  "%leint", Pintcomp Cle;
  "%gtint", Pintcomp Cgt;
  "%geint", Pintcomp Cge;
  "%incr", Poffsetref(1);
  "%decr", Poffsetref(-1);
  "%intoffloat", Pintoffloat;
  "%floatofint", Pfloatofint;
  "%negfloat", Pnegfloat;
  "%absfloat", Pabsfloat;
  "%addfloat", Paddfloat;
  "%subfloat", Psubfloat;
  "%mulfloat", Pmulfloat;
  "%divfloat", Pdivfloat;
  "%eqfloat", Pfloatcomp Ceq;
  "%noteqfloat", Pfloatcomp Cneq;
  "%ltfloat", Pfloatcomp Clt;
  "%lefloat", Pfloatcomp Cle;
  "%gtfloat", Pfloatcomp Cgt;
  "%gefloat", Pfloatcomp Cge;
  "%string_length", Pstringlength;
(* PPPP creer %string_create et le remplacer dans la stdlib, le compiler comme du code en ligne *)
  "%string_safe_get", Pstringrefs;
  "%string_safe_set", Pstringsets;
  "%string_unsafe_get", Pstringrefu;
  "%string_unsafe_set", Pstringsetu;
  "%array_length", Parraylength Pgenarray;
  "%array_safe_get", Parrayrefs Pgenarray;
  "%array_safe_set", Parraysets Pgenarray;
  "%array_unsafe_get", Parrayrefu Pgenarray;
  "%array_unsafe_set", Parraysetu Pgenarray;
  "%obj_size", Parraylength Pgenarray;
  "%obj_field", Parrayrefu Pgenarray;
  "%obj_set_field", Parraysetu Pgenarray;
  "%obj_is_int", Pisint;
  "%nativeint_of_int", Pbintofint Pnativeint;
  "%nativeint_to_int", Pintofbint Pnativeint;
  "%nativeint_neg", Pnegbint Pnativeint;
  "%nativeint_add", Paddbint Pnativeint;
  "%nativeint_sub", Psubbint Pnativeint;
  "%nativeint_mul", Pmulbint Pnativeint;
  "%nativeint_div", Pdivbint Pnativeint;
  "%nativeint_mod", Pmodbint Pnativeint;
  "%nativeint_and", Pandbint Pnativeint;
  "%nativeint_or",  Porbint Pnativeint;
  "%nativeint_xor", Pxorbint Pnativeint;
  "%nativeint_lsl", Plslbint Pnativeint;
  "%nativeint_lsr", Plsrbint Pnativeint;
  "%nativeint_asr", Pasrbint Pnativeint;
  "%int32_of_int", Pbintofint Pint32;
  "%int32_to_int", Pintofbint Pint32;
  "%int32_neg", Pnegbint Pint32;
  "%int32_add", Paddbint Pint32;
  "%int32_sub", Psubbint Pint32;
  "%int32_mul", Pmulbint Pint32;
  "%int32_div", Pdivbint Pint32;
  "%int32_mod", Pmodbint Pint32;
  "%int32_and", Pandbint Pint32;
  "%int32_or",  Porbint Pint32;
  "%int32_xor", Pxorbint Pint32;
  "%int32_lsl", Plslbint Pint32;
  "%int32_lsr", Plsrbint Pint32;
  "%int32_asr", Pasrbint Pint32;
  "%int64_of_int", Pbintofint Pint64;
  "%int64_to_int", Pintofbint Pint64;
  "%int64_neg", Pnegbint Pint64;
  "%int64_add", Paddbint Pint64;
  "%int64_sub", Psubbint Pint64;
  "%int64_mul", Pmulbint Pint64;
  "%int64_div", Pdivbint Pint64;
  "%int64_mod", Pmodbint Pint64;
  "%int64_and", Pandbint Pint64;
  "%int64_or",  Porbint Pint64;
  "%int64_xor", Pxorbint Pint64;
  "%int64_lsl", Plslbint Pint64;
  "%int64_lsr", Plsrbint Pint64;
  "%int64_asr", Pasrbint Pint64;
  "%nativeint_of_int32", Pcvtbint(Pint32, Pnativeint);
  "%nativeint_to_int32", Pcvtbint(Pnativeint, Pint32);
  "%int64_of_int32", Pcvtbint(Pint32, Pint64);
  "%int64_to_int32", Pcvtbint(Pint64, Pint32);
  "%int64_of_nativeint", Pcvtbint(Pnativeint, Pint64);
  "%int64_to_nativeint", Pcvtbint(Pint64, Pnativeint);
  "%bigarray_ref_1", Pbigarrayref(1, Pbigarray_unknown, Pbigarray_c_layout);
  "%bigarray_ref_2", Pbigarrayref(2, Pbigarray_unknown, Pbigarray_c_layout);
  "%bigarray_ref_3", Pbigarrayref(3, Pbigarray_unknown, Pbigarray_c_layout);
  "%bigarray_set_1", Pbigarrayset(1, Pbigarray_unknown, Pbigarray_c_layout);
  "%bigarray_set_2", Pbigarrayset(2, Pbigarray_unknown, Pbigarray_c_layout);
  "%bigarray_set_3", Pbigarrayset(3, Pbigarray_unknown, Pbigarray_c_layout)
]

let prim_makearray =
  { prim_name = "make_vect"; prim_arity = 2; prim_alloc = true;
    prim_native_name = ""; prim_native_float = false; 
    prim_IL = Some { ilprim_class=Ilpredef.array_ref ; ilprim_name="make_vect";
		     ilprim_rt=(Il.Tvector (Tobject) ); ilprim_virt=false;
		     ilprim_sig=[Il.Tint32,None;Tobject,None]}}

let prim_obj_dup =
  { prim_name = "obj_dup"; prim_arity = 1; prim_alloc = true;
    prim_native_name = ""; prim_native_float = false; 
    prim_IL =  Some { ilprim_class=Ilpredef.camil_obj_ref ; ilprim_name="obj_dup";
		      ilprim_rt=Tobject ; ilprim_virt=false;
		      ilprim_sig=[Tobject,None]}}


let transl_prim prim args =
  try
    let (gencomp, intcomp, floatcomp, stringcomp,
         nativeintcomp, int32comp, int64comp) =
      Hashtbl.find comparisons_table prim.prim_name in
    begin match args with
(*MOD*)
(* PPPP removed this case for lightning *)
(* equality between pointers differs from int equality *)
(***
      [arg1; {exp_desc = Texp_construct({cstr_tag = Cstr_constant _}, _)}] ->
        intcomp
    | [{exp_desc = Texp_construct({cstr_tag = Cstr_constant _}, _)}; arg2] ->
        intcomp
****)

    | [arg1; arg2] when has_base_type arg1 Predef.path_int
                     || has_base_type arg1 Predef.path_char ->
        intcomp
    | [arg1; arg2] when has_base_type arg1 Predef.path_float ->
        floatcomp
    | [arg1; arg2] when has_base_type arg1 Predef.path_string ->
        stringcomp
    | [arg1; arg2] when has_base_type arg1 Predef.path_nativeint ->
        nativeintcomp
    | [arg1; arg2] when has_base_type arg1 Predef.path_int32 ->
        int32comp
    | [arg1; arg2] when has_base_type arg1 Predef.path_int64 ->
        int64comp
    | _ ->
        gencomp
    end
  with Not_found ->
  try
    let p = Hashtbl.find primitives_table prim.prim_name in
    (* Try strength reduction based on the type of the argument *)
    begin match (p, args) with
        (Psetfield(n, _), [arg1; arg2]) -> Psetfield(n, maybe_pointer arg2)
      | (Parraylength Pgenarray, [arg])   -> Parraylength(array_kind arg)
      | (Parrayrefu Pgenarray, arg1 :: _) -> Parrayrefu(array_kind arg1)
      | (Parraysetu Pgenarray, arg1 :: _) -> Parraysetu(array_kind arg1)
      | (Parrayrefs Pgenarray, arg1 :: _) -> Parrayrefs(array_kind arg1)
      | (Parraysets Pgenarray, arg1 :: _) -> Parraysets(array_kind arg1)
      | (Pbigarrayref(n, Pbigarray_unknown, _), arg1 :: _) ->
            let (k, l) = bigarray_kind_and_layout arg1 in
            Pbigarrayref(n, k, l)
      | (Pbigarrayset(n, Pbigarray_unknown, _), arg1 :: _) ->
            let (k, l) = bigarray_kind_and_layout arg1 in
            Pbigarrayset(n, k, l)
      | _ -> p
    end
  with Not_found ->
    Pccall prim


(* Eta-expand a primitive without knowing the types of its arguments *)
let transl_primitive p =
  let prim =
    try
      let (gencomp, _, _, _, _, _, _) =
        Hashtbl.find comparisons_table p.prim_name in
      gencomp
    with Not_found ->
    try
      Hashtbl.find primitives_table p.prim_name
    with Not_found ->
      Pccall p in
  let rec make_params n =
    if n<=0 then [] else (Ident.create "prim",None) :: (make_params (n-1)) in
  let params = make_params p.prim_arity in
    build_term (TypLfunction(Curried, params,build_term (TypLprim(prim, List.map build_varterm params)) None)) None

let transl_primitive_T p typ env =
  let prim =
    try
      let (gencomp, _, _, _, _, _, _) =
        Hashtbl.find comparisons_table p.prim_name in
      gencomp
    with Not_found ->
    try
      Hashtbl.find primitives_table p.prim_name
    with Not_found ->
      Pccall p in
  let rec make_params n ty accu =
    let typ = Ctype.full_expand env ty in
      if n<=0 then (List.rev accu,build_type_annotation ty env) 
      else match typ.desc with
	| Tarrow (_,t1,t2,_) ->
	    make_params (n-1) t2 ((Ident.create "prim",build_type_annotation t1 env)::accu)
	| _ -> failwith "Transclore.transl_primitive" in
  let params,rettyp = make_params p.prim_arity typ [] in
    build_term (TypLfunction(Curried, params,build_term (TypLprim(prim, List.map build_varterm params)) rettyp)) 
      (build_type_annotation typ env)


(* To check the well-formedness of r.h.s. of "let rec" definitions *)

let check_recursive_lambda idlist lam =
  let rec check_top idlist = function
      Lfunction(kind, params, body) as funct -> true
    | Lprim(Pmakeblock(tag, mut), args) ->
        List.for_all (check idlist) args
    | Lprim(Pmakearray(Paddrarray|Pintarray), args) ->
        List.for_all (check idlist) args
    | Llet(str, id, arg, body) ->
        check idlist arg && check_top (add_let id arg idlist) body
    | Lletrec(bindings, body) ->
        let idlist' = add_letrec bindings idlist in
        List.for_all (fun (id, arg) -> check idlist' arg) bindings &&
        check_top idlist' body
    | Levent (lam, _) -> check_top idlist lam
    | _ -> false

  and check idlist = function
      Lvar _ -> true
    | Lconst cst -> true
    | Lfunction(kind, params, body) -> true
    | Llet(str, id, arg, body) ->
        check idlist arg && check (add_let id arg idlist) body
    | Lletrec(bindings, body) ->
        let idlist' = add_letrec bindings idlist in
        List.for_all (fun (id, arg) -> check idlist' arg) bindings &&
        check idlist' body
    | Lprim(Pmakeblock(tag, mut), args) ->
        List.for_all (check idlist) args
    | Lprim(Pmakearray(Paddrarray|Pintarray), args) ->
        List.for_all (check idlist) args
    | Levent (lam, _) -> check idlist lam
    | lam ->
        let fv = Lambda.free_variables lam in
        List.for_all (fun id -> not(Lambda.IdentSet.mem id fv)) idlist

  and add_let id arg idlist =
    match arg with
      Lvar id' -> if List.mem id' idlist then id :: idlist else idlist
    | Llet(_, _, _, body) -> add_let id body idlist
    | Lletrec(_, body) -> add_let id body idlist
    | _ -> idlist

  and add_letrec bindings idlist =
    List.fold_right (fun (id, arg) idl -> add_let id arg idl)
                    bindings idlist
  in check_top idlist lam

let check_recursive_typedlambda idlist tlam =
  check_recursive_lambda (List.map fst idlist) (Typedlambda.to_lambda tlam)

(* To propagate structured constants *)

exception Not_constant

let extract_constant = function
    {tlterm=TypLconst sc} -> sc 
  | _ -> raise Not_constant

let extract_float = function
    Const_base(Const_float f) -> f
  | _ -> fatal_error "Translcore.extract_float"

(* To find reasonable names for let-bound and lambda-bound idents *)

let rec name_pattern default = function
    [] -> Ident.create default
  | (p, e) :: rem ->
      match p.pat_desc with
        Tpat_var id -> id
      | Tpat_alias(p, id) -> id
      | _ -> name_pattern default rem

(* Push the default values under the functional abstractions *)

let rec push_defaults loc bindings pat_expr_list partial =
  match pat_expr_list with
    [pat, ({exp_desc = Texp_function(pl,partial)} as exp)] ->
      let pl = push_defaults exp.exp_loc bindings pl partial in
      [pat, {exp with exp_desc = Texp_function(pl, partial)}]
  | [pat, ({exp_desc = Texp_let
             (Default, cases, ({exp_desc = Texp_function _} as e2))} as e1)] ->
      push_defaults loc (cases :: bindings) [pat, e2] partial
  | [pat, exp] ->
      let exp =
        List.fold_left
          (fun exp cases ->
            {exp with exp_desc = Texp_let(Nonrecursive, cases, exp)})
          exp bindings
      in
      [pat, exp]
  | (pat, exp) :: _ when bindings <> [] ->
      let param = name_pattern "param" pat_expr_list in
      let exp =
        { exp with exp_loc = loc; exp_desc =
          Texp_match
            ({exp with exp_type = pat.pat_type; exp_desc =
              Texp_ident (Path.Pident param,
                          {val_type = pat.pat_type; val_kind = Val_reg})},
             pat_expr_list, partial) }
      in
      push_defaults loc bindings
        [{pat with pat_desc = Tpat_var param}, exp] Total
  | _ ->
      pat_expr_list

(* Insertion of debugging events *)

let event_before exp lam = match lam with
| {tlterm=TypLstaticraise (_,_)} -> lam
| _ ->
  if !Clflags.debug
  then build_term (TypLevent(lam, {lev_loc = exp.exp_loc.Location.loc_start;
                    lev_kind = Lev_before;
                    lev_repr = None;
                    lev_env = Env.summary exp.exp_env})) None
  else lam

let event_after exp lam =
  if !Clflags.debug
  then build_term (TypLevent(lam, {lev_loc = exp.exp_loc.Location.loc_end;
                    lev_kind = Lev_after exp.exp_type;
                    lev_repr = None;
                    lev_env = Env.summary exp.exp_env})) None
  else lam

let event_function exp lam =
  if !Clflags.debug then
    let repr = Some (ref 0) in
    let (info, body) = lam repr in
    (info,
     build_term (TypLevent(body, {lev_loc = exp.exp_loc.Location.loc_start;
                   lev_kind = Lev_function;
                   lev_repr = repr;
                   lev_env = Env.summary exp.exp_env})) None)
  else
    lam None

let assert_failed loc =
  build_term (TypLprim(Praise, [build_term (TypLprim(Pmakeblock(0, Immutable),
          [transl_path Predef.path_assert_failure;
						      build_term (TypLconst(TConst_block(0,
							 [(TConst_base(Const_string !Location.input_name),coretype_annotation Predef.type_string);
							  (TConst_base(Const_int loc.Location.loc_start),coretype_annotation Predef.type_int);
							  (TConst_base(Const_int loc.Location.loc_end),coretype_annotation Predef.type_int)]))) 
							(coretype_annotation (Btype.newgenty (Ttuple[Predef.type_string; Predef.type_int; Predef.type_int])))])) 
				  (coretype_annotation Predef.type_exn)])
	     ) None
;;

(* Translation of expressions *)



let rec transl_exp e =
  let exptype = build_type_annotation e.exp_type e.exp_env in
  match e.exp_desc with
    Texp_ident(path, {val_kind = Val_prim p}) ->
      transl_primitive_T p e.exp_type e.exp_env
  | Texp_ident(path, {val_kind = Val_anc _}) ->
      raise(Error(e.exp_loc, Free_super_var))
  | Texp_ident(path, {val_kind = Val_reg | Val_self _}) ->
      transl_path_with_type exptype path
  | Texp_ident _ -> fatal_error "Translcore.transl_exp: bad Texp_ident"
  | Texp_constant cst ->
      build_term (TypLconst(TConst_base cst)) exptype
  | Texp_let(rec_flag, pat_expr_list, body) ->
      transl_let rec_flag pat_expr_list (event_before body (transl_exp body))
  | Texp_function (pat_expr_list, partial) ->
      let ((kind, params), body) =
        event_function e
          (function repr ->
            let pl = push_defaults e.exp_loc [] pat_expr_list partial in
            transl_function e.exp_loc !Clflags.native_code repr partial pl)
      in
      build_term (TypLfunction(kind, params, body)) exptype
  | Texp_apply({exp_desc = Texp_ident(path, {val_kind = Val_prim p})}, args)
    when List.length args = p.prim_arity
    && List.for_all (fun (arg,_) -> arg <> None) args ->
      let args = List.map (function Some x, _ -> x | _ -> assert false) args in
      let prim = transl_prim p args in
      begin match (prim, args) with
        (Praise, [arg1]) ->
          build_term (TypLprim(Praise, [event_after arg1 (transl_exp arg1)])) exptype
      | (Pccall _, _) ->
          event_after e (build_term (TypLprim(prim, transl_list args)) exptype)
      | (_, _) ->
          build_term (TypLprim(prim, transl_list args)) exptype
      end
  | Texp_apply(funct, oargs) ->
      event_after e (transl_apply (transl_exp funct) oargs exptype)
  | Texp_match({exp_desc = Texp_tuple argl} as arg, pat_expr_list, partial) ->
      Matching.for_multiple_match e.exp_loc
        (transl_list argl) (transl_cases pat_expr_list) partial
  | Texp_match(arg, pat_expr_list, partial) ->
      Matching.for_function e.exp_loc None
        (transl_exp arg) (transl_cases pat_expr_list) partial
  | Texp_try(body, pat_expr_list) ->
      let exn_type = coretype_annotation Predef.type_exn in
      let id = name_pattern "exn" pat_expr_list,exn_type in
      build_term (TypLtrywith(transl_exp body, id,
               Matching.for_trywith (build_varterm id) (transl_cases pat_expr_list))) exptype
  | Texp_tuple el ->
      let ll = transl_list el in
      begin (*try
        build_term (TypLconst(Const_block(0, List.map extract_constant ll))) exptype (* !! a réhabiliter *)
      with Not_constant -> *)
        build_term (TypLprim(Pmakeblock(0, Immutable), ll)) exptype
      end
  | Texp_construct(cstr, args) ->
      let ll = transl_list args in
      begin match cstr.cstr_tag with
        Cstr_constant n ->
          build_term (TypLconst(TConst_pointer n)) exptype
      | Cstr_block n ->
          begin (*try
            build_term (TypLconst(Const_block(n, List.map extract_constant ll))) exptype (* !! a réhabiliter *)
          with Not_constant -> *)
            build_term (TypLprim(Pmakeblock(n, Immutable), ll)) exptype
          end
      | Cstr_exception path ->
          build_term (TypLprim(Pmakeblock(0, Immutable), transl_path path :: ll)) exptype
      end
  | Texp_variant(l, arg) ->
      let tag = Btype.hash_variant l in
      let int_type = coretype_annotation Predef.type_int in
      begin match arg with
          None -> build_term (TypLprim(Pmakeblock(0, Immutable),
				       [build_term (TypLconst(TConst_base(Const_int tag))) int_type])) exptype
	    (* build_term (TypLconst(TConst_pointer tag)) exptype *) (* cst poly variant must be implemented as blocks too *)
      | Some arg ->
          let lam = transl_exp arg in
	      (*          try
			  build_term (TypLconst(Const_block(0, [Const_base(Const_int tag); (* !! a réhabiliter *)
                          extract_constant lam]))) exptype
			  with Not_constant -> *)
              build_term (TypLprim(Pmakeblock(0, Immutable),
				   [build_term (TypLconst(TConst_base(Const_int tag))) int_type ; lam])) exptype
      end
  | Texp_record ((lbl1, _) :: _ as lbl_expr_list, opt_init_expr) ->
      transl_record lbl1.lbl_all lbl1.lbl_repres lbl_expr_list opt_init_expr exptype
  | Texp_record ([], _) -> fatal_error "Translcore.transl_exp: bad Texp_record"
  | Texp_field(arg, lbl) ->
      let access =
        match lbl.lbl_repres with
          Record_regular -> Pfield lbl.lbl_pos
        | Record_float -> Pfloatfield lbl.lbl_pos in
      build_term (TypLprim(access, [transl_exp arg])) exptype
  | Texp_setfield(arg, lbl, newval) ->
      let access =
        match lbl.lbl_repres with
          Record_regular -> Psetfield(lbl.lbl_pos, maybe_pointer newval)
        | Record_float -> Psetfloatfield lbl.lbl_pos in
      build_term (TypLprim(access, [transl_exp arg; transl_exp newval])) exptype
  | Texp_array expr_list ->
      let kind = array_kind e in
      let len = List.length expr_list in
      if len <= Config.max_young_wosize then
        build_term (TypLprim(Pmakearray kind, transl_list expr_list)) exptype
      else begin
        let v = Ident.create "makearray",exptype in
	let int_type = coretype_annotation Predef.type_int in
        let rec fill_fields pos = function
          [] ->
            build_varterm v
        | arg :: rem ->
	    let follow = fill_fields (pos+1) rem in
            build_term (TypLsequence(build_term (TypLprim(Parraysetu kind,
							  [build_varterm v;
							   build_term (TypLconst(TConst_base(Const_int pos))) int_type;
							   transl_exp arg])) None,
						 follow)) follow.tltype in
          build_term (TypLlet(Strict, v,
			      build_term (TypLprim(Pccall prim_makearray,
						   [build_term (TypLconst(TConst_base(Const_int len))) int_type;
						    transl_exp (List.hd expr_list)])) exptype,
					  fill_fields 1 (List.tl expr_list))) exptype
      end
  | Texp_ifthenelse(cond, ifso, Some ifnot) ->
      build_term (TypLifthenelse(transl_exp cond,
                  event_before ifso (transl_exp ifso),
                  event_before ifnot (transl_exp ifnot))) exptype
  | Texp_ifthenelse(cond, ifso, None) ->
      build_term (TypLifthenelse(transl_exp cond,
                  event_before ifso (transl_exp ifso),
                  lambda_unit)) exptype
  | Texp_sequence(expr1, expr2) ->
      build_term (TypLsequence(transl_exp expr1, event_before expr2 (transl_exp expr2))) exptype
  | Texp_while(cond, body) ->
      build_term (TypLwhile(transl_exp cond, event_before body (transl_exp body))) exptype
  | Texp_for(param, low, high, dir, body) ->
      build_term (TypLfor( (param,(coretype_annotation Predef.type_int)) , transl_exp low, transl_exp high, dir,
           event_before body (transl_exp body))) exptype
  | Texp_when(cond, body) ->
      event_before cond
        (build_term (TypLifthenelse(transl_exp cond, event_before body (transl_exp body),
                     staticfail)) exptype)
  | Texp_send(expr, met) ->
      let met_id =
        match met with
          Tmeth_name nm -> Translobj.meth nm
        | Tmeth_val id  -> id
      in
      event_after e (build_term (TypLsend(build_varterm (met_id,(coretype_annotation Predef.type_int)), transl_exp expr, [])) generic_moditem_type (*exptype*))
  | Texp_new (cl, _) ->
      build_term (TypLapply(build_term (TypLprim(Pfield 0, [transl_path cl])) None, [build_term (TypLconst (TConst_block(0,[]))) (coretype_annotation (Predef.type_list Predef.type_unit)) (* lambda_unit = NO, we need a CTS reference type here*) ])) exptype
  | Texp_instvar(path_self, path) ->
      build_term (TypLprim(Parrayrefu Paddrarray, [transl_path path_self; transl_path_with_type (coretype_annotation Predef.type_int) path])) exptype
  | Texp_setinstvar(path_self, path, expr) ->
      transl_setinstvar (transl_path path_self) path expr

  | Texp_override(path_self, modifs) ->
      let cpy = Ident.create "copy" in
      build_term (TypLlet(Strict, (cpy,generic_module_type) (* 'a array *) ,
	build_term (TypLapply(Translobj.oo_prim "copy", [transl_path path_self])) generic_module_type,
           List.fold_right
             (fun (path, expr) rem ->
                build_term (TypLsequence(transl_setinstvar (build_term (TypLvar cpy) generic_module_type) path expr, rem)) (coretype_annotation Predef.type_unit))
             modifs
             (build_term (TypLvar cpy) generic_module_type))) (coretype_annotation Predef.type_unit)
  | Texp_letmodule(id, modl, body) ->
      let locmod = !transl_module Tcoerce_none None modl in
	build_term (TypLlet(Strict, (id,locmod.tltype),locmod , transl_exp body)) exptype
  | Texp_assert (cond) ->
      if !Clflags.noassert
      then lambda_unit
      else build_term (TypLifthenelse (transl_exp cond, lambda_unit, assert_failed e.exp_loc)) exptype
  | Texp_assertfalse -> assert_failed e.exp_loc
  | Texp_lazy e ->
      let unittype = coretype_annotation Predef.type_unit in
      let e' = transl_exp e in
      let funtype = match e'.tltype with 
	  None -> None
	| Some {taexpr=typ;taenv=env} -> build_type_annotation (Btype.newgenty (Types.Tarrow("",Predef.type_unit,typ,Types.Cunknown))) env in
      let fn = build_term (TypLfunction (Curried, [(Ident.create "param",unittype)], e')) funtype in
	build_term (TypLprim(Pmakeblock(Obj.lazy_tag, Immutable), [fn])) exptype

and transl_list expr_list =
  List.map transl_exp expr_list

and transl_cases pat_expr_list =
  List.map
    (fun (pat, expr) -> (pat, event_before expr (transl_exp expr)))
    pat_expr_list

and transl_tupled_cases patl_expr_list =
  List.map (fun (patl, expr) -> (patl, transl_exp expr)) patl_expr_list

and transl_apply lam sargs etype =
  let lapply funct args typ =
    match funct.tlterm with
      TypLsend(lmet, lobj, largs) ->
        build_term (TypLsend(lmet, lobj, largs @ args)) typ
   | TypLevent({tlterm=TypLsend(lmet, lobj, largs)}, _) ->
        build_term (TypLsend(lmet, lobj, largs @ args)) typ
    | TypLapply(lexp, largs) ->
        build_term (TypLapply(lexp, largs @ args)) typ
    | lexp ->
        build_term (TypLapply(funct, args)) typ
  in
  let rec build_apply lam args typ = function (* !! might be a bug in the optional cas !! PB *)
      (None, optional) :: l ->
        let defs = ref [] in
        let protect name lam =
          match lam.tlterm with
            TypLvar _ | TypLconst _ -> lam
          | _ ->
              let id = (Ident.create name,None) in
              defs := (id, lam) :: !defs;
              build_varterm id
        in
        let args, args' =
          if List.for_all (fun (_,opt) -> opt = Optional) args then [], args
          else args, [] in
        let lam =
          if args = [] then lam else lapply lam (List.rev_map fst args) None (* !! PB *)  in
        let handle = protect "func" lam
        and l = List.map (fun (arg, opt) -> may_map (protect "arg") arg, opt) l
        and id_arg = Ident.create "param",None in (* !! PB *)
        let body =
          match build_apply handle ((build_varterm id_arg, optional)::args') None l (* !! PB *) with
            {tlterm=TypLfunction(Curried, ids, lam)} ->
              build_term (TypLfunction(Curried, id_arg::ids, lam)) typ
          | {tlterm=TypLevent({tlterm=TypLfunction(Curried, ids, lam)}, _)} ->
              build_term (TypLfunction(Curried, id_arg::ids, lam)) typ
          | lam ->
              build_term (TypLfunction(Curried, [id_arg], lam)) typ
        in
        List.fold_left
          (fun body (id, lam) -> build_term (TypLlet(Strict, id, lam, body)) body.tltype)
          body !defs
    | (Some arg, optional) :: l ->
        build_apply lam ((arg, optional) :: args) typ l
    | [] ->
        lapply lam (List.rev_map fst args) typ
  in
  build_apply lam [] etype (List.map (fun (x,o) -> may_map transl_exp x, o) sargs)

and transl_function loc untuplify_fn repr partial pat_expr_list =
  match pat_expr_list with
    [pat, ({exp_desc = Texp_function(pl,partial')} as exp)] ->
      let param = (name_pattern "param" pat_expr_list),(build_type_annotation pat.pat_type pat.pat_env) in 
      let ((_, params), body) =
        transl_function exp.exp_loc false repr partial' pl in
      ((Curried, param :: params),
       Matching.for_function loc None (build_varterm param) [pat, body] partial)
(* !! KESAKO ? *)
(*
  | [({pat_desc = Tpat_var id} as pat),
     ({exp_desc = Texp_let(Nonrecursive, cases,
                          ({exp_desc = Texp_function _} as e2))} as e1)]
    when Ident.name id = "*opt*" ->
      transl_function loc untuplify_fn repr (cases::bindings) partial [pat, e2]
  | [pat, exp] when bindings <> [] ->
      let exp =
        List.fold_left
          (fun exp cases ->
            {exp with exp_desc = Texp_let(Nonrecursive, cases, exp)})
          exp bindings
      in
      transl_function loc untuplify_fn repr [] partial [pat, exp]
  | (pat, exp)::_ when bindings <> [] ->
      let param = name_pattern "param" pat_expr_list in
      let exp =
        { exp with exp_loc = loc; exp_desc =
          Texp_match
            ({exp with exp_type = pat.pat_type; exp_desc =
              Texp_ident (Path.Pident param,
                          {val_type = pat.pat_type; val_kind = Val_reg})},
             pat_expr_list, partial) }
      in
      transl_function loc untuplify_fn repr bindings Total
        [{pat with pat_desc = Tpat_var param}, exp]
*)
  | ({pat_desc = Tpat_tuple pl} as pat, _) :: _ when untuplify_fn ->
      begin try
        let size = List.length pl in
        let pats_expr_list =
          List.map
            (fun (pat, expr) -> (Matching.flatten_pattern size pat, expr))
            pat_expr_list in
        let params = List.map (fun p -> Ident.create "param",build_type_annotation p.pat_type p.pat_env) pl in
        ((Tupled, params),
         Matching.for_tupled_function loc params
           (transl_tupled_cases pats_expr_list) partial)
      with Matching.Cannot_flatten ->
        let param = name_pattern "param" pat_expr_list,(build_type_annotation pat.pat_type pat.pat_env) in
        ((Curried, [param]),
         Matching.for_function loc repr (build_varterm param)
           (transl_cases pat_expr_list) partial)
      end
  | _ ->
      let pat = try fst (List.hd pat_expr_list) with _ -> failwith "translcore LIST PATTERN VIDE !!" in
      let param = name_pattern "param" pat_expr_list,(build_type_annotation pat.pat_type pat.pat_env) in
      ((Curried, [param]),
       Matching.for_function loc repr (build_varterm param)
         (transl_cases pat_expr_list) partial)

and transl_let rec_flag pat_expr_list body =
  match rec_flag with
    Nonrecursive | Default ->
      let rec transl = function
        [] ->
          body
      | (pat, expr) :: rem ->
          Matching.for_let pat.pat_loc (transl_exp expr) pat (transl rem)
      in transl pat_expr_list
  | Recursive ->
      let idlist =
        List.map
          (fun (pat, expr) ->
            match pat.pat_desc with
              Tpat_var id -> id,(build_type_annotation pat.pat_type pat.pat_env)
            | _ -> raise(Error(pat.pat_loc, Illegal_letrec_pat)))
        pat_expr_list in
      let transl_case (pat, expr) id =
        let lam = transl_exp expr in
        if not (check_recursive_typedlambda idlist lam) then
          raise(Error(expr.exp_loc, Illegal_letrec_expr));
        (id, lam) in
      build_term (TypLletrec(List.map2 transl_case pat_expr_list idlist, body)) body.tltype

and transl_setinstvar self var expr =
  build_term (TypLprim(Parraysetu (if maybe_pointer expr then Paddrarray else Pintarray),
                    [self; transl_path_with_type (coretype_annotation Predef.type_int) var; transl_exp expr])) (coretype_annotation Predef.type_unit)

and transl_record all_labels repres lbl_expr_list opt_init_expr exptype =
  (* Determine if there are "enough" new fields *)
  if 3 + 2 * List.length lbl_expr_list >= Array.length all_labels
  then begin
    (* Allocate new record with given fields (and remaining fields
       taken from init_expr if any) *)
    let lv = Array.create (Array.length all_labels) staticfail in
    let init_id = Ident.create "init",exptype in
    begin match opt_init_expr with
      None -> ()
    | Some init_expr ->
        for i = 0 to Array.length all_labels - 1 do
          let access =
            match all_labels.(i).lbl_repres with
              Record_regular -> Pfield i
            | Record_float -> Pfloatfield i in
	  let typ = match exptype with None -> None 
	    | Some {taenv=env} -> build_type_annotation all_labels.(i).lbl_arg env in (*!! doit peut etre etre instancié pour être précis .. *)
          lv.(i) <- build_term (TypLprim(access, [build_varterm init_id])) typ
        done
    end;
    List.iter
      (fun (lbl, expr) -> lv.(lbl.lbl_pos) <- transl_exp expr)
      lbl_expr_list;
    let ll = Array.to_list lv in
    let mut =
      if List.exists (fun (lbl, expr) -> lbl.lbl_mut = Mutable) lbl_expr_list
      then Mutable
      else Immutable in
    let lam =
(*      try
        if mut = Mutable then raise Not_constant;
        let cl = List.map extract_constant ll in
        match repres with
          Record_regular -> build_term (TypLconst(Const_block(0, cl))) exptype (* !! a réhabiliter *)
        | Record_float ->
            build_term (TypLconst(Const_float_array(List.map extract_float cl))) exptype
      with Not_constant -> *)
        match repres with
          Record_regular -> build_term (TypLprim(Pmakeblock(0, mut), ll)) exptype
        | Record_float -> build_term (TypLprim(Pmakearray Pfloatarray, ll)) exptype in
    begin match opt_init_expr with
      None -> lam
    | Some init_expr -> build_term (TypLlet(Strict, init_id, transl_exp init_expr, lam)) exptype
    end
  end else begin
    (* Take a shallow copy of the init record, then mutate the fields
       of the copy *)
    let copy_id = Ident.create "newrecord",exptype in
    let rec update_field (lbl, expr) cont =
      let upd =
        match lbl.lbl_repres with
          Record_regular -> Psetfield(lbl.lbl_pos, maybe_pointer expr)
        | Record_float -> Psetfloatfield lbl.lbl_pos in
      build_term (TypLsequence(build_term (TypLprim(upd, [build_varterm copy_id; transl_exp expr])) None (*!! void en fait *), cont)) cont.tltype in
    begin match opt_init_expr with
      None -> assert false
    | Some init_expr ->
        build_term (TypLlet(Strict, copy_id,
             build_term (TypLprim(Pccall prim_obj_dup, [transl_exp init_expr])) exptype,
             List.fold_right update_field lbl_expr_list (build_varterm copy_id))) exptype
    end
  end

(* Compile an exception definition *)

let transl_exception id path decl =
  let exn_type = coretype_annotation Predef.type_exn
  and str_type = coretype_annotation Predef.type_string in
  let name =
    match path with
      None -> Ident.name id
    | Some p -> Path.name p in
  build_term (TypLprim(Pmakeblock(0, Immutable), [build_term (TypLconst(TConst_base(Const_string name))) str_type])) exn_type

(* Error report *)

open Format

let report_error ppf = function
  | Illegal_letrec_pat ->
      fprintf ppf
        "Only variables are allowed as left-hand side of `let rec'"
  | Illegal_letrec_expr ->
      fprintf ppf
        "This kind of expression is not allowed as right-hand side of `let rec'"
  | Free_super_var ->
      fprintf ppf
        "Ancestor names can only be used to select inherited methods"



