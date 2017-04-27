(************************************************************************)
(*                                                                      *)
(*                                CamIL                                 *)
(*                                                                      *)
(* created by  Bruno Pagano, projet Cristal, INRIA Rocquencourt (2000)  *)
(* modified by Emmanuel Chailloux & Raphael Montelatici, PPS (2003-2006)*)
(*                                                                      *)
(************************************************************************)

(* $Id: typedlambda.ml,v 1.18 2007/03/25 09:45:59 montela Exp $ *)

open Misc
open Path
open Asttypes
open Lambda
open Types

(* structured_constants and *)
(* primitives are those of Lambda *)

type typing_annotation = {
  tapath:string list;
  taexpr:type_expr;
  taenv:Env.t
}

let current_path = ref []

(* debug 
let print_path () = 
  Printf.printf "Immersion %s\n"
  (List.fold_left (fun s md -> if s="" then md else md^"."^s) "" !current_path)
*)

let lengthen_typepath mname =
  current_path := mname::(!current_path)

let shorten_typepath () =
  current_path := match !current_path with
      h::q -> q
    | [] -> failwith "Typedlambda:shorten_typepath"


let build_type_annotation expr env =
  Some {tapath = !current_path; taexpr = expr; taenv = env}

let coretype_annotation exp =
  Some {tapath=[];taexpr=exp;taenv=Env.initial}

let globaltype = Predef.type_array (Btype.newgenty Tvar)
let generic_module_type = coretype_annotation globaltype (* 'a array *)
let generic_functor_type x y = 
  match x,y with
      Some t1,Some t2 -> coretype_annotation (Btype.newgenty (Tarrow("",t1.taexpr,t2.taexpr,Cunknown))) (* x -> y *)
    | _,_ -> failwith "Typedlambda.generic_functor_type"
let generic_moditem_type = coretype_annotation (Btype.newgenty Tvar) (* 'a *)


let arrow_apply_camltypes tann nb =
  match tann with 
      None -> None
    | Some {taexpr=ti;taenv=env;tapath=tpath} -> 
	let rec arrow_apply_aux ti n =
	  let t = Ctype.repr ti in
	  if n = 0 then t else
	    begin match t.desc with
		Tarrow(_,t1,t2,_) -> arrow_apply_aux t2 (n-1)
	      | Tsubst _ -> failwith "Ctypedlambda.arrow_apply_camltypes Tsubst" 
	      | Tpoly(t1,_) -> arrow_apply_aux t1 n
	      | _ -> 
		begin
		  !Oprint.out_type Format.str_formatter (Printtyp.tree_of_type_scheme ti);
		  failwith (Printf.sprintf "Ctypedlambda.arrow_apply_camltypes (%s,%d)" (Format.flush_str_formatter()) nb)
		end
	    end in
	  (* nb < 0 in case of uncurrified function : all parameters are grouped in a single tuple type *)
	let ti = if nb > 0 then arrow_apply_aux ti nb else arrow_apply_aux ti 1 in
	  Some {taexpr=ti;taenv=env;tapath=tpath}

type typed_structured_constant =
  | TConst_base of constant (* Asttypes.constant already bear types *)
  | TConst_pointer of int
  | TConst_block of int * (typed_structured_constant * (typing_annotation option)) list
  | TConst_float_array of string list

type typedident = Ident.t * (typing_annotation option)

type typedlambda = 
    {tlterm:typedlambdaterm;tltype:typing_annotation option}
and typedlambdaterm =
    TypLvar of Ident.t
  | TypLconst of typed_structured_constant
  | TypLapply of typedlambda * typedlambda list
  | TypLfunction of function_kind * typedident list * typedlambda
  | TypLlet of let_kind * typedident * typedlambda * typedlambda
  | TypLletrec of (typedident * typedlambda) list * typedlambda
  | TypLprim of primitive * typedlambda list
  | TypLswitch of typedlambda * typedlambda_switch
  | TypLstaticraise of int * typedlambda list
  | TypLstaticcatch of typedlambda * (int * typedident list) * typedlambda
  | TypLtrywith of typedlambda * typedident * typedlambda
  | TypLifthenelse of typedlambda * typedlambda * typedlambda
  | TypLsequence of typedlambda * typedlambda
  | TypLwhile of typedlambda * typedlambda
  | TypLfor of typedident * typedlambda * typedlambda * direction_flag * typedlambda
  | TypLassign of typedident * typedlambda
  | TypLsend of typedlambda * typedlambda * typedlambda list
  | TypLevent of typedlambda * lambda_event
  | TypLifused of typedident * typedlambda

and typedlambda_switch =
  { tsw_numconsts: int;
    tsw_consts: (int * typedlambda) list;
    tsw_numblocks: int;
    tsw_blocks: (int * typedlambda) list;
    tsw_failaction : typedlambda option}



let build_term term typ = {tlterm=term;tltype=typ}

let build_varterm tid = {tlterm=TypLvar (fst tid);tltype=snd tid}

let build_letterm (kind,tid,tl1,tl2) =
  (* TODO plus tard : virer le tid au profit de id (redondant ...) *)
  build_term (TypLlet(kind,tid,tl1,tl2)) tl2.tltype

let build_staticcatchterm (tl,hdlarg,hdl) =
   let rt = match tl.tltype,hdl.tltype with
      ((Some _ as ta),_) -> ta
    | (_,(Some _ as ta)) -> ta
    | (None,None) -> None 
   in
     build_term (TypLstaticcatch(tl,hdlarg,hdl)) rt

let build_ifthenelseterm (tl,tl1,tl2) =
  let rt = match tl1.tltype,tl2.tltype with
      ((Some _ as ta),_) -> ta
    | (_,(Some _ as ta)) -> ta
    | (None,None) -> None 
  in
  let tl' = build_term tl.tlterm (coretype_annotation Predef.type_bool)
  and tl1' = build_term tl1.tlterm rt 
  and tl2' = build_term tl2.tlterm rt 
  in build_term (TypLifthenelse(tl',tl1',tl2')) rt

let build_switchterm (tl,sw) =
  let rec extract_type = function
      [] -> None
    | {tltype=(Some ta) as type_ann}::rem -> type_ann
    | {tltype=None}::rem -> extract_type rem 
  in
  let rt = extract_type ((List.map snd (sw.tsw_consts @ sw.tsw_blocks)) @ (match sw.tsw_failaction with None -> [] | Some t -> [t]) ) in
  let insert_return_type (i,tl) = (i,build_term tl.tlterm rt) in
  let sw2 = {sw with tsw_consts = List.map insert_return_type sw.tsw_consts;
	       tsw_blocks = List.map insert_return_type sw.tsw_blocks;
	       tsw_failaction = match sw.tsw_failaction with None -> None
		 | Some tl -> Some (build_term tl.tlterm rt)}
 in
    build_term (TypLswitch(tl,sw2)) rt


let lambda_unit = build_term (TypLconst (TConst_pointer 0)) (coretype_annotation Predef.type_unit)
(* has to be consistent with the definition of Lambda.const_unit *)


let name_lambda arg fn =
  match arg with
    {tlterm=TypLvar id} -> fn (id,arg.tltype)
  | _ -> let id = Ident.create "let" in 
    let tid = id,arg.tltype in
      build_term (TypLlet(Strict, tid, arg, fn tid)) arg.tltype

let name_lambda_list args fn =
  let rec name_list names = function
    [] -> fn (List.rev names)
  | {tlterm=TypLvar id} as arg :: rem ->
      name_list (arg :: names) rem
  | arg :: rem ->
      let id = Ident.create "let" in
      let tid = (id,arg.tltype) in
      let term0 = name_list ((build_term (TypLvar id) arg.tltype ) :: names) rem in
      build_term (TypLlet(Strict, tid, arg, term0)) term0.tltype  in
  name_list [] args

module IdentSet =
  Set.Make(struct
    type t = typedident
    let compare ti1 ti2 = compare (fst ti1) (fst ti2)
  end)

let free_variables l =
  let fv = ref IdentSet.empty in
  let rec freevars = function
    {tlterm=TypLvar id} as arg ->
      fv := IdentSet.add (id,arg.tltype) !fv
  | {tlterm=TypLconst sc} -> ()
  | {tlterm=TypLapply(fn, args)} ->
      freevars fn; List.iter freevars args
  | {tlterm=TypLfunction(kind, params, body)} ->
      freevars body;
      List.iter (fun param -> fv := IdentSet.remove param !fv) params
  | {tlterm=TypLlet(str, id, arg, body)} ->
      freevars arg; freevars body; fv := IdentSet.remove id !fv
  | {tlterm=TypLletrec(decl, body)} ->
      freevars body;
      List.iter (fun (id, exp) -> freevars exp) decl;
      List.iter (fun (id, exp) -> fv := IdentSet.remove id !fv) decl
  | {tlterm=TypLprim(p, args)} ->
      List.iter freevars args
  | {tlterm=TypLswitch(arg, sw)} ->
      freevars arg; 
      List.iter (fun (key, case) -> freevars case) sw.tsw_consts;
      List.iter (fun (key, case) -> freevars case) sw.tsw_blocks;
      begin match sw.tsw_failaction with
      | None -> ()
      | Some l -> freevars l
      end
  | {tlterm=TypLstaticraise (_,args)} ->
      List.iter freevars args
  | {tlterm=TypLstaticcatch(e1, (_,vars), e2)} ->
      freevars e1; freevars e2 ;
      List.iter (fun id -> fv := IdentSet.remove id !fv) vars        
  | {tlterm=TypLtrywith(e1, exn, e2)} ->
      freevars e1; freevars e2; fv := IdentSet.remove exn !fv
  | {tlterm=TypLifthenelse(e1, e2, e3)} ->
      freevars e1; freevars e2; freevars e3
  | {tlterm=TypLsequence(e1, e2)} ->
      freevars e1; freevars e2
  | {tlterm=TypLwhile(e1, e2)} ->
      freevars e1; freevars e2
  | {tlterm=TypLfor(v, e1, e2, dir, e3)} -> 
      freevars e1; freevars e2; freevars e3; fv := IdentSet.remove v !fv
  | {tlterm=TypLassign(id, e)} ->
      fv := IdentSet.add id !fv; freevars e
  | {tlterm=TypLsend (met, obj, args)} ->
      List.iter freevars (met::obj::args)
  | {tlterm=TypLevent (lam, evt)} ->
      freevars lam
  | {tlterm=TypLifused (v, e)} ->
      freevars e
  in freevars l; !fv

(* Check if an action has a "when" guard *)
let raise_count = ref 0

let next_raise_count () =
  incr raise_count ;
  !raise_count

(* Anticipated staticraise, for guards *)
let staticfail = build_term (TypLstaticraise (0,[])) None (* !! *)

let rec is_guarded tl = match tl.tlterm with
  | TypLifthenelse( cond, body, {tlterm=TypLstaticraise (0,[])}) -> true
  | TypLlet(str, id, lam, body) -> is_guarded body
  | TypLevent(lam, ev) -> is_guarded lam
  | _ -> false

let rec patch_guarded patch = function
  | {tlterm=TypLifthenelse (cond, body, {tlterm=TypLstaticraise (0,[])})} as arg ->
      build_term (TypLifthenelse (cond, body, patch))  arg.tltype
  | {tlterm=TypLlet(str, id, lam, body)} as arg ->
      build_term (TypLlet (str, id, lam, patch_guarded patch body))  arg.tltype
  | {tlterm=TypLevent(lam, ev)} as arg ->
      build_term (TypLevent (patch_guarded patch lam, ev)) arg.tltype
  | _ -> fatal_error "TypLambda.patch_guarded"

(* Translate an access path *)

let rec transl_path_with_type typ = function
    Pident id ->
      if Ident.global id then build_term (TypLprim(Pgetglobal id, [])) typ else build_term (TypLvar id) typ
  | Pdot(p, s, pos) ->
      build_term (TypLprim(Pfield pos, [transl_path p])) typ
  | Papply(p1, p2) ->
      fatal_error "Typedlambda.transl_path_with_type"
and transl_path p = transl_path_with_type generic_module_type p

(* Compile a sequence of expressions *)

let rec make_sequence fn = function
    [] -> lambda_unit
  | [x] -> fn x
  | x::rem ->
      let lam = fn x and term0 = make_sequence fn rem 
      in build_term (TypLsequence(lam, term0)) term0.tltype 

(* Apply a substitution to a lambda-term.
   Assumes that the bound variables of the lambda-term do not
   belong to the domain of the substitution.
   Assumes that the image of the substitution is out of reach
   of the bound variables of the lambda-term (no capture). *)

let subst_lambda s lam =
  let rec subst tl = 
    let slam = (
      match tl.tlterm with
	  TypLvar id as t -> 
	    begin try (Ident.find_same id s).tlterm with Not_found -> t end
	| TypLconst sc as t -> t
	| TypLapply(fn, args) -> TypLapply(subst fn, List.map subst args)
	| TypLfunction(kind, params, body) ->  TypLfunction(kind, params, subst body)
	| TypLlet(str, id, arg, body) ->  TypLlet(str, id, subst arg, subst body)
	| TypLletrec(decl, body) ->  TypLletrec(List.map subst_decl decl, subst body)
	| TypLprim(p, args) ->  TypLprim(p, List.map subst args)
	| TypLswitch(arg, sw) ->
	    TypLswitch(subst arg,
              {sw with tsw_consts = List.map subst_case sw.tsw_consts;
                 tsw_blocks = List.map subst_case sw.tsw_blocks;
                 tsw_failaction =
                         match sw.tsw_failaction with
                           | None -> None
                           | Some l -> Some (subst l)})
            
	| TypLstaticraise (i,args) ->  TypLstaticraise (i, List.map subst args)
	| TypLstaticcatch(e1, io, e2) -> TypLstaticcatch(subst e1, io, subst e2)
	| TypLtrywith(e1, exn, e2) -> TypLtrywith(subst e1, exn, subst e2)
	| TypLifthenelse(e1, e2, e3) -> TypLifthenelse(subst e1, subst e2, subst e3)
	| TypLsequence(e1, e2) -> TypLsequence(subst e1, subst e2)
	| TypLwhile(e1, e2) -> TypLwhile(subst e1, subst e2)
	| TypLfor(v, e1, e2, dir, e3) -> TypLfor(v, subst e1, subst e2, dir, subst e3) 
	| TypLassign(id, e) -> TypLassign(id, subst e)
	| TypLsend (met, obj, args) -> TypLsend (subst met, subst obj, List.map subst args)
	| TypLevent (lam, evt) -> TypLevent (subst lam, evt)
	| TypLifused (v, e) -> TypLifused (v, subst e) 
    ) in build_term slam tl.tltype
    and subst_decl (id, exp) = (id, subst exp)
    and subst_case (key, case) = (key, subst case)
  in subst lam


(* To let-bind expressions to variables *)

let bind str var exp body =
  match exp.tlterm with
    TypLvar var' when Ident.same var var' -> body
  | _ -> build_term (TypLlet(str, (var,exp.tltype) , exp, body)) body.tltype


let rec to_lambda_const = function
  | TConst_base c -> Const_base c
  | TConst_pointer i -> Const_pointer i
  | TConst_block (tag,scl) -> Const_block (tag,List.map (fun (sc,tann) -> to_lambda_const sc) scl)
  | TConst_float_array fls -> Const_float_array fls

let rec to_lambda tl = match tl.tlterm with 
    TypLvar id -> Lvar id
  | TypLconst sc -> Lconst (to_lambda_const sc)
  | TypLapply(fn, args) -> Lapply(to_lambda fn, List.map to_lambda args)
  | TypLfunction(kind, params, body) ->  Lfunction(kind, (List.map fst) params, to_lambda body)
  | TypLlet(str, id, arg, body) ->  Llet(str, (fst id), to_lambda arg, to_lambda body)
  | TypLletrec(decl, body) ->  Lletrec(List.map to_lambda_decl decl, to_lambda body)
  | TypLprim(p, args) ->  Lprim(p, List.map to_lambda args)
  | TypLswitch(arg, sw) ->
      Lswitch(to_lambda arg,
              {sw_numconsts=sw.tsw_numconsts;
	       sw_numblocks=sw.tsw_numblocks;
	       sw_consts = List.map to_lambda_case sw.tsw_consts;
               sw_blocks = List.map to_lambda_case sw.tsw_blocks;
               sw_failaction =
      match sw.tsw_failaction with
        | None -> None
        | Some l -> Some (to_lambda l)})
      
  | TypLstaticraise (i,args) ->  Lstaticraise (i, List.map to_lambda args)
  | TypLstaticcatch(e1, (i,tidl) , e2) -> 
      let io = (i,List.map fst tidl) in
	Lstaticcatch(to_lambda e1, io, to_lambda e2)
  | TypLtrywith(e1, exn, e2) -> Ltrywith(to_lambda e1, fst exn, to_lambda e2)
  | TypLifthenelse(e1, e2, e3) -> Lifthenelse(to_lambda e1, to_lambda e2, to_lambda e3)
  | TypLsequence(e1, e2) -> Lsequence(to_lambda e1, to_lambda e2)
  | TypLwhile(e1, e2) -> Lwhile(to_lambda e1, to_lambda e2)
  | TypLfor(v, e1, e2, dir, e3) -> Lfor(fst v, to_lambda e1, to_lambda e2, dir, to_lambda e3) 
  | TypLassign(id, e) -> Lassign(fst id, to_lambda e)
  | TypLsend (met, obj, args) -> Lsend (to_lambda met, to_lambda obj, List.map to_lambda args)
  | TypLevent (lam, evt) -> Levent (to_lambda lam, evt)
  | TypLifused (v, e) -> Lifused (fst v, to_lambda e) 
and to_lambda_decl (tid,tl) = (fst tid,to_lambda tl)
and to_lambda_case (key,case) = (key,to_lambda case)
