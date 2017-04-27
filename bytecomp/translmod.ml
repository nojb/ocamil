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

(* $Id: translmod.ml,v 1.31 2006/10/22 07:20:03 montela Exp $ *)

(* Translation from typed abstract syntax to lambda terms,
   for the module language *)

open Misc
open Asttypes
open Path
open Types
open Typedtree
open Lambda
open Typedlambda 
open Translobj
open Translcore
open Translclass


(* we must define some type annotations used thereafter *)
let ta_funarize ta1 ta2 =
  match ta1,ta2 with
      Some t1,Some t2 -> coretype_annotation (Btype.newgenty (Tarrow("",t1.taexpr,t2.taexpr,Cunknown))) (* x -> y *)
    | _,_ -> None
let ta_string = coretype_annotation Predef.type_string
let ta_unit = coretype_annotation Predef.type_unit
let ta_obj = coretype_annotation (Btype.newgenty Tvar)
let ta_getvalue = ta_funarize ta_string ta_obj
let ta_setvalue = ta_funarize ta_string (ta_funarize ta_obj ta_unit)
let ta_getvalue_register = ta_funarize ta_getvalue ta_unit
let ta_setvalue_register = ta_funarize ta_setvalue ta_unit



(** **)
(* Ajoutés pour la propagation des types *)

let primitives_types = ref []

let idents = ref([]: typedident list)

let rec bound_idents_T pat =
  match pat.pat_desc with
    Tpat_any -> ()
  | Tpat_var id -> idents := (id,build_type_annotation pat.pat_type pat.pat_env) :: !idents
  | Tpat_alias(p, id) -> bound_idents_T p; idents := (id,build_type_annotation pat.pat_type pat.pat_env) :: !idents
  | Tpat_constant cst -> ()
  | Tpat_tuple patl -> List.iter bound_idents_T patl
  | Tpat_construct(cstr, patl) -> List.iter bound_idents_T patl
  | Tpat_variant(_, pat, _) -> Misc.may bound_idents_T pat
  | Tpat_record lbl_pat_list ->
      List.iter (fun (lbl, pat) -> bound_idents_T pat) lbl_pat_list
  | Tpat_array patl -> List.iter bound_idents_T patl
  | Tpat_or(p1, _, _) ->
      (* Invariant : both arguments binds the same variables *)
      bound_idents_T p1

let rev_let_bound_idents_T pat_expr_list =
  idents := [];
  List.iter (fun (pat, expr) -> bound_idents_T pat) pat_expr_list;
  let res = !idents in idents := []; res

let let_bound_idents_T pat_expr_list =
  List.rev(rev_let_bound_idents_T pat_expr_list)

(** **)

let split_arrowtype = function
    Some {taexpr={desc=Tarrow(_,t1,t2,_)};tapath=path;taenv=env} -> (Some {taexpr=t1;tapath=path;taenv=env},
									      Some {taexpr=t2;tapath=path;taenv=env})
  | None -> failwith "Translmod.split_arrowtype_of (1)"
  | _ -> failwith "Translmod.split_arrowtype_of (2)"

let split_arrowtype_of lam = split_arrowtype lam.tltype

let rec map_modtype env = function
    Tmty_ident pa -> 
      begin try 
	let mty' = Env.find_modtype_expansion pa env in
	  map_modtype env mty' (* !!! pas sur que ca ne boucle pas ! *)
      with Not_found -> None 
      end
  | Tmty_signature _ -> generic_module_type
  | Tmty_functor(_,mt1,mt2) -> generic_functor_type (map_modtype env mt1) (map_modtype env mt2)

(* Compile a coercion *)

let rec apply_coercion restr arg =
  match restr with
    Tcoerce_none ->
      arg
  | Tcoerce_structure pos_cc_list ->
      name_lambda arg (fun id ->
			 Typedlambda.build_term (TypLprim(Pmakeblock(0, Immutable),
			       List.map (apply_coercion_field id) pos_cc_list)) generic_module_type)
  | Tcoerce_functor(cc_arg, cc_res) ->
      let paramtype,restype = split_arrowtype_of arg in 
      let param = (Ident.create "funarg", paramtype) in
      name_lambda arg 
	(fun id -> Typedlambda.build_term 
	   (TypLfunction(Curried, [param],
          apply_coercion cc_res
			   (Typedlambda.build_term (TypLapply(Typedlambda.build_varterm id, 
							      [apply_coercion cc_arg (Typedlambda.build_varterm param)])) restype ) ) )
	   (generic_functor_type paramtype restype)) 
  | Tcoerce_primitive p ->
      let primtype,primenv = List.assoc p !primitives_types in
      transl_primitive_T p primtype primenv

and apply_coercion_field id (pos, cc) =
  apply_coercion cc (Typedlambda.build_term 
		       (TypLprim(Pfield pos, 
				 [Typedlambda.build_varterm id])) generic_moditem_type   )


(* Compose two coercions
   apply_coercion c1 (apply_coercion c2 e) behaves like
   apply_coercion (compose_coercions c1 c2) e. *)

let rec compose_coercions c1 c2 =
  match (c1, c2) with
    (Tcoerce_none, c2) -> c2
  | (c1, Tcoerce_none) -> c1
  | (Tcoerce_structure pc1, Tcoerce_structure pc2) ->
      let v2 = Array.of_list pc2 in
      Tcoerce_structure
        (List.map
          (function (p1, Tcoerce_primitive p) ->
                      (p1, Tcoerce_primitive p)
                  | (p1, c1) ->
                      let (p2, c2) = v2.(p1) in (p2, compose_coercions c1 c2))
             pc1)
  | (Tcoerce_functor(arg1, res1), Tcoerce_functor(arg2, res2)) ->
      Tcoerce_functor(compose_coercions arg2 arg1,
                      compose_coercions res1 res2)
  | (_, _) ->
      fatal_error "Translmod.compose_coercions"

(* Record the primitive declarations occurring in the module compiled *)

let primitive_declarations = ref ([] : string list)

(* Keep track of the root path (from the root of the namespace to the
   currently compiled module expression).  Useful for naming exceptions. *)

let global_path glob = Some(Pident glob)
let functor_path path param =
  match path with
    None -> None
  | Some p -> Some(Papply(p, Pident param))
let field_path path field =
  match path with
    None -> None
  | Some p -> Some(Pdot(p, Ident.name field, Path.nopos))

(* Compile a module expression *)

let rec transl_module cc rootpath mexp =
  match mexp.mod_desc with
    Tmod_ident path ->
	  apply_coercion cc (transl_path_with_type (map_modtype mexp.mod_env mexp.mod_type) path)
  | Tmod_structure str ->
	  let lam = transl_structure [] cc rootpath str in
	    {lam with tltype=generic_module_type}
  | Tmod_functor(param, mty, body) ->
      let bodypath = functor_path rootpath param in
	  let paramtype,restype = split_arrowtype (map_modtype mexp.mod_env mexp.mod_type) in
      begin match cc with
        Tcoerce_none ->
		  let bodylam = transl_module Tcoerce_none bodypath body in
(*		  let restype = bodylam.tltype in *)
		    build_term (TypLfunction(Curried, [param,paramtype], bodylam)) (generic_functor_type paramtype restype)
      | Tcoerce_functor(ccarg, ccres) ->
		  let param' = Ident.create "funarg",paramtype in
		  let bodylam = transl_module ccres bodypath body in
(*		  let restype = bodylam.tltype in *)
		    build_term (TypLfunction(Curried, [param'],
					     build_term (TypLlet(Alias, (param,paramtype), apply_coercion ccarg (build_varterm param'),
								 bodylam)) generic_module_type)) (generic_functor_type paramtype restype)
      | _ ->
          fatal_error "Translmod.transl_module"
      end
  | Tmod_apply(funct, arg, ccarg) ->
	  let functlam = transl_module Tcoerce_none None funct in
	  let _,restype = split_arrowtype_of functlam in
      apply_coercion cc
          (build_term (TypLapply(functlam,
				 [transl_module ccarg None arg])) restype)
  | Tmod_constraint(arg, mty, ccarg) ->
      transl_module (compose_coercions cc ccarg) rootpath arg

and transl_structure fields cc rootpath = function
    [] ->
      begin match cc with
        Tcoerce_none ->
	  let blockargs = List.map build_varterm (List.rev fields) in
            build_term (TypLprim(Pmakeblock(0, Immutable),blockargs)) generic_module_type
      | Tcoerce_structure pos_cc_list ->
          let v = Array.of_list (List.rev fields) in
          build_term (TypLprim(Pmakeblock(0, Immutable),
                List.map
                  (fun (pos, cc) ->
                    match cc with
                      Tcoerce_primitive p -> 
			let primtype,primenv = List.assoc p !primitives_types in
			  transl_primitive_T p primtype primenv
                    | _ -> apply_coercion cc (build_varterm v.(pos)))
                  pos_cc_list)) generic_module_type
      | _ ->
          fatal_error "Translmod.transl_structure"
      end
  | Tstr_eval expr :: rem ->
      let t1=transl_exp expr in
      let t2=transl_structure fields cc rootpath rem in
      build_term (TypLsequence(t1,t2)) t2.tltype
  | Tstr_value(rec_flag, pat_expr_list) :: rem ->
      let ext_fields = (rev_let_bound_idents_T pat_expr_list) @ fields in
      transl_let rec_flag pat_expr_list
                 (transl_structure ext_fields cc rootpath rem)
  | Tstr_primitive(id, descr) :: rem ->
       begin match rem with 
	   (Tstr_eval exp)::rem' -> 
      begin match descr.val_kind with
		 Val_prim p -> 
		   (* camil types propag *)
		   primitives_types := (p,(exp.exp_type,exp.exp_env)) :: !primitives_types; 
		   primitive_declarations := p.Primitive.prim_name :: !primitive_declarations;
		   transl_structure fields cc rootpath rem'
	       | _ -> transl_structure fields cc rootpath rem'
	     end
	 | _ -> failwith "CAMIL: no 'external' in module types and functor structures(0)"
       end
  | Tstr_type(decls) :: rem ->
      transl_structure fields cc rootpath rem
  | Tstr_exception(id, decl) :: rem ->
      let exn_tann = coretype_annotation Predef.type_exn in
      build_term (TypLlet(Strict, (id,exn_tann), transl_exception id (field_path rootpath id) decl,
           transl_structure ((id,exn_tann) :: fields) cc rootpath rem)) None (* !! ?? *)
  | Tstr_exn_rebind(id, path) :: rem ->
      build_term (TypLlet(Strict, (id,None), transl_path path,
           transl_structure ((id,None) :: fields) cc rootpath rem)) None (* !! ?? *)
  | Tstr_module(id, modl) :: rem ->
      lengthen_typepath (Ident.name id);
      let modbody = transl_module Tcoerce_none (field_path rootpath id) modl in
      let modtype = modbody.tltype in
	shorten_typepath();
	build_term (TypLlet(Strict, (id,modtype), modbody,
			    transl_structure ((id,modtype) :: fields) cc rootpath rem)) None (* !! ?? *)
  | Tstr_modtype(id, decl) :: rem ->
      transl_structure fields cc rootpath rem
  | Tstr_open path :: rem ->
      transl_structure fields cc rootpath rem
  | Tstr_class cl_list :: rem ->
      let ids = List.map (fun (i, _, _, _) -> (i,generic_module_type)) cl_list in
      build_term (TypLletrec(List.map
                (fun (id, arity, meths, cl) ->
                  ((id,generic_module_type), transl_class ids id arity meths cl))
                cl_list,
              transl_structure (List.rev ids @ fields) cc rootpath rem)) (coretype_annotation Predef.type_unit)
  | Tstr_cltype cl_list :: rem ->
      transl_structure fields cc rootpath rem
  | Tstr_include(modl, ids) :: rem -> (* !! ?? *)
      let ids = List.map (fun id -> id,generic_moditem_type) ids in 
      let mid = Ident.create "include",generic_module_type in 
      let rec rebind_idents pos newfields = function
        [] ->
          transl_structure newfields cc rootpath rem
      | id :: ids ->
          build_term (TypLlet(Alias, id, build_term (TypLprim(Pfield pos, [build_varterm mid])) generic_moditem_type,
               rebind_idents (pos + 1) (id :: newfields) ids)) generic_module_type in
      build_term (TypLlet(Alias, mid, transl_module Tcoerce_none None modl,
           rebind_idents 0 fields ids)) generic_module_type

(* Update forward declaration in Translcore *)
let _ =
  Translcore.transl_module := transl_module

(* Compile an implementation *)

let transl_implementation module_name (str, cc) =
  reset_labels ();
  primitive_declarations := [];
  let module_id = Ident.create_persistent module_name in
  (33,build_term (TypLprim(Psetglobal module_id,
        [transl_label_init
            (transl_structure [] cc (global_path module_id) str)])) ta_unit, module_id)



(* A variant of transl_structure used to compile toplevel structure definitions
   for the native-code compiler. Store the defined values in the fields
   of the global as soon as they are defined, in order to reduce register
   pressure.  Also rewrites the defining expressions so that they
   refer to earlier fields of the structure through the fields of
   the global, not by their names.
   "map" is a table from defined idents to (pos in global block, coercion).
   "prim" is a list of (pos in global block, primitive declaration). *)



let transl_store_structure glob map prims str =
  let rec transl_store subst = function
    [] ->
      lambda_unit
  | Tstr_eval expr :: rem ->
      let t1=subst_lambda subst (transl_exp expr) in
      let t2=transl_store subst rem in
      build_term (TypLsequence(t1,t2)) t2.tltype
  | Tstr_value(rec_flag, pat_expr_list) :: rem ->
      let ids = let_bound_idents_T pat_expr_list in
      let lam = transl_let rec_flag pat_expr_list (store_idents ids) in
      let t1 = subst_lambda subst lam in
      let t2=transl_store (add_idents false ids subst) rem in
      build_term (TypLsequence(t1,t2)) t2.tltype
  | Tstr_primitive(id, descr) :: rem ->
       begin match rem with 
	   (Tstr_eval exp)::rem' -> 
      begin match descr.val_kind with
		 Val_prim p -> 
		   (* camil types propag *)
		   primitives_types := (p,(exp.exp_type,exp.exp_env)) :: !primitives_types; 
		   primitive_declarations := p.Primitive.prim_name :: !primitive_declarations;
		   transl_store subst rem'
	       | _ -> transl_store subst rem'
	     end
	 | _ -> failwith "CAMIL: no 'external' in module types and functor structures"
       end
  | Tstr_type(decls) :: rem ->
      transl_store subst rem
  | Tstr_exception(id, decl) :: rem ->
      let exn_tann= coretype_annotation Predef.type_exn in
      let lam = transl_exception id (field_path (global_path glob) id) decl in
      let t1=build_term (TypLlet(Strict, (id,exn_tann), lam, store_ident (id,exn_tann))) None in (* !! ?? *)
      let t2=transl_store (add_ident false (id,None) subst) rem in
      build_term (TypLsequence(t1,t2)) t2.tltype
  | Tstr_exn_rebind(id, path) :: rem ->
      let lam = subst_lambda subst (transl_path path) in
      let t1=build_term (TypLlet(Strict, (id,None), lam, store_ident (id,None))) None in (* !! ?? *)
      let t2=transl_store (add_ident false (id,None) subst) rem in
      build_term (TypLsequence(t1,t2)) t2.tltype
  | Tstr_module(id, modl) :: rem ->
      lengthen_typepath (Ident.name id);
      let lam =
        transl_module Tcoerce_none (field_path (global_path glob) id) modl in
      (* Careful: the module value stored in the global may be different
         from the local module value, in case a coercion is applied.
         If so, keep using the local module value (id) in the remainder of
         the compilation unit (add_ident true returns subst unchanged).
         If not, we can use the value from the global
         (add_ident true adds id -> Pgetglobal... to subst). *)
	shorten_typepath();
      let tid = (id,lam.tltype) in
      let t1=store_ident tid in
      let t2=transl_store(add_ident true tid subst) rem in
      build_letterm (Strict, tid, subst_lambda subst lam,
        build_term (TypLsequence(t1,t2)) t2.tltype)
  | Tstr_modtype(id, decl) :: rem ->
      transl_store subst rem
  | Tstr_open path :: rem ->
      transl_store subst rem
  | Tstr_class cl_list :: rem ->
      let ids = List.map (fun (i, _, _, _) -> i, generic_module_type) cl_list in
      let lam =
        build_term (TypLletrec(List.map
                  (fun (id, arity, meths, cl) ->
                     ((id,generic_module_type), transl_class ids id arity meths cl))
                  cl_list,
                store_idents ids)) (coretype_annotation Predef.type_unit) in
      let t1=subst_lambda subst lam in
      let t2=transl_store (add_idents false ids subst) rem in
      build_term (TypLsequence(t1,t2)) t2.tltype
  | Tstr_cltype cl_list :: rem ->
      transl_store subst rem
  | Tstr_include(modl, ids) :: rem ->
      let ids  = List.map (fun id -> id,generic_moditem_type) ids in
      let mid = Ident.create "include",generic_module_type in
      let rec store_idents pos = function
        [] -> transl_store (add_idents true ids subst) rem
      | id :: idl ->
	  let t1=store_ident id in
	  let t2=  store_idents (pos + 1) idl in
          build_term (TypLlet(Alias, id, build_term (TypLprim(Pfield pos, [build_varterm mid])) generic_moditem_type,
               build_term (TypLsequence(t1,t2)) t2.tltype)) t2.tltype in
      build_term (TypLlet(Strict, mid,
           subst_lambda subst (transl_module Tcoerce_none None modl),
           store_idents 0 ids)) None

  and store_ident id =
    try
      let (pos, cc) = Ident.find_same (fst id) map in
      let init_val = apply_coercion cc (build_varterm id) in
	build_term (TypLprim(Psetfield(pos, false), [build_term (TypLprim(Pgetglobal glob, [])) generic_module_type; init_val])) None
    with Not_found ->
      fatal_error("Translmod.store_ident: " ^ Ident.unique_name (fst id))

  and store_idents idlist =
    make_sequence store_ident idlist

  and add_ident may_coerce id subst =
    try
      let (pos, cc) = Ident.find_same (fst id) map in
      match cc with
        Tcoerce_none ->
          Ident.add (fst id) (build_term (TypLprim(Pfield pos, [build_term (TypLprim(Pgetglobal glob, [])) None])) (snd id)) subst 
      | _ ->
          if may_coerce then subst else assert false
    with Not_found ->
      assert false

  and add_idents may_coerce idlist subst =
    List.fold_right (add_ident may_coerce) idlist subst

  and store_primitive (pos, prim) cont =
    let primtype,primenv = List.assoc prim !primitives_types in
      build_term (TypLsequence(build_term (TypLprim(Psetfield(pos, false),
						    [build_term (TypLprim(Pgetglobal glob, [])) None; transl_primitive_T prim primtype primenv])) None, 
			       cont)) cont.tltype

  in List.fold_right store_primitive prims (transl_store Ident.empty str)

(* Build the list of value identifiers defined by a toplevel structure
   (excluding primitive declarations). *)

let rec defined_idents = function
    [] -> []
  | Tstr_eval expr :: rem -> defined_idents rem
  | Tstr_value(rec_flag, pat_expr_list) :: rem ->
      let_bound_idents pat_expr_list @ defined_idents rem
  | Tstr_primitive(id, descr) :: rem -> defined_idents rem
  | Tstr_type decls :: rem -> defined_idents rem
  | Tstr_exception(id, decl) :: rem -> id :: defined_idents rem
  | Tstr_exn_rebind(id, path) :: rem -> id :: defined_idents rem
  | Tstr_module(id, modl) :: rem -> id :: defined_idents rem
  | Tstr_modtype(id, decl) :: rem -> defined_idents rem
  | Tstr_open path :: rem -> defined_idents rem
  | Tstr_class cl_list :: rem ->
      List.map (fun (i, _, _, _) -> i) cl_list @ defined_idents rem
  | Tstr_cltype cl_list :: rem -> defined_idents rem
  | Tstr_include(modl, ids) :: rem -> ids @ defined_idents rem

(* Transform a coercion and the list of value identifiers defined by
   a toplevel structure into a table [id -> (pos, coercion)],
   with [pos] being the position in the global block where the value of
   [id] must be stored, and [coercion] the coercion to be applied to it.
   A given identifier may appear several times
   in the coercion (if it occurs several times in the signature); remember
   to assign it the position of its last occurrence.
   Identifiers that are not exported are assigned positions at the
   end of the block (beyond the positions of all exported idents).
   Also compute the total size of the global block,
   and the list of all primitives exported as values. *)

let build_ident_map restr idlist =
  let rec natural_map pos map prims = function
    [] ->
      (map, prims, pos)
  | id :: rem ->
      natural_map (pos+1) (Ident.add id (pos, Tcoerce_none) map) prims rem in
  match restr with
    Tcoerce_none ->
      natural_map 0 Ident.empty [] idlist
  | Tcoerce_structure pos_cc_list ->
      let idarray = Array.of_list idlist in
      let rec export_map pos map prims undef = function
        [] ->
          natural_map pos map prims undef
      | (source_pos, Tcoerce_primitive p) :: rem ->
          export_map (pos + 1) map ((pos, p) :: prims) undef rem
      | (source_pos, cc) :: rem ->
          let id = idarray.(source_pos) in
          export_map (pos + 1) (Ident.add id (pos, cc) map)
                     prims (list_remove id undef) rem
      in export_map 0 Ident.empty [] idlist pos_cc_list
  | _ ->
      fatal_error "Translmod.build_ident_map"

(* Compile an implementation using transl_store_structure 
   (for the native-code compiler). *)

let transl_store_implementation module_name (str, restr) =
  reset_labels ();
  primitive_declarations := [];
  let module_id = Ident.create_persistent module_name in
  let (map, prims, size) = build_ident_map restr (defined_idents str) in
  (size, transl_label_init (transl_store_structure module_id map prims str),
     module_id)

(* Compile a toplevel phrase *)

let toploop_ident = Ident.create_persistent "Toploop"
let toploop_getvalue_pos = 0 (* position of getvalue in module Toploop *)
let toploop_setvalue_pos = 1 (* position of setvalue in module Toploop *)

let aliased_idents = ref Ident.empty

let set_toplevel_name id name =
  aliased_idents := Ident.add id name !aliased_idents

let toplevel_name id =
  try Ident.find_same id !aliased_idents
  with Not_found -> Ident.name id

(*
let toploop_getvalue id =
  Lapply(Lprim(Pfield toploop_getvalue_pos,
                 [Lprim(Pgetglobal toploop_ident, [])]),
         [Lconst(Const_base(Const_string (toplevel_name id)))])


let toploop_setvalue id lam =
  Lapply(Lprim(Pfield toploop_setvalue_pos,
                 [Lprim(Pgetglobal toploop_ident, [])]),
         [Lconst(Const_base(Const_string (toplevel_name id))); lam])

let toploop_setvalue_id id = toploop_setvalue id (Lvar id)
*)

(* reserver quatre emplacements a la fin des globals de myself *)
(* 96 = getvalueref *)
(* 97 = setvalueref *)
(* 98 = definegetvalue *)
(* 99 = definesetvalue *)
(* 100 = runme *)



let myself_ident = Ident.create "$specialuse_toplevel"
let setvalueref_ident = Ident.create "$setvalueref",generic_module_type
let getvalueref_ident = Ident.create "$getvalueref",generic_module_type
let definesetvalue_ident = Ident.create "$definesetvalue",ta_setvalue_register
let definegetvalue_ident = Ident.create "$definegetvalue",ta_getvalue_register
let runme_ident = Ident.create "$runme"


let toploop_getvalue id =
  let id=fst id in
  build_term (TypLapply(build_term (TypLprim(Pfield 0,[build_term (TypLprim(Pfield 96,[build_term (TypLprim(Pgetglobal myself_ident,[])) generic_module_type])) generic_module_type])) ta_getvalue,
         [build_term (TypLconst(TConst_base(Const_string (toplevel_name id)))) ta_string])) ta_obj

let toploop_setvalue id lam (* myself_ident *) = 
  let id=fst id in
  build_term (TypLapply(build_term (TypLprim(Pfield 0,[build_term (TypLprim(Pfield 97,[build_term (TypLprim(Pgetglobal myself_ident,[])) generic_module_type])) generic_module_type])) ta_setvalue,
	  [build_term (TypLconst(TConst_base(Const_string (toplevel_name id)))) ta_string; lam])) ta_unit

let toploop_setvalue_id id (* myself_ident*) = 
    toploop_setvalue id (build_varterm id) (* myself_ident*)

let toploop_wrap_it lam (* myself_ident *) =
  let funparam = Ident.create "f",ta_setvalue in
  let funparam0 = Ident.create "f0",ta_getvalue in
  let funparam1 = Ident.create "x",ta_unit in
  let ta_runme = ta_funarize ta_unit lam.tltype in
  build_term (TypLsequence (
  (* mise en place de la référence qui pointera sur setvalue *)
  build_letterm (Strict,setvalueref_ident, build_term (TypLprim(Pmakeblock(0,Mutable),[build_term (TypLconst (TConst_pointer 0)) ta_obj])) generic_module_type,
       build_term (TypLprim(Psetfield(97,false),[build_term (TypLprim(Pgetglobal myself_ident,[])) generic_module_type;build_varterm setvalueref_ident])) ta_unit),
  build_term (TypLsequence(
  (* mise en place de la référence qui pointera sur getvalue *)
  build_letterm (Strict,getvalueref_ident, build_term (TypLprim(Pmakeblock(0,Mutable),[build_term (TypLconst (TConst_pointer 0)) ta_obj])) generic_module_type,
       build_term (TypLprim(Psetfield(96,false),[build_term (TypLprim(Pgetglobal myself_ident,[])) generic_module_type;build_varterm getvalueref_ident])) ta_unit),
  build_term (TypLsequence(
  (* definition de la fonction d'enregistrement de setvalue *)
  build_letterm (Strict,definesetvalue_ident,
       build_term (TypLfunction(Curried,[funparam],build_term (TypLprim(Psetfield(0,true),
	  [build_term (TypLprim(Pfield 97,[build_term (TypLprim(Pgetglobal myself_ident,[])) generic_module_type])) generic_module_type;build_varterm funparam])) ta_unit)) ta_setvalue_register,
       build_term (TypLprim(Psetfield(99,false),[build_term (TypLprim(Pgetglobal myself_ident,[])) generic_module_type;build_varterm definesetvalue_ident])) ta_unit),
  build_term (TypLsequence( 
  (* definition de la fonction d'enregistrement de getvalue *)
  build_letterm (Strict,definegetvalue_ident,
       build_term (TypLfunction(Curried,[funparam0],build_term (TypLprim(Psetfield(0,true),
	  [build_term (TypLprim(Pfield 96,[build_term (TypLprim(Pgetglobal myself_ident,[])) generic_module_type])) generic_module_type;build_varterm funparam0])) ta_unit)) ta_getvalue_register,
       build_term (TypLprim(Psetfield(98,false),[build_term (TypLprim(Pgetglobal myself_ident,[])) generic_module_type;build_varterm definegetvalue_ident])) ta_unit),
  (* code proprement dit *)
  build_letterm (Strict,(runme_ident,ta_runme),build_term (TypLfunction(Curried,[funparam1],lam)) ta_runme,
       build_term (TypLprim(Psetfield(100,false),[build_term (TypLprim(Pgetglobal myself_ident,[])) generic_module_type;build_varterm (runme_ident,ta_runme)])) ta_unit)
 )) None)) None)) None)) None

let close_toplevel_term lam =
  IdentSet.fold (fun id l -> build_letterm (Strict, id, toploop_getvalue id, l))
                (free_variables lam) lam


(*TEMPO*)
let dummy_toplevel_var = Ident.create "$prev"

let transl_toplevel_item = function
    Tstr_eval expr ->
      (*TEMPOtransl_exp expr*)
      let lam=transl_exp expr in
      let typed_dummy_toplevel_var = dummy_toplevel_var,lam.tltype in 
      build_letterm (Strict,typed_dummy_toplevel_var,lam,toploop_setvalue_id typed_dummy_toplevel_var)

  | Tstr_value(rec_flag, pat_expr_list) ->
      let idents = List.map (fun id -> id,None) (let_bound_idents pat_expr_list) in
      transl_let rec_flag pat_expr_list
                 (make_sequence toploop_setvalue_id idents)
  | Tstr_primitive(id, descr) ->
      lambda_unit
  | Tstr_type(decls) ->
      lambda_unit
  | Tstr_exception(id, decl) ->
      toploop_setvalue (id,None) (transl_exception id None decl)
  | Tstr_exn_rebind(id, path) ->
      toploop_setvalue (id,None) (transl_path path)
  | Tstr_module(id, modl) ->
      lengthen_typepath (Ident.name id);
      let modbody = transl_module Tcoerce_none (Some(Pident id)) modl in
	shorten_typepath();
        toploop_setvalue (id,None) modbody
  | Tstr_modtype(id, decl) ->
      lambda_unit
  | Tstr_open path ->
      lambda_unit
  | Tstr_class cl_list ->
      let ids = List.map (fun (i, _, _, _) -> i,None) cl_list in
      List.iter
        (fun id -> set_toplevel_name (fst id) (Ident.name (fst id) ^ "(c)"))
        ids;
      build_term (TypLletrec(List.map
                (fun (id, arity, meths, cl) ->
                   ((id,None), transl_class ids id arity meths cl))
                cl_list,
              make_sequence
                (fun (id, _, _, _) -> toploop_setvalue_id (id,None))
                cl_list)) None
  | Tstr_cltype cl_list ->
      lambda_unit
  | Tstr_include(modl, ids) ->
      let ids = List.map (fun id -> id,None) ids in
      let mid = Ident.create "include",None in
      let rec set_idents pos = function
        [] ->
          lambda_unit
      | id :: ids ->
          build_term (TypLsequence(toploop_setvalue id (build_term (TypLprim(Pfield pos, [build_varterm mid])) None),
                    set_idents (pos + 1) ids)) None in
     build_term (TypLlet(Strict, mid, transl_module Tcoerce_none None modl, set_idents 0 ids)) None

let transl_toplevel_item_and_close itm =
  close_toplevel_term (transl_label_init (transl_toplevel_item itm))

let transl_toplevel_definition str =
  reset_labels ();
 (* MOD avec la derniere ligne *)
 let lam = make_sequence transl_toplevel_item_and_close str
     in toploop_wrap_it lam
