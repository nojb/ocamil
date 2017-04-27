(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*         Jerome Vouillon, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: translclass.ml,v 1.11 2007/03/24 11:04:29 montela Exp $ *)

open Misc
open Asttypes
open Types
open Typedtree
open Lambda
open Typedlambda
open Translobj
open Translcore


(* CAMIL *)
let ta_int = coretype_annotation Predef.type_int
let ta_string = coretype_annotation Predef.type_string
let ta_unit = coretype_annotation Predef.type_unit
let ta_block = generic_module_type
let ta_obj = generic_moditem_type

(* DEBUG
let generic_functor_type x y = 
  match x,y with
      Some t1,Some t2 -> coretype_annotation (Btype.newgenty (Tarrow("",t1.taexpr,t2.taexpr,Cunknown))) (* x -> y *)
    | None,None  -> failwith "SSSSSSSSSS Arg&Res"
    | None,_ -> failwith "SSSSSSSSSS Arg"
    | _,None -> failwith "SSSSSSSSSS Res"
*)

let ta_function params body = List.fold_right (fun (_,targ) tann -> generic_functor_type targ tann) params body.tltype

let camlOO_type_typann tname =
  let tpath,cdesc = Env.lookup_type (Longident.Ldot (Longident.Lident "CamlinternalOO", tname)) Env.empty in 
    Some {tapath = ["CamlinternalOO"]; taexpr = Ctype.newconstr tpath []; taenv = Env.initial}

let type_obj_init = generic_functor_type ta_block ta_block




(* XXX Rajouter des evenements... *)

type error = Illegal_class_expr

exception Error of Location.t * error

let lfunction params body =
  match body.tlterm with
    TypLfunction (Curried, params', body') ->
      build_term (TypLfunction (Curried, params @ params', body')) (ta_function (params @ params') body')
  |  _ ->
      build_term (TypLfunction (Curried, params, body)) (ta_function params body)

(* NOT USED !!!
let lapply func args =
  match func.tlterm with
    TypLapply(func', args') ->
      build_term (TypLapply(func', args' @ args)) (arrow_apply_camltypes func'.tltype ((List.length args')+(List.length args)))
  | _ ->
      build_term (TypLapply(func, args)) (arrow_apply_camltypes func.tltype (List.length args))
*)

let lsequence l1 l2 =
  if l2 = lambda_unit then l1 else build_term (TypLsequence(l1, l2)) l2.tltype

let transl_label l = build_term (TypLconst (TConst_base (Const_string l))) (coretype_annotation Predef.type_string)

(*
let rec transl_meth_list lst =
  Lconst
    (List.fold_right
       (fun lab rem -> Const_block (0, [Const_base (Const_string lab); rem]))
       lst (Const_pointer 0))
*)

(* modif 10/05 *)
(* the representation of lists has changed *)
let rec transl_meth_list lst =
  let type_string_list = coretype_annotation (Predef.type_list Predef.type_string) in
  build_term (TypLconst
    (List.fold_right
       (fun lab rem -> TConst_block (1, [(TConst_base (Const_string lab),ta_string); (rem,type_string_list)]))
       (* tag of non-empty list is now 1 *)
       lst (TConst_block (0, [])))) type_string_list (* the empty list is now a block *)

let set_inst_var obj id expr =
  let kind = if Typeopt.maybe_pointer expr then Paddrarray else Pintarray in
  build_term (TypLprim(Parraysetu kind, [build_varterm obj; build_varterm id; transl_exp expr])) ta_unit

(* doesn't seem to be used ! *)
let copy_inst_var obj id expr templ offset =
  let kind = if Typeopt.maybe_pointer expr then Paddrarray else Pintarray in
  let id' = Ident.create (Ident.name (fst id)),ta_int in
  build_letterm (Strict, id', build_term (TypLprim (Pidentity, [build_varterm id])) None,
  build_term (TypLprim(Parraysetu kind,
        [build_varterm obj; build_varterm id';
         build_term (TypLprim(Parrayrefu kind, [build_varterm templ; build_term (TypLprim(Paddint,
                                                   [build_varterm id';
                                                    build_varterm offset])) ta_int])) None])) ta_unit)

let transl_val tbl create name id rem =
  build_letterm (StrictOpt, (id,ta_int), build_term (TypLapply (oo_prim (if create then "new_variable"
                                       else           "get_variable"),
                              [build_varterm tbl; transl_label name])) ta_int,
       rem)

let transl_vals tbl create vals rem =
  List.fold_right
    (fun (name, id) rem -> transl_val tbl create name id rem)
    vals rem

let transl_super tbl meths inh_methods rem =
  List.fold_right
    (fun (nm, id) rem ->
       begin try
         build_letterm (StrictOpt, (id,ta_obj), build_term (TypLapply (oo_prim "get_method",
                                     [build_varterm tbl; build_varterm ((Meths.find nm meths),ta_int)])) ta_obj,
              rem)
       with Not_found ->
         rem
       end)
    inh_methods rem

let create_object cl obj init =
  let obj' = Ident.create "self",ta_block in
  let (inh_init, obj_init) = init obj' in
(* original code discarded because of a typing problem:
   a conflict between lists and blocks appear in "create_object_and_run_initializers", written using Obj.*
   it occurs when creating objects with no field and no contructor argument
   -------
  if obj_init = lambda_unit then
   (inh_init,
    build_term (TypLapply (oo_prim "create_object_and_run_initializers",
            [build_varterm obj; build_varterm cl])) ta_block)
  else ----
 *) 
  if obj_init = lambda_unit then
    (inh_init,
     build_letterm (Strict, obj',
		    build_term (TypLapply (oo_prim "create_object_opt", [build_varterm obj; build_varterm cl])) ta_block,
		    build_term (TypLapply (oo_prim "run_initializers_opt", [build_varterm obj; build_varterm obj'; build_varterm cl])) ta_block))
  else begin
   (inh_init,
    build_letterm (Strict, obj',
            build_term (TypLapply (oo_prim "create_object_opt", [build_varterm obj; build_varterm cl])) ta_block,
         build_term (TypLsequence(obj_init,
                  build_term (TypLapply (oo_prim "run_initializers_opt",
                          [build_varterm obj; build_varterm obj'; build_varterm cl])) ta_block)) ta_block))
  end

let rec build_object_init cl_table obj params inh_init cl =
  match cl.cl_desc with
    Tclass_ident path ->
      let obj_init = Ident.create "obj_init",type_obj_init in
      (obj_init::inh_init, build_term (TypLapply(build_varterm obj_init, [build_varterm obj])) ta_block)
  | Tclass_structure str ->
      create_object cl_table obj (fun obj ->
        let (inh_init, obj_init) =
          List.fold_right
            (fun field (inh_init, obj_init) ->
               match field with
                 Cf_inher (cl, _, _) ->
                   let (inh_init, obj_init') =
                     build_object_init cl_table obj [] inh_init cl
                   in
                   (inh_init, lsequence obj_init' obj_init)
               | Cf_val (_, id, exp) ->
                   (inh_init, lsequence (set_inst_var obj (id,ta_int) exp) obj_init)
               | Cf_meth _ | Cf_init _ ->
                   (inh_init, obj_init)
               | Cf_let (rec_flag, defs, vals) ->
                   (inh_init,
                    Translcore.transl_let rec_flag defs
                      (List.fold_right
                         (fun (id, expr) rem ->
                            lsequence (build_term (TypLifused(id, set_inst_var obj id expr)) ta_unit)
                                      rem)
                         (List.map (fun (id,texp) -> ((id, ta_int(* here types are int32(labels) NOT build_type_annotation texp.exp_type texp.exp_env*)),texp)) vals) obj_init)))
            str.cl_field
            (inh_init, lambda_unit)
        in
        (inh_init,
         List.fold_right
           (fun (id, expr) rem ->
              lsequence (build_term (TypLifused (id, set_inst_var obj id expr)) ta_unit) rem)
           params obj_init))
  | Tclass_fun (pat, vals, cl, partial) ->
      let (inh_init, obj_init) =
        build_object_init cl_table obj ( (List.map (fun (id,e)->(id, ta_int(* here types are int32(labels) NOT build_type_annotation e.exp_type e.exp_env*)),e) vals) @ params) inh_init cl
      in
      (inh_init,
       let build params rem =
         let param = name_pattern "param" [pat, ()],(build_type_annotation pat.pat_type pat.pat_env) in
         build_term (TypLfunction (Curried, param::params,
                    Matching.for_function
                      pat.pat_loc None (build_varterm param) [pat, rem] partial)) None
       in
       begin match obj_init.tlterm with
         TypLfunction (Curried, params, rem) -> build params rem
       | rem                              -> build [] obj_init
       end)
  | Tclass_apply (cl, oexprs) ->
      let (inh_init, obj_init) =
        build_object_init cl_table obj params inh_init cl
      in
      (inh_init, transl_apply obj_init oexprs None)
  | Tclass_let (rec_flag, defs, vals, cl) ->
      let (inh_init, obj_init) =
        build_object_init cl_table obj ( (List.map (fun (id,e)->(id, ta_int(* here types are int32(labels) NOT build_type_annotation e.exp_type e.exp_env*)),e) vals) @ params) inh_init cl
      in
      (inh_init, Translcore.transl_let rec_flag defs obj_init)
  | Tclass_constraint (cl, vals, pub_meths, concr_meths) ->
      build_object_init cl_table obj params inh_init cl

let rec build_object_init_0 cl_table params cl =
  match cl.cl_desc with
    Tclass_let (rec_flag, defs, vals, cl) ->
      let (inh_init, obj_init) =
        build_object_init_0 cl_table ( (List.map (fun (id,e)->(id, ta_int(* here types are int32(labels) NOT build_type_annotation e.exp_type e.exp_env*)),e) vals) @ params) cl
      in
      (inh_init, Translcore.transl_let rec_flag defs obj_init)
  | _ ->
      let obj = Ident.create "self",ta_obj in
      let (inh_init, obj_init) = build_object_init cl_table obj params [] cl in
      let obj_init = lfunction [obj] obj_init in
      (inh_init, obj_init)

let bind_method tbl public_methods lab id cl_init =
  if List.mem lab public_methods then
    build_letterm (Alias, (id,ta_int), build_varterm ((meth lab),ta_int), cl_init)
  else
    build_letterm (StrictOpt, (id,ta_int), build_term (TypLapply (oo_prim "get_method_label",
                                [build_varterm tbl; transl_label lab])) ta_int,
    cl_init)

let bind_methods tbl public_methods meths cl_init =
  Meths.fold (bind_method tbl public_methods) meths cl_init

let rec build_class_init cla pub_meths cstr inh_init cl_init cl =
  match cl.cl_desc with
    Tclass_ident path ->
      begin match inh_init with
        obj_init::inh_init ->
	  let type_table = camlOO_type_typann "table" in
	  let type_class_init = generic_functor_type type_table type_obj_init in
          (inh_init,
           build_letterm (Strict, obj_init, 
                 build_term (TypLapply(build_term (TypLprim(Pfield 1, [transl_path(*with_type ?? *) path])) type_class_init, [build_varterm cla])) type_obj_init,
                 cl_init))
      | _ ->
          assert false
      end
  | Tclass_structure str ->
      let (inh_init, cl_init) =
        List.fold_right
          (fun field (inh_init, cl_init) ->
            match field with
              Cf_inher (cl, vals, meths) ->
                build_class_init cla pub_meths false inh_init
                  (transl_vals cla false (List.map (fun (s,id) -> (s,id)) vals)
                     (transl_super cla str.cl_meths meths cl_init))
                  cl
            | Cf_val (name, id, exp) ->
                (inh_init, transl_val cla true name id cl_init)
            | Cf_meth (name, exp) ->
                let met_code =
                  if !Clflags.native_code then begin
                    (* Force correct naming of method for profiles *)
		    let exp' = transl_exp exp in
                    let met = Ident.create ("method_" ^ name), (*ta_obj?*) exp'.tltype in
                    build_letterm (Strict, met, exp', build_varterm met)
                  end else
                    transl_exp exp in
                (inh_init,
                 build_term (TypLsequence(build_term (TypLapply (oo_prim "set_method",
                                   [build_varterm cla;
                                    build_varterm ((Meths.find name str.cl_meths), ta_int);
                                    met_code])) ta_unit,
                           cl_init)) cl_init.tltype)
            | Cf_let (rec_flag, defs, vals) ->
                let vals =
                  List.map (function (id, _) -> (Ident.name id, id)) vals
                in
                (inh_init, transl_vals cla true (List.map (fun (s,id) -> (s,id)) vals) cl_init)
            | Cf_init exp ->
                (inh_init,
                 build_term (TypLsequence(build_term (TypLapply (oo_prim "add_initializer",
                                   [build_varterm cla; transl_exp exp])) None,
                           cl_init)) None))
          str.cl_field
          (inh_init, cl_init)
      in
      (inh_init, bind_methods cla pub_meths str.cl_meths cl_init)
  | Tclass_fun (pat, vals, cl, _) ->
      let (inh_init, cl_init) =
        build_class_init cla pub_meths cstr inh_init cl_init cl
      in
      let vals = List.map (function (id, _) -> (Ident.name id, id)) vals in
      (inh_init, transl_vals cla true (List.map (fun (s,id) -> (s,id)) vals) cl_init)
  | Tclass_apply (cl, exprs) ->
      build_class_init cla pub_meths cstr inh_init cl_init cl
  | Tclass_let (rec_flag, defs, vals, cl) ->
      let (inh_init, cl_init) =
        build_class_init cla pub_meths cstr inh_init cl_init cl
      in
      let vals = List.map (function (id, _) -> (Ident.name id, id)) vals in
      (inh_init, transl_vals cla true (List.map (fun (s,id) -> (s,id)) vals) cl_init)
  | Tclass_constraint (cl, vals, meths, concr_meths) ->
      let core cl_init =
        build_class_init cla pub_meths true inh_init cl_init cl
      in
      if cstr then
        core cl_init
      else
        let virt_meths =
          List.fold_right
            (fun lab rem ->
               if Concr.mem lab concr_meths then rem else lab::rem)
            meths
            []
        in
        let (inh_init, cl_init) =
          core (build_term (TypLsequence (build_term (TypLapply (oo_prim "widen", [build_varterm cla])) None,
                           cl_init)) None)
        in
        (inh_init,
         build_term (TypLsequence(build_term (TypLapply (oo_prim "narrow",
                           [build_varterm cla;
                             transl_meth_list vals;
                             transl_meth_list virt_meths;
                             transl_meth_list (Concr.elements concr_meths)])) None,
                   cl_init)) None)


(*
   XXX Il devrait etre peu couteux d'ecrire des classes :
     class c x y = d e f
*)
(*
   XXX
   Exploiter le fait que les methodes sont definies dans l'ordre pour
   l'initialisation des classes (et les variables liees par un
   let ???) ?
*)

let transl_class ids cl_id arity pub_meths cl =
  let type_table = camlOO_type_typann "table" in
  let type_class_init = generic_functor_type type_table type_obj_init in
  let cla = Ident.create "class",type_table in
  let (inh_init, obj_init) = build_object_init_0 cla [] cl in
  if not (Translcore.check_recursive_typedlambda ids obj_init) then
    raise(Error(cl.cl_loc, Illegal_class_expr));
  let (inh_init, cl_init) =
    build_class_init cla pub_meths true (List.rev inh_init) obj_init cl
  in
  assert (inh_init = []);
  let table = Ident.create "table",type_table in
  let class_init = Ident.create "class_init",type_class_init in
  let obj_init = Ident.create "obj_init",type_obj_init in
  build_letterm (Strict, table,
       build_term (TypLapply (oo_prim "create_table", [transl_meth_list pub_meths])) type_table,
  build_letterm (Strict, class_init,
       build_term (TypLfunction(Curried, [cla], cl_init)) type_class_init,
  build_letterm (Strict, obj_init, build_term (TypLapply(build_varterm class_init, [build_varterm table])) type_obj_init,
  build_term (TypLsequence(build_term (TypLapply (oo_prim "init_class", [build_varterm table])) ta_unit,
            build_term (TypLprim(Pmakeblock(0, Immutable),
                  [build_varterm obj_init;
                   build_varterm class_init;
                   build_varterm table])) ta_block)) ta_block)))

let class_stub =
  build_term (TypLprim(Pmakeblock(0, Mutable), [lambda_unit; lambda_unit; lambda_unit])) ta_block

(* Error report *)

open Format

let report_error ppf = function
  | Illegal_class_expr ->
      fprintf ppf "This kind of class expression is not allowed"
