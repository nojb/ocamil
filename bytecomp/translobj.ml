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

(* $Id: translobj.ml,v 1.3 2006/04/24 00:19:51 montela Exp $ *)

open Misc
open Asttypes
open Longident
open Lambda
open Typedlambda
open Types

(* Get oo primitives identifiers *)

let oo_prim name =
  try
    transl_path
      (fst (Env.lookup_value (Ldot (Lident "CamlinternalOO", name)) Env.empty))
  with Not_found ->
    fatal_error ("Primitive " ^ name ^ " not found.")

(* Collect labels *)

let used_methods = ref ([] : (string * Ident.t) list);;

let meth lab =
  try
    List.assoc lab !used_methods
  with Not_found ->
    let id = Ident.create lab in
    used_methods := (lab, id)::!used_methods;
    id

let reset_labels () =
  used_methods := []

(* Insert labels *)

let string s = build_term (TypLconst (TConst_base (Const_string s))) (coretype_annotation Predef.type_string)

let transl_label_init expr =
  let tint = coretype_annotation Predef.type_int in
  if !used_methods = [] then
    expr
  else
    let init = Ident.create "new_method",(coretype_annotation (Btype.newgenty (Tarrow("",Predef.type_string,Predef.type_int,Cunknown)))) (* string->int*) in
    let expr' =
      build_letterm (StrictOpt, init, oo_prim "new_method",
      List.fold_right
        (fun (lab, id) expr ->
           build_letterm (StrictOpt, (id,tint), build_term (TypLapply(build_varterm init, [string lab])) tint, expr))
        !used_methods
        expr)
    in
    reset_labels ();
    expr'
