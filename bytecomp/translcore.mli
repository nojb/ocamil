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

(* $Id: translcore.mli,v 1.6 2005/01/04 09:50:36 montela Exp $ *)

(* Translation from typed abstract syntax to lambda terms,
   for the core language *)

open Asttypes
open Types
open Typedtree
open Lambda
open Typedlambda

val name_pattern: string -> (pattern * 'a) list -> Ident.t

val transl_exp: expression -> typedlambda
val transl_apply: typedlambda -> (expression option * optional) list -> typing_annotation option -> typedlambda
val transl_let:
      rec_flag -> (pattern * expression) list -> typedlambda -> typedlambda
val transl_primitive: Primitive.description -> typedlambda
val transl_primitive_T : Primitive.description -> Types.type_expr -> Env.t -> typedlambda
val transl_exception:
      Ident.t -> Path.t option -> exception_declaration -> typedlambda

val check_recursive_lambda: Ident.t list -> lambda -> bool
val check_recursive_typedlambda: typedident list -> typedlambda -> bool

type error =
    Illegal_letrec_pat
  | Illegal_letrec_expr
  | Free_super_var

exception Error of Location.t * error

open Format

val report_error: formatter -> error -> unit

(* Forward declaration -- to be filled in by Translmod.transl_module *)
val transl_module :
      (module_coercion -> Path.t option -> module_expr -> typedlambda) ref
