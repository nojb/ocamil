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

(* $Id: translmod.mli,v 1.5 2006/07/23 03:04:14 montela Exp $ *)

(* Translation from typed abstract syntax to lambda terms,
   for the module language *)

open Typedtree
open Lambda
open Typedlambda

val transl_implementation: string -> structure * module_coercion -> int * typedlambda * Ident.t
val transl_store_implementation: string -> structure * module_coercion -> int * typedlambda * Ident.t
val transl_toplevel_definition: structure -> typedlambda
val toplevel_name: Ident.t -> string

val primitive_declarations: string list ref

(* Ajout propag Camil *)
(*val get_globals_types: unit -> typing_annotation*)
