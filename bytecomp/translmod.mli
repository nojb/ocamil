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

(* $Id: translmod.mli,v 1.9 2000/06/12 14:22:37 garrigue Exp $ *)

(* Translation from typed abstract syntax to lambda terms,
   for the module language *)

open Typedtree
open Lambda

val transl_implementation: string -> structure * module_coercion -> lambda
val transl_store_implementation:
      string -> structure * module_coercion -> int * lambda
val transl_toplevel_definition: structure -> lambda
val toplevel_name: Ident.t -> string

val primitive_declarations: string list ref
