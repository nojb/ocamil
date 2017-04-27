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

(* $Id: matching.mli,v 1.2 2004/07/28 13:02:24 montela Exp $ *)

(* Compilation of pattern-matching *)

open Typedtree
open Typedlambda

val for_function:
        Location.t -> int ref option -> typedlambda -> (pattern * typedlambda) list ->
        partial -> typedlambda
val for_trywith:
        typedlambda -> (pattern * typedlambda) list -> typedlambda
val for_let:
        Location.t -> typedlambda -> pattern -> typedlambda -> typedlambda
val for_multiple_match:
        Location.t -> typedlambda list -> (pattern * typedlambda) list -> partial ->
        typedlambda

val for_tupled_function:
        Location.t -> typedident list -> (pattern list * typedlambda) list ->
        partial -> typedlambda

exception Cannot_flatten

val flatten_pattern: int -> pattern -> pattern list
