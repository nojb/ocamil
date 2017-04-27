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

(* $Id: simplif.mli,v 1.1 2004/08/07 09:10:01 montela Exp $ *)

(* Elimination of useless Llet(Alias) bindings *)

open Typedlambda

val simplify_lambda: typedlambda -> typedlambda
