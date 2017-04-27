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

(* $Id: closure.mli,v 1.5 2005/12/25 04:01:25 montela Exp $ *)

(* Introduction of closures, uncurrying, recognition of direct calls *)

val occurs_var_untyped : Ident.t -> Clambda.ulambda -> bool (* encore utilisé dans compint *)
val occurs_var : Ident.t -> Clambda.typeinfo Ctypedlambda.utypedlambda -> bool 
val intro: int -> Typedlambda.typedlambda -> Clambda.typeinfo Ctypedlambda.utypedlambda

