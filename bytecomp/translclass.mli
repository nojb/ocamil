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

(* $Id: translclass.mli,v 1.1 2004/07/28 13:02:24 montela Exp $ *)

open Typedtree
open Lambda
open Typedlambda

val class_stub : typedlambda
val transl_class :
  typedident list -> Ident.t -> int -> string list -> class_expr -> typedlambda;;

type error = Illegal_class_expr

exception Error of Location.t * error

open Format

val report_error: formatter -> error -> unit
