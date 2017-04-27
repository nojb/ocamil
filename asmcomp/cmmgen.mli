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

(* $Id: cmmgen.mli,v 1.1 2003/03/18 15:57:16 emmanuel Exp $ *)

(* Translation from closed lambda to C-- *)

val compunit: int -> Clambda.ulambda -> Cmm.phrase list

val package: string list -> string -> Cmm.phrase list
val apply_function: int -> Cmm.phrase
val curry_function: int -> Cmm.phrase list
val entry_point: string list -> Cmm.phrase
val global_table: string list -> Cmm.phrase
val globals_map: (string * string) list -> Cmm.phrase
val frame_table: string list -> Cmm.phrase
val data_segment_table: string list -> Cmm.phrase
val code_segment_table: string list -> Cmm.phrase
val predef_exception: string -> Cmm.phrase
