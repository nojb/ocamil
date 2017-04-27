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

(* $Id: optcompile.mli,v 1.1 2003/03/18 15:57:24 emmanuel Exp $ *)

(* Compile a .ml or .mli file *)

open Format

val interface: formatter -> string -> unit
val implementation: formatter -> string -> unit
val c_file: string -> unit

val initial_env: unit -> Env.t
val init_path: unit -> unit
