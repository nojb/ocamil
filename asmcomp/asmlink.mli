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

(* $Id: asmlink.mli,v 1.4 2004/07/11 21:37:47 montela Exp $ *)

(* Link a set of .cmx/.o files and produce an executable *)

open Format

(*val link: formatter -> string list -> string -> unit*)
val check_consistency: string -> Compilenv.unit_infos -> Digest.t -> unit
val extract_crc_interfaces: unit -> (string * Digest.t) list
val extract_crc_implementations: unit -> (string * Il.typeref *Digest.t) list

type error =
    File_not_found of string
  | Not_an_object_file of string
  | Missing_implementations of (string * string list) list
  | Inconsistent_interface of string * string * string
  | Inconsistent_implementation of string * string * string
  | Assembler_error of string
  | Linking_error
  | Multiple_definition of string * string * string
  | Inconsistent_compilation_modes of string*Clflags.compmode*Clflags.compmode
exception Error of error

val report_error: formatter -> error -> unit
