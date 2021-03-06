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

(* $Id: output.mli,v 1.6 2002/01/04 10:01:49 ddr Exp $ *)

(* Output the DFA tables and its entry points *)

val output_lexdef:
      string -> in_channel -> out_channel ->
      Syntax.location ->
      Compact.lex_tables ->
      (unit, Syntax.location) Lexgen.automata_entry list ->
      Syntax.location ->
      unit

exception Table_overflow
