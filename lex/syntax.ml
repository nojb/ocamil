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

(* $Id: syntax.ml,v 1.5 2002/01/04 10:01:50 ddr Exp $ *)

(* The shallow abstract syntax *)

type location =
    { start_pos: int;
      end_pos: int;
      start_line: int;
      start_col: int }

type regular_expression =
    Epsilon
  | Characters of int list
  | Sequence of regular_expression * regular_expression
  | Alternative of regular_expression * regular_expression
  | Repetition of regular_expression

type lexer_definition =
    { header: location;
      entrypoints: ((string * unit) * (regular_expression * location) list) list;
      trailer: location }
