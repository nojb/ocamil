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

(* $Id: terminfo.mli,v 1.1 2003/09/17 08:14:40 montela Exp $ *)

(* Basic interface to the terminfo database *)

type status =
  | Uninitialised
  | Bad_term
  | Good_term of int  (* number of lines of the terminal *)
;;
val setup : out_channel -> status
val backup : int -> unit
val standout : bool -> unit
val resume : int -> unit
