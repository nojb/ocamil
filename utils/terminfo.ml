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

(* $Id: terminfo.ml,v 1.1 2003/09/17 08:14:39 montela Exp $ *)

(* Basic interface to the terminfo database *)

type status =
  | Uninitialised
  | Bad_term
  | Good_term of int
;;
let setup oc = Bad_term
let backup i = raise (Invalid_argument "Terminfo.backup");;
let standout b = raise (Invalid_argument "Terminfo.standout");;
let resume i = raise (Invalid_argument "Terminfo.resume");;

