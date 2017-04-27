(* camlp4r *)
(***********************************************************************)
(*                                                                     *)
(*                             Camlp4                                  *)
(*                                                                     *)
(*        Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id: reloc.mli,v 1.2 2002/07/19 14:53:44 mauny Exp $ *)

value patt : (MLast.loc -> MLast.loc) -> int -> MLast.patt -> MLast.patt;
value expr : (MLast.loc -> MLast.loc) -> int -> MLast.expr -> MLast.expr;
