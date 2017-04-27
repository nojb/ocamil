(************************************************************************)
(*                                                                      *)
(*                                CamIL                                 *)
(*                                                                      *)
(* created by  Bruno Pagano, projet Cristal, INRIA Rocquencourt (2000)  *)
(* modified by Emmanuel Chailloux & Raphael Montelatici, PPS (2003-2006)*)
(*                                                                      *)
(************************************************************************)

(* $Id: printulambda.mli,v 1.11 2006/10/16 12:45:55 montela Exp $ *)

(* print a (maybe typed) ulambda-term *)

val ulambda : bool -> (Format.formatter -> 'a -> unit) -> Format.formatter ->  'a Ctypedlambda.utypedlambda -> unit

val prtypeinfo : Format.formatter -> Clambda.typeinfo -> unit

