(************************************************************************)
(*                                                                      *)
(*                                CamIL                                 *)
(*                                                                      *)
(* created by  Bruno Pagano, projet Cristal, INRIA Rocquencourt (2000)  *)
(* modified by Emmanuel Chailloux & Raphael Montelatici, PPS (2003-2006)*)
(*                                                                      *)
(************************************************************************)

(* $Id: ilmcompile.mli,v 1.1 2006/10/16 12:45:55 montela Exp $ *)


val comp_unit:  Ident.t -> int -> Clambda.typeinfo Ctypedlambda.utypedlambda -> Ilm.tunitdec


