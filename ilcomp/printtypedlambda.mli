(************************************************************************)
(*                                                                      *)
(*                                CamIL                                 *)
(*                                                                      *)
(* created by  Bruno Pagano, projet Cristal, INRIA Rocquencourt (2000)  *)
(* modified by Emmanuel Chailloux & Raphael Montelatici, PPS (2003-2006)*)
(*                                                                      *)
(************************************************************************)

val type_expr_option : Format.formatter -> Typedlambda.typing_annotation option -> unit
val typedlambda : Format.formatter -> Typedlambda.typedlambda -> unit
