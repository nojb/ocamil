(************************************************************************)
(*                                                                      *)
(*                                CamIL                                 *)
(*                                                                      *)
(* created by  Bruno Pagano, projet Cristal, INRIA Rocquencourt (2000)  *)
(* modified by Emmanuel Chailloux & Raphael Montelatici, PPS (2003-2006)*)
(*                                                                      *)
(************************************************************************)

val toplevel_deftypes_listener_hook : (string -> unit) ref
val tintfdec : string -> Il.il_compilation_unit -> unit
val tunitdec : Ilm.tunitdec -> Il.il_compilation_unit -> unit
