(************************************************************************)
(*                                                                      *)
(*                                CamIL                                 *)
(*                                                                      *)
(* created by  Bruno Pagano, projet Cristal, INRIA Rocquencourt (2000)  *)
(* modified by Emmanuel Chailloux & Raphael Montelatici, PPS (2003-2006)*)
(*                                                                      *)
(************************************************************************)

type assembly
val read_assemblyinfo : string -> (string * string) * assembly option
val read_assemblyinfo_of_core_camil : unit -> (string * string) * assembly option
val read_assemblyinfo_of_self : unit -> (string * string) * assembly option
