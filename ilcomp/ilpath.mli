(************************************************************************)
(*                                                                      *)
(*                                CamIL                                 *)
(*                                                                      *)
(* created by  Bruno Pagano, projet Cristal, INRIA Rocquencourt (2000)  *)
(* modified by Emmanuel Chailloux & Raphael Montelatici, PPS (2003-2006)*)
(*                                                                      *)
(************************************************************************)

(* $Id: ilpath.mli,v 1.13 2006/10/16 12:45:55 montela Exp $ *)

val find_codebase : string -> string
val stdlib_items : string list
val get_token_version : string -> string*string
val get_assembly_object : string -> Painfo.assembly
val get_token_self : unit -> string
val path_to_url : string -> string

val build_extref: string -> Il.assemblyref
val build_extscope: string -> Il.assemblyscope

