(************************************************************************)
(*                                                                      *)
(*                                CamIL                                 *)
(*                                                                      *)
(* created by  Bruno Pagano, projet Cristal, INRIA Rocquencourt (2000)  *)
(* modified by Emmanuel Chailloux & Raphael Montelatici, PPS (2003-2006)*)
(*                                                                      *)
(************************************************************************)

val core_camil_assbly: string

val quote : string -> string
val quote_qualified : string -> string
val unbracket : string -> string

val init_name_table : string -> unit

val get_unit_id : unit -> Il.id 
val new_class_name : Il.id -> string -> Il.id
val new_func_label : Clambda.function_label -> unit
val new_field_name : Il.typeref -> string -> Il.id
val new_local_name : Il.typeref -> Il.id -> Ident.t -> string

