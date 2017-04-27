(************************************************************************)
(*                                                                      *)
(*                                CamIL                                 *)
(*                                                                      *)
(* created by  Bruno Pagano, projet Cristal, INRIA Rocquencourt (2000)  *)
(* modified by Emmanuel Chailloux & Raphael Montelatici, PPS (2003-2006)*)
(*                                                                      *)
(************************************************************************)

val fl : Format.formatter -> unit -> unit
val sp : Format.formatter -> unit -> unit
val exec : string -> unit
val compile_ilcompunit :
  Format.formatter -> Il.il_compilation_unit -> int * Typedlambda.typedlambda * Ident.t -> Il.il_compilation_unit
val compile_implementation :
  string -> Format.formatter -> int * Typedlambda.typedlambda * Ident.t -> unit
val compile_interface :
  string -> Env.t -> Parsetree.signature -> unit

val missing_globals : (string, string list ref) Hashtbl.t
val is_required : string -> bool
val add_required : string -> string * 'a * 'b -> unit
val remove_required : string -> unit
val extract_missing_globals : unit -> (string * string list) list
val scan_file :
  string ->
  (Compilenv.unit_infos * string * Digest.t) list ->
  (Compilenv.unit_infos * string * Digest.t) list
val output_config_file : string -> unit
val create_cmxa : string -> string -> unit
val detect_missing_implementations : unit -> unit
val write_external_assemblies_references : Il.il_compilation_unit -> unit
val link : string list -> unit

val write_assembly_header : Il.il_compilation_unit -> string -> string -> unit
val write_MLTop_fields  : Il.typedef -> unit
val write_MLTop_startupAll_setup_fields : Il.methode -> unit

val link_resolve_references_plainIL : out_channel -> string -> string -> string list -> unit
val link_resolve_references : Il.il_compilation_unit -> out_channel -> string -> string -> string list -> unit
val link_resolve_references_toplevel: Il.il_compilation_unit -> Obj.t

val new_assemblyref : string -> string -> unit
val toplevel_deftypes_association : (string * Il.assemblyref) list ref
val external_assemblies : unit -> string list
val initialize_assemblyref : unit -> unit
