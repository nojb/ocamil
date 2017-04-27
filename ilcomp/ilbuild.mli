(************************************************************************)
(*                                                                      *)
(*                                CamIL                                 *)
(*                                                                      *)
(* created by  Bruno Pagano, projet Cristal, INRIA Rocquencourt (2000)  *)
(* modified by Emmanuel Chailloux & Raphael Montelatici, PPS (2003-2006)*)
(*                                                                      *)
(************************************************************************)

val class_of :  Il.il_compilation_unit -> Il.nsid -> Il.id -> Il.typedef
val method_of : Il.typedef -> Il.id -> Il.methode
val field_of : Il.typedef -> Il.id -> Il.field
val new_class : Il.il_compilation_unit -> Il.nsid -> Il.id -> Il.typedef
val new_field : Il.typedef -> Il.id -> Il.elementType -> Il.field
val new_method : Il.typedef -> Il.id -> Il.elementType -> Il.signature -> Il.methode
val new_ctor : Il.typedef -> Il.signature -> Il.methode
val add_catt : Il.typedef -> Il.classattribute -> unit
val add_extd : Il.typedef -> Il.typeref -> unit
val add_cfld : Il.typedef -> Il.id -> Il.elementType -> unit
val add_sfld : Il.typedef -> Il.id -> Il.elementType -> unit
val add_fatt : Il.field -> Il.fieldattribute -> unit
val add_matt : Il.methode -> Il.methodattribute -> unit
val add_miat : Il.methode -> Il.implementattribute -> unit
val add_inst : Il.methode -> Il.instruction -> unit
val add_locals : Il.methode -> Il.id -> Il.elementType -> unit

val real_loc : Il.methode -> Il.id -> Il.id

val create_local : Il.methode -> Il.id -> Il.elementType -> unit
val release_loc : Il.methode -> Il.id -> unit
val assoc_loc : Il.methode -> Il.id -> int -> unit

val init_label : Il.methode -> Il.label
val new_label : Il.methode -> Il.label
val new_register : Il.methode -> Il.elementType -> Il.id 

val type_ref : Il.nsid -> Il.id -> Il.typeref
val method_ref :  Il.callingconvention -> Il.typeref -> Il.id -> Il.elementType -> Il.signature -> Il.methodref
val ctor_ref : Il.typeref -> Il.signature -> Il.methodref
val field_ref : Il.typeref -> Il.id -> Il.elementType -> Il.fieldref

val typeref_of_typedef : Il.typedef -> Il.typeref
val fieldref_of_fielddef : Il.typedef -> Il.field -> Il.fieldref
val methodref_of_methode : Il.typedef -> Il.methode -> Il.methodref

