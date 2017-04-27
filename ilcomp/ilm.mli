(************************************************************************)
(*                                                                      *)
(*                                CamIL                                 *)
(*                                                                      *)
(* created by  Bruno Pagano, projet Cristal, INRIA Rocquencourt (2000)  *)
(* modified by Emmanuel Chailloux & Raphael Montelatici, PPS (2003-2006)*)
(*                                                                      *)
(************************************************************************)

val qualifiedtype_of_namedtype : Clambda.namedtype -> string * string
val record_classes : Il.typeref Ctypedlambda.HashedPath.t
val variant_classes : Il.typeref list Ctypedlambda.HashedPath.t
val init_tables : unit -> unit
val record_class_server : Ctypedlambda.HashedPath.key -> Il.typeref
val variant_class_server :  Ctypedlambda.HashedPath.key -> int option -> Il.typeref

type ctstype =
    CTvoid
  | CTobject
  | CTint
  | CTbint
  | CTint32
  | CTbint32
  | CTint64
  | CTbint64
  | CTnint
  | CTbnint
  | CTchar
  | CTbchar
  | CTfloat
  | CTbfloat
  | CTbool
  | CTbbool
  | CTstring
  | CTstrbld
  | CTexception
  | CTarray of ctstype
  | CTlist of ctstype
  | CTrecord of Clambda.namedtype
  | CTtuple of ctstype list
  | CTvariant of Clambda.namedtype * int option
  | CTlazy of ctstype
  | CTclosure of Il.typeref
  | CTgenclosure
  | CTsharedclosure
  | CTclass of Il.typeref

val get_closure_repr : ctstype -> Il.typeref
val is_variant : ctstype -> bool
val is_immediate : ctstype -> bool

val get_comparison_method : ctstype -> Il.methodref
val get_equal_method : ctstype -> Il.methodref
val get_hashcode_method : ctstype -> Il.methodref
val is_immediate_typeinfo : Clambda.typeinfo -> bool

val ctstype_of_typeinfo : Clambda.typeinfo -> ctstype

val to_il : ctstype -> Il.elementType
val of_il : Il.elementType -> ctstype

val str_class : Il.typeref -> string (* ?? !! *)
val to_string : ctstype -> string
val type_printer : Format.formatter -> ctstype -> unit

type locl_tid = { lid : Il.id; mutable ltype : ctstype; } 
and fild_tid = { did : Il.typeref * Il.id; mutable dtype : ctstype; } 
and tconstant =
    Tconst_int of int
  | Tconst_char of char
  | Tconst_bool of bool
  | Tconst_float of string
  | Tconst_bfloat of string
  | Tconst_string of ctstype * string
  | Tconst_null
and newprim =
    TP_get_builtin of string
  | TP_get_field of fild_tid
  | TP_set_field of fild_tid
  | TP_get_global of Il.nsid
  | TP_pushint of int
  | TP_mktop of Il.typeref
  | TP_mkenv of Il.typeref
  | TP_mkrec of Il.typeref
  | TP_mkclos of Il.typeref
  | TP_buildobject of Il.typeref * Il.signature * Il.fieldref list
  | TP_raise of ctstype
  | TP_set_block of int
  | TP_get_block of int
  | TP_string_length of ctstype
  | TP_string_set of ctstype * bool
  | TP_string_ref of ctstype * bool
  | TP_stringcomp of Lambda.comparison
  | TP_eq
  | TP_neq
  | TP_cast of ctstype * ctstype
  | TP_pushdummy of ctstype
  | TP_convint of Lambda.boxed_integer
  | TP_offsetref of int
and tprimitive = CPlegacyprim of Lambda.primitive | CPnewprim of newprim
and tlambda =
    Tconst of tconstant
  | Tdirect_apply of Clambda.function_label * tlambda list
  | Tgeneric_apply of tlambda * tlambda list
  | Tlet of locl_tid * tlambda * tlambda
  | Tletrec of (locl_tid * tlambda) list * tlambda
  | Tprim of tprimitive * tlambda list
  | Tswitch of tlambda * ctstype * tlambda_switch
  | Tstaticfail of int * tlambda list
  | Tcatch of int * locl_tid list * tlambda * tlambda
  | Ttrywith of tlambda * locl_tid * tlambda
  | Tifthenelse of tlambda * tlambda * tlambda
  | Tsequence of tlambda * tlambda
  | Twhile of tlambda * tlambda
  | Tfor of locl_tid * tlambda * tlambda * Asttypes.direction_flag * tlambda
  | Tassign of locl_tid * tlambda
  | Tsend of tlambda * tlambda * tlambda list
  | Tifvar of tlambda * tlambda * tlambda
  | Tlocal of locl_tid
  | Targument of locl_tid
  | Tnop
and tlambda_switch = { ts_indexes : int array; ts_actions : tlambda array; } 
and tfunction_status =
    TFStop
  | TFSenv of fild_tid list
  | TFSrec of Il.typeref
and tfundec = {
  tfd_id : Clambda.function_label;
  tfd_cur : bool;
  tfd_sts : tfunction_status;
  tfd_var : locl_tid list;
  tfd_exe : tlambda;
  tfd_rt : ctstype;
} 
and tclasskind = CKrecord | CKvariant of int | CKsharedclosure
and tclassdec = {
  tcd_kind : tclasskind;
  tcd_id : Il.typeref;
  tcd_fld : locl_tid list;
} 
and tunitdec = {
  tud_id : Il.nsid;
  tud_fd : tfundec list;
  tud_cd : tclassdec list;
  tud_size : int;
  tud_start : tlambda;
} 
