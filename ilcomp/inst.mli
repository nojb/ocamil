(************************************************************************)
(*                                                                      *)
(*                                CamIL                                 *)
(*                                                                      *)
(* created by  Bruno Pagano, projet Cristal, INRIA Rocquencourt (2000)  *)
(* modified by Emmanuel Chailloux & Raphael Montelatici, PPS (2003-2006)*)
(*                                                                      *)
(************************************************************************)


val push : Il.methode -> Il.elementType -> unit
val spop : Il.methode -> unit
val nspop : Il.methode -> int -> unit
val change_stack : Il.methode -> Il.elementType -> unit
val force_stack : Il.methode -> Il.elementType -> unit
val dump_stack : Il.methode -> Il.elementType list * int
val restore_stack : Il.methode -> Il.elementType list * int -> unit
val comment : Il.methode -> string -> unit
val label : Il.methode -> Il.label * (Il.elementType list * int) -> unit
val label_force : Il.methode -> Il.label * (Il.elementType list * int) -> unit
val label_follow : Il.methode -> Il.label -> unit
val try_main : Il.methode -> unit
val try_catch : Il.methode -> unit
val try_end : Il.methode -> unit
val add : Il.methode -> unit
val _and : Il.methode -> unit
val beq : Il.methode -> Il.label -> Il.label * (Il.elementType list * int)
val bgt : Il.methode -> Il.label -> Il.label * (Il.elementType list * int)
val blt : Il.methode -> Il.label -> Il.label * (Il.elementType list * int)
val bge : Il.methode -> Il.label -> Il.label * (Il.elementType list * int)
val ble : Il.methode -> Il.label -> Il.label * (Il.elementType list * int)
val br : Il.methode -> Il.label -> Il.label * (Il.elementType list * int)
val brfalse : Il.methode -> Il.label -> Il.label * (Il.elementType list * int)
val brtrue : Il.methode -> Il.label -> Il.label * (Il.elementType list * int)
val convi4 : Il.methode -> unit
val convi8 : Il.methode -> unit
val convi : Il.methode -> unit
val ceq : Il.methode -> unit
val cgt : Il.methode -> unit
val clt : Il.methode -> unit
val cgt_un : Il.methode -> unit
val clt_un : Il.methode -> unit
val div : Il.methode -> unit
val dup : Il.methode -> unit
val ldarg : Il.methode -> int -> unit
val ldci4 : Il.methode -> int -> unit
val ldci8 : Il.methode -> int64 -> unit
val ldcr8 : Il.methode -> float -> unit
val ldindi1 : Il.methode -> unit
val ldindi2 : Il.methode -> unit
val ldindi4 : Il.methode -> unit
val ldindi8 : Il.methode -> unit
val ldindi : Il.methode -> unit
val ldindr8 : Il.methode -> unit
val ldelem_ref : Il.methode -> unit
val ldelem_char : Il.methode -> unit
val ldlen : Il.methode -> unit
val ldloc : Il.methode -> int -> Il.elementType -> unit
val ldnull : ?of_type:Il.elementType -> Il.methode -> unit
val ldstr : Il.methode -> string -> unit
val leave : Il.methode -> Il.label -> Il.label * (Il.elementType list * int)
val mul : Il.methode -> unit
val neg : Il.methode -> unit
val _or : Il.methode -> unit
val pop : Il.methode -> unit
val rem : Il.methode -> unit
val ret : Il.methode -> unit
val rethrow : Il.methode -> unit
val shl : Il.methode -> unit
val shr : Il.methode -> unit
val shr_un : Il.methode -> unit
val stelem_ref : Il.methode -> unit
val stelem_char : Il.methode -> unit
val stloc : Il.methode -> int -> unit
val starg : Il.methode -> int -> unit
val switch :  Il.methode -> Il.label list -> (Il.label * (Il.elementType list * int)) list
val sub : Il.methode -> unit
val tail : Il.methode -> unit
val _xor : Il.methode -> unit
val box : Il.methode -> Il.typeref -> unit
val castclass : Il.methode -> Il.typeref -> unit
val newarr : Il.methode -> Il.typeref -> unit
val unbox : Il.methode -> Il.typeref -> unit
val isinst : Il.methode -> Il.typeref -> unit
val box_spec : Il.methode -> Il.elementType -> unit
val castclass_spec : Il.methode -> Il.elementType -> unit
val newarr_spec : Il.methode -> Il.elementType -> unit
val unbox_spec : Il.methode -> Il.elementType -> unit
val isinst_spec : Il.methode -> Il.elementType -> unit
val call : Il.methode -> Il.typeref -> Il.id -> Il.elementType -> Il.signature -> Il.callingconvention -> unit
val callvirt : Il.methode -> Il.typeref -> Il.id -> Il.elementType -> Il.signature -> unit
val newobj : Il.methode -> Il.typeref -> Il.id -> Il.elementType -> Il.signature -> unit
val ldfld : Il.methode -> Il.fieldref -> unit
val ldsfld : Il.methode -> Il.fieldref -> unit
val stfld : Il.methode -> Il.fieldref -> unit
val stsfld : Il.methode -> Il.fieldref -> unit
val call_method : Il.methode -> Il.methodref -> unit
val callvirt_method : Il.methode -> Il.methodref -> unit
val newobj_ctor : Il.methode -> Il.methodref -> unit
val ldarg_name : Il.methode -> Il.id -> unit
val ldfld_field : Il.methode -> Il.fieldref -> Il.callingconvention -> unit
val ldloc_name : Il.methode -> Il.id -> unit
val newobj_class : Il.methode -> Il.typedef -> unit
val newobj_class_with_size : Il.methode -> Il.typedef -> int -> unit
val stfld_field : Il.methode -> Il.fieldref -> Il.callingconvention -> unit
val stloc_name : Il.methode -> Il.id -> unit
val new_local : Il.methode -> Il.id -> Il.elementType -> unit
val register_stack : Il.methode -> Il.id list
val stack_register : Il.methode -> Il.id list -> unit
val lbl_st_rest : (Il.label * Il.id list) list ref
val remember_lbl_st_restore : Il.methode -> Il.label -> unit
val execute_lbl_st_restore : Il.methode -> Il.label -> unit
val br_for_staticfail : Il.methode -> Il.label -> unit
val compose_casts : Il.castsort -> Il.castsort -> Il.castsort
val castsort_implicit : bool -> Il.elementType -> Il.castsort
