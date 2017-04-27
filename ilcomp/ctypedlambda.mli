(************************************************************************)
(*                                                                      *)
(*                                CamIL                                 *)
(*                                                                      *)
(* created by  Bruno Pagano, projet Cristal, INRIA Rocquencourt (2000)  *)
(* modified by Emmanuel Chailloux & Raphael Montelatici, PPS (2003-2006)*)
(*                                                                      *)
(************************************************************************)

type 'a ti_structured_constant =
    TypTConst_base of Asttypes.constant
  | TypTConst_pointer of int
  | TypTConst_block of int * ('a ti_structured_constant * 'a) list
  | TypTConst_float_array of string list
and 'a utypedident = Ident.t * 'a
and 'a utypedlambda = { utlterm : 'a utypedlambdaterm; utltype : 'a; } 
and 'a utypedlambdaterm =
    TypUvar of Ident.t
  | TypUconst of 'a ti_structured_constant
  | TypUdirect_apply of Clambda.function_label * 'a utypedlambda list
  | TypUgeneric_apply of 'a utypedlambda * 'a utypedlambda list
  | TypUclosure of
      (Clambda.function_label * int * 'a utypedident list * 'a utypedlambda)
      list * 'a utypedlambda list
  | TypUoffset of 'a utypedlambda * int
  | TypUlet of 'a utypedident * 'a utypedlambda * 'a utypedlambda
  | TypUletrec of ('a utypedident * 'a utypedlambda) list * 'a utypedlambda
  | TypUprim of Lambda.primitive * 'a utypedlambda list
  | TypUswitch of 'a utypedlambda * 'a utypedlambda_switch
  | TypUstaticfail of int * 'a utypedlambda list
  | TypUcatch of int * 'a utypedident list * 'a utypedlambda *
      'a utypedlambda
  | TypUtrywith of 'a utypedlambda * 'a utypedident * 'a utypedlambda
  | TypUifthenelse of 'a utypedlambda * 'a utypedlambda * 'a utypedlambda
  | TypUsequence of 'a utypedlambda * 'a utypedlambda
  | TypUwhile of 'a utypedlambda * 'a utypedlambda
  | TypUfor of 'a utypedident * 'a utypedlambda * 'a utypedlambda *
      Asttypes.direction_flag * 'a utypedlambda
  | TypUassign of 'a utypedident * 'a utypedlambda
  | TypUsend of 'a utypedlambda * 'a utypedlambda * 'a utypedlambda list
and 'a utypedlambda_switch = {
  tus_index_consts : int array;
  tus_actions_consts : 'a utypedlambda array;
  tus_index_blocks : int array;
  tus_actions_blocks : 'a utypedlambda array;
} 
and function_description = {
  fun_label : Clambda.function_label;
  fun_arity : int;
  mutable fun_closed : bool;
} 
and value_approximation =
    Value_closure of function_description * value_approximation
  | Value_tuple of value_approximation array
  | Value_unknown
  | Value_integer of int
  | Value_constptr of int

val build_uterm : 'a utypedlambdaterm -> 'a -> 'a utypedlambda
val build_uvarterm : Ident.t * 'a -> 'a utypedlambda
val build_uterm_back : 'a utypedlambdaterm -> 'a -> 'a utypedlambda
val build_usequenceterm : Clambda.typeinfo utypedlambda * Clambda.typeinfo utypedlambda -> Clambda.typeinfo -> Clambda.typeinfo utypedlambda
val build_uletterm : Clambda.typeinfo utypedident * Clambda.typeinfo utypedlambda * Clambda.typeinfo utypedlambda -> Clambda.typeinfo -> Clambda.typeinfo utypedlambda
val build_ucatchterm : int * Clambda.typeinfo utypedident list * Clambda.typeinfo utypedlambda * Clambda.typeinfo utypedlambda -> Clambda.typeinfo -> Clambda.typeinfo utypedlambda
val build_uifthenelseterm : Clambda.typeinfo utypedlambda * Clambda.typeinfo utypedlambda *  Clambda.typeinfo utypedlambda -> Clambda.typeinfo -> Clambda.typeinfo utypedlambda
val build_uswitchterm : Clambda.typeinfo utypedlambda * Clambda.typeinfo utypedlambda_switch -> Clambda.typeinfo -> Clambda.typeinfo utypedlambda
val arrow_apply : Clambda.typeinfo -> int -> Clambda.typeinfo
val build_uprimterm : Lambda.primitive * Clambda.typeinfo utypedlambda list -> Clambda.typeinfo -> Clambda.typeinfo utypedlambda

val to_ulambda_const : 'a ti_structured_constant -> Lambda.structured_constant
val to_ulambda : 'a utypedlambda -> Clambda.ulambda

module HashedPath : Hashtbl.S with type key = Clambda.namedtype

type namedtype_info =
    NTIrecord of (string * Clambda.typeinfo) list
  | NTIvariant of (string * Clambda.typeinfo list) list

val algebtypes_table : namedtype_info HashedPath.t
val get_record_description : HashedPath.key -> (string * Clambda.typeinfo) list
val get_variant_description : HashedPath.key -> (string * Clambda.typeinfo list) list
val list_id : Clambda.namedtype
val list_cstrs : (string * Clambda.typeinfo list) list
val option_id : Clambda.namedtype
val option_cstrs : (string * Clambda.typeinfo list) list
val init_algebtypes_table : unit -> unit

val convert_typeannotation : bool -> Env.t -> string list -> Types.type_expr -> Clambda.typeinfo

val get_accurate_typeinfo : Typedlambda.typing_annotation option -> Clambda.typeinfo
val const_accurate_typeinfo : Typedlambda.typed_structured_constant -> Clambda.typeinfo ti_structured_constant
