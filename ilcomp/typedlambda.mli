(************************************************************************)
(*                                                                      *)
(*                                CamIL                                 *)
(*                                                                      *)
(* created by  Bruno Pagano, projet Cristal, INRIA Rocquencourt (2000)  *)
(* modified by Emmanuel Chailloux & Raphael Montelatici, PPS (2003-2006)*)
(*                                                                      *)
(************************************************************************)

 type typing_annotation = {
  tapath : string list;
  taexpr : Types.type_expr;
  taenv : Env.t;
} 
val lengthen_typepath : string -> unit
val shorten_typepath : unit -> unit
val build_type_annotation : Types.type_expr -> Env.t -> typing_annotation option
val coretype_annotation : Types.type_expr -> typing_annotation option
val generic_module_type : typing_annotation option
val generic_functor_type : typing_annotation option -> typing_annotation option -> typing_annotation option
val generic_moditem_type : typing_annotation option
val arrow_apply_camltypes : typing_annotation option -> int -> typing_annotation option
type typed_structured_constant =
    TConst_base of Asttypes.constant
  | TConst_pointer of int
  | TConst_block of int *
      (typed_structured_constant * typing_annotation option) list
  | TConst_float_array of string list
and typedident = Ident.t * typing_annotation option
and typedlambda = {
  tlterm : typedlambdaterm;
  tltype : typing_annotation option;
} 
and typedlambdaterm =
    TypLvar of Ident.t
  | TypLconst of typed_structured_constant
  | TypLapply of typedlambda * typedlambda list
  | TypLfunction of Lambda.function_kind * typedident list * typedlambda
  | TypLlet of Lambda.let_kind * typedident * typedlambda * typedlambda
  | TypLletrec of (typedident * typedlambda) list * typedlambda
  | TypLprim of Lambda.primitive * typedlambda list
  | TypLswitch of typedlambda * typedlambda_switch
  | TypLstaticraise of int * typedlambda list
  | TypLstaticcatch of typedlambda * (int * typedident list) * typedlambda
  | TypLtrywith of typedlambda * typedident * typedlambda
  | TypLifthenelse of typedlambda * typedlambda * typedlambda
  | TypLsequence of typedlambda * typedlambda
  | TypLwhile of typedlambda * typedlambda
  | TypLfor of typedident * typedlambda * typedlambda *
      Asttypes.direction_flag * typedlambda
  | TypLassign of typedident * typedlambda
  | TypLsend of typedlambda * typedlambda * typedlambda list
  | TypLevent of typedlambda * Lambda.lambda_event
  | TypLifused of typedident * typedlambda
and typedlambda_switch = {
  tsw_numconsts : int;
  tsw_consts : (int * typedlambda) list;
  tsw_numblocks : int;
  tsw_blocks : (int * typedlambda) list;
  tsw_failaction : typedlambda option;
} 
val build_term : typedlambdaterm -> typing_annotation option -> typedlambda
val build_varterm : Ident.t * typing_annotation option -> typedlambda
val build_letterm :
  Lambda.let_kind * typedident * typedlambda * typedlambda -> typedlambda
val build_staticcatchterm :
  typedlambda * (int * typedident list) * typedlambda -> typedlambda
val build_ifthenelseterm :
  typedlambda * typedlambda * typedlambda -> typedlambda
val build_switchterm : typedlambda * typedlambda_switch -> typedlambda

val lambda_unit : typedlambda

val name_lambda : typedlambda -> (Ident.t * typing_annotation option -> typedlambda) -> typedlambda
val name_lambda_list : typedlambda list -> (typedlambda list -> typedlambda) -> typedlambda

module IdentSet: Set.S with type elt = typedident
val free_variables : typedlambda -> IdentSet.t

val next_raise_count : unit -> int
val staticfail : typedlambda

val is_guarded : typedlambda -> bool
val patch_guarded : typedlambda -> typedlambda -> typedlambda

val transl_path_with_type : typing_annotation option -> Path.t -> typedlambda
val transl_path : Path.t -> typedlambda
val make_sequence : ('a -> typedlambda) -> 'a list -> typedlambda

val subst_lambda : typedlambda Ident.tbl -> typedlambda -> typedlambda
val bind : Lambda.let_kind -> Ident.t -> typedlambda -> typedlambda -> typedlambda

val to_lambda : typedlambda -> Lambda.lambda

