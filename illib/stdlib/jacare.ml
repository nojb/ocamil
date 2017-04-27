type obj

external get_null: unit -> obj = 
"object" "CamIL.Jacare" "get_null" "object"

let null = get_null()


exception Null_pointer

exception Exception of obj

let raise_exception = (fun obj -> raise (Exception obj))

(* String operations *)


external string_to_cts: string -> obj = 
  "string" "CamIL.Jacare" "string_to_cts" "bldstr"
external string_from_cts: obj -> string = 
  "bldstr" "CamIL.Jacare" "string_from_cts" "string"

let null_string = ""

(** erreur de verification : CamIL.compare.eq appelé sur 2 StringBuilder **)
(** au lieu d'objets **) (** BUG CAMIL **)
(** let is_null_string s = s == null_string **)

type bug
let is_null_string s = (Obj.magic s:bug) == (Obj.magic null_string:bug)

(* Class operations *)

type clazz
        (* The type of class identifiers *)


external find_class: string -> string -> clazz
        = "class System.Type" "CamIL.Jacare" "find_class" "string" "string"
external get_superclass: clazz -> clazz
        = "class System.Type" "CamIL.Jacare" "get_superclass" "class System.Type"
external is_assignable_from_aux: clazz -> clazz -> int
        = "int" "CamIL.Jacare" "is_assignable_from" "class System.Type" "class System.Type"
let is_assignable_from c1 c2 = (is_assignable_from_aux c1 c2)<>0

(* Field and method identifiers *)

type type_argument =
    Tvoid
  | Tbool
  | Tbyte 
  | Tchar 
  | Tshort 
  | Tcamlint 
  | Tint 
  | Tlong  
  | Tfloat 
  | Tdouble 
  | Tstring
  | Tobject
  | Tclazz of clazz
  | Tarray of type_argument

type fieldID
type methodID
type constructorID

external get_fieldID: clazz -> string -> type_argument -> fieldID
        = "class System.Reflection.FieldInfo" "CamIL.Jacare" "get_fieldID" "class System.Type" "string" "class CamIL.Variant"
external get_static_fieldID: clazz -> string -> type_argument -> fieldID
        = "class System.Reflection.FieldInfo" "CamIL.Jacare" "get_static_fieldID" "class System.Type" "string" "class CamIL.Variant"
external get_methodID: clazz -> string -> type_argument array * type_argument -> methodID
        = "class System.Reflection.MethodInfo" "CamIL.Jacare" "get_methodID" "class System.Type" "string" "object[]"
external get_static_methodID: clazz -> string -> type_argument array * type_argument -> methodID
        = "class System.Reflection.MethodInfo" "CamIL.Jacare" "get_static_methodID" "class System.Type" "string" "object[]"
external get_constructorID: clazz -> type_argument array -> constructorID
        = "class System.Reflection.ConstructorInfo" "CamIL.Jacare" "get_constructorID" "class System.Type" "object[]"

(* Field access *)

external get_object_field: obj -> fieldID -> obj
        = "object" "CamIL.Jacare" "get_object_field" "object" "class System.Reflection.FieldInfo" 
external get_camlint_field: obj -> fieldID -> int
        = "int" "CamIL.Jacare" "get_camlint_field" "object" "class System.Reflection.FieldInfo" 

let get_boolean_field ob fi = 
  if get_camlint_field ob fi ==0 then false else true;;  

external get_byte_field: obj -> fieldID -> int
        = "int" "CamIL.Jacare" "get_byte_field" "object" "class System.Reflection.FieldInfo" 
external get_char_field: obj -> fieldID -> int
        = "int" "CamIL.Jacare" "get_char_field" "object" "class System.Reflection.FieldInfo" 
external get_short_field: obj -> fieldID -> int
        = "int" "CamIL.Jacare" "get_short_field" "object" "class System.Reflection.FieldInfo" 

external get_long_field: obj -> fieldID -> int64
        = "int64" "CamIL.Jacare" "get_long_field" "object" "class System.Reflection.FieldInfo" 

external get_double_field: obj -> fieldID -> float
        = "float" "CamIL.Jacare" "get_double_field" "object" "class System.Reflection.FieldInfo" 

let get_float_field = get_double_field;;


external set_object_field: obj -> fieldID -> obj -> unit
    = "object" "CamIL.Jacare" "set_object_field" "object" "class System.Reflection.FieldInfo" "object"


external set_camlint_field: obj -> fieldID -> int -> unit
        = "object" "CamIL.Jacare" "set_camlint_field" "object" "class System.Reflection.FieldInfo" "int"

let set_boolean_field ob fi v = 
  set_camlint_field ob fi (if v then 1 else 0);;

external set_byte_field: obj -> fieldID -> int -> unit
    = "object" "CamIL.Jacare" "set_byte_field" "object" "class System.Reflection.FieldInfo" "int"
external set_char_field: obj -> fieldID -> int -> unit
    = "object" "CamIL.Jacare" "set_char_field" "object" "class System.Reflection.FieldInfo" "int"
external set_short_field: obj -> fieldID -> int -> unit
    = "object" "CamIL.Jacare" "set_short_field" "object" "class System.Reflection.FieldInfo" "int"

external set_long_field: obj -> fieldID -> int64 -> unit
    = "object" "CamIL.Jacare" "set_long_field" "object" "class System.Reflection.FieldInfo" "int64"


external set_float_field: obj -> fieldID -> float -> unit
    = "object" "CamIL.Jacare" "set_float_field" "object" "class System.Reflection.FieldInfo" "float"

external set_double_field: obj -> fieldID -> float -> unit
    = "object" "CamIL.Jacare" "set_double_field" "object" "class System.Reflection.FieldInfo" "float"


external get_static_object_field: clazz -> fieldID -> obj
        = "object" "CamIL.Jacare" "get_object_field" "object" "class System.Reflection.FieldInfo" 
external get_static_camlint_field: clazz -> fieldID -> int
        = "int" "CamIL.Jacare" "get_camlint_field" "object" "class System.Reflection.FieldInfo" 

let get_static_boolean_field ob fi = 
  if get_static_camlint_field ob fi ==0 then false else true;;  

external get_static_byte_field: clazz -> fieldID -> int
        = "int" "CamIL.Jacare" "get_byte_field" "object" "class System.Reflection.FieldInfo" 
external get_static_char_field: clazz -> fieldID -> int
        = "int" "CamIL.Jacare" "get_char_field" "object" "class System.Reflection.FieldInfo" 
external get_static_short_field: clazz -> fieldID -> int
        = "int" "CamIL.Jacare" "get_short_field" "object" "class System.Reflection.FieldInfo" 

external get_static_long_field: clazz -> fieldID -> int64
        = "int64" "CamIL.Jacare" "get_long_field" "object" "class System.Reflection.FieldInfo" 
   
external get_static_double_field: clazz -> fieldID -> float
        = "float" "CamIL.Jacare" "get_double_field" "object" "class System.Reflection.FieldInfo" 

let get_static_float_field = get_static_double_field;;


external set_static_object_field: clazz -> fieldID -> obj -> unit
    = "object" "CamIL.Jacare" "set_object_field" "object" "class System.Reflection.FieldInfo" "object"


external set_static_camlint_field: clazz -> fieldID -> int -> unit
        = "object" "CamIL.Jacare" "set_camlint_field" "object" "class System.Reflection.FieldInfo" "int"

let set_static_boolean_field ob fi v = 
  set_static_camlint_field ob fi (if v then 1 else 0);;

external set_static_byte_field: clazz -> fieldID -> int -> unit
    = "object" "CamIL.Jacare" "set_byte_field" "object" "class System.Reflection.FieldInfo" "int"
external set_static_char_field: clazz -> fieldID -> int -> unit
    = "object" "CamIL.Jacare" "set_char_field" "object" "class System.Reflection.FieldInfo" "int"
external set_static_short_field: clazz -> fieldID -> int -> unit
    = "object" "CamIL.Jacare" "set_short_field" "object" "class System.Reflection.FieldInfo" "int"

external set_static_long_field: clazz -> fieldID -> int64 -> unit
    = "object" "CamIL.Jacare" "set_long_field" "object" "class System.Reflection.FieldInfo" "int64"

external set_static_float_field: clazz -> fieldID -> float -> unit
    = "object" "CamIL.Jacare" "set_float_field" "object" "class System.Reflection.FieldInfo" "float"

external set_static_double_field: clazz -> fieldID -> float -> unit
    = "object" "CamIL.Jacare" "set_double_field" "object" "class System.Reflection.FieldInfo" "float"

type argument =
    Boolean of bool
  | Byte of int
  | Char of int
  | Short of int
  | Camlint of int
  | Int of int32
  | Long of int64
  | Float of float
  | Double of float
  | Obj of obj

external call_constructor : constructorID -> argument array -> obj
    = "object" "CamIL.Jacare" "call_constructor" "class System.Reflection.ConstructorInfo" "object[]"

external call_object_method: obj -> methodID -> argument array -> obj
        = "object" "CamIL.Jacare" "call_object_method" "object" "class System.Reflection.MethodInfo" "object[]"
external call_byte_method: obj -> methodID -> argument array -> int
      = "int" "CamIL.Jacare" "call_byte_method" "object" "class System.Reflection.MethodInfo" "object[]"
external call_char_method: obj -> methodID -> argument array -> int
    = "int" "CamIL.Jacare" "call_char_method" "object" "class System.Reflection.MethodInfo" "object[]"
external call_short_method: obj -> methodID -> argument array -> int
      = "int" "CamIL.Jacare" "call_short_method" "object" "class System.Reflection.MethodInfo" "object[]"

external call_camlint_method: obj -> methodID -> argument array -> int
        = "int" "CamIL.Jacare" "call_camlint_method" "object" "class System.Reflection.MethodInfo" "object[]"

external call_boolean_method: obj -> methodID -> argument array -> bool
        = "int" "CamIL.Jacare" "call_boolean_method" "object" "class System.Reflection.MethodInfo" "object[]"

external call_long_method: obj -> methodID -> argument array -> int64
        = "int64" "CamIL.Jacare" "call_long_method" "object" "class System.Reflection.MethodInfo" "object[]"

external call_double_method: obj -> methodID -> argument array -> float
        = "float" "CamIL.Jacare" "call_double_method" "object" "class System.Reflection.MethodInfo" "object[]"

let call_float_method = call_double_method

let call_void_method ob mi vargs =
  let _ = call_object_method ob mi vargs
    in ()


external call_static_object_method: clazz -> methodID -> argument array -> obj
        = "object" "CamIL.Jacare" "call_object_method" "object" "class System.Reflection.MethodInfo" "object[]"

external call_static_byte_method: clazz -> methodID -> argument array -> int
      = "int" "CamIL.Jacare" "call_byte_method" "object" "class System.Reflection.MethodInfo" "object[]"
external call_static_char_method: clazz -> methodID -> argument array -> int
    = "int" "CamIL.Jacare" "call_char_method" "object" "class System.Reflection.MethodInfo" "object[]"
external call_static_short_method: clazz -> methodID -> argument array -> int
      = "int" "CamIL.Jacare" "call_short_method" "object" "class System.Reflection.MethodInfo" "object[]"

external call_static_camlint_method: clazz -> methodID -> argument array -> int
        = "int" "CamIL.Jacare" "call_camlint_method" "object" "class System.Reflection.MethodInfo" "object[]"

external call_static_boolean_method: obj -> methodID -> argument array -> bool
        = "int" "CamIL.Jacare" "call_boolean_method" "object" "class System.Reflection.MethodInfo" "object[]"

external call_static_long_method: clazz -> methodID -> argument array -> int64
        = "int64" "CamIL.Jacare" "call_long_method" "object" "class System.Reflection.MethodInfo" "object[]"

external call_static_double_method: clazz -> methodID -> argument array -> float
        = "float" "CamIL.Jacare" "call_double_method" "object" "class System.Reflection.MethodInfo" "object[]"
let call_static_float_method = call_static_double_method

let  call_static_void_method cl mi vargs =
  let _ = call_static_object_method cl mi vargs
    in ()


external call_nonvirtual_object_method: obj -> methodID -> argument array -> obj
        = "object" "CamIL.Jacare" "call_nonvirtual_object_method" "object" "class System.Reflection.MethodInfo" "object[]"

external call_nonvirtual_byte_method: obj -> methodID -> argument array -> int
        = "int" "CamIL.Jacare" "call_nonvirtual_byte_method" "object" "class System.Reflection.MethodInfo" "object[]"
external call_nonvirtual_char_method: obj -> methodID -> argument array -> int
        = "int" "CamIL.Jacare" "call_nonvirtual_char_method" "object" "class System.Reflection.MethodInfo" "object[]"
external call_nonvirtual_short_method: obj -> methodID -> argument array -> int
        = "int" "CamIL.Jacare" "call_nonvirtual_short_method" "object" "class System.Reflection.MethodInfo" "object[]"

external call_nonvirtual_camlint_method: obj -> methodID -> argument array -> int
        = "int" "CamIL.Jacare" "call_nonvirtual_camlint_method" "object" "class System.Reflection.MethodInfo" "object[]"

external call_nonvirtual_boolean_method: obj ->methodID -> argument array -> bool
        = "int" "CamIL.Jacare" "call_nonvirtual_boolean_method" "object" "class System.Reflection.MethodInfo" "object[]"

external call_nonvirtual_long_method: obj -> methodID -> argument array -> int64
        = "int64" "CamIL.Jacare" "call_nonvirtual_long_method" "object" "class System.Reflection.MethodInfo" "object[]"

external call_nonvirtual_double_method: obj -> methodID -> argument array -> float
        = "float" "CamIL.Jacare" "call_nonvirtual_double_method" "object" "class System.Reflection.MethodInfo" "object[]"

let call_nonvirtual_float_method = call_nonvirtual_double_method

let  call_nonvirtual_void_method cl mi vargs =
  let _ = call_nonvirtual_object_method cl mi vargs
    in ()

(* Arrays *)

external get_array_length: obj -> int =
  "int" "CamIL.Jacare" "get_array_length" "object[]" (* System.Array ?*)

external new_object_array: int -> clazz -> obj
        = "class System.Array" "CamIL.Jacare" "new_object_array" "int" "class System.Type"
external get_object_array_element: obj -> int -> obj
    = "object" "CamIL.Jacare" "get_object_array_element" "class System.Array" "int"
external set_object_array_element: obj -> int -> obj -> unit
    = "object" "CamIL.Jacare" "set_object_array_element" "class System.Array" "int" "object"

external new_boolean_array: int -> obj
        = "class System.Array" "CamIL.Jacare" "new_boolean_array" "int"

external get_boolean_array_element_aux: obj -> int -> int
    = "int" "CamIL.Jacare" "get_boolean_array_element" "class System.Array" "int" 

let get_boolean_array_element arr i = 
  (get_boolean_array_element_aux arr i) != 0;;

external set_boolean_array_element_aux: obj -> int -> int -> unit
     = "object" "CamIL.Jacare" "set_boolean_array_element" "class System.Array" "int" "int" 
    
let set_boolean_array_element arr i b =
  let _ = set_boolean_array_element_aux arr i (if b then 1 else 0)
  in ();;
    
external new_byte_array: int -> obj
    = "class System.Array" "CamIL.Jacare" "new_byte_array" "int"
external get_byte_array_element: obj -> int -> int
    = "int" "CamIL.Jacare" "get_byte_array_element" "class System.Array" "int"
external set_byte_array_element: obj -> int -> int -> unit
    = "object" "CamIL.Jacare" "set_byte_array_element" "class System.Array" "int" "int"

external get_byte_array_region: obj -> int -> string -> int -> int -> unit
    = "object" "CamIL.Jacare" "get_byte_array_region" "class System.Array" "int" "bldstr" "int" "int"
external set_byte_array_region: string -> int -> obj -> int -> int -> unit
    = "object" "CamIL.Jacare" "set_byte_array_region" "string" "int" "class System.Array" "int" "int"

external new_char_array: int -> obj
    = "class System.Array" "CamIL.Jacare" "new_char_array" "int"
external get_char_array_element: obj -> int -> int
    = "int" "CamIL.Jacare" "get_char_array_element" "class System.Array" "int"
external set_char_array_element: obj -> int -> int -> unit
    = "object" "CamIL.Jacare" "set_char_array_element" "class System.Array" "int" "int"
external new_short_array: int -> obj
    = "class System.Array" "CamIL.Jacare" "new_short_array" "int"
external get_short_array_element: obj -> int -> int
    = "int" "CamIL.Jacare" "get_short_array_element" "class System.Array" "int"
external set_short_array_element: obj -> int -> int -> unit
    = "object" "CamIL.Jacare" "set_short_array_element" "class System.Array" "int" "int"

external new_int_array: int -> obj
    = "class System.Array" "CamIL.Jacare" "new_int_array" "int"

external get_camlint_array_element: obj -> int -> int
    = "int" "CamIL.Jacare" "get_camlint_array_element" "class System.Array" "int"
external set_camlint_array_element: obj -> int -> int -> unit
    = "object" "CamIL.Jacare" "set_camlint_array_element" "class System.Array" "int" "int"

external new_long_array: int -> obj
    = "class System.Array" "CamIL.Jacare" "new_long_array" "int"
external get_long_array_element: obj -> int -> int64
    = "int64" "CamIL.Jacare" "get_long_array_element" "class System.Array" "int"
external set_long_array_element: obj -> int -> int64 -> unit
    = "object" "CamIL.Jacare" "set_long_array_element" "class System.Array" "int" "int64"


external new_float_array: int -> obj
    = "class System.Array" "CamIL.Jacare" "new_float_array" "int"
external get_float_array_element: obj -> int -> float
    = "float" "CamIL.Jacare" "get_float_array_element" "class System.Array" "int"
external set_float_array_element: obj -> int -> float -> unit
    = "object" "CamIL.Jacare" "set_float_array_element" "class System.Array" "int" "float"
external new_double_array: int -> obj
    = "class System.Array" "CamIL.Jacare" "new_double_array" "int"
external get_double_array_element: obj -> int -> float
    = "float" "CamIL.Jacare" "get_double_array_element" "class System.Array" "int"
external set_double_array_element: obj -> int -> float -> unit
    = "object" "CamIL.Jacare" "set_double_array_element" "class System.Array" "int" "float"

(* Object operations *)


external is_null_aux: obj -> int = "int32" "CamIL.Jacare" "is_null" "object"
let is_null o = (is_null_aux o)<>0

external get_object_class: obj -> clazz = 
   "class System.Type" "CamIL.Jacare" "get_object_class" "object"
external is_instance_of_aux: obj -> clazz -> int = 
"int" "CamIL.Jacare" "is_instance_of" "object" "class System.Type"

let is_instance_of ob cl =
  (is_instance_of_aux ob cl)<>0


(* special *)
external box_valuetype: clazz -> int -> obj
    = "object" "CamIL.Jacare" "box_valuetype" "class System.Type" "int"



