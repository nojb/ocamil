(***********************************************************************)
(*                                                                     *)
(*             OCamlJava: Objective Caml / Java interface              *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License.         *)
(*                                                                     *)
(***********************************************************************)

(* $Id: jacare.mli,v 1.1 2006/10/16 12:45:55 montela Exp $ *)

(* Low-level Cts interface *)


(* Object operations *)

type obj
        (* The type of Cts object references *)
val null: obj
        (* The [null] object reference *)
exception Null_pointer
        (* Exception raised by the operations below when they
           encounter a null object reference in arguments that must be non-null. *)

val is_null: obj -> bool 
        (* Determine if the given object reference is [null] *)

(* String operations.  Cts strings are represented in Caml
   by their UTF8 encoding. *)

external string_to_cts: string -> obj = 
  "string" "CamIL.Jacare" "string_to_cts" "bldstr"
external string_from_cts: obj -> string = 
  "bldstr" "CamIL.Jacare" "string_from_cts" "string"
        (* Conversion between Caml strings and Cts strings. *)
val null_string: string
        (* A distinguished Caml string that represents the [null]
           Cts string reference. *)
val is_null_string: string -> bool
        (* Determine whether its argument is the distinguished Caml string
           representing the [null] Cts string reference. *)

(* Class operations *)

type clazz
        (* The type of class identifiers *)

external find_class: string -> string -> clazz
        = "class System.Type" "CamIL.Jacare" "find_class" "string" "string"
        (* Find a class given its fully qualified name, e.g. 
           Slashes are used to separate components of the name. *)

external get_superclass: clazz -> clazz
        = "class System.Type" "CamIL.Jacare" "get_superclass" "class System.Type"
        (* Return the super-class of the given class. *)
val is_assignable_from: clazz -> clazz -> bool
  (*      = "bool" "CamIL.Jacare" "is_assignable_from" "class System.Type" "class System.Type" *)
        (* Assignment compatibility predicate. *)

external get_object_class: obj -> clazz = 
   "class System.Type" "CamIL.Jacare" "get_object_class" "object"
        (* Return the class of an object. *)
val is_instance_of: obj -> clazz -> bool
        (* Determine if the given object reference is an instance of the
           given class *)

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
        (* The type of field identifiers *)
type methodID
        (* The type of method identifiers *)
type constructorID
        (* The type of constructor identifiers *)

external get_fieldID: clazz -> string -> type_argument -> fieldID
        = "class System.Reflection.FieldInfo" "CamIL.Jacare" "get_fieldID" "class System.Type" "string" "class CamIL.Variant"
        (* [get_fieldID cls name descr] returns the identifier of
           the instance field named [name] with descriptor (type) [descr]
           in class [cls]. *)
external get_static_fieldID: clazz -> string -> type_argument -> fieldID
        = "class System.Reflection.FieldInfo" "CamIL.Jacare" "get_static_fieldID" "class System.Type" "string" "class CamIL.Variant"
        (* Same, for a static field. *)
external get_methodID: clazz -> string -> type_argument array * type_argument -> methodID
        = "class System.Reflection.MethodInfo" "CamIL.Jacare" "get_methodID" "class System.Type" "string" "object[]"
        (* [get_methodID cls name descr] returns the identifier of
           the virtual method named [name] with descriptor (type) [descr]
           in class [cls]. *)
external get_static_methodID: clazz -> string -> type_argument array * type_argument -> methodID
        = "class System.Reflection.MethodInfo" "CamIL.Jacare" "get_static_methodID" "class System.Type" "string" "object[]"
        (* Same, for a static method. *)

external get_constructorID: clazz -> type_argument array -> constructorID
        = "class System.Reflection.ConstructorInfo" "CamIL.Jacare" "get_constructorID" "class System.Type" "object[]"
        (* [get_constructorID cls descr] returns the identifier of
           the constructor with descriptor (type) [descr]
           in class [cls]. *)


(* Field access *)

external get_object_field: obj -> fieldID -> obj
        = "object" "CamIL.Jacare" "get_object_field" "object" "class System.Reflection.FieldInfo" 

external get_camlint_field: obj -> fieldID -> int
        = "int" "CamIL.Jacare" "get_camlint_field" "object" "class System.Reflection.FieldInfo" 

val get_boolean_field: obj -> fieldID -> bool

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

val get_float_field: obj -> fieldID -> float


external set_camlint_field: obj -> fieldID -> int -> unit
        = "object" "CamIL.Jacare" "set_camlint_field" "object" "class System.Reflection.FieldInfo" "int"

external set_object_field: obj -> fieldID -> obj -> unit
    = "object" "CamIL.Jacare" "set_object_field" "object" "class System.Reflection.FieldInfo" "object"
val set_boolean_field: obj -> fieldID -> bool -> unit

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

val get_static_boolean_field: clazz -> fieldID -> bool

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

val get_static_float_field: clazz -> fieldID -> float

external set_static_object_field: clazz -> fieldID -> obj -> unit
    = "object" "CamIL.Jacare" "set_object_field" "object" "class System.Reflection.FieldInfo" "object"


external set_static_camlint_field: clazz -> fieldID -> int -> unit
        = "object" "CamIL.Jacare" "set_camlint_field" "object" "class System.Reflection.FieldInfo" "int"

val set_static_boolean_field:clazz -> fieldID -> bool -> unit

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

(* Method invocation *)

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
        (* Datatype representing one argument of a Cts method. *)

external call_constructor : constructorID -> argument array -> obj
    = "object" "CamIL.Jacare" "call_constructor" "class System.Reflection.ConstructorInfo" "object[]"

external call_object_method: obj -> methodID -> argument array -> obj
        = "object" "CamIL.Jacare" "call_object_method" "object" "class System.Reflection.MethodInfo" "object[]"

external call_boolean_method: obj -> methodID -> argument array -> bool
        = "int" "CamIL.Jacare" "call_boolean_method" "object" "class System.Reflection.MethodInfo" "object[]"

external call_byte_method: obj -> methodID -> argument array -> int
      = "int" "CamIL.Jacare" "call_byte_method" "object" "class System.Reflection.MethodInfo" "object[]"
external call_char_method: obj -> methodID -> argument array -> int
    = "int" "CamIL.Jacare" "call_char_method" "object" "class System.Reflection.MethodInfo" "object[]"
external call_short_method: obj -> methodID -> argument array -> int
      = "int" "CamIL.Jacare" "call_short_method" "object" "class System.Reflection.MethodInfo" "object[]"

external call_camlint_method: obj -> methodID -> argument array -> int
        = "int" "CamIL.Jacare" "call_camlint_method" "object" "class System.Reflection.MethodInfo" "object[]"

external call_long_method: obj -> methodID -> argument array -> int64
        = "int64" "CamIL.Jacare" "call_long_method" "object" "class System.Reflection.MethodInfo" "object[]"


external call_double_method: obj -> methodID -> argument array -> float
        = "float" "CamIL.Jacare" "call_double_method" "object" "class System.Reflection.MethodInfo" "object[]"
val call_float_method: obj -> methodID -> argument array -> float
val call_void_method: obj -> methodID -> argument array -> unit


external call_static_object_method: clazz -> methodID -> argument array -> obj
        = "object" "CamIL.Jacare" "call_object_method" "object" "class System.Reflection.MethodInfo" "object[]"

external call_static_boolean_method: obj -> methodID -> argument array -> bool
        = "int" "CamIL.Jacare" "call_boolean_method" "object" "class System.Reflection.MethodInfo" "object[]"

external call_static_byte_method: clazz -> methodID -> argument array -> int
      = "int" "CamIL.Jacare" "call_byte_method" "object" "class System.Reflection.MethodInfo" "object[]"
external call_static_char_method: clazz -> methodID -> argument array -> int
    = "int" "CamIL.Jacare" "call_char_method" "object" "class System.Reflection.MethodInfo" "object[]"
external call_static_short_method: clazz -> methodID -> argument array -> int
      = "int" "CamIL.Jacare" "call_short_method" "object" "class System.Reflection.MethodInfo" "object[]"

external call_static_camlint_method: clazz -> methodID -> argument array -> int
        = "int" "CamIL.Jacare" "call_camlint_method" "object" "class System.Reflection.MethodInfo" "object[]"

external call_static_long_method: clazz -> methodID -> argument array -> int64
        = "int64" "CamIL.Jacare" "call_long_method" "object" "class System.Reflection.MethodInfo" "object[]"

external call_static_double_method: clazz -> methodID -> argument array -> float
        = "float" "CamIL.Jacare" "call_double_method" "object" "class System.Reflection.MethodInfo" "object[]"
val call_static_float_method: clazz -> methodID -> argument array -> float
val call_static_void_method: clazz -> methodID -> argument array -> unit


external call_nonvirtual_object_method: obj -> methodID -> argument array -> obj
        = "object" "CamIL.Jacare" "call_nonvirtual_object_method" "object" "class System.Reflection.MethodInfo" "object[]"

external call_nonvirtual_boolean_method: obj -> methodID -> argument array -> bool
        = "int" "CamIL.Jacare" "call_nonvirtual_boolean_method" "object" "class System.Reflection.MethodInfo" "object[]"

external call_nonvirtual_byte_method: obj -> methodID -> argument array -> int
        = "int" "CamIL.Jacare" "call_nonvirtual_byte_method" "object" "class System.Reflection.MethodInfo" "object[]"
external call_nonvirtual_char_method: obj -> methodID -> argument array -> int
        = "int" "CamIL.Jacare" "call_nonvirtual_char_method" "object" "class System.Reflection.MethodInfo" "object[]"
external call_nonvirtual_short_method: obj -> methodID -> argument array -> int
        = "int" "CamIL.Jacare" "call_nonvirtual_short_method" "object" "class System.Reflection.MethodInfo" "object[]"

external call_nonvirtual_camlint_method: obj ->  methodID -> argument array -> int
        = "int" "CamIL.Jacare" "call_nonvirtual_camlint_method" "object" "class System.Reflection.MethodInfo" "object[]"

external call_nonvirtual_long_method: obj ->  methodID -> argument array -> int64
        = "int64" "CamIL.Jacare" "call_nonvirtual_long_method" "object" "class System.Reflection.MethodInfo" "object[]"

val call_nonvirtual_float_method:
                 obj ->  methodID -> argument array -> float
external call_nonvirtual_double_method: obj ->  methodID -> argument array -> float
        = "float" "CamIL.Jacare" "call_nonvirtual_double_method" "object" "class System.Reflection.MethodInfo" "object[]"

val call_nonvirtual_void_method: obj ->  methodID -> argument array -> unit

(* Arrays *)

external get_array_length: obj -> int =
  "int" "CamIL.Jacare" "get_array_length" "object[]"

external new_object_array: int -> clazz -> obj
        = "class System.Array" "CamIL.Jacare" "new_object_array" "int" "class System.Type"
external get_object_array_element: obj -> int -> obj
    = "object" "CamIL.Jacare" "get_object_array_element" "class System.Array" "int"
external set_object_array_element: obj -> int -> obj -> unit
    = "object" "CamIL.Jacare" "set_object_array_element" "class System.Array" "int" "object"

external new_boolean_array: int -> obj
        = "class System.Array" "CamIL.Jacare" "new_boolean_array" "int"
val get_boolean_array_element: obj -> int -> bool

val set_boolean_array_element: obj -> int -> bool -> unit

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

(* special *)
external box_valuetype: clazz -> int -> obj
    = "object" "CamIL.Jacare" "box_valuetype" "class System.Type" "int"



