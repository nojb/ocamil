(** jniArray : manipulation de tableau Java *)

class type ['a] cArray =
  object
    method _get_ctsobj : Jacare.obj
    method get : int -> 'a
    method set : int -> 'a -> unit
  end
(** type des tableau java encapsulé *)

val init_boolean_cArray : int -> (int -> bool) -> bool cArray
val init_byte_cArray : int -> (int -> int) -> int cArray
val init_char_cArray : int -> (int -> int) -> int cArray
val init_short_cArray : int -> (int -> int) -> int cArray
val init_int_cArray : int -> (int -> int) -> int cArray

val init_float_cArray : int -> (int -> float) -> float cArray
val init_double_cArray : int -> (int -> float) -> float cArray
val init_string_cArray : int -> (int -> string) -> string cArray
val init_top_cArray :
  int -> (int -> CtsHierarchy.top) -> CtsHierarchy.top cArray
(** fonctions d'initialisations *)


(** class capsule à l'usage du générateur de code ... *)
class ['a, 'b] _Array :
  (Jacare.obj -> int -> 'a) ->
  (Jacare.obj -> int -> 'a -> unit) ->
  ('a -> 'b) ->
  ('b -> 'a) ->
  Jacare.obj ->
  object
    method _get_ctsobj : Jacare.obj
    method get : int -> 'b
    method set : int -> 'b -> unit
  end


(** fonction d'allocation à l'usage du générateur de code *)
val _new_boolean_cArray : int -> bool cArray
val _new_byte_cArray : int -> int cArray
val _new_char_cArray : int -> int cArray
val _new_short_cArray : int -> int cArray
val _new_int_cArray : int -> int cArray

val _new_float_cArray : int -> float cArray
val _new_double_cArray : int -> float cArray
val _new_string_cArray : int -> string cArray
val _new_top_cArray : int -> CtsHierarchy.top cArray
