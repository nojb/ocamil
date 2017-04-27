
class type ['a] cArray =
   object
     inherit CtsHierarchy.cTop  
     method get : int -> 'a
     method set : int -> 'a -> unit
   end

let id = fun x -> x

(*********** Capsule ***************)
class ['cts,'jCaml] _Array  
    (cts_get: Jacare.obj -> int -> 'cts)
    (cts_set: Jacare.obj -> int -> 'cts -> unit)
    (from__cts_: 'cts -> 'jCaml) 
    (to__cts_: 'jCaml -> 'cts) 
    (_cts__obj  :Jacare.obj) =
  object
    inherit CtsHierarchy.top _cts__obj
    method get i = 
(* try *)
      from__cts_ (cts_get _cts__obj i)
(* with Jacare.Exception e -> invalid_arg "_Array#get" *)
    method set i (obj:'jCaml) = 
(* try  *)
      cts_set _cts__obj i (to__cts_ obj) 
(* with Jacare.Exception e -> invalid_arg "_Array#set" *)
  end
    

(*********** Allocations ************)
let _new_boolean_cArray size =
  let _cts__obj = Jacare.new_boolean_array size in 
  (new _Array Jacare.get_boolean_array_element Jacare.set_boolean_array_element id id _cts__obj : bool cArray)

let _new_byte_cArray size =
  let _cts__obj = Jacare.new_byte_array size in 
  (new _Array Jacare.get_byte_array_element Jacare.set_byte_array_element id id _cts__obj : int cArray)

let _new_char_cArray size =
  let _cts__obj = Jacare.new_char_array size in 
  (new _Array Jacare.get_char_array_element Jacare.set_char_array_element id id _cts__obj : int cArray)

let _new_short_cArray size =
  let _cts__obj = Jacare.new_short_array size in 
  (new _Array Jacare.get_short_array_element Jacare.set_short_array_element id id _cts__obj : int cArray)

(* int pour 'Jacare.camlint' *)
let _new_int_cArray size =
  let _cts__obj = Jacare.new_int_array size in 
  (new _Array Jacare.get_camlint_array_element Jacare.set_camlint_array_element id id _cts__obj : int cArray)

let _new_float_cArray size =
  let _cts__obj = Jacare.new_float_array size in 
  (new _Array Jacare.get_float_array_element Jacare.set_float_array_element id id _cts__obj : float cArray)
    
let _new_double_cArray size =
  let _cts__obj = Jacare.new_double_array size in 
  (new _Array Jacare.get_double_array_element Jacare.set_double_array_element id id _cts__obj : float cArray)

let _new_string_cArray size =
  let _cts__obj = Jacare.new_object_array size (Jacare.find_class "mscorlib" "System.String") in 
  (new _Array Jacare.get_object_array_element Jacare.set_object_array_element Jacare.string_from_cts Jacare.string_to_cts _cts__obj
     : string cArray)

let _new_top_cArray size =
  let _cts__obj = Jacare.new_object_array size (Jacare.find_class "" "System.Object") in 
  (new _Array 
     Jacare.get_object_array_element 
     Jacare.set_object_array_element
     (fun ctsobj -> new CtsHierarchy.top ctsobj) 
     (fun obj -> obj#_get_ctsobj)
     _cts__obj : CtsHierarchy.top cArray)

(****** Fonctions de construction *********)

let init_boolean_cArray size f =
  let a = _new_boolean_cArray size in
  for i = 0 to pred size do
    a#set i (f i)
  done;
  a
    
let init_byte_cArray size f =
  let a = _new_byte_cArray size in
  for i = 0 to pred size do
    a#set i (f i)
  done;
  a
 
let init_char_cArray size f =
  let a = _new_char_cArray size in
  for i = 0 to pred size do
    a#set i (f i)
  done;
  a
 
let init_short_cArray size f =
  let a = _new_short_cArray size in
  for i = 0 to pred size do
    a#set i (f i)
  done;
  a
 
let init_int_cArray size f =
  let a = _new_int_cArray size in
  for i = 0 to pred size do
    a#set i (f i)
  done;
  a
 
let init_float_cArray size f =
  let a = _new_float_cArray size in
  for i = 0 to pred size do
    a#set i (f i)
  done;
  a
 
let init_double_cArray size f =
  let a = _new_double_cArray size in
  for i = 0 to pred size do
    a#set i (f i)
  done;
  a

let init_string_cArray size f =
  let a = _new_string_cArray size in
  for i = 0 to pred size do
    a#set i (f i)
  done;
  a

let init_top_cArray size f =
  let a = _new_top_cArray size in
  for i = 0 to pred size do
    a#set i (f i)
  done;
  a
