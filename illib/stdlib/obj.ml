(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id: obj.ml,v 1.15 2006/06/28 12:40:00 montela Exp $ *)


(* Operations on internal representations of values *)

(* CamIL representations change the deal *)

type t

(* Avant : les 3 suivants etaient la primitive %identity *)
external repr : 'a -> t = "object" "CamIL.Obj" "boxed_identity" "object";;
external obj : t -> 'a = "object" "CamIL.Obj" "boxed_identity" "object";;
external magic : 'a -> 'b = "object" "CamIL.Obj" "boxed_identity" "object";;

(**
external is_block : t -> bool = "obj_is_block"
**)

external is_int : t -> bool = 
 "bool" "CamIL.Obj" "obj_is_int" "object"

(* refinments for CamIL *)
external is_bool : t -> bool = 
 "bool" "CamIL.Obj" "obj_is_bool" "object"

external is_char : t -> bool = 
 "bool" "CamIL.Obj" "obj_is_char" "object"

let is_block o = not (is_int o)

external tag : t -> int =
 "int" "CamIL.BoxInt" "safe_tag" "object"

(* Caution : only correct on object[] blocks ... *)
external set_tag : t -> int -> unit = 
 "void" "CamIL.Obj" "obj_set_tag" "object" "int"

external variant_size : t -> int =
  "int" "CamIL.Obj" "variant_size" "object"

(* rem pour les blocs : en CamIL, on a une case supplémentaire pour le tag *)
(* Caution : only correct on strings, floats, variants and object[] blocks ... *)
let size ob = 
  let n = variant_size ob in
    if n >= 0 then n else begin
      match tag ob with
	| 252 -> (* chaines de caracteres *)  
	    let byte_length = (String.length (obj ob))+1 in (* Ajouter 1 pour le caractere \000 *)
	    let (div, rem)= (byte_length /(Sys.word_size/8), byte_length  mod (Sys.word_size/8)) in
	      if rem=0 then div else div+1
	| 253 -> (* flottants *) 2
	| _ -> Array.length (obj ob)
    end
    
external field : t -> int -> t = 
 "object" "CamIL.Obj" "reflection_getfield" "object" "int"

external record_field : t -> string -> t = 
 "object" "CamIL.Obj" "reflection_getrecordfield" "object" "string"


(* !! utiliser reflexion comme pour field *)

(* used by camlInternalOO *)
external set_field : t -> int -> t -> unit = 
 "void" "CamIL.Obj" "reflection_setfield" "object" "int" "object" 


(* used by camlInternalOO *)
external new_block : int -> int -> t = 
 "object" "CamIL.Obj" "block" "int" "int";;


external dup : 'a -> 'a = "object" "CamIL.Obj" "obj_dup" "object";;

(*
let func_truncate o n = 
  if n<1 || n > (size o) then raise (Invalid_argument "Obj.truncate");
  let o2= new_block (tag o) n in
    for i=1 to n do
      set_field o2 i (field o i)
    done;
    o2
*)

(**
external truncate : t -> int -> unit = "obj_truncate"

let marshal (obj : t) =
  Marshal.to_string obj []
let unmarshal str pos =
  (Marshal.from_string str pos, pos + Marshal.total_size str pos)
**)

let lazy_tag = 246
let closure_tag = 247
let object_tag = 248
let infix_tag = 249
let forward_tag = 250

let no_scan_tag = 251

let abstract_tag = 251
let string_tag = 252
let double_tag = 253
let double_array_tag = 254
let custom_tag = 255
let final_tag = custom_tag
