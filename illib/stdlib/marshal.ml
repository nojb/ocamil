(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1997 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id: marshal.ml,v 1.3 2005/01/24 08:25:40 montela Exp $ *)

type extern_flags =
    No_sharing
  | Closures

(* TODO : se servir des flags ... *)
let to_channel oc v flags = output_value oc v

external to_string: 'a -> extern_flags list -> string  = 
 "bldstr" "CamIL.Extern" "output_value_to_string" "object" "object"

external to_buffer_unsafe:
      string -> int -> int -> 'a -> extern_flags list -> int = 
	"int" "CamIL.Extern" "output_value_to_buffer" "bldstr" "int" "int" "object" "object" 

let to_buffer buff ofs len v flags =
  if ofs < 0 || len < 0 || ofs > String.length buff - len
  then invalid_arg "Marshal.to_buffer: substring out of bounds"
  else to_buffer_unsafe buff ofs len v flags

let from_channel = input_value

external from_string_unsafe: string -> int -> 'a = 
 "object" "CamIL.Extern" "input_value_from_string" "bldstr" "int"

external data_size_unsafe: string -> int -> int = 
   "int" "CamIL.Extern" "marshal_data_size" "bldstr" "int"

(* A VOIR *)
let header_size = 0

let data_size buff ofs =
  if ofs < 0 || ofs > String.length buff - header_size
  then invalid_arg "Marshal.data_size"
  else data_size_unsafe buff ofs
let total_size buff ofs = header_size + data_size buff ofs

let from_string buff ofs =
  if ofs < 0 || ofs > String.length buff - header_size
  then invalid_arg "Marshal.from_size"
  else begin
    let len = data_size_unsafe buff ofs in
    if ofs > String.length buff - (header_size + len)
    then invalid_arg "Marshal.from_string"
    else from_string_unsafe buff ofs
  end  
