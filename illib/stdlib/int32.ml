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

(* $Id: int32.ml,v 1.2 2006/07/23 03:04:14 montela Exp $ *)

(* Module [Int32]: 32-bit integers *)

external neg : int32 -> int32 = "%int32_neg"
external add : int32 -> int32 -> int32 = "%int32_add"
external sub : int32 -> int32 -> int32 = "%int32_sub"
external mul : int32 -> int32 -> int32 = "%int32_mul"
external div : int32 -> int32 -> int32 = "%int32_div"
external rem : int32 -> int32 -> int32 = "%int32_mod"
external logand : int32 -> int32 -> int32 = "%int32_and"
external logor : int32 -> int32 -> int32 = "%int32_or"
external logxor : int32 -> int32 -> int32 = "%int32_xor"
external shift_left : int32 -> int -> int32 = "%int32_lsl"
external shift_right : int32 -> int -> int32 = "%int32_asr"
external shift_right_logical : int32 -> int -> int32 = "%int32_lsr"
external of_int : int -> int32 = "%int32_of_int"
external to_int : int32 -> int = "%int32_to_int"
external of_float : float -> int32 = "int" "System.Convert" "ToInt32" "float"
external to_float : int32 -> float = "float" "System.Convert" "ToDouble" "int"

let zero = of_int 0
let one = of_int 1
let minus_one = of_int (-1)
let succ n = add n one
let pred n = sub n one
let abs n = if n >= zero then n else neg n
let min_int = shift_left one 31
let max_int = sub min_int one
let lognot n = logxor n minus_one

(* recopie sur Printf !! Attention *)
external il_format: string -> int32 -> string = 
"string" "CamIL.String" "format_int" "string" "int"

let rec parse_flags fmt i remain zero minus before = 
  if remain = 0 then failwith "Pb in Print.format_int" else
    match fmt.[i] with 
	'0' -> parse_flags fmt (i+1) (remain-1) true minus before
      | '+' -> parse_flags fmt (i+1) (remain-1) zero minus "+"
      | ' ' -> parse_flags fmt (i+1) (remain-1) zero minus " "
      | '#' -> failwith "Cannot deal with # format yet"
      | '-' -> parse_flags fmt (i+1) (remain-1) zero true before
      | _ -> (i,zero,minus,before)

let format fmt i =
(* TODO *)
(* ne gère pas correctement les flags +,space et # *)
(* ne gère pas o *)
  let (pos,zero,minus,before)=parse_flags fmt 1 ((String.length fmt)-1) false false ""  in
  let f = fun s-> if s="l" || s ="" then "00" else s in
  let n1,n2 =
    try (let dot=String.index fmt '.' in (String.sub fmt pos (dot-pos),String.sub fmt (dot+1) ((String.length fmt)-dot-2)))
    with Not_found -> let n1 = String.sub fmt pos ((String.length fmt)-pos-1) in (n1, if zero then n1 else "")
  in 
  let intfmt = String.make 1 (match fmt.[(String.length fmt)-1] with 'i' -> 'd' | c -> c) in
  let ilfmt = "{0,"^(if minus then "-" else "")^(f n1)^":"^intfmt^(f n2)^"}" in 
  let res =  il_format ilfmt i in
      if before<>"" then (
	if minus then before^res else
	  try let i = String.rindex res ' ' in
	    res.[i]<-before.[0];res
	  with Not_found -> before^res )
      else res 
(* *)

let to_string n = format "%d" n

let of_string s = Obj.magic (int_of_string s)

type t = int32

let compare = (Pervasives.compare: t -> t -> int)
