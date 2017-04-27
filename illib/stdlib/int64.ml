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

(* $Id: int64.ml,v 1.3 2006/07/23 03:04:14 montela Exp $ *)

(* Module [Int64]: 64-bit integers *)

external neg : int64 -> int64 = "%int64_neg"
external add : int64 -> int64 -> int64 = "%int64_add"
external sub : int64 -> int64 -> int64 = "%int64_sub"
external mul : int64 -> int64 -> int64 = "%int64_mul"
external div : int64 -> int64 -> int64 = "%int64_div"
external rem : int64 -> int64 -> int64 = "%int64_mod"
external logand : int64 -> int64 -> int64 = "%int64_and"
external logor : int64 -> int64 -> int64 = "%int64_or"
external logxor : int64 -> int64 -> int64 = "%int64_xor"
external shift_left : int64 -> int -> int64 = "%int64_lsl"
external shift_right : int64 -> int -> int64 = "%int64_asr"
external shift_right_logical : int64 -> int -> int64 = "%int64_lsr"
external of_int : int -> int64 = "%int64_of_int"
external to_int : int64 -> int = "%int64_to_int"
external of_float : float -> int64 = "int64" "System.Convert" "ToInt64" "float"
external to_float : int64 -> float = "float" "System.Convert" "ToDouble" "int64"
external of_int32 : int32 -> int64 = "%int64_of_int32"
external to_int32 : int64 -> int32 = "%int64_to_int32"
external of_nativeint : nativeint -> int64 = "%int64_of_nativeint"
external to_nativeint : int64 -> nativeint = "%int64_to_nativeint"

let zero = try of_int 0 with Invalid_argument _ -> Obj.magic Int32.zero
let one = try of_int 1 with Invalid_argument _ -> Obj.magic Int32.one
let minus_one = try of_int (-1) with Invalid_argument _ -> Obj.magic Int32.minus_one
let succ n = add n one
let pred n = sub n one
let abs n = if n >= zero then n else neg n
let min_int =
  try shift_left one 63 with Invalid_argument _ -> Obj.magic Int32.min_int
let max_int =
  try sub min_int one with Invalid_argument _ -> Obj.magic Int32.max_int
let lognot n = logxor n minus_one


(* recopie sur Printf !! Attention *)
external il_format: string -> int64 -> string = 
"string" "CamIL.String" "format_int64" "string" "int64"

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
  let f = fun s-> if s="L" || s ="" then "00" else s in
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

external il_of_string : string -> int64 = 
  "int64" "CamIL.String" "int64_of_string" "string"

(* Trois fonctions nouvelles, a optimiser ou a ecrire en IL ... *)
let rec parsehexa s i n accu =
  if i>=n then accu 
  else let digit = 
    ( match (String.get s i) with 
    | '0' -> zero    | '1' -> one    | '2' -> of_int 2    | '3' -> of_int 3    | '4' -> of_int 4
    | '5' -> of_int 5    | '6' -> of_int 6    | '7' -> of_int 7    | '8' -> of_int 8    | '9' -> of_int 9    
    | 'a' | 'A' -> of_int 10
    | 'b' | 'B' -> of_int 11
    | 'c' | 'C' -> of_int 12
    | 'd' | 'D' -> of_int 13
    | 'e' | 'E' -> of_int 14
    | 'f' | 'F' -> of_int 15
    | _ -> raise (Failure "Int64.of_string"))
  in parsehexa s (i+1) n (add (mul accu (of_int 16)) digit);;

let rec parseoctal s i n accu =
  if i>=n then accu 
  else let digit = 
    ( match (String.get s i) with 
    | '0' -> zero    | '1' -> one    | '2' -> of_int 2    | '3' -> of_int 3    | '4' -> of_int 4
    | '5' -> of_int 5    | '6' -> of_int 6    | '7' -> of_int 7
    | _ -> raise (Failure "Int64.of_string"))
  in parseoctal s (i+1) n (add (mul accu (of_int 8)) digit);;

let rec parsebinary s i n accu =
  if i>=n then accu 
  else let digit = 
    ( match (String.get s i) with 
    | '0' -> zero    | '1' -> one    
    | _ -> raise (Failure "Int64.of_string"))
  in parsebinary s (i+1) n (add (mul accu (of_int 2)) digit);;


let of_string s =
  if (String.length s)>2 && (String.get s 0)='0' then (
    match (String.get s 1) with 
    | 'x' | 'X' -> parsehexa s 2 (String.length s) zero
    | 'b' | 'B' -> parsebinary s 2 (String.length s) zero
    | 'o' | 'O' -> parseoctal s 2 (String.length s) zero
    | _ -> il_of_string s
   )
  else il_of_string s


external bits_of_float : float -> int64 = 
"int64" "System.BitConverter" "DoubleToInt64Bits" "float" 

external float_of_bits : int64 -> float = 
"float" "System.BitConverter" "Int64BitsToDouble" "int64" 

type t = int64

let compare = (Pervasives.compare: t -> t -> int)
