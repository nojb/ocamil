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

(* $Id: nativeint.ml,v 1.2 2006/07/23 03:04:14 montela Exp $ *)

(* Module [Nativeint]: processor-native integers *)

external neg: nativeint -> nativeint = "%nativeint_neg"
external add: nativeint -> nativeint -> nativeint = "%nativeint_add"
external sub: nativeint -> nativeint -> nativeint = "%nativeint_sub"
external mul: nativeint -> nativeint -> nativeint = "%nativeint_mul"
external div: nativeint -> nativeint -> nativeint = "%nativeint_div"
external rem: nativeint -> nativeint -> nativeint = "%nativeint_mod"
external logand: nativeint -> nativeint -> nativeint = "%nativeint_and"
external logor: nativeint -> nativeint -> nativeint = "%nativeint_or"
external logxor: nativeint -> nativeint -> nativeint = "%nativeint_xor"
external shift_left: nativeint -> int -> nativeint = "%nativeint_lsl"
external shift_right: nativeint -> int -> nativeint = "%nativeint_asr"
external shift_right_logical: nativeint -> int -> nativeint = "%nativeint_lsr"
external of_int: int -> nativeint = "%nativeint_of_int"
external to_int: nativeint -> int = "%nativeint_to_int"
external of_int32: int32 -> nativeint = "%nativeint_of_int32"
external to_int32: nativeint -> int32 = "%nativeint_to_int32"

let zero = of_int 0
let one = of_int 1
let minus_one = of_int (-1)
let succ n = add n one
let pred n = sub n one
let abs n = if n >= zero then n else neg n
let size = Sys.word_size
let min_int = shift_left one (size - 1)
let max_int = sub min_int one
let lognot n = logxor n minus_one


let of_float f =
  if size = 64 then
    Int64.to_nativeint (Int64.of_float f)
  else     
    of_int32 (Int32.of_float f)

let to_float n =
  if size = 64 then
    Int64.to_float (Int64.of_nativeint n)
  else     
    Int32.to_float (to_int32 n)


(* recopie sur Printf !! Attention *)
external il_format: string -> nativeint -> string = 
"string" "CamIL.String" "format_nativeint" "string" "native int"

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
  let f = fun s-> if s="n" || s ="" then "00" else s in
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

(* REM : implantation differente de Int32 et Int64 car IntPtr.Parse n'existe pas *)
let of_string s =
(*  if (Int64.compare Int64.max_int (Int64.of_nativeint max_int))=0 then*)
  if size = 64 then
    Int64.to_nativeint (Int64.of_string s)
  else     
    of_int32 (Int32.of_string s)


type t = nativeint

let compare = (Pervasives.compare: t -> t -> int)
