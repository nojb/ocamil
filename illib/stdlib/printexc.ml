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

(* $Id: printexc.ml,v 1.6 2006/06/12 11:55:18 montela Exp $ *)

open Printf;;

let locfmt =
  match Sys.os_type with
  | "MacOS" -> ("File \"%s\"; line %d; characters %d to %d ### %s"
                : ('a, 'b, 'c) format)
  | _ -> ("File \"%s\", line %d, characters %d-%d: %s" : ('a, 'b, 'c) format)
;;

let field x i =
  try 
  let f = Obj.field x i in
  if not (Obj.is_block f) then
    sprintf "%d" (Obj.magic f : int)           (* can also be a char *)
  else if Obj.tag f = Obj.string_tag then
    sprintf "%S" (Obj.magic f : string)
  else if Obj.tag f = Obj.double_tag then
    string_of_float (Obj.magic f : float)
  else
    "_"
  with CLIinteraction.ManagedException ("System.InvalidCastException",e) -> "_"
    
;;
let rec other_fields x i =
  if i >= Obj.size x then ""
  else sprintf ", %s%s" (field x i) (other_fields x (i+1))
;;
let fields x =
  match Obj.size x with
  | 0 -> ""
  | 1 -> ""
  | 2 -> sprintf "(%s)" (field x 1)
  | n -> sprintf "(%s%s)" (field x 1) (other_fields x 2)
;;

let to_string = function
  | Out_of_memory -> "Out of memory";
  | Stack_overflow -> "Stack overflow";
  | CLIinteraction.ManagedException(s,e)->
      sprintf "ManagedException [Name=\"%s\", Message=\"%s\"]" s (CLIinteraction.getMessage e)
  | Match_failure(file, first_char, last_char) ->
      sprintf locfmt file 0 first_char last_char "Pattern matching failed";
  | Assert_failure(file, first_char, last_char) ->
      sprintf locfmt file 0 first_char last_char "Assertion failed";
  | x ->
      try
      let x = Obj.repr x in
	let constructor = 
	  (Obj.magic(Obj.field (Obj.field x 0) 0) : string) in
      constructor ^ (fields x)
      with CLIinteraction.ManagedException(_,_) -> "<Unable to print out>"
;;

let print fct arg =
  try
    fct arg
  with x ->
    eprintf "Uncaught exception: %s\n" (to_string x);
    flush stderr;
    raise x

let catch fct arg =
  try
    fct arg
  with x ->
    flush stdout;
    eprintf "Uncaught exception: %s\n" (to_string x);
    exit 2
