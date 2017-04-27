(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: primitive.ml,v 1.9 2006/10/16 12:45:55 montela Exp $ *)

open Il
open Misc
open Format

type il_description = 
  { ilprim_class : Il.typeref ;    (* class name *)
    ilprim_name : Il.id ;          (* method name *)
    ilprim_rt : Il.elementType ;   (* return type *)
    ilprim_sig : Il.signature ;    (* arguments types *)
    ilprim_virt : bool }           (* virtual method *)

type description =
  { prim_name: string;         (* Name of primitive  or C function *)
    prim_arity: int;           (* Number of arguments *)
    prim_alloc: bool;          (* Does it allocates or raise? *)
    prim_native_name: string;  (* Name of C function for the nat. code gen. *)
    prim_native_float: bool;   (* Does the above operate on unboxed floats? *)
    prim_IL: il_description option  }     (* Types for lightning primitives *)


(* Addition : for MSIL externals  *)

let parse_namespace str = 
  try 
    let pos = String.rindex str '.' in
      Ilbuild.type_ref (String.sub str 0 pos) (String.sub str (pos+1) ((String.length str)-pos-1))
  with Not_found ->  Ilbuild.type_ref "" str

let rec parse_type t = 
  match t with 
    "int" | "int32" -> Il.Tint32 
(* | "int16" -> Il.Tint16
  | "int8" -> Il.Tint 8 pas géré par Tlambda ...*)
  | "int64" -> Il.Tint64
  | "native int" | "nativeint" -> Il.Tnint
  | "char" -> Il.Tchar 
  | "bool" -> Il.Tbool 
  | "float" -> Il.Tfloat64
  | "void" -> Il.Tvoid 
  | "string" -> Il.Tstring
  | "bldstr" -> Ilpredef.builder_type
  | "exception" -> Ilpredef.exn_type
  | "object" -> Il.Tobject
  | "array" -> Il.Tvector (Il.Tobject)
  | x when (String.length x) > 6 && (String.sub x 0 6) = "class " ->
      let clstr =  String.sub x 6 ((String.length x)-6) in
	(match clstr with 
	     "System.Object" -> Il.Tobject
	   | "System.String" -> Il.Tstring
	   | _ -> Il.Tclass (parse_namespace clstr)
	)
  | x when (String.length x) > 2 && 
      (String.sub x ((String.length x)-2) 2) = "[]" ->
      Il.Tvector (parse_type (String.sub x 0 ((String.length x)-2)))
  | _ -> fatal_error ("Primitive declaration unknowm IL type : " ^ t)
      
let parse_type_list = function
    [] -> []
  | ["void"] -> []
  | l -> List.map (fun x -> parse_type x , None) l
 
let parse_il_decl arity = function 
    "virtual" :: return_type :: ns :: name :: args_type -> 
      if (List.length args_type) <> arity then fatal_error ("Incorrect number of parameters in definition of external "^name)
      else
      { prim_name = name ; 
        prim_native_name = name ; 
        prim_arity = arity ;
        prim_alloc = true;
        prim_native_float = false ;
        prim_IL = Some 
          { ilprim_class = parse_namespace ns ;
            ilprim_name = name ;
            ilprim_rt = parse_type return_type ;
            ilprim_sig = parse_type_list args_type ;
            ilprim_virt = true }}
  | return_type :: ns :: name :: args_type -> 
      if (List.length args_type) <> arity then fatal_error ("Incorrect number of parameters in definition of external "^name)
      else
      { prim_name = name ; 
        prim_native_name = name ; 
        prim_arity = arity ;
        prim_alloc = true;
        prim_native_float = false ;
        prim_IL = Some 
          { ilprim_class = parse_namespace ns ;
            ilprim_name = name ;
            ilprim_rt = parse_type return_type ;
            ilprim_sig = parse_type_list args_type ;
            ilprim_virt = false }}
  | x ->  fatal_error ("Incorrect declaration of CLI external function " ^ ((List.hd x)))


(* regular primitive declaration *)

let parse_declaration arity decl =
  if !Clflags.lightning && (List.hd decl).[0]<>'%'
  then parse_il_decl arity decl 
  else match decl with
    name :: "noalloc" :: name2 :: "float" :: _ ->
      {prim_name = name; prim_arity = arity; prim_alloc = false;
       prim_native_name = name2; prim_native_float = true; prim_IL = None}
  | name :: "noalloc" :: name2 :: _ ->
      {prim_name = name; prim_arity = arity; prim_alloc = false;
       prim_native_name = name2; prim_native_float = false; prim_IL = None}
  | name :: name2 :: "float" :: _ ->
      {prim_name = name; prim_arity = arity; prim_alloc = true;
       prim_native_name = name2; prim_native_float = true; prim_IL = None}
  | name :: "noalloc" :: _ ->
      {prim_name = name; prim_arity = arity; prim_alloc = false;
       prim_native_name = ""; prim_native_float = false; prim_IL = None}
  | name :: name2 :: _ ->
      {prim_name = name; prim_arity = arity; prim_alloc = true;
       prim_native_name = name2; prim_native_float = false; prim_IL = None}
  | name :: _ ->
      {prim_name = name; prim_arity = arity; prim_alloc = true;
       prim_native_name = ""; prim_native_float = false; prim_IL = None}
  | [] ->
      fatal_error "Primitive.parse_declaration"

let print_quoted s = print_char '"'; print_string s; print_char '"'

let print_description p =
  print_quoted p.prim_name;
  if not p.prim_alloc then
    (print_space(); print_quoted "noalloc");
  if p.prim_native_name <> "" then
    (print_space(); print_quoted p.prim_native_name);
  if p.prim_native_float then
    (print_space(); print_quoted "float")


let description_list p =
  let list = [p.prim_name] in
  let list = if not p.prim_alloc then "noalloc" :: list else list in
  let list =
    if p.prim_native_name <> "" then p.prim_native_name :: list else list
  in
  let list = if p.prim_native_float then "float" :: list else list in
  List.rev list

