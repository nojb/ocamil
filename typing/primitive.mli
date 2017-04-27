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

(* $Id: primitive.mli,v 1.2 2004/07/11 21:37:48 montela Exp $ *)

(* COM+ : Description of COM+ primitive functions *)


type il_description = 
  { ilprim_class : Il.typeref ;   (* class name *)
    ilprim_name : Il.id ;          (* method name *)
    ilprim_rt : Il.elementType ;         (* return type *)
    ilprim_sig : Il.signature ;    (* arguments types *)
    ilprim_virt : bool }           (* virtual method *)

(* Description of primitive functions *)

type description =
  { prim_name: string;         (* Name of primitive  or C function *)
    prim_arity: int;           (* Number of arguments *)
    prim_alloc: bool;          (* Does it allocates or raise? *)
    prim_native_name: string;  (* Name of C function for the nat. code gen. *)
    prim_native_float: bool;   (* Does the above operate on unboxed floats? *)
    prim_IL: il_description option  }     (* Types for lightning primitives *)

val parse_declaration: int -> string list -> description
(*val print_description: description -> unit*)
val description_list: description -> string list

