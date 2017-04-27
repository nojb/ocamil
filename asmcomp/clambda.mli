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

(* $Id: clambda.mli,v 1.24 2006/02/01 14:44:55 montela Exp $ *)

(* A variant of the "lambda" code with direct / indirect calls explicit
   and closures explicit too *)

open Asttypes
open Lambda
open Typedlambda

type namedtype = {nt_ext:bool;nt_path:string list}

type typeinfo =
  | TIint | TIchar | TIint32 | TIint64 | TInint |  TIbool 
  | TIunit  | TIvoid
  | TIfloat | TIstring
  | TIgenclosure | TIsharedclosure 
  | TIobject | TIpureIL of Il.typeref
  | TIdontknow | TInotimplemented of string
  | TIarrow of typeinfo list * typeinfo (* args -> res *)
  | TIarray of typeinfo

  | TIblock

  | TIexception (*of string * typeinfo list*)
  | TIlist of typeinfo
  | TIoption of typeinfo
  | TIrecord of namedtype
  | TIvariant of namedtype
  | TItuple of typeinfo list
  | TIlazy of typeinfo 

(*   | TIclosure of Il.typeref *)
(* objets, variants polymorphes ... *)
(* format, lazy_t *)

(* COM+ : function_labels include more information about 
          the nature of imported/exported direct call site *) 
type complus = 
  { ilns: Il.nsid ;          (* COM+ namespace *)
    ilname: Il.id ;              (* COM+ name *)
    ilrt: Il.elementType ;             (* COM+ return type *)
    ilsig : Il.signature ;       (* COM+ signature *)
  }

type function_label = { opt: string; mutable funtype:typeinfo; mutable ilinfo: complus option } 

type ulambda =
    Uvar of Ident.t
  | Uconst of structured_constant
  | Udirect_apply of function_label * ulambda list
  | Ugeneric_apply of ulambda * ulambda list
  | Uclosure of (function_label * int * Ident.t list * ulambda) list
              * ulambda list
  | Uoffset of ulambda * int
  | Ulet of Ident.t * ulambda * ulambda
  | Uletrec of (Ident.t * ulambda) list * ulambda
  | Uprim of primitive * ulambda list
  | Uswitch of ulambda * ulambda_switch
  | Ustaticfail of int * ulambda list
  | Ucatch of int * Ident.t list * ulambda * ulambda
  | Utrywith of ulambda * Ident.t * ulambda
  | Uifthenelse of ulambda * ulambda * ulambda
  | Usequence of ulambda * ulambda
  | Uwhile of ulambda * ulambda
  | Ufor of Ident.t * ulambda * ulambda * direction_flag * ulambda
  | Uassign of Ident.t * ulambda
  | Usend of ulambda * ulambda * ulambda list

and ulambda_switch =
  { us_index_consts: int array;
    us_actions_consts: ulambda array;
    us_index_blocks: int array;
    us_actions_blocks: ulambda array}

(* Description of known functions *)

type function_description =
  { fun_label: function_label;          (* Label of direct entry point *)
    fun_arity: int;                     (* Number of arguments *)
    mutable fun_closed: bool;           (* True if environment not used *)
    mutable fun_inline: (Ident.t list * ulambda) option }

(* Approximation of values *)

type value_approximation =
    Value_closure of function_description * value_approximation
  | Value_tuple of value_approximation array
  | Value_unknown
  | Value_integer of int
  | Value_constptr of int


val print_namedtype_path : namedtype -> string
val typeinfo_to_string : typeinfo -> string



