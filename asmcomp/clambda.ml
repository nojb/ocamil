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

(* $Id: clambda.ml,v 1.24 2006/02/01 14:44:55 montela Exp $ *)

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


(*  | TIboxed
  | TIclosure of Il.typeref *)
(* objets, variants polymorphes ... *)
(* array,list,format,option,lazy_t *)
(* magic a eliminer ou a mettre dans accurate !! *)

open Il
let str_class x = x.trnsp ^ "." ^ x.trnme

let print_namedtype_path apa =
  (if apa.nt_ext then " (ext)" else "")^(List.fold_left (fun s md -> if s="" then md else md^"."^s) "" apa.nt_path)

let rec typeinfo_to_string = function
    TIint ->  "int"
  | TIchar ->  "char"
  | TIint32 ->  "int32"
  | TIint64 ->  "int64"
  | TInint ->  "nint"
  | TIfloat ->  "float"
  | TIbool ->  "bool"
  | TIunit ->  "unit"
  | TIstring ->  "string"
  | TIvoid ->  "void"
  | TIgenclosure -> "genclos"
  | TIsharedclosure -> "mclos"
  | TIobject ->  "<obj>"
(*  | TIclosure cid -> Printf.sprintf "<C!%s>" (str_class cid) *)
  | TIarrow (arg,res) -> Printf.sprintf  "(%s->%s)" (prlist "->" arg) (typeinfo_to_string res)
  | TIarray ti ->  Printf.sprintf "%s[]" (typeinfo_to_string ti)
  | TIpureIL cid -> Printf.sprintf "<IL:%s>" (str_class cid)
  | TInotimplemented x -> failwith ("to_il Not Impl "^x)
  | TIdontknow ->  "<?>"

  | TIblock -> "block"

  | TIexception ->  "exception"
  | TIlazy ti ->  Printf.sprintf "%s lazy_t" (typeinfo_to_string ti)
  | TIlist ti ->  Printf.sprintf  "%s list" (typeinfo_to_string  ti)
  | TIoption ti ->  Printf.sprintf  "%s option" (typeinfo_to_string ti)
  | TIrecord id -> print_namedtype_path id
  | TIvariant id -> print_namedtype_path id
  | TItuple tilist ->  Printf.sprintf  "(%s)" (prlist "*" tilist)

and prlist sep = function
    [t] ->  Printf.sprintf "%s" (typeinfo_to_string t)
  | t::q -> Printf.sprintf "%s%s%s" (typeinfo_to_string t) sep (prlist sep q)
  | [] -> failwith "Printulambda.prlist"


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
    us_actions_consts : ulambda array;
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
