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

(* $Id: compilenv.mli,v 1.7 2006/07/08 05:11:38 montela Exp $ *)

(* Compilation environments for compilation units *)

open Ctypedlambda

(* Each .o file has a matching .cmx file that provides the following infos
   on the compilation unit:
     - list of other units imported, with CRCs of their .cmx files
     - approximation of the structure implemented
       (includes descriptions of known functions: arity and direct entry
        points)
     - list of currying functions and application functions needed
   The .cmx file contains these infos (as an externed record) plus a CRC
   of these infos *)

type unit_infos =
  { mutable ui_name: string;                    (* Name of unit implemented *)
    mutable ui_defines: string list;      (* Unit and sub-units implemented *)
    mutable ui_imports_cmi: (string * Digest.t) list; (* Interfaces imported *)
    mutable ui_imports_cmx: (string * Il.typeref * Digest.t) list; (* Infos imported *)
    mutable ui_approx: value_approximation;     (* Approx of the structure *)
    mutable ui_curry_fun: int list;             (* Currying functions needed *)
    mutable ui_apply_fun: int list;             (* Apply functions needed *)
    mutable ui_force_link: bool;                (* Always linked *)
    mutable ui_class: Il.typeref;              (* IL class name *)
    mutable ui_compilationmode: Clflags.compmode;            
(*    mutable ui_nb_const_ctr: int; *)
  }

(* Each .a library has a matching .cmxa file that provides the following
   infos on the library: *)

type library_infos =
  { lib_units: (unit_infos * Digest.t) list;  (* List of unit infos w/ CRCs *)
    lib_ccobjs: string list;            (* C object files needed *)
    lib_ccopts: string list }           (* Extra opts to C compiler *)

val reset: string -> unit
        (* Reset the environment and record the name of the unit being
           compiled (arg). *)

val current_unit_name: unit -> string
        (* Return the name of the unit being compiled *)

val global_approx: Ident.t -> Ctypedlambda.value_approximation
        (* Return the approximation for the given global identifier *)
val set_global_approx: Ctypedlambda.value_approximation -> unit
        (* Record the approximation of the unit being compiled *)

val need_curry_fun: int -> unit
val need_apply_fun: int -> unit
        (* Record the need of a currying (resp. application) function
           with the given arity *)

val read_unit_info: string -> unit_infos * Digest.t
        (* Read infos and CRC from a [.cmx] file. *)
val write_unit_info: unit_infos -> string -> unit
        (* Save the given infos in the given file *)
val save_unit_info: string -> unit
        (* Save the infos for the current unit in the given file *)

val cmx_not_found_crc: Digest.t
        (* Special digest used in the [ui_imports_cmx] list to signal
           that no [.cmx] file was found and used for the imported unit *)

type error =
    Not_a_unit_info of string
  | Corrupted_unit_info of string
  | Illegal_renaming of string * string

exception Error of error

val report_error: Format.formatter -> error -> unit

val get_current_unit : unit -> unit_infos

(*  AJOUT RAF *)
val current_unit_addit : string list ref
val init_toplevel : unit -> unit
val set_appending_tocmx_fun : (out_channel -> unit) -> unit
val skip_header_unit_info : in_channel -> string -> unit
