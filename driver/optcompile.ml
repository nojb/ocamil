(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: optcompile.ml,v 1.16 2006/06/26 15:22:23 montela Exp $ *)

(* The batch compiler *)

open Misc
open Config
open Format
open Typedtree

(* addition for Camil types propagation *)
open Parsetree
let insert_primitives_types =
  let rec insertin_structure = function
      [] -> []
    | sit::rem -> begin match sit.pstr_desc with 
	  Pstr_primitive(prim,vd) -> 
	    (* insertion of a dummy "let _ = prim" after each "external prim ..." declaration *)
	    let insertedexp = {pexp_desc = Pexp_ident (Longident.Lident prim);pexp_loc = sit.pstr_loc} in
	      sit::{sit with pstr_desc=Pstr_eval insertedexp}::(insertin_structure rem)
	| Pstr_module(id,modexp) -> {sit with pstr_desc=Pstr_module(id,insertin_module modexp)}::(insertin_structure rem)
	| _ -> sit::(insertin_structure rem)
      end
  and insertin_module modexp =
    match modexp.pmod_desc with
	Pmod_structure str -> {modexp with pmod_desc=Pmod_structure (insertin_structure str)}
      | _ -> (* attention ! *) modexp
  in insertin_structure
(* *)


(* Initialize the search path.
   The current directory is always searched first,
   then the directories specified with the -I option (in command-line order),
   then the standard library directory. *)

let init_path () =
  let dirs =
    if !Clflags.thread_safe
    then "+threads" :: !Clflags.include_dirs
    else !Clflags.include_dirs in
  let exp_dirs =
    List.map (expand_directory Config.standard_library) dirs in
  load_path := "" :: List.rev_append exp_dirs (Clflags.std_include_dir ());
  Env.reset_cache()

(* Return the initial environment in which compilation proceeds. *)

let initial_env () =
  init_path();
  try
    if !Clflags.nopervasives
    then Env.initial
    else Env.open_pers_signature "Pervasives" Env.initial
  with Not_found ->
    fatal_error "cannot open Pervasives.cmi"

(* Compile a .mli file *)

let interface ppf sourcefile =
  let prefixname = Misc.chop_extension_if_any sourcefile in
  let modulename = String.capitalize(Filename.basename prefixname) in
  let inputfile = Pparse.preprocess sourcefile in
  try
    let ast =
      Pparse.file ppf inputfile Parse.interface ast_intf_magic_number in
    if !Clflags.dump_parsetree then fprintf ppf "%a@." Printast.interface ast;
      Typemod.camil_clear_typedecls ();
    let sg = Typemod.transl_signature (initial_env()) ast in
      (* TEMPO CAMIL !! *)
      if not !Clflags.rebuiltmode then
	Ilcompile.compile_interface prefixname (initial_env()) ast;
      (*  *)
    if !Clflags.print_types then
      fprintf std_formatter "%a@." Printtyp.signature
                                   (Typemod.simplify_signature sg);
    Warnings.check_fatal ();
    Env.save_signature sg modulename (prefixname ^ ".cmi");
    Pparse.remove_preprocessed inputfile
  with e ->
    Pparse.remove_preprocessed_if_ast inputfile;
    raise e

(* Compile a .ml file *)

let print_if ppf flag printer arg =
  if !flag then fprintf ppf "%a@." printer arg;
  arg

let print_if_conv ppf flag printer conv arg =
  if !flag then fprintf ppf "%a@." printer (conv arg);
  arg

let (++) x f = f x
let (+++) (x, y, z) f = (x, f y, z)

let implementation ppf sourcefile =
  let prefixname = Misc.chop_extension_if_any sourcefile in
  let modulename = String.capitalize(Filename.basename prefixname) in
  let inputfile = Pparse.preprocess sourcefile in
  let env = initial_env() in
  Compilenv.reset modulename;
    Typemod.camil_clear_typedecls ();
  try
  ignore(  Pparse.file ppf inputfile Parse.implementation ast_impl_magic_number
    ++ insert_primitives_types
    ++ print_if ppf Clflags.dump_parsetree Printast.implementation
    ++ Typemod.type_implementation sourcefile prefixname modulename env
    ++ Translmod.transl_store_implementation modulename
    +++ print_if ppf Clflags.dump_trawlambda Printtypedlambda.typedlambda
    +++ print_if_conv ppf Clflags.dump_rawlambda Printlambda.lambda Typedlambda.to_lambda
    +++ Simplif.simplify_lambda
    +++ print_if ppf Clflags.dump_tlambda Printtypedlambda.typedlambda
    +++ print_if_conv ppf Clflags.dump_lambda Printlambda.lambda Typedlambda.to_lambda
    ++ Ilcompile.compile_implementation prefixname ppf);
    Warnings.check_fatal ();
    Pparse.remove_preprocessed inputfile
  with x ->
    Pparse.remove_preprocessed_if_ast inputfile;
    raise x

let c_file name =
  if Ccomp.compile_file name <> 0 then exit 2
