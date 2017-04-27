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

(* $Id: topmain.ml,v 1.16 2006/07/10 00:35:55 montela Exp $ *)

open Clflags

let _ = reflection_linker := true
let _ = toplevel_mode := true

let usage = "Usage: ocaml <options> <object-files> [script-file]\noptions are:"

let preload_objects = ref []

let prepare ppf =
  Toploop.set_paths ();
  try List.for_all (Topdirs.load_file ppf) (List.rev !preload_objects)
  with x ->
    try Opterrors.report_error ppf x; false
    with x ->
      Format.fprintf ppf "Uncaught exception: %s\n" (Printexc.to_string x);
      false


let file_argument name =
  let ppf = Format.err_formatter in
  if (*TODO Filename.check_suffix name ".cmx" || *) Filename.check_suffix name ".cmxa"
  then preload_objects := name :: !preload_objects
  else exit
      (if prepare ppf && Toploop.run_script ppf name Sys.argv then 0 else 2)

let main () =
  Arg.parse [
     "-I", Arg.String(fun dir ->
       let dir = Misc.expand_directory Config.standard_library dir in
       include_dirs := dir :: !include_dirs),
           "<dir>  Add <dir> to the list of include directories";
     "-labels", Arg.Clear classic, " Labels commute (default)";
     "-noassert", Arg.Set noassert, " Do not compile assertion checks";
     "-nolabels", Arg.Set classic, " Ignore labels and do not commute";
     "-nostdlib", Arg.Set no_std_include,
           " do not add default directory to the list of include directories";
     "-principal", Arg.Set principal, " Check principality of type inference";
     "-rectypes", Arg.Set recursive_types, " Allow arbitrary recursive types";
     "-unsafe", Arg.Set fast, " No bound checking on array and string access";
     "-w", Arg.String (Warnings.parse_options false),
           "<flags>  Enable or disable warnings according to <flags>:\n\
       \032    A/a enable/disable all warnings\n\
       \032    C/c enable/disable suspicious comment\n\
       \032    D/d enable/disable deprecated features\n\
       \032    F/f enable/disable partially applied function\n\
       \032    M/m enable/disable overriden method\n\
       \032    P/p enable/disable partial match\n\
       \032    S/s enable/disable non-unit statement\n\
       \032    U/u enable/disable unused match case\n\
       \032    V/v enable/disable hidden instance variable\n\
       \032    X/x enable/disable all other warnings\n\
       \032    default setting is \"Al\" (all warnings but labels enabled)";
     "-warn-error" , Arg.String (Warnings.parse_options true),
       "<flags>  Enable or disable fatal warnings according to <flags>\n\
         \032    (see option -w for the list of flags)\n\
         \032    default setting is a (all warnings are non-fatal)";

     "-dparsetree", Arg.Set dump_parsetree, " (undocumented)";
     "-drawlambda", Arg.Set dump_rawlambda, " (undocumented)";
     "-dlambda", Arg.Set dump_lambda, " (undocumented)";
(*     "-dinstr", Arg.Set dump_instr, " (undocumented)";*)
	 "-dulambda", Arg.Set dump_ulambda, " (undocumented)";
       "-dtlambda", Arg.Set dump_tlambda, " (undocumented)";
       "-dtulambda", Arg.Set dump_tulambda, " (undocumented)";
       "-drtlambda", Arg.Set dump_rtlambda, " (undocumented)";
       "-dil", Arg.Set dump_il, " (undocumented)";
       "-ildebug", Arg.Set lightning_debug, " il debug mode";
       "-noCLIexception", Arg.Set noILexceptionHandling, " Disable CLI exception handling";
       "-strictorder", Arg.Set ocaml_eval_order, " Strictly compliant evaluation order";
       "-noctropt", Arg.Set noctropt, " (undocumented)";
       "-plainIL", Arg.Unit (fun () -> compilation_mode := PlainIL), " <undocumented>";
       "-reflection", Arg.Set reflection_linker, " (undocumented)";
       "-verbose", Arg.Set verbose, " Print calls to external commands";
    ] file_argument usage;
  if not (prepare Format.err_formatter) then exit 2;
  Toploop.loop Format.std_formatter

