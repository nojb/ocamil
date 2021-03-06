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

(* $Id: optmain.ml,v 1.31 2007/03/25 11:24:59 montela Exp $ *)

open Config
open Clflags

let process_interface_file ppf name =
  Optcompile.interface ppf name

let process_implementation_file ppf name =
  Optcompile.implementation ppf name;
  objfiles := (Misc.chop_extension_if_any name ^ ".cmx") :: !objfiles

let process_file ppf name =
(*  Il.initialize_assemblyref(); *)
  if Filename.check_suffix name ".ml"
  || Filename.check_suffix name ".mlt" then begin
    Optcompile.implementation ppf name;
    objfiles := (Misc.chop_extension_if_any name ^ ".cmx") :: !objfiles
  end
  else if Filename.check_suffix name !Config.interface_suffix then
    Optcompile.interface ppf name
  else if Filename.check_suffix name ".cmx" 
       || Filename.check_suffix name ".cmxa" then
    objfiles := name :: !objfiles
  else if Filename.check_suffix name ext_obj
       || Filename.check_suffix name ext_lib then
    ccobjs := name :: !ccobjs
  else if Filename.check_suffix name ".c" then begin
    Optcompile.c_file name;
    ccobjs := (Filename.chop_suffix (Filename.basename name) ".c" ^ ext_obj)
    :: !ccobjs
  end
  else
    raise(Arg.Bad("don't know what to do with " ^ name))

let print_version_and_library () =
  print_string "The OCamIL compiler for Objective Caml version ";
  print_string Config.version; print_newline();
  print_string "Standard library directory: ";
  print_string Config.standard_library; print_newline();
  exit 0

let print_version_string () =
  print_string Config.version; print_newline(); exit 0

let print_standard_library () =
  print_string Config.standard_library; print_newline(); exit 0

let extract_output = function
  | Some s -> s
  | None ->
      prerr_endline
        "Please specify the name of the output file, using option -o";
      exit 2

let default_output = function
  | Some s -> s
  | None -> Config.default_executable_name

let usage = "Usage: ocamil <options> <files>\nOptions are:"

let main () =
  native_code := true;
  c_compiler := Config.native_c_compiler;
  c_linker := Config.native_c_linker;
  let ppf = Format.err_formatter in
  try
    Arg.parse [
(*       "-a", Arg.Set make_archive, " Build a library"; *)
       "-c", Arg.Set compile_only, " Compile only (do not link)";
(*       "-cc", Arg.String(fun s -> c_compiler := s; c_linker := s),
             "<comp>  Use <comp> as the C compiler and linker";
       "-cclib", Arg.String(fun s ->
                              ccobjs := Misc.rev_split_words s @ !ccobjs),
             "<opt>  Pass option <opt> to the C linker";
       "-ccopt", Arg.String(fun s -> ccopts := s :: !ccopts),
             "<opt>  Pass option <opt> to the C compiler and linker"; *)
       "-compact", Arg.Clear optimize_for_speed,
             " Optimize code size rather than speed";
       "-i", Arg.Set print_types, " Print the types";
       "-I", Arg.String(fun dir -> include_dirs := dir :: !include_dirs),
             "<dir>  Add <dir> to the list of include directories";
       "-impl", Arg.String (process_implementation_file ppf),
             "<file>  Compile <file> as a .ml file";
       "-inline", Arg.Int(fun n -> inline_threshold := n * 8),
             "<n>  Set aggressiveness of inlining to <n>";
       "-intf", Arg.String (process_interface_file ppf),
             "<file>  Compile <file> as a .mli file";
       "-intf-suffix", Arg.String (fun s -> Config.interface_suffix := s),
             "<file>  Suffix for interface files (default: .mli)";
       "-intf_suffix", Arg.String (fun s -> Config.interface_suffix := s),
             "<file>  (deprecated) same as -intf-suffix";
       "-labels", Arg.Clear classic, " Use commuting label mode";
(*       "-linkall", Arg.Set link_everything,
             " Link all modules, even unused ones"; *)
       "-noassert", Arg.Set noassert, " Don't compile assertion checks";
(*       "-noautolink", Arg.Set no_auto_link,
             " Don't automatically link C libraries specified in .cma files"; *)
       "-nolabels", Arg.Set classic, " Ignore non-optional labels in types";
       "-nostdlib", Arg.Set no_std_include,
           " do not add standard directory to the list of include directories";
       "-o", Arg.String(fun s -> output_name := Some s),
             "<file>  Set output file name to <file>";
(*       "-output-obj", Arg.Unit(fun () -> output_c_object := true),
             " Output a C object file instead of an executable"; *)
       "-p", Arg.Set gprofile,
             " Compile and link with profiling support for \"gprof\"\n\
               \t(not supported on all platforms)";
(*       "-pack", Arg.Set make_package,
              " Package the given .cmx files into one .cmx"; *)
       "-pp", Arg.String(fun s -> preprocessor := Some s),
             "<command>  Pipe sources through preprocessor <command>";
       "-principal", Arg.Set principal,
             " Check principality of type inference";
       "-rectypes", Arg.Set recursive_types,
             " Allow arbitrary recursive types";
       "-S", Arg.Set keep_asm_file, " Keep intermediate assembly file";
       "-thread", Arg.Set thread_safe, " Use thread-safe standard library";
       "-unsafe", Arg.Set fast,
             " No bounds checking on array and string access";
       "-v", Arg.Unit print_version_and_library,
             " Print compiler version and standard library location and exit";
       "-version", Arg.Unit print_version_string,
             " Print compiler version and exit";
       "-verbose", Arg.Set verbose, " Print calls to external commands";
       "-w", Arg.String (Warnings.parse_options false),
             "<flags>  Enable or disable warnings according to <flags>:\n\
         \032    A/a enable/disable all warnings\n\
         \032    C/c enable/disable suspicious comment\n\
         \032    D/d enable/disable deprecated features\n\
         \032    F/f enable/disable partially applied function\n\
         \032    L/l enable/disable labels omitted in application\n\
         \032    M/m enable/disable overriden methods\n\
         \032    P/p enable/disable partial match\n\
         \032    S/s enable/disable non-unit statement\n\
         \032    U/u enable/disable unused match case\n\
         \032    V/v enable/disable hidden instance variables\n\
         \032    X/x enable/disable all other warnings\n\
         \032    default setting is \"Al\" (all warnings but labels enabled)";
       "-warn-error" , Arg.String (Warnings.parse_options true),
         "<flags>  Enable or disable fatal warnings according to <flags>\n\
           \032    (see option -w for the list of flags)\n\
           \032    default setting is a (all warnings are non-fatal)";
    "-where", Arg.Unit print_standard_library,
      " Print location of standard library and exit";

       "-nopervasives", Arg.Set nopervasives, " (undocumented)";
       "-dparsetree", Arg.Set dump_parsetree, " (undocumented)";
       "-drawlambda", Arg.Set dump_rawlambda, " (undocumented)";
       "-dtrawlambda", Arg.Set dump_trawlambda, " (undocumented)"; (* ajout CamIL *)
       "-dlambda", Arg.Set dump_lambda, " (undocumented)";
       "-dtlambda", Arg.Set dump_tlambda, " (undocumented)"; (* ajout CamIL *)
       "-dstartup", Arg.Set keep_startup_file, " (undocumented)";
(* ajouts CamIL *)
       "-dulambda", Arg.Set dump_ulambda, " (undocumented)";
       "-dtulambda", Arg.Set dump_tulambda, " (undocumented)";
       "-drtlambda", Arg.Set dump_rtlambda, " (undocumented)";
       "-dil", Arg.Set dump_il, " (undocumented)";
       "-inlineIL", Arg.String(fun il -> inlined_il := il :: !inlined_il),
             "<ilfile>  Inline <ilfile> (undocumented)";
       "-key", Arg.String (fun s-> snk_file := s), "<snk file> Generate strong name assembly with <snk file> key pair";
(*       "-ilasm", Arg.String(fun s -> lightning_ilasm := s),
             "<ilasm>  Use <ilasm> as the ilasm ilassembler";  C'est quoi ca ??? *)
       "-ildebug", Arg.Set lightning_debug, " IL debug mode";
       "-peverify", Arg.Set peverify, " call PEVerify";
       "-ccl", Arg.Set compiling_camil_corelib, " must be set when compiling CamIL core library";
       "-noCLIexception", Arg.Set noILexceptionHandling, " Disable CLI exception handling";
       "-strictorder", Arg.Set ocaml_eval_order, " Strictly compliant evaluation order";
       "-noctropt", Arg.Set noctropt, " (undocumented)";
       "-plainIL", Arg.Unit (fun () -> compilation_mode := PlainIL), " (undocumented)";
       "-reflection", Arg.Set reflection_linker, " (undocumented)";
       "-rebuildtypes", Arg.Set rebuiltmode, " (undocumented)";
       "-variantasarray", Arg.Set variantrepr_objarray, " (undocumented)";
       "-recordasarray", Arg.Set recordrepr_objarray, " (undocumented)";
       "-stringrepr", Arg.String (fun s -> stringrepr := (match s with "string" -> SRO_string
							    | "strbuilder" -> SRO_strbuilder
							    | "chararray" -> SRO_chararray
							    | _ -> SRO_strbuilder (* PPPP raise error *) )), "<repr> is either string, strbuilder or chararray"; 
       "-unverifiable", Arg.Set unverif, " Emit unverifiable CIL";
       "-a", Arg.Set make_dll, "Make archive / dll";
       "-", Arg.String (process_file ppf),
            "<file>  Treat <file> as a file name (even if it starts with `-')"
      ] (process_file ppf) usage;
    if !Clflags.rebuiltmode then
      begin
	Clflags.variantrepr_objarray:=true;
	Clflags.recordrepr_objarray:=true
      end;
      

(*    Il.initialize_assemblyref();    (* sert a rien ? *) *)
(*    if !make_archive then  begin
      Optcompile.init_path();
      Asmlibrarian.create_archive (List.rev !objfiles)
                                  (extract_output !output_name)
    end
    else if !make_package then begin
      Optcompile.init_path();
      Asmpackager.package_files ppf (List.rev !objfiles)
                                    (extract_output !output_name)
    end
 else *)
    if not !compile_only && !objfiles <> [] then begin
      Optcompile.init_path();
      Ilcompile.link (List.rev !objfiles)
    end;
    exit 0
  with x ->
    Opterrors.report_error ppf x;
    exit 2

let _ = main ()
