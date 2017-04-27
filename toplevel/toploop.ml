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

(* $Id: toploop.ml,v 1.25 2006/06/28 12:40:00 montela Exp $ *)

(* The interactive toplevel loop *)

open Path
open Lexing
open Format
open Config
open Misc
open Parsetree
open Types
open Typedtree
open Outcometree

type directive_fun =
   | Directive_none of (unit -> unit)
   | Directive_string of (string -> unit)
   | Directive_int of (int -> unit)
   | Directive_ident of (Longident.t -> unit)
   | Directive_bool of (bool -> unit)

(* The table of toplevel value bindings and its accessors *)

let toplevel_value_bindings =
  (Hashtbl.create 37 : (string, Obj.t) Hashtbl.t)

let getvalue name =
  try
    Hashtbl.find toplevel_value_bindings name
  with Not_found ->
    fatal_error (name ^ " unbound at toplevel")

let setvalue name v =
  Hashtbl.replace toplevel_value_bindings name v

(* Return the value referred to by a path *)

let rec eval_path = function
  | Pident id ->
      if Ident.persistent id || Ident.global id then
        Symtable.get_global_value id
      else begin
        let name = Ident.name id in
        try
          Hashtbl.find toplevel_value_bindings name
        with Not_found ->
          raise (Symtable.Error(Symtable.Undefined_global name))
      end
  | Pdot(p, s, pos) ->
      Obj.field (eval_path p) pos
  | Papply(p1, p2) ->
      fatal_error "Toploop.eval_path"

(* To print values *)


module ModifiedObj = struct
  type t = Obj.t
  let is_block o = (Obj.is_block o) && (Obj.size o > 0)
  let obj o = 
    if is_block o then Obj.obj o
    else if Obj.is_block o then (Obj.magic (Obj.tag o))
    else Obj.obj o
  let tag = Obj.tag
  let size = Obj.size
  let field = Obj.field
  let record_field = Obj.record_field
end

module EvalPath = struct
  type value = ModifiedObj.t
  exception Error
  let eval_path p = try eval_path p with Symtable.Error _ -> raise Error
  let same_value v1 v2 = (v1 == v2)
end

module Printer = Genprintval.Make(ModifiedObj)(EvalPath)

let max_printer_depth = ref 100
let max_printer_steps = ref 300

let print_out_value = Oprint.out_value
let print_out_type = Oprint.out_type
let print_out_class_type = Oprint.out_class_type
let print_out_module_type = Oprint.out_module_type
let print_out_sig_item = Oprint.out_sig_item
let print_out_signature = Oprint.out_signature
let print_out_phrase = Oprint.out_phrase

let print_untyped_exception ppf obj =
  !print_out_value ppf (Printer.outval_of_untyped_exception obj)
let outval_of_value env obj ty =
  Printer.outval_of_value !max_printer_steps !max_printer_depth
    (fun _ _ _ -> None) env obj ty
let print_value env obj ppf ty =
  !print_out_value ppf (outval_of_value env obj ty)

let install_printer = Printer.install_printer
let remove_printer = Printer.remove_printer

(* Hooks for parsing functions *)

let parse_toplevel_phrase = ref Parse.toplevel_phrase
let parse_use_file = ref Parse.use_file
let print_location = Location.print
let print_warning = Location.print_warning
let input_name = Location.input_name

(* Load in-core and execute a lambda term *)

let may_trace = ref false (* Global lock on tracing *)
type evaluation_outcome = Result of Obj.t | Exception of exn


open Ident

let load_lambda ppf lam =
  if !Clflags.dump_rawlambda then fprintf ppf "%a@." Printtypedlambda.typedlambda lam;
  let slam = Simplif.simplify_lambda lam in
  if !Clflags.dump_lambda then fprintf ppf "%a@." Printtypedlambda.typedlambda slam;
  let size = 101 in (* !!!! c'est la taille des globals ! que mettre ? *)

  Ildynamic.compile_phrase ppf (size,slam);
(*TEMPO
  let (init_code, fun_code) = Bytegen.compile_phrase slam in
  let (code, code_size, reloc) = Emitcode.to_memory init_code fun_code in
  let can_free = (fun_code = []) in
 *)
  let initial_symtable = Symtable.current_state() in
      (*TEMPO Symtable.patch_object code reloc;*)
  Symtable.update_global_table();
  try
    may_trace := true;
	(*TEMPO	let retval = (Meta.reify_bytecode code code_size) () in*)
	(*TEMPO*)	Ildynamic.execute_last_phrase setvalue getvalue;
    may_trace := false;
	(*TEMPO	if can_free then Meta.static_free code;
	  Result retval *)
	(*TEMPO*) Result (Obj.repr (getvalue "$prev"))
      with 
	  CLIinteraction.ManagedException (name,e) -> 
	    let message=CLIinteraction.getMessage e in
	      Utils.bug "Toploop: exception during Phrase evaluation " (name^":"^message)
	| x ->
    may_trace := false;
	    (*TEMPO	if can_free then Meta.static_free code;
	      Symtable.restore_state initial_symtable;*)
    Exception x




(* Print the outcome of an evaluation *)

let pr_item env = function
  | Tsig_value(id, decl) :: rem ->
      let tree = Printtyp.tree_of_value_description id decl in
      let valopt =
        match decl.val_kind with
        | Val_prim _ -> None
        | _ ->
            let v =
              outval_of_value env (getvalue (Translmod.toplevel_name id))
                decl.val_type
            in
            Some v
      in
      Some (tree, valopt, rem)
  | Tsig_type(id, decl) :: rem ->
      let tree = Printtyp.tree_of_type_declaration id decl in
      Some (tree, None, rem)
  | Tsig_exception(id, decl) :: rem ->
      let tree = Printtyp.tree_of_exception_declaration id decl in
      Some (tree, None, rem)
  | Tsig_module(id, mty) :: rem ->
      let tree = Printtyp.tree_of_module id mty in
      Some (tree, None, rem)
  | Tsig_modtype(id, decl) :: rem ->
      let tree = Printtyp.tree_of_modtype_declaration id decl in
      Some (tree, None, rem)
  | Tsig_class(id, decl) :: cltydecl :: tydecl1 :: tydecl2 :: rem ->
      let tree = Printtyp.tree_of_class_declaration id decl in
      Some (tree, None, rem)
  | Tsig_cltype(id, decl) :: tydecl1 :: tydecl2 :: rem ->
      let tree = Printtyp.tree_of_cltype_declaration id decl in
      Some (tree, None, rem)
  | _ -> None

let rec item_list env = function
  | [] -> []
  | items ->
     match pr_item env items with
     | None -> []
     | Some (tree, valopt, items) -> (tree, valopt) :: item_list env items

(* The current typing environment for the toplevel *)

let toplevel_env = ref Env.empty

(* Print an exception produced by an evaluation *)

let print_out_exception ppf exn outv =
  !print_out_phrase ppf (Ophr_exception (exn, outv))

let print_exception_outcome ppf exn =
  if exn = Out_of_memory then (*TEMPO Gc.full_major*) ();
  let outv = outval_of_value !toplevel_env (Obj.repr exn) Predef.type_exn in
  print_out_exception ppf exn outv

(* The table of toplevel directives. 
   Filled by functions from module topdirs. *)

let directive_table = (Hashtbl.create 13 : (string, directive_fun) Hashtbl.t)

(* Execute a toplevel phrase *)

let execute_phrase print_outcome ppf phr =
  match phr with
  | Ptop_def sstr ->
      Typemod.camil_clear_typedecls ();
      let oldenv = !toplevel_env in
      Typecore.reset_delayed_checks ();
      let (str, sg, newenv) = Typemod.type_structure oldenv sstr in
      Typecore.force_delayed_checks ();
      let lam = Translmod.transl_toplevel_definition str in
      Warnings.check_fatal ();
      begin try
        toplevel_env := newenv;
        let res = load_lambda ppf lam in
        let out_phr =
          match res with
          | Result v ->
              if print_outcome then
                match str with
                | [Tstr_eval exp] ->
                    let outv = outval_of_value newenv v exp.exp_type in
                    let ty = Printtyp.tree_of_type_scheme exp.exp_type in
                    Ophr_eval (outv, ty)
                | [] -> Ophr_signature []
                | _ -> Ophr_signature (item_list newenv
                                             (Typemod.simplify_signature sg))
              else Ophr_signature []
          | Exception exn ->
              toplevel_env := oldenv;
(*TEMPO              if exn = Out_of_memory then Gc.full_major();*)
              let outv =
                outval_of_value !toplevel_env (Obj.repr exn) Predef.type_exn
              in
              Ophr_exception (exn, outv)
        in
        !print_out_phrase ppf out_phr;
        begin match out_phr with
        | Ophr_eval (_, _) | Ophr_signature _ -> true
        | Ophr_exception _ -> false
        end
      with x ->
        toplevel_env := oldenv; raise x
      end      
  | Ptop_dir(dir_name, dir_arg) ->
      try
        match (Hashtbl.find directive_table dir_name, dir_arg) with
        | (Directive_none f, Pdir_none) -> f (); true
        | (Directive_string f, Pdir_string s) -> f s; true
        | (Directive_int f, Pdir_int n) -> f n; true
        | (Directive_ident f, Pdir_ident lid) -> f lid; true
        | (Directive_bool f, Pdir_bool b) -> f b; true
        | (_, _) ->
            fprintf ppf "Wrong type of argument for directive `%s'.@." dir_name;
            false
      with Not_found ->
        fprintf ppf "Unknown directive `%s'.@." dir_name;
        false

(* Temporary assignment to a reference *)

let protect r newval body =
  let oldval = !r in
  try
    r := newval; 
    let res = body() in
    r := oldval;
    res
  with x ->
    r := oldval;
    raise x

(* Read and execute commands from a file *)

let use_print_results = ref true

let use_file ppf name =
  try
    let filename = find_in_path !Config.load_path name in
    let ic = open_in_bin filename in
    let lb = Lexing.from_channel ic in
    (* Skip initial #! line if any *)
    Lexer.skip_sharp_bang lb;
    let success =
      protect Location.input_name filename (fun () ->
        try
          List.iter
            (fun ph ->
              if !Clflags.dump_parsetree then Printast.top_phrase ppf ph;
              if not (execute_phrase !use_print_results ppf ph) then raise Exit)
            (!parse_use_file lb);
          true
        with
        | Exit -> false
        | Sys.Break -> fprintf ppf "Interrupted.@."; false
        | x -> Opterrors.report_error ppf x; false) in
    close_in ic;
    success
  with Not_found -> fprintf ppf "Cannot find file %s.@." name; false

let use_silently ppf name =
  protect use_print_results false (fun () -> use_file ppf name)

(* Reading function for interactive use *)

let first_line = ref true
let got_eof = ref false;;

let input_char_function = ref (fun () -> input_char stdin);;

let refill_lexbuf buffer len =
  if !got_eof then (got_eof := false; 0) else begin
    let prompt =
      if !first_line then "# "
      else if Lexer.in_comment () then "* "
      else "  "
    in
    output_string stdout prompt; flush stdout;
    first_line := false;
    let i = ref 0 in
    try
      while true do
        if !i >= len then raise Exit;
        let c = !input_char_function() in
        buffer.[!i] <- c;
        incr i;
        if c = '\n' then raise Exit;
      done;
      !i
    with
    | End_of_file ->
        Location.echo_eof ();
        if !i > 0 then (got_eof := true; !i) else 0
    | Exit -> !i
  end

(* Discard data left in lexer buffer. *)

let empty_lexbuf lb =
  lb.lex_curr_pos <- 0;
  lb.lex_abs_pos <- 0;
  lb.lex_buffer_len <- 0

(* Toplevel initialization. Performed here instead of at the
   beginning of loop() so that user code linked in with ocamlmktop
   can call directives from Topdirs. *)

let init_toplevel() =
  Compilenv.init_toplevel();
  Ilcompile.initialize_assemblyref() (*; TODO !!
  Il.new_assemblyref "Toploop" "[ocamiltop]" *)

let _ =
  Sys.interactive := true;
(*TEMPO  Symtable.init_toplevel(); *) (* Comprendre ce que ça fait exactement ... *)
  (*TEMPO*) (* Compilenv.*)init_toplevel();
  Optcompile.init_path()

let load_ocamlinit ppf =
  if Sys.file_exists ".ocamlinit" then ignore(use_silently ppf ".ocamlinit")

let set_paths () =
  (* Add whatever -I options have been specified on the command line,
     but keep the directories that user code linked in with ocamlmktop
     may have added to load_path. *)
  load_path := !load_path @ [Filename.concat Config.standard_library "camlp4"];
  load_path := "" :: (List.rev !Clflags.include_dirs @ !load_path)
(*TEMPO ; Dll.add_path !load_path*)

let initialize_toplevel_env () =
 (*TEMPO*) setvalue "$prev" (Obj.repr ());
  toplevel_env := Optcompile.initial_env()
    

(* The interactive loop *)

exception PPerror

let loop ppf =
  fprintf ppf "(CAMIL) Objective Caml version %s@.@." Config.version;
  initialize_toplevel_env ();
  let lb = Lexing.from_function refill_lexbuf in
  Location.input_name := "";
  Location.input_lexbuf := Some lb;
(*TEMPO  Sys.catch_break true;*)
  load_ocamlinit ppf;
  while true do
    try
      empty_lexbuf lb;
      Location.reset();
      first_line := true;
      let phr = try !parse_toplevel_phrase lb with Exit -> raise PPerror in
      if !Clflags.dump_parsetree then Printast.top_phrase ppf phr;
      ignore(execute_phrase true ppf phr)
    with
    | End_of_file -> exit 0
    | Sys.Break -> fprintf ppf "Interrupted.@."
    | PPerror -> ()
    | x -> Opterrors.report_error ppf x
  done

let result_for_form () =
  Buffer.contents Format.stdbuf;;

let init_loop_for_form dummyformatter = 
  let ppf = Format.str_formatter in
    fprintf ppf "Objective Caml version %s@.@." Config.version;
    initialize_toplevel_env ();
    let lb = Lexing.from_function refill_lexbuf in
	Location.input_name := "";
	Location.input_lexbuf := Some lb;;
	(*      load_ocamlinit ppf; *)
	
let loop_for_form windowcontent = 
  Buffer.clear Format.stdbuf;
  let ppf = Format.str_formatter in
  let index=ref (-1) in
  let loopform_inputchar () =
    incr index;
    windowcontent.[!index]
  in
    input_char_function := loopform_inputchar;
    try
      let lb = Lexing.from_function refill_lexbuf in
	empty_lexbuf lb;
	Location.reset();
	first_line := true;
	let phr = try !parse_toplevel_phrase lb with Exit -> raise PPerror in
	  if !Clflags.dump_parsetree then Printast.top_phrase ppf phr;
	  ignore(execute_phrase true ppf phr)
    with
      | End_of_file -> exit 0
      | Sys.Break -> fprintf ppf "Interrupted.@."
      | PPerror -> ()
      | x -> Opterrors.report_error ppf x
	  


(* Execute a script *)
external shift_argv : unit -> unit =
  "void" "CamIL.Sys" "shift_argv" "object"

let run_script ppf name args =
  let rec find n =
    if n >= Array.length args then invalid_arg "Toploop.run_script";
    if args.(n) = name then n else find (n+1) 
  in
  let pos = find 0 in
  let len = Array.length args - pos in
  if Array.length Sys.argv < len then invalid_arg "Toploop.run_script";
(* OLD..    Array.blit args pos Sys.argv 0 len;
   Obj.func_truncate (Obj.repr Sys.argv) len; *)
  ignore(shift_argv());
  Arg.current := 0;
  Optcompile.init_path();
  initialize_toplevel_env();
  Sys.interactive := false;
  use_silently ppf name
