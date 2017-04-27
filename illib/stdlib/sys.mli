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

(* $Id: sys.mli,v 1.2 2006/07/23 03:04:14 montela Exp $ *)

(** System interface. *)

val argv : string array
(** The command line arguments given to the process.
   The first element is the command name used to invoke the program.
   The following elements are the command-line arguments
   given to the program. *)

val executable_name : string
(** The name of the file containing the executable currently running. *)

external file_exists : string -> bool = 
 "int" "CamIL.Sys" "sys_file_exists" "string"
(** Test if a file with the given name exists. *)

external remove : string -> unit = 
 "void" "CamIL.Sys" "sys_remove" "string"
(** Remove the given file name from the file system. *)

external rename : string -> string -> unit = 
 "void" "CamIL.Sys" "sys_rename" "string" "string"
(** Rename a file. The first argument is the old name and the
   second is the new name. *)

val getenv : string -> string 
(** Return the value associated to a variable in the process
   environment. Raise [Not_found] if the variable is unbound. *)

external command : string -> int = 
 "int" "CamIL.Sys" "sys_system_command" "string"
(** Execute the given shell command and return its exit code. *)

external time : unit -> float = 
 "float" "CamIL.Sys" "sys_time" "void"
(** Return the processor time, in seconds, used by the program
   since the beginning of execution. *)

external chdir : string -> unit = 
 "void" "CamIL.Sys" "sys_chdir" "string"
(** Change the current working directory of the process. *)

external getcwd : unit -> string = 
 "string" "CamIL.Sys" "sys_getcwd" "void"
(** Return the current working directory of the process. *)

val interactive : bool ref
(** This reference is initially set to [false] in standalone
   programs and to [true] if the code is being executed under
   the interactive toplevel system [ocaml]. *)

val os_type : string
(** Operating system currently executing the Caml program. One of
   ["Unix"] (for all Unix versions, including Linux and Mac OS X),
   ["Win32"] (for MS-Windows, OCaml compiled with MSVC++),
   ["Cygwin"] (for MS-Windows, OCaml compiled with Cygwin),
   ["MacOS"] (for MacOS 9). *)

val word_size : int
(** Size of one word on the machine currently executing the Caml
   program, in bits: 32 or 64. *)

val max_string_length : int
(** Maximum length of a string. *)

val max_array_length : int
(** Maximum length of an array. *)


(** {6 Signal handling} *)


type signal_behavior =
    Signal_default
  | Signal_ignore 
  | Signal_handle of (int -> unit)
(** What to do when receiving a signal:
   - [Signal_default]: take the default behavior
     (usually: abort the program)
   - [Signal_ignore]: ignore the signal
   - [Signal_handle f]: call function [f], giving it the signal
   number as argument. *)

(* external signal :
  int -> signal_behavior -> signal_behavior = "install_signal_handler" *)
(** Set the behavior of the system on receipt of a given signal.
   The first argument is the signal number.  Return the behavior
   previously associated with the signal. *)

(* val set_signal : int -> signal_behavior -> unit *)
(** Same as {!Sys.signal} but return value is ignored. *)


(** {7 Signal numbers for the standard POSIX signals.} *) 

val sigabrt : int
(** Abnormal termination *)

val sigalrm : int
(** Timeout *)

val sigfpe : int
(** Arithmetic exception *)

val sighup : int
(** Hangup on controlling terminal *)

val sigill : int
(** Invalid hardware instruction *)

val sigint : int
(** Interactive interrupt (ctrl-C) *)

val sigkill : int
(** Termination (cannot be ignored) *)

val sigpipe : int
(** Broken pipe *)

val sigquit : int
(** Interactive termination *)

val sigsegv : int
(** Invalid memory reference *)

val sigterm : int
(** Termination *)

val sigusr1 : int
(** Application-defined signal 1 *)

val sigusr2 : int
(** Application-defined signal 2 *)

val sigchld : int
(** Child process terminated *)

val sigcont : int
(** Continue *)

val sigstop : int
(** Stop *)

val sigtstp : int
(** Interactive stop *)

val sigttin : int
(** Terminal read from background process *)

val sigttou : int
(** Terminal write from background process *)

val sigvtalrm : int
(** Timeout in virtual time *)

val sigprof : int
(** Profiling interrupt *)


exception Break
(** Exception raised on interactive interrupt if {!Sys.catch_break}
   is on. *)

(* val catch_break : bool -> unit *)
(** [catch_break] governs whether interactive interrupt (ctrl-C)
   terminates the program or raises the [Break] exception. 
   Call [catch_break true] to enable raising [Break],
   and [catch_break false] to let the system
   terminate the program on user interrupt. *)


val ocaml_version : string;;
(** [ocaml_version] is the version of Objective Caml.
    It is a string of the form ["major.minor[additional-info]"]
    Where major and minor are integers, and [additional-info] is
    a string that is empty or starts with a '+'. *)
