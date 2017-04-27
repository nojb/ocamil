(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../../LICENSE.  *)
(*                                                                     *)
(***********************************************************************)

(* $Id: thread.ml,v 1.3 2006/07/23 03:04:14 montela Exp $ *)

(* User-level threads *)

type t

external threads_self: unit -> t 
= "class System.Threading.Thread" "CamIL.Threads" "self" "void"

let self = fun () -> threads_self() (* cf pb de void<->unit *)

external threads_get_name: t ->  string
= "string" "CamIL.Threads" "get_name" "class System.Threading.Thread"

let id th = int_of_string (threads_get_name th)

external threads_abort: t-> unit
= "void" "CamIL.Threads" "abort" "class System.Threading.Thread"

external threads_join: t-> unit
= "void" "CamIL.Threads" "join" "class System.Threading.Thread"


let join th = 
  let _= threads_join th in ()

let kill th =
  threads_abort th;
  join th

let exit th =
  let _ = threads_abort (self()) in ()


external threads_create: Obj.t -> string -> t
= "class System.Threading.Thread" "CamIL.Threads" "create" "class CamIL.Closure" "string"

let arg_in_closure f x = Obj.repr (fun () -> ignore (f x))

(* TODO faire un generateur d'IDs fraiches *)
let last_nb = ref 0;;

let id_generator () =
  incr last_nb;
  string_of_int(!last_nb);;  

let create f x =
  let f2 = arg_in_closure f x in
    threads_create f2 (id_generator())

external threads_delay: int -> int -> int -> int -> unit
= "void" "CamIL.Threads" "delay" "int" "int" "int" "int"

let delay d =
  let secs = int_of_float (mod_float d 60.0) in
  let d2 = floor (d /. 60.0) in
  let mins = int_of_float (mod_float d2 60.0) in
  let d3 = floor (d2 /. 60.0) in
  let hours = int_of_float (mod_float d3 24.0) in
  let days = int_of_float (floor (d2 /. 24.0)) in
  let _= threads_delay days hours mins secs in ();;

external threads_sleep:t -> unit
= "void" "CamIL.Threads" "sleep" "class System.Threading.Thread"

external threads_wakeup:t -> unit
= "void" "CamIL.Threads" "wakeup" "class System.Threading.Thread"

let sleep () =
  let _ = threads_sleep (self()) in ()

let wakeup th = 
  let _ = threads_wakeup th in ()
