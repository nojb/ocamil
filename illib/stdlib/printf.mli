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

(* $Id: printf.mli,v 1.2 2004/01/20 14:37:20 montela Exp $ *)



val fprintf : out_channel -> ('a, out_channel, unit) format -> 'a

val printf : ('a, out_channel, unit) format -> 'a


val eprintf : ('a, out_channel, unit) format -> 'a


val sprintf : ('a, unit, string) format -> 'a

val bprintf : Buffer.t -> ('a, Buffer.t, unit) format -> 'a

val kprintf : (string -> string) -> ('a, unit, string) format -> 'a

(* For system use only.  Don't call directly. *)

val scan_format :
  string -> int -> (string -> int -> 'a) -> ('b -> 'c -> int -> 'a) ->
    ('e -> int -> 'a) -> 'a
