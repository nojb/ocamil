(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*      Damien Doligez and Francois Rouaix, INRIA Rocquencourt         *)
(*   Ported to Objective Caml by John Malecki and Xavier Leroy         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id: profiling.mli,v 1.5 2001/12/07 13:41:02 xleroy Exp $ *)

(* Run-time library for profiled programs *)

val counters: (string * (string * int array)) list ref
