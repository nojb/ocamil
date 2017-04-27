(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*         Jerome Vouillon, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id: oo.ml,v 1.26 2002/06/26 09:12:49 xleroy Exp $ *)

let copy = CamlinternalOO.copy
external id : < .. > -> int = "%field1"
let new_method = CamlinternalOO.new_method
let public_method_label = CamlinternalOO.public_method_label
