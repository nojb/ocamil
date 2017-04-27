(* camlp4r pa_extend.cmo q_MLast.cmo *)
(***********************************************************************)
(*                                                                     *)
(*                             Camlp4                                  *)
(*                                                                     *)
(*        Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* This file has been generated by program: do not edit! *)

open Pa_extend;;

Grammar.extend
  [Grammar.Entry.obj (symbol : 'symbol Grammar.Entry.e),
   Some (Gramext.Level "top"),
   [None, Some Gramext.NonA,
    [[Gramext.Stoken ("UIDENT", "SOPT"); Gramext.Sself],
     Gramext.action
       (fun (s : 'symbol) _ (loc : int * int) -> (ssopt loc s : 'symbol));
     [Gramext.srules
        [[Gramext.Stoken ("UIDENT", "SLIST1")],
         Gramext.action (fun _ (loc : int * int) -> (true : 'e__1));
         [Gramext.Stoken ("UIDENT", "SLIST0")],
         Gramext.action (fun _ (loc : int * int) -> (false : 'e__1))];
      Gramext.Sself;
      Gramext.Sopt
        (Gramext.srules
           [[Gramext.Stoken ("UIDENT", "SEP");
             Gramext.Snterm
               (Grammar.Entry.obj (symbol : 'symbol Grammar.Entry.e))],
            Gramext.action
              (fun (t : 'symbol) _ (loc : int * int) -> (t : 'e__2))])],
     Gramext.action
       (fun (sep : 'e__2 option) (s : 'symbol) (min : 'e__1)
          (loc : int * int) ->
          (sslist loc min sep s : 'symbol))]]];;