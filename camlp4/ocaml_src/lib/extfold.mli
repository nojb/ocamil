(* camlp4r *)
(* This file has been generated by program: do not edit! *)

type ('te, 'a, 'b) t =
  'te Gramext.g_entry -> 'te Gramext.g_symbol list -> ('te Stream.t -> 'a) ->
    'te Stream.t -> 'b
;;

type ('te, 'a, 'b) tsep =
  'te Gramext.g_entry -> 'te Gramext.g_symbol list -> ('te Stream.t -> 'a) ->
    ('te Stream.t -> unit) -> 'te Stream.t -> 'b
;;

val sfold0 : ('a -> 'b -> 'b) -> 'b -> (_, 'a, 'b) t;;
val sfold1 : ('a -> 'b -> 'b) -> 'b -> (_, 'a, 'b) t;;
val sfold0sep : ('a -> 'b -> 'b) -> 'b -> (_, 'a, 'b) tsep;;
val sfold1sep : ('a -> 'b -> 'b) -> 'b -> (_, 'a, 'b) tsep;;

val slist0 : (_, 'a, 'a list) t;;
val slist1 : (_, 'a, 'a list) t;;
val slist0sep : (_, 'a, 'a list) tsep;;
val slist1sep : (_, 'a, 'a list) tsep;;

val sopt : (_, 'a, 'a option) t;;
