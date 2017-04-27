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
(*  adapted by Raphael Montelatici                                     *)
(*                                                                     *)
(***********************************************************************)

(* $Id: pervasives.ml,v 1.36 2007/03/25 14:05:17 montela Exp $ *)


(* ******************************************* *)

(* BOULOT : *)
(* Vérifier que les primitives lèvent bien les bonnes exceptions *)

(* MANQUENT : *)

(* frexp, ldexp, modf, float_of_bytes, infinity, neg_infinity  *)
(* nan, max_float, min_float, epsilon_float, classify_float *)
(* format_int, format_float, valid_float_lexem *)
(* out_channels_list, flush_all, marshal_to_channel *)
(* pos_out, set_binary_mode_out *)
(* pos_in, set_binary_mode_in *)
(* *** le module LargeFile *** *)
(* type format, format_of_string, string_of_format, string_to_format, (^^) *)
(* register_named_value *)

(* ******************************************* *)

(* type 'a option = None | Some of 'a *)

(* Exceptions *)

external raise : exn -> 'a = "%raise"

let failwith s = raise(Failure s)
let invalid_arg s = raise(Invalid_argument s)

exception Exit

(* Comparisons *)

external (=) : 'a -> 'a -> bool = "%equal"
external (<>) : 'a -> 'a -> bool = "%notequal"
external (<) : 'a -> 'a -> bool = "%lessthan"
external (>) : 'a -> 'a -> bool = "%greaterthan"
external (<=) : 'a -> 'a -> bool = "%lessequal"
external (>=) : 'a -> 'a -> bool = "%greaterequal"

(* ATTENTION, compare n'est peut-etre pas correct ... *)
(*
let compare a b = failwith ("Vous utilisez Pervasives.compare : c'est dangereux ")
*)

external compare: 'a -> 'a -> int =
   "int" "CamIL.Compare" "compare" "object" "object"


let min x y = if x <= y then x else y
let max x y = if x >= y then x else y

external (==) : 'a -> 'a -> bool = "%eq"
external (!=) : 'a -> 'a -> bool = "%noteq"

(* Boolean operations *)

external not : bool -> bool = "%boolnot"
external (&) : bool -> bool -> bool = "%sequand"
external (&&) : bool -> bool -> bool = "%sequand"
external (or) : bool -> bool -> bool = "%sequor"
external (||) : bool -> bool -> bool = "%sequor"

(* Integer operations *)

(* Attention, on a pas les entiers 31 bits, mais 32 bits ... *)

external (~-) : int -> int = "%negint"
external succ : int -> int = "%succint"
external pred : int -> int = "%predint"
external (+) : int -> int -> int = "%addint"
external (-) : int -> int -> int = "%subint"
external ( * ) : int -> int -> int = "%mulint"
external (/) : int -> int -> int = "%divint"
external (mod) : int -> int -> int = "%modint"

let abs x = if x >= 0 then x else -x

external (land) : int -> int -> int = "%andint"
external (lor) : int -> int -> int = "%orint"
external (lxor) : int -> int -> int = "%xorint"

let lnot x = x lxor (-1)

external (lsl) : int -> int -> int = "%lslint"
external (lsr) : int -> int -> int = "%lsrint"
external (asr) : int -> int -> int = "%asrint"

let get_shift () =
  if 1 lsl 31 = 0 || 1 lsl 32 = 0 then 31 else 63
(* we use a function call to prevent the compiler from inlining the lsl value *)
(* which is incorrect when compiling with the non-bootstrap CamIL compiler *)
(* because inlining is performed in the Ocaml runtime with 31[63]bits integers, for a CLR library *)
let min_int = 1 lsl (get_shift())

let max_int = min_int - 1

(* Floating-point operations *)

external (~-.) : float -> float = "%negfloat"
external (+.) : float -> float -> float = "%addfloat"
external (-.) : float -> float -> float = "%subfloat"
external ( *. ) : float -> float -> float = "%mulfloat"
external (/.) : float -> float -> float = "%divfloat"
external ( ** ) : float -> float -> float = 
  "float" "System.Math" "Pow" "float" "float"
external exp : float -> float =  "float" "System.Math" "Exp" "float"
external acos : float -> float = "float" "System.Math" "Acos" "float"
external asin : float -> float = "float" "System.Math" "Asin" "float"
external atan : float -> float = "float" "System.Math" "Atan" "float"
external atan2 : float -> float -> float = 
  "float" "System.Math" "Atan2" "float" "float"
external cos : float -> float = "float" "System.Math" "Cos" "float"
let cosh x = ((exp x) +. (exp (-. x))) /. 2.0 
external log : float -> float = "float" "System.Math" "Log" "float"
external log10 : float -> float = "float" "System.Math" "Log10" "float"
external sin : float -> float = "float" "System.Math" "Sin" "float"
let sinh x = ((exp x) -. (exp (-. x))) /. 2.0 
external sqrt : float -> float = "float" "System.Math" "Sqrt" "float"
external tan : float -> float = "float" "System.Math" "Tan" "float"
let tanh x = (sinh x) /. (cosh x) 
external ceil : float -> float = "float" "System.Math" "Ceiling" "float"
external floor : float -> float = "float" "System.Math" "Floor" "float"
external abs_float : float -> float = 
  "float" "System.Math" "Abs" "float"
external mod_float : float -> float -> float = 
  "float" "System.Math" "IEEERemainder" "float" "float"

external float : int -> float = "float" "System.Convert" "ToDouble" "int"
external float_of_int : int -> float = "float" "System.Convert" "ToDouble" "int"
external truncate : float -> int = "int" "System.Convert" "ToInt32" "float"
external int_of_float : float -> int = "int" "System.Convert" "ToInt32" "float"


    
let frexp x =
  if x = 0.0 then (0.0 , 0) else begin
    let e = ceil ((log (abs_float x)) /. (log 2.0)) in
      (x /. (2.0 ** e), int_of_float e)
  end

let ldexp x y = x *. (2.0 ** (float y))

let modf x =
  let intg = if x < 0.0 then ceil x else floor x in
    (x -. intg,intg)
    

external int64_of_int : int -> int64 = "%int64_of_int"
external int64_add : int64 -> int64 -> int64 = "%int64_add"
external int64_shift_left : int64 -> int -> int64 = "%int64_lsl"
external int64_float_of_bits : int64 -> float = 
"float" "System.BitConverter" "Int64BitsToDouble" "int64" 
external int64_bits_of_float : float -> int64 = 
"int64" "System.BitConverter" "DoubleToInt64Bits" "float" 
external int64_logand : int64 -> int64 -> int64 = "%int64_and"
external int64_logor : int64 -> int64 -> int64 = "%int64_or"
external int64_shift_right_logical : int64 -> int -> int64 = "%int64_lsr"

let infinity =
  let base = int64_of_int 0x7ff0 in 
    int64_float_of_bits(int64_shift_left base 48);;
              (* 0x7F F0 00 00 00 00 00 00 *)

let neg_infinity =
  let base = int64_of_int 0xfff0 in 
    int64_float_of_bits(int64_shift_left base 48);;
              (* 0xFF F0 00 00 00 00 00 00 *)

let nan =
  let base = int64_of_int 0x7ff0 in 
    int64_float_of_bits(int64_add (int64_of_int 1)(int64_shift_left base 48));;
              (* 0x7F F0 00 00 00 00 00 01 *)

let max_float =
  let base = int64_of_int 0x7ff0 in 
    int64_float_of_bits(int64_add (int64_of_int (-1))(int64_shift_left base 48));;
              (* 0x7f ef ff ff ff ff ff ff *)

let min_float =
  let base = int64_of_int 0x0010 in 
    int64_float_of_bits(int64_shift_left base 48);;
              (* 0x00 10 00 00 00 00 00 00 *)

let epsilon_float =
  let base = int64_of_int 0x3cb0 in 
    int64_float_of_bits(int64_shift_left base 48);;
              (* 0x3c b0 00 00 00 00 00 00 *)

type fpclass =
    FP_normal
  | FP_subnormal
  | FP_zero
  | FP_infinite
  | FP_nan

(* external classify_float: float -> fpclass = "classify_float" *)
(* recopie du code c de byterun/floats.c *)
let classify_float f =
  let i64=int64_bits_of_float f in
  let l = int64_logand (int64_of_int 0xFFFFFFFF) i64 
  and h = int64_shift_right_logical i64 32 in
  let l = int64_logor l (int64_logand h (int64_of_int 0xFFFFF)) in
  let h = int64_logand h (int64_of_int 0x7FF00000) in
    if (compare (int64_logor h l) (int64_of_int 0)) = 1 then FP_zero 
    else 
      if (compare h (int64_of_int 0)) = 1 then FP_subnormal
	  else 
	    if (compare h (int64_of_int 0x7FF00000)) = 1 then
		  begin
		    if (compare l (int64_of_int 0)) = 1 then FP_infinite
			else FP_nan
		  end
		  else FP_normal
		    

(* String operations -- more in module String *)

external string_length : string -> int = "%string_length"
external string_create: int -> string = "char[]" "CamIL.String" "string_create" "int" 
external string_blit : string -> int -> string -> int -> int -> unit =
  "void" "CamIL.String" "blit_string" "string" "int" "char[]" "int" "int"

let (^) s1 s2 =
  let l1 = string_length s1 and l2 = string_length s2 in
  let s = string_create (l1 + l2) in
  string_blit s1 0 s 0 l1;
  string_blit s2 0 s l1 l2;
  s

(* Character operations -- more in module Char *)

external int_of_char : char -> int = "%identity"
external unsafe_char_of_int : int -> char = "%identity"
let char_of_int n =
  if n < 0 || n > 255 (* STRICTCAML 65535 *) then invalid_arg "char_of_int" else unsafe_char_of_int n

(* Unit operations *)

external ignore : 'a -> unit = "%ignore"

(* Pair operations *)

external fst : 'a * 'b -> 'a = "%field0"
external snd : 'a * 'b -> 'b = "%field1"

(* String conversion functions *)
(*
external format_int: string -> int -> string = "format_int"
external format_float: string -> float -> string = "format_float"
*)
let string_of_bool b =
  if b then "true" else "false"
let bool_of_string = function
  | "true" -> true
  | "false" -> false
  | _ -> invalid_arg "bool_of_string"
(* Version standard
let string_of_int n =
  format_int "%d" n
*)
external string_of_int : int -> string = 
  "string" "CamIL.String" "string_of_int" "int"
external il_int_of_string : string -> int = 
  "int" "CamIL.String" "int_of_string" "string"

external string_get : string -> int -> char = "%string_safe_get"


(* Trois fonctions nouvelles, a optimiser ou a ecrire en IL ... *)
let rec parsehexa s i n accu =
  if i>=n then accu 
  else let digit = 
    ( match (string_get s i) with 
    | '0' -> 0    | '1' -> 1    | '2' -> 2    | '3' -> 3    | '4' -> 4
    | '5' -> 5    | '6' -> 6    | '7' -> 7    | '8' -> 8    | '9' -> 9    
    | 'a' | 'A' -> 10
    | 'b' | 'B' -> 11
    | 'c' | 'C' -> 12
    | 'd' | 'D' -> 13
    | 'e' | 'E' -> 14
    | 'f' | 'F' -> 15
    | _ -> raise (Failure "int_of_string"))
  in parsehexa s (succ i) n (accu*16+digit);;

let rec parseoctal s i n accu =
  if i>=n then accu 
  else let digit = 
    ( match (string_get s i) with 
    | '0' -> 0    | '1' -> 1    | '2' -> 2    | '3' -> 3    | '4' -> 4
    | '5' -> 5    | '6' -> 6    | '7' -> 7 
    | _ -> raise (Failure "int_of_string"))
  in parseoctal s (succ i) n (accu*8+digit);;

let rec parsebinary s i n accu =
  if i>=n then accu 
  else let digit = 
    ( match (string_get s i) with 
    | '0' -> 0    | '1' -> 1    
    | _ -> raise (Failure "int_of_string"))
  in parsebinary s (succ i) n (accu*2+digit);;


let int_of_string s =
  if (string_length s)>2 && (string_get s 0)='0' then (
    match (string_get s 1) with 
    | 'x' | 'X' -> parsehexa s 2 (string_length s) 0
    | 'b' | 'B' -> parsebinary s 2 (string_length s) 0
    | 'o' | 'O' -> parseoctal s 2 (string_length s) 0
    | _ -> il_int_of_string s
   )
  else il_int_of_string s


(* version standard
let valid_float_lexem s =
  let l = string_length s in
  let rec loop i =
    if i >= l then s ^ "." else
    if s.[i] = '.' || s.[i] = 'e' then s
    else loop (i+1)
  in
  loop 0
;;

   let string_of_float f = valid_float_lexem (format_float "%.17g" f);;
*)

external string_set : string -> int -> char -> unit = "void" "CamIL.String" "set_safe" "char[]" "int" "char"
external string_of_float : float -> string = 
  "string" "CamIL.String" "string_of_float" "float"
external float_of_string : string -> float = 
  "float" "CamIL.String" "float_of_string" "string"

(* List operations -- more in module List *)

let rec (@) l1 l2 =
  match l1 with
    [] -> l2
  | hd :: tl -> hd :: (tl @ l2)

(* References *)

type 'a ref = { mutable contents: 'a }
external ref: 'a -> 'a ref = "%makemutable"
external (!): 'a ref -> 'a = "%field0"
external (:=): 'a ref -> 'a -> unit = "%setfield0"
external incr: int ref -> unit = "%incr"
external decr: int ref -> unit = "%decr"


(* I/O operations *)

type in_channel
type out_channel

external cli_stdin : unit -> in_channel =
  "class System.IO.BinaryReader" "CamIL.IO" "get_stdin" "void"
external cli_stdout : unit -> out_channel =
  "class System.IO.BinaryWriter" "CamIL.IO" "get_stdout" "void"
external cli_stderr : unit -> out_channel =
  "class System.IO.BinaryWriter" "CamIL.IO" "get_stderr" "void"

let stdin = cli_stdin ();;
let stdout = cli_stdout ();;
let stderr = cli_stderr ();; 




(* General output functions *)

type open_flag =
    Open_rdonly | Open_wronly | Open_append
  | Open_creat | Open_trunc | Open_excl
  | Open_binary | Open_text | Open_nonblock

let cliFMcreatenew = 1 (* FE -> exception ; FNE -> creer nouveau *)
let cliFMcreate = 2 (* FE -> createnew ; FNE -> truncate (seulement en FA=write) *)
let cliFMopen = 3 (* FE -> open ; FNE -> exception *)
let cliFMopenorcreate = 4 (* FE -> open ; FNE -> create *)
let cliFMtruncate = 5 (* FE -> truncate ; FNE -> exception *)
let cliFMappend = 6 (* FE -> append ; FNE -> creer (seulement en FA=write) *)

let cliFAread = 1
let cliFAwrite = 2
let cliFAreadwrite = 3

let cliFSnone = 0
let cliFSread = 1
let cliFSwrite = 2
let cliFSreadwrite = 3

let rec list_mem x = function
    [] -> false
  | a::l -> a = x || list_mem x l

let transflag_ml2cli flags = 
  (
  (       (* Open_excl est prioritaire pour le comportement si le fichier existe *)
    if (list_mem Open_excl flags) then 
      (
	if (list_mem Open_creat flags) || (list_mem Open_append flags) then cliFMcreatenew
	else cliFMcreatenew (* NON, RAJOUTER QQCH DE PLUS *)
      )
    else if (list_mem Open_creat flags) && (list_mem Open_trunc flags) then cliFMcreate 
    else if (list_mem Open_creat flags) then cliFMopenorcreate (* PAS SUR !! *)
    else if (list_mem Open_trunc flags) then cliFMtruncate
    else if (list_mem Open_append flags) then cliFMappend
    else cliFMopen)
  ,
  (if (list_mem Open_rdonly flags) && not (list_mem Open_wronly flags) then cliFAread 
   else if (list_mem Open_wronly flags) && not (list_mem Open_rdonly flags) then cliFAwrite 
   else cliFAreadwrite) (* si a la fois rdonly et wronly on met readwrite, pas forcément correct A VOIR *) 
  , 
  cliFSreadwrite (* attribut FileShare : que mettre ? *) 
  ) 

external open_descriptor_out: string -> int -> int -> int -> out_channel = 
"class System.IO.BinaryWriter" "CamIL.IO" "open_descriptor_out" "string" "int" "int" "int"

let open_out_gen mode perm name =
  let (fm,fa,fs) = transflag_ml2cli mode in
    open_descriptor_out name fm fa fs
(* TODO : traiter les permissions ??? *)      

let open_out name =
  open_out_gen [Open_wronly; Open_creat; Open_trunc; Open_text] 438 (* = 0o666 *) name

let open_out_bin name =
  open_out_gen [Open_wronly; Open_creat; Open_trunc; Open_binary] 438 (* = 0o666 *) name


(* Versions directes
let open_out_bin s = open_descriptor_out s cliFMcreate cliFAwrite cliFSnone
let open_out = open_out_bin
*)

external flush : out_channel -> unit =
  "void" "CamIL.IO" "flush" "object"

(*
external out_channels_list : unit -> out_channel list 
                           = "caml_out_channels_list"

let flush_all () = 
  let rec iter = function
      [] -> ()
    | a::l -> (try flush a with _ -> ()); iter l
  in iter (out_channels_list ())
*)

external unsafe_output : out_channel -> string -> int -> int -> unit =
"void" "CamIL.IO" "unsafe_output" "class System.IO.BinaryWriter" "string" "int" "int"

external output_char : out_channel -> char -> unit =
 "void" "CamIL.IO" "output_char" "class System.IO.BinaryWriter" "int"

let output_string oc s =
  unsafe_output oc s 0 (string_length s)

let output oc s ofs len =
  if ofs < 0 || len < 0 || ofs > string_length s - len
  then invalid_arg "output"
  else unsafe_output oc s ofs len

external output_byte : out_channel -> int -> unit =
 "void" "CamIL.IO" "output_char" "class System.IO.BinaryWriter" "int"

let output_binary_int oc i = (* A PASSER EN IL *)
  output_byte oc ((i lsr 24) land 255);
  output_byte oc ((i lsr 16) land 255);
  output_byte oc ((i lsr 8) land 255);
  output_byte oc (i land 255)

(*
external marshal_to_channel : out_channel -> 'a -> unit list -> unit
     = "output_value"
let output_value chan v = marshal_to_channel chan v []
*)

external output_value : out_channel -> 'a -> unit =
"void" "CamIL.IO" "serialize" "class System.IO.BinaryWriter" "object"

(*
external pos_out : out_channel -> int = "caml_pos_out"

*)




external seek_out : out_channel -> int -> unit =
"void" "CamIL.IO" "seek_out" "class System.IO.BinaryWriter" "int"

(* ajouter la recuperation d'EXC *)
external out_channel_length : out_channel -> int = 
"int" "CamIL.IO" "out_channel_length" "class System.IO.BinaryWriter"


external close_out_channel : out_channel -> unit =
 "void" "CamIL.IO" "close_out" "class System.IO.BinaryWriter"

let close_out oc = flush oc; close_out_channel oc

let close_out_noerr oc =
  (try flush oc with _ -> ());
  (try close_out_channel oc with _ -> ())

(*
external set_binary_mode_out : out_channel -> bool -> unit
                             = "caml_set_binary_mode"
*)

(* General input functions *)

external open_descriptor_in : string -> int -> int -> int -> in_channel =
 "class System.IO.BinaryReader" "CamIL.IO" "open_descriptor_in" "string" "int" "int" "int"

let open_in_gen mode perm name =
  let (fm,fa,fs) = transflag_ml2cli mode in
    open_descriptor_in name fm fa fs
(* TODO : traiter les permissions *)      


let open_in name =
  open_in_gen [Open_rdonly; Open_text] 0 name

let open_in_bin name =
  open_in_gen [Open_rdonly; Open_binary] 0 name

(* Versions directes
   let open_in_bin s = open_descriptor_in s cliFMopen cliFAread cliFSnone
   let open_in = open_in_bin
*)


external input_char : in_channel -> char = 
 "int" "CamIL.IO" "input_char"  "class System.IO.BinaryReader"

external unsafe_input : in_channel -> string -> int -> int -> int = 
"int" "CamIL.IO" "unsafe_input" "class System.IO.BinaryReader" "char[]" "int" "int"

let input ic s ofs len =
  if ofs < 0 || len < 0 || ofs > string_length s - len
  then invalid_arg "input"
  else unsafe_input ic s ofs len

let rec unsafe_really_input ic s ofs len =
  if len <= 0 then () else begin
    let r = unsafe_input ic s ofs len in
    if r = 0
    then raise End_of_file
    else unsafe_really_input ic s (ofs+r) (len-r)
  end

let really_input ic s ofs len =
  if ofs < 0 || len < 0 || ofs > string_length s - len
  then invalid_arg "really_input"
  else unsafe_really_input ic s ofs len

external input_singleton_line : in_channel -> string = "char[]" "CamIL.IO" "input_singleton_line" "class System.IO.BinaryReader"

let rec input_line chan =
  let s = input_singleton_line chan in
  if (string_length s) = 0 then                         (* n = 0: we are at EOF *)
    raise End_of_file
  else match string_get s 0 with
    | '\013' -> (try input_line chan with End_of_file  -> "") (* ignorer le CR sous windows *)
    | '\n' -> ""
    | _ -> (try s ^ input_line chan with End_of_file -> s)


external input_byte : in_channel -> int = 
 "int" "CamIL.IO" "input_char"  "class System.IO.BinaryReader"


let input_binary_int ic = (* A PASSER EN IL *)
  let res = ref 0 in
    for i=0 to 3 do
      res := (!res lsl 8) + input_byte ic
    done;
    !res

(*
external pos_in : in_channel -> int = "caml_pos_in"
*)

external input_value : in_channel -> 'a =
"object" "CamIL.IO" "deserialize" "class System.IO.BinaryReader"

external seek_in : in_channel -> int -> unit =
"void" "CamIL.IO" "seek_in" "class System.IO.BinaryReader" "int"

(* ajouter la recuperation d'EXN *)
external in_channel_length : in_channel -> int = 
"int" "CamIL.IO" "in_channel_length" "class System.IO.BinaryReader"

external close_in : in_channel -> unit =
 "void" "CamIL.IO" "close_in" "class System.IO.BinaryReader"

let close_in_noerr ic = (try close_in ic with _ -> ());;

(*
external set_binary_mode_in : in_channel -> bool -> unit
                            = "caml_set_binary_mode"
*)





(* Output functions on standard output *)

let print_char c = output_char stdout c
let print_string s = output_string stdout s
let print_int i = output_string stdout (string_of_int i)
let print_float f = output_string stdout (string_of_float f)
let print_endline s =
  output_string stdout s; output_char stdout '\n'; flush stdout
let print_newline () = output_char stdout '\n'; flush stdout

(* Output functions on standard error *)

let prerr_char c = output_char stderr c
let prerr_string s = output_string stderr s
let prerr_int i = output_string stderr (string_of_int i)
let prerr_float f = output_string stderr (string_of_float f)
let prerr_endline s =
  output_string stderr s; output_char stderr '\n'; flush stderr
let prerr_newline () = output_char stderr '\n'; flush stderr

(* Input functions on standard input *)

let read_line () = flush stdout; input_line stdin
let read_int () = int_of_string(read_line())
let read_float () = float_of_string(read_line())

(* Operations on large files *)

(*
module LargeFile =
  struct
    external seek_out : out_channel -> int64 -> unit = "caml_seek_out_64"
    external pos_out : out_channel -> int64 = "caml_pos_out_64"
    external out_channel_length : out_channel -> int64 = "caml_channel_size_64"
    external seek_in : in_channel -> int64 -> unit = "caml_seek_in_64"
    external pos_in : in_channel -> int64 = "caml_pos_in_64"
    external in_channel_length : in_channel -> int64 = "caml_channel_size_64"
  end
*)

(* Formats *)
(*
type ('a, 'b, 'c) format = ('a, 'b, 'c, 'c) format4
external format_of_string :
 ('a, 'b, 'c, 'd) format4 -> ('a, 'b, 'c, 'd) format4 = "%identity"
external string_of_format : ('a, 'b, 'c, 'd) format4 -> string = "%identity"

external string_to_format : string -> ('a, 'b, 'c, 'd) format4 = "%identity"
let (( ^^ ) : ('a, 'b, 'c, 'd) format4 -> ('d, 'b, 'c, 'e) format4 ->
              ('a, 'b, 'c, 'e) format4) = fun fmt1 fmt2 ->
  string_to_format (string_of_format fmt1 ^ string_of_format fmt2);;
*)

(* Miscellaneous *)

(* version standard
external sys_exit : int -> 'a = "sys_exit" *)

(* OLD external sys_exit : int -> 'a = "object" "CamIL.Exception" "sys_exit" "int" *)
(* TEST HEVEA *)
external sys_exit : int -> unit = "void" "System.Environment" "Exit" "int"


(*
let exit_function = ref flush_all
*)
let exit_function = ref (fun () -> ())

let at_exit f =
  let g = !exit_function in
  exit_function := (fun () -> f(); g())

let do_at_exit () = (!exit_function) ()

external magic : 'a -> 'b = "object" "CamIL.Obj" "boxed_identity" "object";;

(*TEMPO HEVEA*)
let exit : int -> 'a = function retcode ->
  do_at_exit ();
  sys_exit retcode;
  magic()
  

(*
external register_named_value: string -> 'a -> unit = "register_named_value"

let _ = register_named_value "Pervasives.do_at_exit" do_at_exit
*)
