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

(* $Id: graphics.ml,v 1.10 2006/07/23 03:04:14 montela Exp $ *)


exception Graphic_failure of string

external raw_open_graph: string -> unit = 
 "void" "CamIL.Graphics" "gr_open_graph" "string"

external raw_close_graph: unit -> unit = 
 "void" "CamIL.Graphics" "gr_close_graph" "void"


let open_graph s = 
  for i=1 to String.length s do
    if s.[i-1]='x' then s.[i-1]<-'*'
  done;
  raw_open_graph s

let close_graph () = raw_close_graph ()
(* pb *)
(*let (open_graph, close_graph) =(raw_open_graph, raw_close_graph);;*)


external gr_set_window_title : string -> unit = 
 "void"  "CamIL.Graphics" "gr_set_window_title" "string"

let set_window_title s = gr_set_window_title s

external gr_clear_graph : unit -> unit =
 "void" "CamIL.Graphics" "gr_clear_graph" "void"

let clear_graph () = gr_clear_graph ()

external gr_size_x : unit -> int = 
 "int" "CamIL.Graphics" "gr_size_x" "void"

let size_x () = gr_size_x ()

external gr_size_y : unit -> int = 
 "int" "CamIL.Graphics" "gr_size_y" "void"

let size_y () = gr_size_y ()



(* Double-buffering *)

external display_mode : bool -> unit = 
   "void" "CamIL.Graphics" "gr_display_mode" "bool"

external synchronize : unit -> unit =
   "void" "CamIL.Graphics" "gr_synchronize" "void"


let auto_synchronize = function
  | true -> display_mode true; (*remember_mode true;*) synchronize ()
  | false -> display_mode false(*; remember_mode true*)
;;


(* Colors *)

type color = int

let rgb r g b = (r lsl 16) + (g lsl 8) + b

external gr_set_color : color -> unit = 
 "void" "CamIL.Graphics" "gr_set_color" "int"

let set_color c = gr_set_color c

let black   = 0x000000
and white   = 0xFFFFFF
and red     = 0xFF0000
and green   = 0x00FF00
and blue    = 0x0000FF
and yellow  = 0xFFFF00
and cyan    = 0x00FFFF
and magenta = 0xFF00FF

let background = white
and foreground = black



(* Drawing *)

external gr_plot : int -> int -> unit = 
"void"  "CamIL.Graphics" "gr_plot" "int" "int"

let plot x y = gr_plot x y

let plots points =
  for i = 0 to Array.length points - 1 do
    let (x, y) = points.(i) in
    plot x y;
  done


external gr_point_color : int -> int -> color = 
  "int" "CamIL.Graphics" "gr_point_color"  "int" "int"

let point_color x y = gr_point_color x y

external gr_moveto : int -> int -> unit =
 "void" "CamIL.Graphics" "gr_moveto" "int" "int"

let moveto x y = gr_moveto x y


external gr_current_x : unit -> int = 
  "int" "CamIL.Graphics" "gr_current_x" "void"

let current_x () = gr_current_x()

external gr_current_y : unit -> int =
  "int" "CamIL.Graphics" "gr_current_x" "void"

let current_y () = gr_current_y()

let current_point () = current_x (), current_y ()


external gr_lineto : int -> int -> unit =
 "void" "CamIL.Graphics" "gr_lineto" "int" "int"

let lineto x y = gr_lineto x y



let rlineto x y = lineto (current_x () + x) (current_y () + y)
let rmoveto x y = moveto (current_x () + x) (current_y () + y)


external gr_draw_rect : int -> int -> int -> int -> unit = 
 "void" "CamIL.Graphics" "gr_draw_rect" "int" "int" "int" "int" 

let draw_rect  a b c d = gr_draw_rect a b c d


let draw_poly, draw_poly_line =
  let dodraw close_flag points =
    if Array.length points > 0 then begin
      let (savex, savey) = current_point () in
      moveto (fst points.(0)) (snd points.(0));
      for i = 1 to Array.length points - 1 do
        let (x, y) = points.(i) in
        lineto x y;
      done;
      if close_flag then lineto (fst points.(0)) (snd points.(0));
      moveto savex savey;
    end;
  in dodraw true, dodraw false
;;

let draw_segments segs =
  let (savex, savey) = current_point () in
  for i = 0 to Array.length segs - 1 do
    let (x1, y1, x2, y2) = segs.(i) in
    moveto x1 y1;
    lineto x2 y2;
  done;
  moveto savex savey;
;;


external gr_draw_arc : int -> int -> int -> int -> int -> int -> unit =
 "void" "CamIL.Graphics" "gr_draw_arc" "int" "int" "int" "int" "int" "int" 

let draw_arc a b c d e f= gr_draw_arc a b c d e f
 
let draw_ellipse x y rx ry = draw_arc x y rx ry 0 360
let draw_circle x y r = draw_arc x y r r 0 360


external gr_set_line_width : int -> unit = 
  "void" "CamIL.Graphics" "gr_set_line_width" "int"

let set_line_width  x = gr_set_line_width x

external gr_fill_rect : int -> int -> int -> int -> unit = 
 "void" "CamIL.Graphics" "gr_fill_rect" "int" "int" "int" "int" 

let fill_rect  a b c d = gr_fill_rect a b c d

(*
external fill_poly : (int * int) array -> unit = "gr_fill_poly"
*)

external gr_fill_arc : int -> int -> int -> int -> int -> int -> unit =
  "void" "CamIL.Graphics" "gr_fill_arc" "int" "int" "int" "int" "int" "int"             
let fill_arc  a b c d e f= gr_fill_arc a b c d e f


let fill_ellipse x y rx ry = fill_arc x y rx ry 0 360
let fill_circle x y r = fill_arc x y r r 0 360


(* Text *)


external draw_char : char -> unit = 
 "void" "CamIL.Graphics" "gr_draw_char" "int"

external gr_draw_string : string -> unit = 
 "void" "CamIL.Graphics" "gr_draw_string" "string"

let draw_string s = gr_draw_string s

external gr_set_font : string -> unit = 
 "void" "CamIL.Graphics" "gr_set_font" "string"

let set_font s = gr_set_font s


external gr_set_text_size : int -> unit = 
 "void" "CamIL.Graphics" "gr_set_text_size" "int"

let set_text_size i = gr_set_text_size i

external gr_text_size : string -> int * int =
 "array" "CamIL.Graphics" "gr_text_size" "string"

let text_size s = gr_text_size s


(* Images *)

type image

let transp = -1

external make_image : color array array -> image = 
"object" "CamIL.Graphics" "gr_make_image" "object[]"

external dump_image : image -> color array array = 
"object[]" "CamIL.Graphics" "gr_dump_image" "object"

external draw_image : image -> int -> int -> unit = 
"void" "CamIL.Graphics" "gr_draw_image" "object" "int" "int"

external create_image : int -> int -> image = 
"object"  "CamIL.Graphics" "gr_create_image" "int" "int"

external blit_image : image -> int -> int -> unit = 
"int" "CamIL.Graphics" "gr_blit_image" "object" "int" "int"

let get_image x y w h =
  let image = create_image w h in
  blit_image image x y;
  image



(* Events *)

type status =
  { mouse_x : int;
    mouse_y : int;
    button : bool;
    keypressed : bool;
    key : char }

type event =
    Button_down
  | Button_up
  | Key_pressed
  | Mouse_motion
  | Poll

external gr_wait_next_event : event list -> int array = 
 "array" "CamIL.Graphics" "gr_wait_event" "object"

let wait_next_event l = 
  let arr = gr_wait_next_event l in
    {mouse_x = arr.(0);
     mouse_y = arr.(1);
     button = arr.(2) <> 0;
     keypressed = arr.(3) <> 0;
     key = char_of_int arr.(4);
    }

let mouse_pos () =
  let e = wait_next_event [Poll] in (e.mouse_x, e.mouse_y)

let button_down () =
  let e = wait_next_event [Poll] in e.button

let read_key () =
  let e = wait_next_event [Key_pressed] in e.key

let key_pressed () =
  let e = wait_next_event [Poll] in e.keypressed


(* Sound *)
external sound : int -> int -> unit = 
 "void" "CamIL.Graphics" "gr_sound" "int" "int"

(* Splines *)

let add (x1, y1) (x2, y2) = (x1 +. x2, y1 +. y2)
and sub (x1, y1) (x2, y2) = (x1 -. x2, y1 -. y2)
and middle (x1, y1) (x2, y2) = ((x1 +. x2) /. 2.0,  (y1 +. y2) /. 2.0)
and area (x1, y1) (x2, y2) = abs_float (x1 *. y2 -. x2 *. y1)
and norm (x1, y1) = sqrt (x1 *. x1 +. y1 *. y1);;

let test a b c d =
 let v = sub d a in
 let s = norm v in
 area v (sub a b) <= s && area v (sub a c) <= s;;

let spline a b c d =
  let rec spl accu a b c d =
   if test a b c d then d :: accu else
   let a' = middle a b
   and o = middle b c in
   let b' = middle a' o
   and d' = middle c d in
   let c' = middle o d' in
   let i = middle b' c' in
   spl  (spl accu a a' b' i) i c' d' d in
  spl [a] a b c d;;

let curveto b c (x, y as d) =
 let float_point (x, y) = (float_of_int x, float_of_int y) in
 let round f = int_of_float (f +. 0.5) in
 let int_point (x, y) = (round x, round y) in
 let points =
   spline
    (float_point (current_point ()))
    (float_point b) (float_point c) (float_point d) in
 draw_poly_line
  (Array.of_list (List.map int_point points));
 moveto x y;;





