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

(* $Id: parsing.ml,v 1.6 2005/01/22 00:34:42 montela Exp $ *)

(* The parsing engine *)

open Lexing

(* Internal interface to the parsing engine *)

type parser_env =
  { mutable s_stack : int array;        (* States *)
    mutable v_stack : Obj.t array;      (* Semantic attributes *)
    mutable symb_start_stack : int array; (* Start positions *)
    mutable symb_end_stack : int array;   (* End positions *)
    mutable stacksize : int;            (* Size of the stacks *)
    mutable stackbase : int;            (* Base sp for current parse *)
    mutable curr_char : int;            (* Last token read *)
    mutable lval : Obj.t;               (* Its semantic attribute *)
    mutable symb_start : int;           (* Start pos. of the current symbol*)
    mutable symb_end : int;             (* End pos. of the current symbol *)
    mutable asp : int;                  (* The stack pointer for attributes *)
    mutable rule_len : int;             (* Number of rhs items in the rule *)
    mutable rule_number : int;          (* Rule number to reduce by *)
    mutable sp : int;                   (* Saved sp for parse_engine *)
    mutable state : int;                (* Saved state for parse_engine *)
    mutable errflag : int }             (* Saved error flag for parse_engine *)

type parse_tables =
  { actions : (parser_env -> Obj.t) array;
    transl_const : int array;
    transl_block : int array;
    lhs : string;
    len : string;
    defred : string;
    dgoto : string;
    sindex : string;
    rindex : string;
    gindex : string;
    tablesize : int;
    table : string;
    check : string;
    error_function : string -> unit;
    names_const : string;
    names_block : string }

exception YYexit of Obj.t
exception Parse_error

type parser_input =
    Start
  | Token_read
  | Stacks_grown_1
  | Stacks_grown_2
  | Semantic_action_computed
  | Error_detected

type parser_output =
    Read_token
  | Raise_parse_error
  | Grow_stacks_1
  | Grow_stacks_2
  | Compute_semantic_action
  | Call_error_function


let token_name names number =
  let n=ref number in
  let index = ref 0 in
    while (!n > 0) && ((Char.code names.[!index]) <> 0) do
      while (Char.code names.[!index])<> 0 do incr index done;
      incr index;
      decr n;
    done;
    if (Char.code names.[!index]) = 0 then "<unknown token>"    
    else 
      let index2 = ref !index in
	while (Char.code names.[!index2]) <> 0 do incr index2 done;
	String.sub names !index (!index2 - !index-1)


let parser_trace = false 
(* mettre a true pour obtenir une trace *)

let print_token tables state tok =
    Printf.fprintf stderr "State %d: read token (???)\n" state

(*  if Obj.is_int tok then (
    Printf.fprintf stderr  "State %d: read token %s\n"
			    state  (token_name tables.names_const (Obj.obj tok))
  ) else (
    Printf.fprintf stderr "State %d: read token %s("
            state (token_name tables.names_block (Obj.obj tok));
    let v = Obj.field tok 0 in
      if Obj.is_int v then
	Printf.fprintf stderr "%ld" (Obj.obj v)
      else if (Obj.tag v) = (Obj.string_tag) then
	Printf.fprintf stderr  "%s"  (Obj.obj v)
      else if  (Obj.tag v) = (Obj.double_tag) then
	Printf.fprintf stderr  "%g"  (Obj.obj v)
      else
	Printf.fprintf stderr "_";
      Printf.fprintf stderr ")\n")
*)



(*
external parse_engine :
    parse_tables -> parser_env -> parser_input -> Obj.t -> parser_output
    = "parse_engine"
*)

let parse_engine tables env cmd arg =
  let short s n = 
    let us = (Char.code s.[2*n]) + ((Char.code s.[2*n+1]) lsl 8) in
      if (us land (1 lsl 15))=0 then us else us - (1 lsl 16)
  in
  let state = ref 0 in
  let sp = ref 0 in
  let asp = ref 0 in
  let errflag = ref 0 in
  let n = ref 0 in 
  let n1 = ref 0 in 
  let n2 = ref 0 in
  let m = ref 0 in
  let state1 = ref 0 in
  let save () = 
    env.sp <- !sp;
    env.state <- !state;
    env.errflag <- !errflag;
    if parser_trace then (Printf.printf "++ Saving state %d\n" !state;flush stdout)
  in
  let restore () =
    sp := env.sp;
    state := env.state;
    errflag := env.errflag ;
    if parser_trace then (Printf.printf "++ Restoring state %d\n" !state;flush stdout)
  in
  let rec main () =
    match cmd with
      |	Start -> 
	  state := 0;
	  sp := env.sp;
	  errflag := 0;
	  loop ()
      | Token_read ->
	  restore ();
(*	  if Obj.is_block arg  then (
	    env.curr_char <- Obj.magic (Obj.field (Obj.repr tables.transl_block) (Obj.tag arg));
	    env.lval <- Obj.field arg 0
	  ) else (
	    env.curr_char <- Obj.magic (Obj.field (Obj.repr tables.transl_const) (Obj.obj arg : int));
	    env.lval <- Obj.repr 0
	  ); 
*)
	  if Obj.is_block arg then begin
	    if (Obj.size arg)>0  then (
	      env.curr_char <- tables.transl_block.(Obj.tag arg);
	      env.lval <- Obj.field arg 0
	    ) else (
	      env.curr_char <- tables.transl_const.(Obj.tag arg (*Obj.magic (Obj.field (Obj.repr arg) 0) : int*));
	      env.lval <- Obj.repr 0
	    )
	  end
	  else (
	    env.curr_char <- tables.transl_const.((Obj.obj arg : int));
	    env.lval <- Obj.repr 0
	  ); 

	  if parser_trace then print_token tables !state arg;
	  testshift ()
      | Error_detected -> 
	  restore ();
	  recover ()
      | Stacks_grown_1 ->  
	  restore ();
	  push ()
      | Stacks_grown_2 ->
	  restore ();
	  semantic_action ()
      | Semantic_action_computed ->
	  restore ();
	  env.s_stack.(!sp) <- !state;
	  env.v_stack.(!sp) <- (Obj.obj arg);
	  asp := env.asp;
	  env.symb_end_stack.(!sp) <- env.symb_end_stack.(!asp);
	  if (!sp > !asp) then 
	    (
	      (* This is an epsilon production. Take symb_start equal to symb_end. *)
	      env.symb_start_stack.(!sp) <- env.symb_end_stack.(!asp)
	    );
	    loop ()
  and loop () =
    n := short tables.defred  !state;
    if !n <> 0 then reduce () else (
      if env.curr_char >= 0 then testshift () else (
	save ();
	(* The ML code calls the lexer and updates *)
        (* symb_start and symb_end *)
	Read_token )
    )
  and testshift () =
    n1 := short tables.sindex  !state;
    n2 := !n1 + env.curr_char;

    if parser_trace then (Printf.printf "** n1:%d n2:%d tables.tablesize:%d env.curr_char:%d (short tables.check !n2):%d\n" !n1 !n2 tables.tablesize env.curr_char (short tables.check !n2);flush stdout);    


    if (!n1 <> 0 && !n2 >= 0 && !n2 <= tables.tablesize &&
	(short tables.check !n2) = env.curr_char) then shift() 
    else 
      begin 
	n1 := short tables.rindex !state;
	n2 := !n1 + env.curr_char;
	if (!n1 <> 0 && !n2 >= 0 && !n2 <= tables.tablesize &&
	    (short tables.check !n2) = env.curr_char) then (
	  n := short tables.table !n2;
	  reduce ()
	) else (
	  if !errflag > 0 then 
	    recover () 
	  else (
	    save ();
	    (* The ML code calls the error function *)
	    Call_error_function) 
	)
      end
  and recover () =
    if !errflag < 3  then begin
      errflag := 3;
      let breakwhile = ref 0 in
	while !breakwhile==0 do
          state1 := env.s_stack.(!sp);
          n1 := short tables.sindex !state1;
          n2 := !n1 + 256;
          if (!n1 <> 0 && !n2 >= 0 && !n2 <= tables.tablesize &&
	      (short tables.check !n2) = 256) then (
	    if parser_trace then Printf.fprintf stderr  "Recovering in state %d\n" !state1;
	    
	    breakwhile:=1;
	  ) else (
	    if parser_trace then Printf.fprintf stderr "Discarding state %d\n" !state1;
	    if (!sp <= env.stackbase) then (
	      if parser_trace then Printf.fprintf stderr "No more states to discard\n";
	      breakwhile:=2;
	    ) else decr sp
	  )
	done; 
	if !breakwhile==1 then shift_recover() else Raise_parse_error
	  
    end
    else (
      if env.curr_char = 0 then (
	Raise_parse_error) 
      else (
	if parser_trace then Printf.fprintf stderr "Discarding last token read\n";
	env.curr_char <- -1;
	loop ()
      )
    )
  and shift () =
    env.curr_char <- -1;
    if !errflag > 0 then decr errflag;
    shift_recover ()
  and shift_recover () =
    if parser_trace then Printf.fprintf stderr "State %d: shift to state %d\n"
      !state (short tables.table !n2);
    state := short tables.table  !n2;
    incr sp;
    if !sp < env.stacksize then push () 
    else (
      save ();
      (* The ML code resizes the stacks *)
      Grow_stacks_1)
  and push () =
    env.s_stack.(!sp) <- !state;
    env.v_stack.(!sp) <- (Obj.obj env.lval);
    env.symb_start_stack.(!sp) <- env.symb_start;
    env.symb_end_stack.(!sp) <- env.symb_end;
    loop ();
  and reduce () =
    if parser_trace then Printf.fprintf stderr "State %d: reduce by rule %d\n" !state !n;
    m := short tables.len  !n;
    env.asp <- !sp;
    env.rule_number <- !n;
    env.rule_len <- !m;
    sp := !sp - !m + 1;
    m := short tables.lhs  !n;
    state1 := env.s_stack.(!sp - 1);
    n1 := short tables.gindex !m;
    n2 := !n1 + !state1;
    if (!n1 <> 0 && !n2 >= 0 && !n2 <= tables.tablesize && (short tables.check !n2) = !state1) 
    then state := short tables.table !n2
    else state := short tables.dgoto !m;
    if (!sp < env.stacksize) then semantic_action () 
    else (
      save ();
      (* The ML code resizes the stacks *)
      Grow_stacks_2)
  and semantic_action () =
    save ();
    (* The ML code calls the semantic action *)
    Compute_semantic_action
  in main()



let env =
  { s_stack = Array.create 100 0;
    v_stack = Array.create 100 (Obj.repr []);
    symb_start_stack = Array.create 100 0;
    symb_end_stack = Array.create 100 0;
    stacksize = 100;
    stackbase = 0;
    curr_char = 0;
    lval = Obj.repr [];
    symb_start = 0;
    symb_end = 0;
    asp = 0;
    rule_len = 0;
    rule_number = 0;
    sp = 0;
    state = 0;
    errflag = 0 }

let grow_stacks() =
  let oldsize = env.stacksize in
  let newsize = oldsize * 2 in
  let new_s = Array.create newsize 0
  and new_v = Array.create newsize (Obj.repr [])
  and new_start = Array.create newsize 0
  and new_end = Array.create newsize 0 in
    Array.blit env.s_stack 0 new_s 0 oldsize;
    env.s_stack <- new_s;
    Array.blit env.v_stack 0 new_v 0 oldsize;
    env.v_stack <- new_v;
    Array.blit env.symb_start_stack 0 new_start 0 oldsize;
    env.symb_start_stack <- new_start;
    Array.blit env.symb_end_stack 0 new_end 0 oldsize;
    env.symb_end_stack <- new_end;
    env.stacksize <- newsize

let clear_parser() =
  Array.fill env.v_stack 0 env.stacksize (Obj.repr []);
  env.lval <- Obj.repr []

let current_lookahead_fun = ref (fun (x : Obj.t) -> false)

let yyparse tables start lexer lexbuf =
  let rec loop cmd arg =
    match parse_engine tables env cmd arg with
      Read_token ->
        let t = Obj.repr(lexer lexbuf) in
        env.symb_start <- lexbuf.lex_abs_pos + lexbuf.lex_start_pos;
        env.symb_end   <- lexbuf.lex_abs_pos + lexbuf.lex_curr_pos;
        loop Token_read t
    | Raise_parse_error ->
        raise Parse_error
    | Compute_semantic_action ->
        let (action, value) =
          try
            (Semantic_action_computed, tables.actions.(env.rule_number) env)
          with Parse_error ->
            (Error_detected, Obj.repr []) in
        loop action value
    | Grow_stacks_1 ->
        grow_stacks(); loop Stacks_grown_1 (Obj.repr [])
    | Grow_stacks_2 ->
        grow_stacks(); loop Stacks_grown_2 (Obj.repr [])
    | Call_error_function ->
        tables.error_function "syntax error";
        loop Error_detected (Obj.repr []) in
  let init_asp = env.asp
  and init_sp = env.sp
  and init_stackbase = env.stackbase
  and init_state = env.state
  and init_curr_char = env.curr_char
  and init_errflag = env.errflag in
  env.stackbase <- env.sp + 1;
  env.curr_char <- start;
  try
    loop Start (Obj.repr [])
  with exn ->
    let curr_char = env.curr_char in
    env.asp <- init_asp;
    env.sp <- init_sp;
    env.stackbase <- init_stackbase;
    env.state <- init_state;
    env.curr_char <- init_curr_char;
    env.errflag <- init_errflag;
    match exn with
      YYexit v ->
        Obj.magic v
    | _ ->
        current_lookahead_fun :=
          (fun tok ->
(*            if Obj.is_block tok
            then tables.transl_block.(Obj.tag tok) = curr_char
            else tables.transl_const.(Obj.magic tok) = curr_char;
*)
	     if Obj.is_block tok then
	       if Obj.size tok > 0 then tables.transl_block.(Obj.tag tok) = curr_char
	       else tables.transl_const.(Obj.tag tok) = curr_char
	     else tables.transl_const.(Obj.obj tok) = curr_char);
        raise exn

let peek_val env n =
  Obj.magic env.v_stack.(env.asp - n)

let symbol_start () =
  if env.rule_len > 0
  then env.symb_start_stack.(env.asp - env.rule_len + 1)
  else env.symb_end_stack.(env.asp)
let symbol_end () =
  env.symb_end_stack.(env.asp)

let rhs_start n =
  env.symb_start_stack.(env.asp - (env.rule_len - n))
let rhs_end n =
  env.symb_end_stack.(env.asp - (env.rule_len - n))

let is_current_lookahead tok =
  (!current_lookahead_fun)(Obj.repr tok)

let parse_error (msg : string) = ()
