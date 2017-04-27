(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: simplif.ml,v 1.6 2006/07/10 00:35:55 montela Exp $ *)

(* Elimination of useless Llet(Alias) bindings.
   Also transform let-bound references into variables. *)

open Asttypes
open Lambda
open Typedlambda

(* To transform let-bound references into variables *)

exception Real_reference

let rec eliminate_ref id lam = 
  let fid = fst id in
    match lam.tlterm with
	TypLvar v ->
	  if Ident.same v fid then raise Real_reference else lam
      | TypLconst cst -> lam
      | TypLapply(e1, el) -> 
	  build_term (TypLapply(eliminate_ref id e1, List.map (eliminate_ref id) el)) lam.tltype
      | TypLfunction(kind, params, body) ->
      if IdentSet.mem id (free_variables lam)
      then raise Real_reference
      else lam
      | TypLlet(str, v, e1, e2) ->
	  build_letterm (str, v, eliminate_ref id e1, eliminate_ref id e2)
      | TypLletrec(idel, e2) ->
	  build_term (TypLletrec(List.map (fun (v, e) -> (v, eliminate_ref id e)) idel,
				 eliminate_ref id e2)) lam.tltype
      | TypLprim(Pfield 0, [{tlterm=TypLvar v}]) when Ident.same v fid ->
	  build_term (TypLvar fid) lam.tltype (* adjusting type *) 
      | TypLprim(Psetfield(0, _), [{tlterm=TypLvar v}; e]) when Ident.same v fid ->
	  let e' = eliminate_ref id e in
	  build_term (TypLassign((fid,e'.tltype),e')) lam.tltype (* adjusting type *) 
      | TypLprim(Poffsetref delta, [{tlterm=TypLvar v}]) when Ident.same v fid ->
	  let inttype = coretype_annotation Predef.type_int in
	  build_term (TypLassign((fid,inttype), build_term (TypLprim(Poffsetint delta, [build_term (TypLvar fid) inttype])) inttype)) lam.tltype (* adjusting type *) 
      | TypLprim(p, el) ->
	  build_term (TypLprim(p, List.map (eliminate_ref id) el)) lam.tltype
      | TypLswitch(e, sw) ->
	  build_switchterm (eliminate_ref id e,
		  {tsw_numconsts = sw.tsw_numconsts;
		   tsw_consts =
		    List.map (fun (n, e) -> (n, eliminate_ref id e)) sw.tsw_consts;
		   tsw_numblocks = sw.tsw_numblocks;
		   tsw_blocks =
	  List.map (fun (n, e) -> (n, eliminate_ref id e)) sw.tsw_blocks;
		   tsw_failaction = match sw.tsw_failaction with
         | None -> None
         | Some l -> Some (eliminate_ref id l)})
      | TypLstaticraise (i,args) ->
	  build_term (TypLstaticraise (i,List.map (eliminate_ref id) args)) lam.tltype
      | TypLstaticcatch(e1, i, e2) ->
	  build_staticcatchterm (eliminate_ref id e1, i, eliminate_ref id e2)
      | TypLtrywith(e1, v, e2) ->
	  build_term (TypLtrywith(eliminate_ref id e1, v, eliminate_ref id e2)) lam.tltype
      | TypLifthenelse(e1, e2, e3) ->
	  build_ifthenelseterm (eliminate_ref id e1,
                  eliminate_ref id e2,
                  eliminate_ref id e3)
      | TypLsequence(e1, e2) ->
	  build_term (TypLsequence(eliminate_ref id e1, eliminate_ref id e2)) lam.tltype
      | TypLwhile(e1, e2) ->
	  build_term (TypLwhile(eliminate_ref id e1, eliminate_ref id e2)) lam.tltype
      | TypLfor(v, e1, e2, dir, e3) ->
	  build_term (TypLfor(v, eliminate_ref id e1, eliminate_ref id e2,
	       dir, eliminate_ref id e3)) lam.tltype
      | TypLassign(v, e) ->
	  build_term (TypLassign(v, eliminate_ref id e)) lam.tltype
      | TypLsend(m, o, el) ->
	  build_term (TypLsend(eliminate_ref id m, eliminate_ref id o,
		List.map (eliminate_ref id) el)) lam.tltype
      | TypLevent(l, ev) ->
	  build_term (TypLevent(eliminate_ref id l, ev)) lam.tltype
      | TypLifused(v, e) ->
	  build_term (TypLifused(v, eliminate_ref id e)) lam.tltype

(* Simplification of exits *)

let simplify_exits lam = 

  (* Count occurrences of (exit n ...) statements *)
  let exits = Hashtbl.create 17 in

  let count_exit i =
    try
      !(Hashtbl.find exits i)
    with
    | Not_found -> 0

  and incr_exit i =
    try
      incr (Hashtbl.find exits i)
    with
    | Not_found -> Hashtbl.add exits i (ref 1) in
  
  let rec count tl = match tl.tlterm with
  | (TypLvar _| TypLconst _) -> ()
  | TypLapply(l1, ll) -> count l1; List.iter count ll
  | TypLfunction(kind, params, l) -> count l
  | TypLlet(str, v, l1, l2) ->
      count l2; count l1
  | TypLletrec(bindings, body) ->
      List.iter (fun (v, l) -> count l) bindings;
      count body
  | TypLprim(p, ll) -> List.iter count ll
  | TypLswitch(l, sw) ->
      count_default sw ;
      count l;
      List.iter (fun (_, l) -> count l) sw.tsw_consts;
      List.iter (fun (_, l) -> count l) sw.tsw_blocks
  | TypLstaticraise (i,ls) -> incr_exit i ; List.iter count ls
  | TypLstaticcatch (l1,(i,[]),{tlterm=TypLstaticraise (j,[])}) ->
      (* i will be replaced by j in l1, so each occurence of i in l1
         increases j's ref count *)
      count l1 ;
      let ic = count_exit i in
      begin try
        let r = Hashtbl.find exits j in r := !r + ic
      with
      | Not_found ->
          Hashtbl.add exits j (ref ic)
      end
  | TypLstaticcatch(l1, (i,_), l2) ->
      count l1;
      (* If l1 does not contain (exit i),
         l2 will be removed, so don't count its exits *)
      if count_exit i > 0 then
        count l2
  | TypLtrywith(l1, v, l2) -> count l1; count l2
  | TypLifthenelse(l1, l2, l3) -> count l1; count l2; count l3
  | TypLsequence(l1, l2) -> count l1; count l2
  | TypLwhile(l1, l2) -> count l1; count l2
  | TypLfor(_, l1, l2, dir, l3) -> count l1; count l2; count l3
  | TypLassign(v, l) ->
      (* Lalias-bound variables are never assigned, so don't increase
         v's refcount *)
      count l
  | TypLsend(m, o, ll) -> List.iter count (m::o::ll)
  | TypLevent(l, _) -> count l
  | TypLifused(v, l) -> count l

  and count_default sw = match sw.tsw_failaction with
  | None -> ()
  | Some al ->
      let nconsts = List.length sw.tsw_consts
      and nblocks = List.length sw.tsw_blocks in
      if
        nconsts < sw.tsw_numconsts && nblocks < sw.tsw_numblocks
      then begin (* default action will occur twice in native code *)
        count al ; count al
      end else begin (* default action will occur once *)
        assert (nconsts < sw.tsw_numconsts || nblocks < sw.tsw_numblocks) ;
        count al
      end
  in
  count lam;

  (*
     Second pass simplify  ``catch body with (i ...) handler''
      - if (exit i ...) does not occur in body, suppress catch
      - if (exit i ...) occurs exactly once in body,
        substitute it with handler
      - If handler is a single variable, replace (exit i ..) with it
   Note:
    In ``catch body with (i x1 .. xn) handler''
     Substituted expression is
      let y1 = x1 and ... yn = xn in
      handler[x1 <- y1 ; ... ; xn <- yn]
     For the sake of preserving the uniqueness  of bound variables.
     (No alpha conversion of ``handler'' is presently needed, since
     substitution of several ``(exit i ...)''
     occurs only when ``handler'' is a variable.)
  *)

  let subst = Hashtbl.create 17 in

  let rec simplif l = match l.tlterm with
  | (TypLvar _|TypLconst _) -> l
  | TypLapply(l1, ll) -> build_term (TypLapply(simplif l1, List.map simplif ll)) l.tltype
  | TypLfunction(kind, params, l1) -> build_term (TypLfunction(kind, params, simplif l1)) l.tltype
  | TypLlet(kind, v, l1, l2) -> build_letterm (kind, v, simplif l1, simplif l2)
  | TypLletrec(bindings, body) ->
      build_term (TypLletrec(List.map (fun (v, l) -> (v, simplif l)) bindings, simplif body)) l.tltype
  | TypLprim(p, ll) -> build_term (TypLprim(p, List.map simplif ll)) l.tltype
  | TypLswitch(l, sw) ->
      let new_l = simplif l
      and new_consts =  List.map (fun (n, e) -> (n, simplif e)) sw.tsw_consts
      and new_blocks =  List.map (fun (n, e) -> (n, simplif e)) sw.tsw_blocks
      and new_fail = match sw.tsw_failaction with
      | None -> None
      | Some l -> Some (simplif l) in
      build_switchterm 
        (new_l,
		     {sw with tsw_consts = new_consts ; tsw_blocks = new_blocks;
			tsw_failaction = new_fail})
  | TypLstaticraise (i,[]) ->
      begin try
        let _,handler =  Hashtbl.find subst i in
        handler
      with
      | Not_found -> l
      end
  | TypLstaticraise (i,ls)  ->
      let ls = List.map simplif ls in
      begin try
        let xs,handler =  Hashtbl.find subst i in
        let ys = List.map (fun (id,typ) -> Ident.rename id,typ) xs in
        let env =
          List.fold_right2
            (fun x y t -> Ident.add (fst x) (build_varterm y) t)
            xs ys Ident.empty in
        List.fold_right2
          (fun y l r -> build_term (TypLlet (Alias, y, l, r)) r.tltype)
             ys ls (Typedlambda.subst_lambda env handler)
      with
      | Not_found -> l
      end
  | TypLstaticcatch (l1,(i,[]),({tlterm=TypLstaticraise (j,[])} as l2)) ->
      Hashtbl.add subst i ([],simplif l2) ;
      simplif l1
  | TypLstaticcatch (l1,(i,xs), ({tlterm=TypLvar _} as l2)) ->
      begin match count_exit i with
      | 0 -> simplif l1
      | _ ->
          Hashtbl.add subst i (xs,l2) ;
          simplif l1
      end
  | TypLstaticcatch (l1,(i,xs),l2) ->
      begin match count_exit i with
      | 0 -> simplif l1
      | 1 ->
          Hashtbl.add subst i (xs,simplif l2) ;
          simplif l1
      | _ ->
          build_staticcatchterm (simplif l1, (i,xs), simplif l2)
      end
  | TypLtrywith(l1, v, l2) -> build_term (TypLtrywith(simplif l1, v, simplif l2)) l.tltype
  | TypLifthenelse(l1, l2, l3) -> build_ifthenelseterm (simplif l1, simplif l2, simplif l3)
  | TypLsequence(l1, l2) -> build_term (TypLsequence(simplif l1, simplif l2)) l.tltype
  | TypLwhile(l1, l2) -> build_term (TypLwhile(simplif l1, simplif l2)) l.tltype
  | TypLfor(v, l1, l2, dir, l3) ->
      build_term (TypLfor(v, simplif l1, simplif l2, dir, simplif l3)) l.tltype
  | TypLassign(v, l1) -> build_term (TypLassign(v, simplif l1)) l.tltype
  | TypLsend(m, o, ll) -> build_term (TypLsend(simplif m, simplif o, List.map simplif ll)) l.tltype
  | TypLevent(l1, ev) -> build_term (TypLevent(simplif l1, ev)) l.tltype
  | TypLifused(v, l1) -> build_term (TypLifused (v,simplif l1)) l.tltype
  in
  simplif lam

(* Simplification of lets *)

let simplify_lets lam =

  (* First pass: count the occurrences of all identifiers *)
  let occ = Hashtbl.create 83 in
  let count_var v =
    try
      !(Hashtbl.find occ v)
    with Not_found ->
      0
  and incr_var v = 
    try
      incr(Hashtbl.find occ v)
    with Not_found ->
      Hashtbl.add occ v (ref 1) in

  let rec count tl = match tl.tlterm with
  | TypLvar v -> incr_var v
  | TypLconst cst -> ()
  | TypLapply(l1, ll) -> count l1; List.iter count ll
  | TypLfunction(kind, params, l) -> count l
  | TypLlet(str, (v,_), {tlterm=TypLvar w}, l2) when not !Clflags.debug ->
      (* v will be replaced by w in l2, so each occurrence of v in l2
         increases w's refcount *)
      count l2;
      let vc = count_var v in
      begin try
        let r = Hashtbl.find occ w in r := !r + vc
      with Not_found ->
        Hashtbl.add occ w (ref vc)
      end
  | TypLlet(str, (v,_), l1, l2) ->
      count l2;
      (* If v is unused, l1 will be removed, so don't count its variables *)
      if str = Strict || count_var v > 0 then count l1
  | TypLletrec(bindings, body) ->
      List.iter (fun (v, l) -> count l) bindings;
      count body
  | TypLprim(p, ll) -> List.iter count ll
  | TypLswitch(l, sw) ->
      count_default sw ;
      count l;
      List.iter (fun (_, l) -> count l) sw.tsw_consts;
      List.iter (fun (_, l) -> count l) sw.tsw_blocks
  | TypLstaticraise (i,ls) -> List.iter count ls
  | TypLstaticcatch(l1, (i,_), l2) ->
      count l1; count l2
  | TypLtrywith(l1, v, l2) -> count l1; count l2
  | TypLifthenelse(l1, l2, l3) -> count l1; count l2; count l3
  | TypLsequence(l1, l2) -> count l1; count l2
  | TypLwhile(l1, l2) -> count l1; count l2
  | TypLfor(_, l1, l2, dir, l3) -> count l1; count l2; count l3
  | TypLassign(v, l) ->
      (* Lalias-bound variables are never assigned, so don't increase
         v's refcount *)
      count l
  | TypLsend(m, o, ll) -> List.iter count (m::o::ll)
  | TypLevent(l, _) -> count l
  | TypLifused((v,_), l) ->
      if count_var v > 0 then count l

  and count_default sw = match sw.tsw_failaction with
  | None -> ()
  | Some al ->
      let nconsts = List.length sw.tsw_consts
      and nblocks = List.length sw.tsw_blocks in
      if
        nconsts < sw.tsw_numconsts && nblocks < sw.tsw_numblocks
      then begin (* default action will occur twice in native code *)
        count al ; count al
      end else begin (* default action will occur once *)
        assert (nconsts < sw.tsw_numconsts || nblocks < sw.tsw_numblocks) ;
        count al
      end
  in
  count lam;
  (* Second pass: remove Lalias bindings of unused variables,
     and substitute the bindings of variables used exactly once. *)

  let subst = Hashtbl.create 83 in

  let rec simplif l = match l.tlterm with
    TypLvar v ->
      begin try
        Hashtbl.find subst v
      with Not_found ->
        l
      end
  | TypLconst cst -> l
  | TypLapply(l1, ll) -> build_term (TypLapply(simplif l1, List.map simplif ll)) l.tltype
  | TypLfunction(kind, params, l1) -> build_term (TypLfunction(kind, params, simplif l1)) l.tltype
  | TypLlet(str, (v,_), {tlterm=TypLvar w;tltype=typw}, l2) when not !Clflags.debug ->
      Hashtbl.add subst v (simplif (build_varterm (w,typw)));
      simplif l2
  | TypLlet(Strict, v, {tlterm=TypLprim(Pmakeblock(0, Mutable), [linit]);tltype=primt}, lbody)
    when not !Clflags.debug ->
      let slinit = simplif linit in
      let slbody = simplif lbody in
      begin try
        build_letterm (Variable, (fst v,slinit.tltype), slinit, eliminate_ref v slbody) (* adjusting type *) 
      with Real_reference ->
        build_letterm (Strict, v, build_term (TypLprim(Pmakeblock(0, Mutable), [slinit])) primt, slbody)
      end
  | TypLlet(Alias, (v,tv), l1, l2) ->
      begin match count_var v with
        0 -> simplif l2
      | 1 when not !Clflags.debug ->
             Hashtbl.add subst v (simplif l1); simplif l2
      | n -> build_letterm (Alias, (v,tv), simplif l1, simplif l2)
      end
  | TypLlet(StrictOpt, (v,tv), l1, l2) ->
      begin match count_var v with
        0 -> simplif l2
      | n -> build_letterm (Alias, (v,tv), simplif l1, simplif l2)
      end
  | TypLlet(kind, v, l1, l2) -> build_letterm (kind, v, simplif l1, simplif l2)
  | TypLletrec(bindings, body) ->
      build_term (TypLletrec(List.map (fun (v, l) -> (v, simplif l)) bindings, simplif body)) l.tltype
  | TypLprim(p, ll) -> build_term (TypLprim(p, List.map simplif ll)) l.tltype
  | TypLswitch(l, sw) ->
      let new_l = simplif l
      and new_consts =  List.map (fun (n, e) -> (n, simplif e)) sw.tsw_consts
      and new_blocks =  List.map (fun (n, e) -> (n, simplif e)) sw.tsw_blocks
      and new_fail = match sw.tsw_failaction with
      | None -> None
      | Some l -> Some (simplif l) in
      build_switchterm 
        (new_l,
         {sw with tsw_consts = new_consts ; tsw_blocks = new_blocks;
                  tsw_failaction = new_fail})
  | TypLstaticraise (i,ls) ->
      build_term (TypLstaticraise (i, List.map simplif ls)) l.tltype
  | TypLstaticcatch(l1, (i,args), l2) ->
      build_staticcatchterm (simplif l1, (i,args), simplif l2)
  | TypLtrywith(l1, v, l2) -> build_term (TypLtrywith(simplif l1, v, simplif l2)) l.tltype
  | TypLifthenelse(l1, l2, l3) -> build_ifthenelseterm (simplif l1, simplif l2, simplif l3)
  | TypLsequence({tlterm=TypLifused((v,_), l1)}, l2) ->
      if count_var v > 0
      then build_term (TypLsequence(simplif l1, simplif l2)) l.tltype
      else simplif l2
  | TypLsequence(l1, l2) -> build_term (TypLsequence(simplif l1, simplif l2)) l.tltype
  | TypLwhile(l1, l2) -> build_term (TypLwhile(simplif l1, simplif l2)) l.tltype
  | TypLfor(v, l1, l2, dir, l3) ->
      build_term (TypLfor(v, simplif l1, simplif l2, dir, simplif l3)) l.tltype
  | TypLassign(v, l1) -> build_term (TypLassign(v, simplif l1)) l.tltype
  | TypLsend(m, o, ll) -> build_term (TypLsend(simplif m, simplif o, List.map simplif ll)) l.tltype
  | TypLevent(l1, ev) -> build_term (TypLevent(simplif l1, ev)) l.tltype
  | TypLifused((v,_), l1) ->
      if count_var v > 0 then simplif l1 else lambda_unit
  in
  simplif lam


let rec patch_reverse lam = 
  match lam.tlterm with
    | TypLapply(f,ll) ->
	let f = patch_reverse f in
	let ll = List.map patch_reverse ll in
	let tids = List.map (fun l -> let id = Ident.create "s" in (id,l.tltype)) ll in
	  List.fold_left2 (fun t0 t tid -> {tlterm=TypLlet(Strict,tid,t,t0);tltype=t0.tltype}) {lam with tlterm=TypLapply(f,List.map build_varterm tids)} ll tids
    | TypLvar _ -> lam
    | TypLconst cst -> lam
    | TypLfunction(kind, params, l) -> build_term (TypLfunction(kind, params, patch_reverse l)) lam.tltype
    | TypLlet(str,tid,l1,l2) -> {lam with tlterm=TypLlet(str,tid,patch_reverse l1,patch_reverse l2)}
    | TypLletrec(bindings,body) ->
	build_term (TypLletrec(List.map (fun (v, l) -> (v, patch_reverse l)) bindings, patch_reverse body)) lam.tltype
    | TypLprim(p,ll) -> {lam with tlterm=TypLprim(p,List.map patch_reverse ll)}
    | TypLswitch(l,sw) ->
	let new_l = patch_reverse l
	and new_consts =  List.map (fun (n, e) -> (n, patch_reverse e)) sw.tsw_consts
	and new_blocks =  List.map (fun (n, e) -> (n, patch_reverse e)) sw.tsw_blocks
	and new_fail = match sw.tsw_failaction with
	  | None -> None
	  | Some l -> Some (patch_reverse l) in
	  build_switchterm 
            (new_l,
             {sw with tsw_consts = new_consts ; tsw_blocks = new_blocks;
                tsw_failaction = new_fail})	
    | TypLstaticraise(i,ls) -> {lam with tlterm=TypLstaticraise(i,List.map patch_reverse ls)}
    | TypLstaticcatch(l1, (i,args), l2) ->
	build_staticcatchterm (patch_reverse l1, (i,args), patch_reverse l2)
    | TypLtrywith(l1,v,l2) -> {lam with tlterm=TypLtrywith(patch_reverse l1,v,patch_reverse l2)}
    | TypLifthenelse(l1,l2,l3) -> {lam with tlterm=TypLifthenelse(patch_reverse l1,patch_reverse l2,patch_reverse l3)}
    | TypLsequence(l1,l2) -> {lam with tlterm=TypLsequence(patch_reverse l1,patch_reverse l2)}
    | TypLwhile(l1,l2) -> {lam with tlterm=TypLwhile(patch_reverse l1,patch_reverse l2)}
    | TypLfor(v,l1,l2,dir,l3) -> {lam with tlterm=TypLfor(v,patch_reverse l1,patch_reverse l2,dir,patch_reverse l3)}
    | TypLassign(v,l) -> {lam with tlterm=TypLassign(v,patch_reverse l)}
    | TypLsend(m,o,ll) -> {lam with tlterm=TypLsend(patch_reverse m,patch_reverse o,List.map patch_reverse ll)}
    | TypLevent(l,ev) -> {lam with tlterm=TypLevent(patch_reverse l,ev)}
    | TypLifused(v,l) -> {lam with tlterm=TypLifused(v,patch_reverse l)}


let simplify_lambda lam = 
  if !Clflags.ocaml_eval_order then patch_reverse (simplify_lets (simplify_exits lam))
  else simplify_lets (simplify_exits lam)
