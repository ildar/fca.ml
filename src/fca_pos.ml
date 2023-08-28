open Base
open Base.String
open Fca_types

(* https://en.wikipedia.org/wiki/Partially_ordered_set *)

(* the relation a<=b in POS l *)
let rec is_lt = fun a b ~l ->
  let { elems=_; rels=rels } = l in
  a=b ||
  List.exists rels ~f:(
    fun rel ->
      let (x,y) = rel in
      x=a && y=b
  ) ||
  List.fold
    ~init: false
    ~f:(
      fun acc rel ->
        let (_,y)=rel in
        acc || is_lt y b ~l
    ) @@
    List.filter rels ~f:(fun rel -> let (x,_)=rel in x=a )

let is_valid = fun a_pos ->
  let { elems=elems; rels=rels } = a_pos in
  (* check for duplicates *)
  not @@ List.contains_dup elems ~compare:compare &&
  (* check relations' elements exist *)
  List.fold
    rels
    ~init: true
    ~f:(
      fun acc rel ->
        let (a,b) = rel in
        acc &&
          List.exists elems ~f:(fun e-> a = e) &&
          List.exists elems ~f:(fun e-> b = e)
    ) &&
  (* check reflexivity relations not included *)
  List.fold
    rels
    ~init: true
    ~f:(
      fun acc rel ->
        let (a,b) = rel in
        acc && a <> b
    )
