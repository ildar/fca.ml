open Base
open Base.String
open Fca_types

(* https://en.wikipedia.org/wiki/Partially_ordered_set *)
let is_valid = fun a_pos ->
  let { elems=elems; rels=rels } = a_pos in
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
