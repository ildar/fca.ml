open Base
open Base.String
open Fca_types

let is_valid = fun a_pos ->
  let { elems=elems; rels=rels } = a_pos in
  List.fold
    rels
    ~init: true
    ~f:(
      fun acc rel ->
        let (a,b) = rel in
        acc &&
          List.exists elems ~f:(fun e-> a = e) &&
          List.exists elems ~f:(fun e-> b = e)
    )
