(* https://en.wikipedia.org/wiki/Partially_ordered_set *)

(** element of a partially ordered set *)
type elem = string

(** partially ordered set *)
type pos = {
  elems: elem list;
  rels: (elem * elem) list;
}

(** the relation a<=b in POS l *)
val is_lt: elem -> elem -> l:pos -> bool

(** the meet of a and b in POS l *)
val meet2: elem -> elem -> l:pos -> elem option

val is_valid: pos -> bool

(* FIXME: should be local *)
val pairs_from_list: elem list -> (elem * elem) list
