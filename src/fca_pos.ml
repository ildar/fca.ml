open Base
open Base.String

type elem = string
type pos = {
  elems: elem list;
  rels: (elem * elem) list;
}

(* FIXME: should be local *)
let pairs_from_list l =
  List.fold l ~init:[] ~f:(
    fun acc e ->
      List.filter l ~f:(
        fun a-> a<>e
      ) |>
      List.map ~f:(
        fun snd -> (e,snd)
      ) |>
      List.append acc
  )

let rec is_lt a b ~l =
  let { elems=_; rels=rels } = l in
  a=b ||
  List.exists rels ~f:(
    fun rel ->
      let (x,y) = rel in
      x=a && y=b
  ) ||
  List.filter rels ~f:(fun rel -> let (x,_)=rel in x=a ) |>
  List.fold
    ~init: false
    ~f:(
      fun acc rel ->
        let (_,y)=rel in
        acc || is_lt y b ~l
    )

let is_valid a_pos =
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
    ) &&
  pairs_from_list elems |>
  List.fold
    ~init: true
    ~f:(
      fun acc pair ->
        let (a,b) = pair in
        acc &&
          not (is_lt a b ~l:a_pos && is_lt b a ~l:a_pos)
    )
