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

let rec meet2 a b ~l =
  let {elems=_; rels=rels} = l in
  if is_lt a b ~l then Some(a) else
  if is_lt b a ~l then Some(b) else
  let lower_bounds =
      List.filter rels ~f:(fun rel -> let (_,y) = rel in y=a) |>
      List.map ~f:(fun rel -> let (x,_)=rel in meet2 x b ~l)
      in
  match List.hd lower_bounds with
    | None -> None
    | Some(bound) ->
        (* looking for the supremum *)
        List.fold lower_bounds ~init:bound ~f:(
          fun mbacc mbelem ->
            match mbacc with
              | None -> None
              | Some(acc) ->
                match mbelem with
                  | None -> None
                  | Some(anotherelem) ->
                    if is_lt acc anotherelem ~l then Some(anotherelem) else
                    if is_lt anotherelem acc ~l then Some(acc) else
                    None (* the bound aren't comparable => no supremum *)
        )

let rec join2 a b ~l =
  let {elems=_; rels=rels} = l in
  if is_lt a b ~l then Some(b) else
  if is_lt b a ~l then Some(a) else
  let upper_bounds =
      List.filter rels ~f:(fun rel -> let (x,_) = rel in x=a) |>
      List.map ~f:(fun rel -> let (_,y)=rel in join2 y b ~l)
      in
  match List.hd upper_bounds with
    | None -> None
    | Some(bound) ->
        (* looking for the infimum *)
        List.fold upper_bounds ~init:bound ~f:(
          fun mbacc mbelem ->
            match mbacc with
              | None -> None
              | Some(acc) ->
                match mbelem with
                  | None -> None
                  | Some(anotherelem) ->
                    if is_lt acc anotherelem ~l then Some(acc) else
                    if is_lt anotherelem acc ~l then Some(anotherelem) else
                    None (* the bound aren't comparable => no infimum *)
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

let is_lattice a_pos =
  let { elems=elems; rels=_ } = a_pos in
  is_valid a_pos &&
  pairs_from_list elems |>
  List.fold
    ~init: true
    ~f:(
      fun acc pair ->
        let (a,b) = pair in
        acc &&
        Option.is_some @@ meet2 a b ~l:a_pos &&
        Option.is_some @@ join2 a b ~l:a_pos
    )
