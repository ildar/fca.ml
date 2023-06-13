open Base
open Stdio

type context = {
    attrs: string list;
    objs: string list;
}
exception File_empty

let context_from_csv filename delim =
  let file_contents = In_channel.read_all filename in
  let lines = String.split file_contents ~on:'\n' in
  let attrs_line =
    match List.hd lines with
    | Some x -> x
    | None -> raise File_empty
    in
  let attrs1 = String.split attrs_line ~on: delim in
  let attrs =
    match List.tl attrs1 with
    | Some x -> x
    | None -> raise File_empty
    in
  let obj_lines =
    match List.tl lines with
    | Some x -> x
    | None -> raise File_empty
    in
  let objs =
    List.filter
      ( List.map ~f:(
        fun str -> str
      ) obj_lines )
      ~f:(
        fun str -> String.length str > 0)
    in
  { attrs = attrs; objs=objs; }
