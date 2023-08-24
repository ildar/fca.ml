open Base
open Stdio
open Fca_types

(* TODO: exclude empty attrs and objs *)
let context_from_csv_text ?delim:(delim=',') file_contents =
  let lines = String.split file_contents ~on:'\n' in

  let attrs_line =
    match List.hd lines with
    | Some x -> x
    | None -> raise File_empty
  in
  let attrs =
    match String.split attrs_line ~on:delim with
    | [] -> raise File_empty
    | _::attrs -> attrs
  in

  let obj_lines =
    match List.tl lines with
    | Some x -> x
    | None -> raise File_empty
  in
  let objs =
    List.map obj_lines ~f:(
      fun str ->
        let str1 = String.split str ~on: delim in
       	let (obj_name,rels) =
          match str1 with
          | [] -> ("",[])
          | ""::_ -> ("",[])
          | obj_name::rels_str -> (obj_name,
                                   List.map ~f: (
                                     fun s ->
                                       match s with
                                       | "" -> false
                                       | "0" -> false
                                       | "n" -> false
                                       | "no" -> false
                                       | "N" -> false
                                       | "NO" -> false
                                       | "F" -> false
                                       | "false" -> false
                                       | _ -> true
                                   ) rels_str)
        in
        (obj_name,rels)
    ) in
  let objs =
    List.filter objs ~f:(
      fun (str,_) -> String.length str > 0)
  in
  { attrs=attrs; objs=objs; }

let context_from_csv ?delim:(delim=',') filename =
  let file_contents = In_channel.read_all filename in
  context_from_csv_text ~delim:delim file_contents
