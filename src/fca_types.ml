type context = {
  attrs: string list;
  objs: (string * bool list) list;
}

type elem = string
(* partially ordered set *)
type pos = {
  elems: elem list;
  rels: (elem * elem) list;
}

exception File_empty
