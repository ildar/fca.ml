type context = {
  attrs: string list;
  objs: (string * bool list) list;
}

exception File_empty
