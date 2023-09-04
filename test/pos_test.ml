open Base
open OUnit2
open Fca.Pos

let tests = "FCA module POS" >::: [
    "can check consistency" >:: (fun _ ->
        let a_pos1 = {
          elems=[];
          rels=[ ("1","1"); ];
        } in
        assert_bool "is_valid false positive" @@ not @@ is_valid a_pos1;
        let a_pos2 = {
          elems=["0";"1"];
          rels=[ ("1","1"); ];
        } in
        assert_bool "is_valid shouldn't have a<=a relations" @@
          not @@ is_valid a_pos2;
        let a_pos3 = {
          elems=["0";"a";"a";"1"];
          rels=[ ("0","a"); ];
        } in
        assert_bool "is_valid shouldn't have duplicate elements" @@
          not @@ is_valid a_pos3;
        let a_pos4 = {
          elems=["0";"1"];
          rels=[ ("0","1"); ("1","0"); ];
        } in
        assert_bool "is_valid shouldn't have a<=b && b<=a" @@
          not @@ is_valid a_pos4;
      );
    "can check the relation of a and b" >:: (fun _ ->
        let a_pos1 = {
          elems=["0";"a";"1"];
          rels=[ ("0","a"); ("a","1"); ];
        } in
        assert_bool {|is_lt "a" "a" ~l is false|} @@
          is_lt "a" "a" ~l:a_pos1;
        assert_bool {|is_lt "0" "a" ~l is false|} @@
          is_lt "0" "a" ~l:a_pos1;
        assert_bool {|is_lt "a" "0" ~l is true|} @@
          not @@ is_lt "a" "0" ~l:a_pos1;
        assert_bool {|is_lt "0" "1" ~l is false|} @@
          is_lt "0" "1" ~l:a_pos1;
      );
    "has utility functions working right" >:: (fun _ ->
        let a_list = ["a";"b";"c";] in
        assert_equal ~msg:"pairs_from_list producing lists" 6 @@
          List.length @@ pairs_from_list a_list
      );
  ]

let _ = run_test_tt_main tests
