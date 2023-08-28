open Base
open OUnit2
open Fca

let tests = "FCA module POS function" >::: [
    "can check consistency" >:: (fun _ ->
        let a_pos1 = {
          elems=[];
          rels=[ ("1","1"); ];
        } in
        assert_bool "Pos.is_valid false positive" @@ not @@ Pos.is_valid a_pos1;
        let a_pos2 = {
          elems=["0";"1"];
          rels=[ ("1","1"); ];
        } in
        assert_bool "Pos.is_valid shouldn't have a<=a relations" @@
          not @@ Pos.is_valid a_pos2;
        let a_pos3 = {
          elems=["0";"a";"a";"1"];
          rels=[ ("0","a"); ];
        } in
        assert_bool "Pos.is_valid shouldn't have duplicate elements" @@
          not @@ Pos.is_valid a_pos3;
      );
    "can check the relation of a and b" >:: (fun _ ->
        let a_pos1 = {
          elems=["0";"a";"1"];
          rels=[ ("0","a"); ("a","1"); ];
        } in
        assert_bool {|Pos.is_lt "a" "a" ~l is false|} @@
          Pos.is_lt "a" "a" ~l:a_pos1;
        assert_bool {|Pos.is_lt "0" "a" ~l is false|} @@
          Pos.is_lt "0" "a" ~l:a_pos1;
        assert_bool {|Pos.is_lt "a" "0" ~l is true|} @@
          not @@ Pos.is_lt "a" "0" ~l:a_pos1;
        assert_bool {|Pos.is_lt "0" "1" ~l is false|} @@
          Pos.is_lt "0" "1" ~l:a_pos1;
      );
  ]

let _ = run_test_tt_main tests
