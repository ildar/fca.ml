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
  ]

let _ = run_test_tt_main tests
