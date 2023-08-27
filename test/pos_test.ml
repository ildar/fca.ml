open Base
open OUnit2
open Fca

let tests = "FCA module POS function" >::: [
    "can check consistency"  >:: (fun _ ->
        let a_pos = {
          elems=[];
          rels=[ ("1","1"); ];
        } in
        assert_bool "Pos.is_valid false positive" @@ not @@ Pos.is_valid a_pos;
      );
  ]

let _ = run_test_tt_main tests
