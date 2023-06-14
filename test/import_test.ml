open Base
open OUnit2
open Fca

let filename = "animals.csv"

let tests = "FCA module importing function" >::: [
    "can import animals.csv"  >:: (fun _ ->
        let context = context_from_csv filename ';' in
        let { attrs=attrs; objs=objs; } = context in
        assert_equal 4 (List.length attrs) ~msg:"number of attributes";
        assert_equal 7 (List.length objs) ~msg:"number of objects"
      );
  ]

let _ = run_test_tt_main tests
