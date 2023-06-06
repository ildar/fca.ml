open OUnit2
open Io

let filename = "animals.csv"

let tests = "FCA module importing function" >::: [
  "can import animals.csv"  >:: (fun _ ->
        let context = Io.context_from_csv filename in
        assert_equal () context
      );
]

let _ = run_test_tt_main tests