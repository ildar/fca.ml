open Base
open Base.Option
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
    "can check the order relation of a and b" >:: (fun _ ->
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
    "can find meet and join of x and y" >:: (fun _ ->
        let a_pos1 = { (* M_2 *)
          elems=["0";"a";"b";"1"];
          rels=[ ("0","a"); ("a","1");
                 ("0","b"); ("b","1"); ];
        } in
        match meet2 "a" "a" ~l:a_pos1 with
          | None -> assert_failure {|meet "a" "a" is None|}
          | Some(m_a_a) -> assert_equal ~msg:{|meet "a" "a" is not "a"|} "a" m_a_a ;
        match meet2 "0" "a" ~l:a_pos1 with
          | None -> assert_failure {|meet "0" "a" ~l is None|}
          | Some(m_0_a) -> assert_equal ~msg:{|meet "0" "a" is not "0"|} "0" m_0_a ;
        match meet2 "a" "0" ~l:a_pos1 with
          | None -> assert_failure {|meet "a" "0" ~l is None|}
          | Some(m_a_0) -> assert_equal ~msg:{|meet "a" "0" is not "0"|} "0" m_a_0 ;
        match meet2 "a" "b" ~l:a_pos1 with
          | None -> assert_failure {|meet "a" "b" ~l is None|}
          | Some(m_a_b) -> assert_equal ~msg:{|meet "a" "b" is not "0"|} "0" m_a_b ;
        match join2 "a" "a" ~l:a_pos1 with
          | None -> assert_failure {|join "a" "a" is None|}
          | Some(j_a_a) -> assert_equal ~msg:{|join "a" "a" is not "a"|} "a" j_a_a ;
        match join2 "0" "a" ~l:a_pos1 with
          | None -> assert_failure {|join "0" "a" ~l is None|}
          | Some(j_0_a) -> assert_equal ~msg:{|join "0" "a" is not "a"|} "a" j_0_a ;
        match join2 "a" "0" ~l:a_pos1 with
          | None -> assert_failure {|join "a" "0" ~l is None|}
          | Some(j_a_0) -> assert_equal ~msg:{|join "a" "0" is not "a"|} "a" j_a_0 ;
        match join2 "a" "b" ~l:a_pos1 with
          | None -> assert_failure {|join "a" "b" ~l is None|}
          | Some(j_a_b) -> assert_equal ~msg:{|join "a" "b" is not "1"|} "1" j_a_b ;
      );
    "can check a POS to be lattice" >:: (fun _ ->
        let a_pos1 = {
          elems=["0";"a";"1"];
          rels=[ ("0","a"); ("a","1"); ];
        } in
        assert_bool {|is_lattice l is false|} @@
          is_lattice a_pos1;
        let a_pos2 = {
          elems=["0";"a";"b";"1"];
          rels=[ ("0","a"); ("a","1"); ("0","b"); ];
        } in
        assert_bool {|is_lattice l is true|} @@
          not @@ is_lattice a_pos2;
      );
    "has utility functions working right" >:: (fun _ ->
        let a_list = ["a";"b";"c";] in
        assert_equal ~msg:"pairs_from_list producing lists" 6 @@
          List.length @@ pairs_from_list a_list
      );
  ]

let _ = run_test_tt_main tests
