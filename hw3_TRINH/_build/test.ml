open OUnit2
open Ast
open Ds
open Interp

(* A few test cases *)
let tests = [
  "int"  >:: (fun _ -> assert_equal (NumVal 22) (interp "22"));
  "add"  >:: (fun _ -> assert_equal (NumVal 22) (interp "11+11"));
  "adds" >:: (fun _ -> assert_equal (NumVal 22) (interp "(10+1)+(5+6)"));
  "let"  >:: (fun _ -> assert_equal (NumVal 22) (interp "let x=22 in x"));
  "lets" >:: (fun _ -> assert_equal (NumVal 22) (interp "let x = 0 in let x = 22 in x"));
  "abs"  >:: (fun _ -> assert_equal (NumVal (-1)) (interp "abs((-5)) - 6"));
  "mul"  >:: (fun _ -> assert_equal (NumVal 12) (interp "3 * 4"));
  "div"  >:: (fun _ -> assert_equal (NumVal 0) (interp "3 / 5"));
  "div2" >:: (fun _ -> assert_equal (NumVal 1) (interp "5 / 5"));
  "null" >:: (fun _ -> assert_equal (BoolVal true) (interp "null?(emptylist)"));
  "null2" >:: (fun _ -> assert_equal (BoolVal false) (interp "let e = cons(1,emptylist) in null?(e)"));
  "idk"  >:: (fun _ -> assert_equal (ListVal [NumVal 4; ListVal [NumVal 3]]) (interp "let x = 4 in
  	cons(x,cons(cons(x-1,emptylist),emptylist))"));
  "idk2" >:: (fun _ -> assert_equal(BoolVal true) (interp "null?(tl(cons(cons(1,emptylist),emptylist)))"));
  "idk3" >:: (fun _ -> assert_equal(ListVal []) (interp "tl(cons(cons(1,emptylist),emptylist))"));
  "idk4" >:: (fun _ -> assert_equal(ListVal[ListVal[NumVal 1]]) (interp "cons(cons(1,emptylist),emptylist)"));
]

let _ = run_test_tt_main ("suite" >::: tests)
