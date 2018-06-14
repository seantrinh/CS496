
open OUnit2
open Main

let tests  = "test suite for the interpreter" >::: [

    (* assert_equal asserts that the two values are equal *)
    "test1"  >:: (fun _ -> assert_equal (Ds.TaggedVariantVal ("Node",
 [Ds.NumVal 3; Ds.TaggedVariantVal ("Nil", []);
  Ds.TaggedVariantVal ("Nil", [])]))
                                  (interp "Node<3,Nil,Nil>"));



        "test2"  >:: (fun _ -> assert_equal (Ds.UnitVal)
                                  (interp "
type treeInt =
   | Nil
   | Node<int,treeInt,treeInt>"));


            "test3"  >:: (fun _ -> assert_equal (Ds.NumVal 1)
                                  (interp "
let getRoot =
   proc (t:treeInt) {
     case t of {
       Nil -> error
       Node<x,y,z> -> x
   } }
in (getRoot (Node<1,Nil,Nil>))
"));

                "test4"  >:: (fun _ -> assert_equal (Ds.BoolVal false)
                                  (interp "
begin
   type treeInt =
     | Nil
     | Node<int,treeInt,treeInt>;
   let isEmpty=
    proc(t:treeInt) {
       case t of {
         Nil -> zero?(0)
         Node<x,y,z> -> zero?(1)
       } }
   in (isEmpty (Node<1,Nil,Nil>))
 end
"));

                    "test5"  >:: (fun _ -> assert_equal (Ds.TaggedVariantVal ("Node",
 [Ds.NumVal 3; Ds.TaggedVariantVal ("Nil", []);
  Ds.TaggedVariantVal ("Nil", [])]))
                                  (interp "
begin
   type treeInt = 
     | Nil
     | Node<int,treeInt,treeInt>;
   letrec ((int -> int) -> treeInt) mapT(t:treeInt) = 
        proc (f:(int -> int)) {
          case t of {
            Nil -> Nil
            Node<x,y,z> -> 
                 Node<(f x), ((mapT y) f), ((mapT z) f)>      
 } }
    in ((mapT (Node<2,Nil,Nil>)) (proc(x:int) {x+1}))
end
"))





(*   "typecheck body inclusion 3"    >:: (fun _ -> assert_raises (Checker.Subtype_failure("m1"))
 *                                 (fun () -> chk " 
 *     module m1 interface
 * [u : bool] 
 * body
 * [u = 33] 
 * 4")); *)

   ]



let _ = run_test_tt_main tests
