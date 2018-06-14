open Ast
open Ds
(* I pledge my honor that I have abided by the Stevens Honor System. - Sean Trinh *)
let rec apply_proc f a =
  match f with
    ProcVal (x,b,env) -> eval (LetEnv (x, a, env)) b
  | _ -> failwith "apply_proc: Not a procVal"
and
  eval (en:env) (e:expr) :exp_val =
  match e with
  | Int n                -> NumVal n
  | Var x                -> lookup en x
  | Add(e1, e2)          ->
    let v1 = eval en e1 in
    let v2 = eval en e2  in
    NumVal ((numVal_to_num v1) + (numVal_to_num v2))
  | Sub(e1, e2)          ->
    let v1 = eval en e1 in
    let v2 = eval en e2  in
    NumVal ((numVal_to_num v1) - (numVal_to_num v2))
  | Mul(e1, e2)          ->
    let v1 = eval en e1 in
    let v2 = eval en e2  in
    NumVal ((numVal_to_num v1) * (numVal_to_num v2))
  | Div(e1, e2)          ->
    let v1 = eval en e1 in
    let v2 = eval en e2  in
    NumVal ((numVal_to_num v1) / (numVal_to_num v2))
  | IsZero(e)            ->
    let v = eval en e in
    let n = numVal_to_num v in
    BoolVal (n == 0)
  | ITE(e1, e2, e3)      ->
    let v1 = eval en e1 in
    if boolVal_to_bool v1 then
      eval en e2
    else eval en e3
  | Abs(e1)      ->
    let v1 = eval en e1 in
    NumVal (abs (numVal_to_num v1))
  | Cons(e1, e2) ->
    let v1 = eval en e1 in
    let v2 = eval en e2  in
    let l1 = listVal_to_list v2 in
    ListVal (v1 :: l1)
  | Hd(e1)      ->
    let v1 = eval en e1 in
    let l1 = listVal_to_list v1 in
    List.hd l1
  | Tl(e1)      ->
    let v1 = eval en e1 in
    let l1 = listVal_to_list v1 in
    ListVal (List.tl l1)
  | Null(e1)     ->
    let v1 = eval en e1 in
    let l1 = listVal_to_list v1 in
    BoolVal (l1 = [])
  | EmptyList    -> ListVal []
  | Let(x, e1, e2)       ->
    let v1 = eval en e1  in
    eval (LetEnv (x, v1, en)) e2
  | Letrec(decs, e2) ->
    (* TODO evaluate e2 with a new LetrecEnv *)
    let newEnv = LetrecEnv(decs, en) in
    eval newEnv e2
  | Proc(x,e)            -> ProcVal (x,e,en)
  | App(e1,e2)           ->
    let v1 = eval en e1 in
    let v2 = eval en e2 in
    apply_proc v1 v2
  | _ -> failwith("Not implemented")


(***********************************************************************)
(* Everything above this is essentially the same as we saw in lecture. *)
(***********************************************************************)

(* Parse a string into an ast *)


let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


(* Interpret an expression *)
let interp (e:string) : exp_val =
  e |> parse |> eval EmptyEnv
