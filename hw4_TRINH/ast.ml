(* The type of the abstract syntax tree (AST). *)
type expr =
  | Var of string
  | Int of int
  | Add of expr*expr
  | Sub of expr*expr
  | Mul of expr*expr
  | Div of expr*expr
  | Let of string*expr*expr
  | Proc of string*expr
  | App of expr*expr
  | IsZero of expr
  | ITE of expr*expr*expr
  | Letrec of (dec list)*expr
  | Set of string*expr
  | BeginEnd of expr list
  | NewRef of expr
  | DeRef of expr
  | SetRef of expr*expr
  | Abs of expr
  | Cons of expr*expr
  | Hd of expr
  | Tl of expr
  | Null of expr
  | EmptyList
and
  dec = Dec of string*string*expr

let rec string_of_expr e =
  match e with
  | Var s -> "Var "^s
  | Int n -> "Int "^string_of_int n
  | Add(e1,e2) -> "Add(" ^ (string_of_expr e1) ^ "," ^ string_of_expr e2 ^ ")"
  | Sub(e1,e2) -> "Sub(" ^ (string_of_expr e1) ^ "," ^ string_of_expr e2 ^ ")"
  | Mul(e1,e2) -> "Mul(" ^ (string_of_expr e1) ^ "," ^ string_of_expr e2 ^ ")"
  | Div(e1,e2) -> "Div(" ^ (string_of_expr e1) ^ "," ^ string_of_expr e2 ^ ")"
  | NewRef(e) -> "Add(" ^ (string_of_expr e) ^ ")"
  | DeRef(e) -> "Add(" ^ (string_of_expr e) ^ ")"
  | SetRef(e1,e2) -> "SetRef(" ^ (string_of_expr e1) ^ "," ^ string_of_expr e2 ^ ")"
  | Let(x,def,body) -> "Let("^x^","^string_of_expr def ^","^ string_of_expr body ^")"
  | Proc(x,body) -> "Proc("^x^"," ^ string_of_expr body ^")"
  | App(e1,e2) -> "App("^string_of_expr e1 ^"," ^ string_of_expr e2^")"
  | IsZero(e) -> "Zero?("^string_of_expr e ^")"
  | ITE(e1,e2,e3) -> "IfThenElse("^string_of_expr e1^"," ^ string_of_expr e2^"," ^ string_of_expr e3  ^")"
  | Letrec(decs, body) -> "Letrec(" ^ string_of_decs decs ^ " | " ^ string_of_expr body ^ ")"
  | Set(x,rhs) -> "Set("^x^","^string_of_expr rhs^")"
  | BeginEnd(es) -> "BeginEnd(" ^ List.fold_left (fun x y -> x^","^y) "" (List.map string_of_expr es) ^")"
  | Abs(e1) -> "Abs("^string_of_expr e1^")"
  | Cons(e1,e2) -> "Cons(" ^ (string_of_expr e1) ^ "," ^ string_of_expr e2 ^ ")"
  | Hd(e1) -> "Hd("^string_of_expr e1 ^")"
  | Tl(e1) -> "Tl("^string_of_expr e1 ^")"
  | Null(e1) -> "Null("^string_of_expr e1 ^")"
  | EmptyList -> "EmptyList"
and
  string_of_decs decs =
    match decs with
    | [] -> ""
    | [Dec(x, y, e1)] -> "Dec(" ^ x ^ ", " ^ y ^ ", " ^ string_of_expr e1 ^ ")"
    | Dec(x, y, e1)::rest ->
      "Dec(" ^ x ^ ", " ^ y ^ ", " ^ string_of_expr e1 ^ "), " ^ string_of_decs rest
