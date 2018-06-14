(* This file defines expressed values and environments *)

(* expressed values and environments are defined mutually recursively *)

(* I pledge my honor that I have abided by the Stevens Honor System. - Sean Trinh *)

type exp_val =
  | NumVal of int
  | BoolVal of bool
  | ProcVal of string*Ast.expr*env
  | ListVal of exp_val list
and
  env =
  | EmptyEnv
  | LetEnv of string*exp_val*env
  | LetrecEnv of (Ast.dec list)*env
  (* Old implementation (without mutual recursion) *)
  (* | LetrecEnv of (string*string*Ast.expr*env) *)

(* Operations on environments *)

(* TODO Return the declaration specified by id, or None if it is not found
   Look at the matches in the LetrecEnv case of lookup for hints on what you
   should be returning *)
let rec find_dec (decs:(Ast.dec list)) (id:string):(Ast.dec option) =
  match decs with
  | [] -> None
  | Ast.Dec(name, var, body)::_ when name = id -> Some (Ast.Dec(name,var,body))
  | Ast.Dec(name, var, body)::rest -> find_dec rest id

let rec lookup (env:env) (id:string):exp_val =
  match env with
  | EmptyEnv                       -> failwith "Id not found"
  | LetEnv (key, value, saved_env) ->
    if id=key then
      value
    else lookup saved_env id
  | LetrecEnv (decs, saved_env)    ->
    let dec = find_dec decs id in
    match dec with
    | None -> lookup saved_env id
    | Some (Ast.Dec(name, var, body)) -> ProcVal(var, body, env)
  (* Old implementation (without mutual recursion) *)
  (* | LetrecEnv (name, var, body, saved_env) ->
   *   if id=name then
   *     ProcVal(var, body, env)
   *   else lookup saved_env id *)

(* operations on expressed values *)

let numVal_to_num =  function
  | NumVal n -> n
  | _ -> failwith "Expected a number!"

let boolVal_to_bool =  function
  | BoolVal b -> b
  | _ -> failwith "Expected a boolean!"

let listVal_to_list =  function
  | ListVal l -> l
  | _ -> failwith "Expected a list!"

let rec string_of_expval = function
  | NumVal n -> "NumVal " ^ string_of_int n
  | BoolVal b -> "BoolVal " ^ string_of_bool b
  | ProcVal (id,body,env) -> "ProcVal ("^id^","^Ast.string_of_expr body^","^ string_of_env env^")"
  | ListVal l -> "ListVal " ^ string_of_listval l
and
  string_of_listval e = String.concat ";" (List.map (fun (x) -> "("^string_of_expval x^")") e)
and
  string_of_env = function
  | EmptyEnv -> ""
  | LetEnv (key, value, saved_env) ->
    "(" ^ key ^ "," ^ string_of_expval value ^ "); " ^ string_of_env saved_env
  | LetrecEnv (decs, saved_env) ->
    "(" ^ Ast.string_of_decs decs ^ " ); " ^ string_of_env saved_env
