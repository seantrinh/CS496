

let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast



(* Interpret an expression *)
let interp (e:string) : Ds.exp_val =
  e |> parse |> Interp.eval_prog 

(* Type-check an expression *)
let chk (e:string) : Ast.texpr =
  e |> parse |> Checker.type_of_prog 
