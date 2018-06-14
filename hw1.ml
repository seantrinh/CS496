(* Sean Trinh *)
(* I pledge my honor that I have abided by the Stevens Honor System.*)

(* Exercise 1 *)
(* ------------------------------------------------------------------------------- *)
(* seven: 'a -> int *)
let seven x = 7

(* sign: int -> int *)
let sign x =
	if x>0 then 1
	else if x<0 then -1 else 0

(* absolute: int -> int *)
let absolute x =
	if x>=0 then x else x*(-1)

(* andp: bool -> bool -> bool *)
let andp x y =
	if x=true && y=true then true else false

(* orp: bool -> bool -> bool *)
let orp x y =
	if x=true || y=true then true else false

(* notp: bool -> bool *)
let notp x =
	if x=true then false else true

(* xorp: bool -> bool -> bool *)
let xorp x y =
	if (x=true && y=true) || (x=false && y=false) then false else true;;

(* dividesBy: int -> int -> bool *)
let dividesBy x y =
	if x mod y = 0 then true else false

(* is_singleton: 'a list -> bool *)
let rec is_singleton xs =
	match xs with
	| [] -> false
	| x::y::xs -> false
	| x::xs -> true

(* swap: 'a * 'b -> 'b * 'a *)
let swap (x,y) = (y,x)

(* app: ('a -> 'b) -> 'a -> 'b *)
let app x y = x y 

(* twice: ('a -> 'a) -> 'a -> 'a *)
let twice x y = x (x y)

(* compose: ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b *)
let compose x y z = x (y z)

(* Exercise 2 *)
(* ------------------------------------------------------------------------------- *)
(* belongsTo_ext: 'a -> 'a list -> bool *)
let rec belongsTo_ext k list  =
	match list with
	|[] -> false
	|x::xs -> if x=k then true else belongsTo_ext k xs

(* belongsTo_char: 'a - > ('a -> bool) -> 'bool *)
let belongsTo_char x f1 = (f1 x)=true;;

(* union_ext: 'a list -> 'a list -> 'a list *)
let rec union_ext list1 list2 =
	match list1 with
	|[] -> list2
	|x::xs -> if belongsTo_ext x list2=true then union_ext xs list2
		else x::union_ext xs list2

(* union_char ('a -> bool) -> ('a -> bool) -> 'a -> bool *)
let union_char f1 f2 x = (f1 x)||(f2 x)

(* intersection_ext: a' list -> a' list -> a' list*)
let rec intersection_ext list1 list2 =
	match list1 with
	|[] -> []
	|x::xs -> if belongsTo_ext list2 x=true then x::intersection_ext xs list2
		else intersection_ext xs list2

(* intersection_char ('a -> bool) -> ('a -> bool) -> ('a -> bool) *)
let intersection_char f1 f2 x = (f1 x)&&(f2 x)

(* remAdjDups: 'a list -> 'a list *)
let rec remAdjDups = function
	|[] -> []
	|[x] -> [x]
	|x::y::xs -> if x=y then remAdjDups(y::xs) else x::remAdjDups(y::xs)

(* sublists: 'a list -> 'a list list *)
let rec sublists list =
	let rec mapAppend k list =
		match list with
		|[] -> []
		|x::xs -> [k::x]@mapAppend k xs in
	match list with
	|[] -> [[]]
	|x::xs -> let sub = sublists(xs) in
				sub@mapAppend x sub

(* Exercise 3 *)
(* ------------------------------------------------------------------------------- *)
type calcExp =
| Const of int
| Add of (calcExp*calcExp)
| Sub of (calcExp*calcExp)
| Mult of (calcExp*calcExp)
| Div of (calcExp*calcExp)

(* mapC: (int -> int) -> calcExp -> calcExp *)
let rec mapC f e =
	match e with
	|Add(x,y) -> Add(mapC f x, mapC f y)
	|Sub(x,y) -> Sub(mapC f x, mapC f y)
	|Mult(x,y) -> Mult(mapC f x, mapC f y)
	|Div(x,y) -> Div(mapC f x, mapC f y)
	|Const(x) -> Const(f x)

(* foldC: (calcExp -> 'a -> 'a -> 'a) -> 'a -> calcExp -> 'a *)
let rec foldC f a e =
	match e with
	|Const(x) -> f e a a
	|Add(x,y) -> f e (foldC f a x) (foldC f a y)
	|Sub(x,y) -> f e (foldC f a x) (foldC f a y)
	|Mult(x,y) -> f e (foldC f a x) (foldC f a y)
	|Div(x,y) -> f e (foldC f a x) (foldC f a y)

(* numAdd: calcExp -> int *)
let numAdd e =
	let add e a b =
	match e with
	|Add(x,y) -> 1 + a + b
	|_ -> a + b in
	foldC add 0 e

(* replaceAddWithMult: calcExp -> calcExp *)
let replaceAddWithMult e =
	let rep e a b =
		match e with
		|Add(x,y) -> Mult(a,b)
		|_ -> e in
	foldC rep e e

(* evalC: calcExp -> int *)
let rec evalC e =
	match e with
	|Const(x) -> x
	|Add(x,y) -> evalC x + evalC y
	|Sub(x,y) -> evalC x - evalC y
	|Mult(x,y) -> evalC x * evalC y
	|Div(x,y) -> evalC x / evalC y

(* evalCf: calcExp -> int *)
let evalCf e =
	let eval e a b =
	match e with
	|Add(x,y) -> a + b
	|Sub(x,y) -> a - b
	|Mult(x,y) -> a * b
	|Div(x,y) -> a / b
	|Const(x) -> x in
	foldC eval 0 e

(* Exercise 3 *)
(* ------------------------------------------------------------------------------- *)
(* Problem 1 *)
(* f: int list -> int *)
let f xs =
	let g = fun x r -> if x mod 2 = 0 then (+) r 1 else r
	in List.fold_right g xs 0
(* f takes in an integer list and counts how many members of that list are divisible by 2. *)
(* Example: f [1;2;3] yields int = 1
			f[1;2;3;4] yields int = 2
			f[0;1;2;3;4] yields int = 3 *)

(* Problem 3 *)
(* append: 'a list -> 'a list -> 'a list *)
let append xs =
	let g = fun x h -> x::h
	in List.fold_right g xs
(* append takes into two lists of type 'a and appends them together. *)
(* Example: append [1;2;3] [4;5;6]  yields 
				int list = [1;2;3;4;5;6]
			append ['a';'b';'d'] ['g';'q';'z'] yields
				char list = ['a'; 'b'; 'd'; 'g'; 'q'; 'z'] 
			append [true;false;false] [true;true;false] yields 
				bool list = [true; false; false; true; true; false] *)




