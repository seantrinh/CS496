(* Sean Trinh *)
(* Hw 2 *)
(* I pledge my honor that I have abided by the Stevens Honor System. *)

(* 1. *)
type dTree = Leaf of int | Node of char*dTree*dTree

(* 2. *)
let tLeft = Node('w',Node('x',Leaf(2),Leaf(5)),Leaf(8))
let tRight = Node('w',Node('x',Leaf(2),Leaf(5)),Node('y',Leaf(7),Leaf(5)))

(* 3.a *)
(* dTree_height: dTree -> int *)
let rec dTree_height t =
	match t with
	|Leaf(x) -> 0
	|Node(c,x,y) -> let lt_height = dTree_height x in
					let rt_height = dTree_height y in
					1 + max lt_height rt_height

(* 3.b *)
(* dTree_size: dTree -> int *)
let rec dTree_size t =
	match t with
	|Leaf(x) -> 1
	|Node(c,x,y) -> 1 + dTree_size x + dTree_size y

(* 3.c *)
(* dTree_paths: dTree -> int list list *)
let rec dTree_paths t =
	let rec mapAppend k list =
		match list with
		|[] -> []
		|x::xs -> [k::x]@mapAppend k xs in
	match t with
	|Leaf(x) -> [[]]
	|Node(c,x,y) -> let lt = mapAppend 0 (dTree_paths x) in
					let rt = mapAppend 1 (dTree_paths y) in
					lt@rt

(* 3.d *)
(* dTree_is_perfect: dTree -> bool *)
let rec dTree_is_perfect t =
	match t with
	|Leaf(x) -> true
	|Node(c,x,y) -> if dTree_height x = dTree_height y
						then dTree_is_perfect x && dTree_is_perfect y
						else false

(* 3.e *)
(* dTree_map: (char -> char) -> (int -> int) -> dTree -> dTree *)
let rec dTree_map f g t =
	match t with
	|Leaf(x) -> Leaf(g x)
	|Node(c,x,y) -> Node(f c, dTree_map f g x, dTree_map f g y)

(* 4 *)
(* list_to_tree: char list -> dTree *)
let rec list_to_tree l =
	match l with
	|[] -> Leaf(0)
	|x::xs -> Node(x,list_to_tree xs, list_to_tree xs)

(* 5 *)
(* replace_leaf_at: dTree -> (int list * int) list -> dTree *)
let replace_leaf_at t g =
	let rec find_change g l x=
		match g with
		|[] -> x
		|(z,v)::xs -> if z = l then v else find_change xs l x in 
	let rec replace_leaf_at_help t g l=
		match t with
		|Leaf(x) -> Leaf(find_change g l x)
		|Node(c,x,y) -> Node(c, replace_leaf_at_help x g (0::l),
								replace_leaf_at_help y g (1::l)) in
	replace_leaf_at_help t g []

(* 6 *)
(* bf_to_dTree: char list * (int list * int) list -> dTree *)
let bf_to_dTree p =
	let (l,g) = p in
	replace_leaf_at (list_to_tree l) g



