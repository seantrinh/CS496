module type Tree =
sig
	type 'a t
		val create: unit -> 'a t
			val node: 'a -> 'a t -> 'a t -> 'a t
			val empty: 'a t -> bool
			val elem: 'a -> 'a t -> bool
			val mapt: ('a -> 'a) -> 'a t -> unit
			val string_of_tree: 'a t -> string
end

(*module MyTree: Tree =
struct
	type 'a t = ...


end *)