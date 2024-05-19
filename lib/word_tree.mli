module CharMap : Map.S with type key = char

(** A tree where each child node is indexed by a letter.
   A word (or prefix) can be represented by a path from the root. *)

type 'a t = { valid_moves : 'a t CharMap.t; value : 'a }

val cata : ('a t -> 'r CharMap.t -> 'r) -> 'a t -> 'r
val foldl : ('r -> 'a t -> 'r) -> 'r -> 'a t -> 'r
val next_node : char -> 'a t -> 'a t option
val find_node : string -> 'a t -> 'a t option
