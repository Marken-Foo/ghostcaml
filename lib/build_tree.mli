module CharMap : Map.S with type key = char

(** A tree where each child node is indexed by a letter.
   A word (or prefix) can be represented by a path from the root. *)
module WordTree : sig
  type 'a t = { valid_moves : 'a t CharMap.t; value : 'a }

  val cata : ('a t -> 'r CharMap.t -> 'r) -> 'a t -> 'r
  val foldl : ('r -> 'a t -> 'r) -> 'r -> 'a t -> 'r
  val next_node : char -> 'a t -> 'a t option
  val find_node : string -> 'a t -> 'a t option
end

type player = Player1 | Player2
type game_string = Word of string | NotWord of string

type game_value =
  | Won of { string : string; winner : player }
  | Winning of { string : string; winner : player; winning_moves : char list }
  | Losing of { string : string; winner : player }

val evaluate : game_string WordTree.t -> game_value WordTree.t
(** Evaluates the winner and winning moves of every node of a [game_string] tree,
    returning a [game_value] tree. *)

val tree_from_words : string list -> game_string WordTree.t
(** Constructs a [game_string] tree from a list of strings. *)
