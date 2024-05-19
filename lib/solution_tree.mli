type player = Player1 | Player2

val other_player : player -> player

module CharMap = Word_tree.CharMap

type game_string = Word of string | NotWord of string

type game_value =
  | Won of { string : string; winner : player }
  | Winning of { string : string; winner : player; winning_moves : char list }
  | Losing of { string : string; winner : player }

type solution_node = game_value Word_tree.t

val evaluate : game_string Word_tree.t -> game_value Word_tree.t
(** Evaluates the winner and winning moves of every node of a [game_string] tree,
    returning a [game_value] tree. *)

val tree_from_words : string list -> game_string Word_tree.t
(** Constructs a [game_string] tree from a list of strings. *)
