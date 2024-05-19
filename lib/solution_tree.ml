open Word_tree

type player = Player1 | Player2

let other_player player =
  match player with Player1 -> Player2 | Player2 -> Player1

module CharMap = Word_tree.CharMap

type game_string = Word of string | NotWord of string

type game_value =
  | Won of { string : string; winner : player }
  | Winning of { string : string; winner : player; winning_moves : char list }
  | Losing of { string : string; winner : player }

let get_winner game_value =
  match game_value with
  | Winning v -> v.winner
  | Losing v -> v.winner
  | Won v -> v.winner

let get_string game_value =
  match game_value with
  | Winning v -> v.string
  | Losing v -> v.string
  | Won v -> v.string

let rec insert_word (word : char list) (node : game_string t) =
  let w = match node.value with Word w | NotWord w -> w in
  match word with
  | [] -> { node with value = Word w }
  | c :: cs ->
      let next_node =
        match CharMap.find_opt c node.valid_moves with
        | Some n -> n
        | None ->
            {
              valid_moves = CharMap.empty;
              value =
                (let next_word = w ^ String.make 1 c in
                 match cs with [] -> Word next_word | _ -> NotWord next_word);
            }
      in
      {
        node with
        valid_moves = CharMap.add c (insert_word cs next_node) node.valid_moves;
      }

(** Given the evaluated children of the current simple node,
    evaluate who wins from the current node (assuming best play),
    and which moves are winning. *)
let _evaluate (node : game_string t) (new_children : game_value t CharMap.t) :
    game_value t =
  (* To be used with the tree catamorphism *)
  let old_value = node.value in
  let depth = match old_value with Word w | NotWord w -> String.length w in
  let player_to_move = if depth mod 2 = 0 then Player1 else Player2 in
  let value =
    match old_value with
    | Word w -> Won { string = w; winner = player_to_move }
    | NotWord w -> (
        let winning_moves =
          new_children |> CharMap.to_list
          |> List.filter (fun (_, (n : game_value t)) ->
                 player_to_move = get_winner n.value)
          |> List.map fst
        in
        match winning_moves with
        | [] -> Losing { string = w; winner = other_player player_to_move }
        | _ -> Winning { string = w; winner = player_to_move; winning_moves })
  in
  { valid_moves = new_children; value }

(** Evaluates the winner and winning moves of every node of a [game_string] tree,
    returning a [game_value] tree. *)
let evaluate (tree : game_string t) = cata _evaluate tree

(** Constructs a [game_string] tree from a list of strings. *)
let tree_from_words words =
  let chars_of_words =
    words |> List.map String.to_seq |> List.map List.of_seq
  in
  let root = { valid_moves = CharMap.empty; value = NotWord "" } in
  List.fold_right insert_word chars_of_words root

(** Temp module for printing trees to test *)
module TreePrinting = struct
  let print_plain_node (node : game_string t) =
    let game_string = node.value in
    let is_word, w =
      match game_string with Word w -> (true, w) | NotWord w -> (false, w)
    in
    Format.printf "(word: %s, depth: %i, children: %s, is word: %b)\n" w
      (String.length w)
      (node.valid_moves |> CharMap.to_seq |> Seq.map fst |> String.of_seq)
      is_word

  let print_evaluated_node (node : game_value t) =
    let game_string = get_string node.value in
    let is_word =
      match node.value with Won _ -> true | Winning _ | Losing _ -> false
    in
    let depth = String.length game_string in
    let children =
      node.valid_moves |> CharMap.to_seq |> Seq.map fst |> String.of_seq
    in
    let winner =
      match get_winner node.value with Player1 -> "Sente" | Player2 -> "Gote"
    in
    let winning_moves =
      match node.value with
      | Won _ -> "ALREADY WON"
      | Winning v -> v.winning_moves |> List.to_seq |> String.of_seq
      | Losing _ -> ""
    in
    Format.printf
      "(word: %s, depth: %i, children: %s, is word: %b, winner: %s)\n\
      \  winning moves: [%s]\n"
      game_string depth children is_word winner winning_moves

  let print_tree (node : game_string t) =
    let print_node _ n = print_plain_node n in
    foldl print_node () node

  let print_evaluated (node : game_value t) =
    let print_node _ n = print_evaluated_node n in
    foldl print_node () node
end
