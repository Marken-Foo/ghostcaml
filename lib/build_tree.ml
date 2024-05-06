(* define tree nodes *)

module CharMap = Map.Make (Char)

type 'a word_tree = { valid_moves : 'a word_tree CharMap.t; value : 'a }

let rec cata f_node (node : 'a word_tree) : 'r =
  let recurse = cata f_node in
  f_node node (node.valid_moves |> CharMap.map recurse)

let rec foldl f_node acc (node : 'a word_tree) : 'r =
  let recurse = foldl f_node in
  let new_acc = f_node acc node in
  CharMap.to_list node.valid_moves
  |> List.map snd
  |> List.fold_left recurse new_acc

type node_details = { string : string; depth : int; is_word : bool }
type game_string = Word of string | NotWord of string
type player = Player1 | Player2

let other_player player =
  match player with Player1 -> Player2 | Player2 -> Player1

(* type game_value =
   | Winning of { string : string; winner : player; winning_moves : char list }
   | Won of { string : string; winner : player }
   | Losing of { string : string; winner : player } *)

type game_value = {
  string : game_string;
  winner : player;
  winning_moves : char list;
}

let rec insert_word (word : char list) (node : game_string word_tree) =
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

let judge (node : game_string word_tree)
    (new_children : game_value word_tree CharMap.t) : game_value word_tree =
  let depth = match node.value with Word w | NotWord w -> String.length w in
  let player_to_move = if depth mod 2 = 0 then Player1 else Player2 in
  match node.value with
  | Word w ->
      {
        valid_moves = new_children;
        value = { string = Word w; winner = player_to_move; winning_moves = [] };
      }
  | NotWord w ->
      let winning_moves =
        new_children |> CharMap.to_list
        |> List.filter (fun (_, node) -> node.value.winner = player_to_move)
        |> List.map fst
      in
      {
        valid_moves = new_children;
        value =
          {
            string = NotWord w;
            winner =
              (match winning_moves with
              | [] -> other_player player_to_move
              | _ -> player_to_move);
            winning_moves;
          };
      }

let judge_tree = cata judge

let print_tree (node : 'a word_tree) (get_game_string : 'a -> game_string) =
  let print_node _ n =
    let game_string = get_game_string n.value in
    let is_word, w =
      match game_string with Word w -> (true, w) | NotWord w -> (false, w)
    in
    Format.printf "(word: %s, depth: %i, children: %s, is word: %b)\n" w
      (String.length w)
      (n.valid_moves |> CharMap.to_seq |> Seq.map fst |> String.of_seq)
      is_word
  in
  foldl print_node () node

let print_judged_node (node : game_value word_tree) =
  let game_string = node.value.string in
  let is_word, word =
    match game_string with Word w -> (true, w) | NotWord w -> (false, w)
  in
  let depth = String.length word in
  let children =
    node.valid_moves |> CharMap.to_seq |> Seq.map fst |> String.of_seq
  in
  let winner =
    match node.value.winner with Player1 -> "Sente" | Player2 -> "Gote"
  in
  let winning_moves =
    node.value.winning_moves |> List.to_seq |> String.of_seq
  in
  Format.printf
    "(word: %s, depth: %i, children: %s, is word: %b, winner: %s)\n\
    \  winning moves: [%s]\n"
    word depth children is_word winner winning_moves

let print_judged (node : game_value word_tree) =
  let print_node _ n = print_judged_node n in
  foldl print_node () node

let words = [ "abc"; "adoa"; "absinthe"; "abs" ]

let tree_from_words words =
  let exploded_words =
    words |> List.map String.to_seq |> List.map List.of_seq
  in
  let root = { valid_moves = CharMap.empty; value = NotWord "" } in
  List.fold_right insert_word exploded_words root

let full_tree = tree_from_words words
let () = print_tree full_tree Fun.id
let judged_tree = judge_tree full_tree
let () = print_judged judged_tree

(* read file, build tree *)
let read_lines file = In_channel.with_open_bin file In_channel.input_lines
let words = read_lines "ghost_words.txt"

let absolute_tree =
  tree_from_words words |> judge_tree |> print_judged_node |> ignore
