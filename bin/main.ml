open Ghost
open Ghost.Solution_tree

type move_outcome = Legal | Illegal | SpellsWord

let move_outcome (c : char) (curr_node : game_value Word_tree.t) =
  match Word_tree.next_node c curr_node with
  | None -> Illegal
  | Some { value = Won _; _ } -> SpellsWord
  | Some { value = Winning _ | Losing _; _ } -> Legal

type game_state = {
  solution : game_value Word_tree.t;
  curr_string : string;
  side_to_move : player;
  human_player : player;
}

type should_play = Play | Quit

(** Return the game state that would arise after playing the given character. *)
let next_game_state (state : game_state) (c : char) =
  {
    state with
    curr_string = state.curr_string ^ String.make 1 c;
    side_to_move = other_player state.side_to_move;
  }

(** Returns a random element of a list. *)
let random_from_list list = List.nth list (Random.int (List.length list))

(** Given the current node, decide the computer's next move. *)
let decide_computer_next_move (curr_node : game_value Word_tree.t) =
  match curr_node.value with
  | Won _ -> assert false
  | Winning { winning_moves; _ } -> random_from_list winning_moves
  | Losing _ -> (
      let valid_moves = curr_node.valid_moves |> CharMap.to_list in
      let not_immediately_losing =
        CharMap.filter
          (fun _ (n : game_value Word_tree.t) ->
            match n.value with Won _ -> false | Winning _ | Losing _ -> true)
          curr_node.valid_moves
      in
      match CharMap.is_empty not_immediately_losing with
      | true -> random_from_list valid_moves |> fst
      | false ->
          random_from_list (not_immediately_losing |> CharMap.to_list) |> fst)

let is_alpha ch =
  let c = int_of_char ch in
  (c >= 65 && c <= 90) || (c >= 97 && c <= 122)

let rec ask_for_side solution =
  Printf.printf "Do you want to go first or second? (enter 1 or 2)\n";
  let player_side =
    match read_line () with
    | "1" -> Some Player1
    | "2" -> Some Player2
    | _ -> None
  in
  match player_side with
  | None ->
      Printf.printf "Invalid input. Please enter '1' or '2'.\n";
      ask_for_side solution
  | Some player ->
      turn
        {
          solution;
          side_to_move = Player1;
          curr_string = "";
          human_player = player;
        }

and turn game_state =
  let { side_to_move; human_player; _ } = game_state in
  if side_to_move = human_player then player_turn game_state
  else computer_turn game_state

and player_turn game_state =
  let { curr_string; _ } = game_state in
  let _ = Printf.printf "Current string: %s\nEnter a letter:\n" curr_string in
  let input = read_line () |> String.to_seq |> List.of_seq in
  let input_char =
    match input with c :: [] when is_alpha c -> Some c | _ -> None
  in
  match input_char with
  | None ->
      Printf.printf "Please enter a single letter.\n";
      player_turn game_state
  | Some c -> evaluate_move game_state c

and computer_turn game_state =
  let { solution; curr_string; _ } = game_state in
  let curr_node = Word_tree.find_node curr_string solution in
  let c =
    Option.fold ~none:(char_of_int 0) ~some:decide_computer_next_move curr_node
  in
  Printf.printf "Computer plays a letter: %c\n" c;
  evaluate_move game_state c

and evaluate_move game_state c =
  let { solution; curr_string; _ } = game_state in
  let next_string = curr_string ^ String.make 1 c in
  let new_state = next_game_state game_state c in
  let outcome =
    Word_tree.find_node game_state.curr_string solution
    |> Option.fold ~none:Illegal ~some:(move_outcome c)
  in
  match outcome with
  | Illegal ->
      Printf.printf "Illegal move! No word begins with '%s'!\n" next_string;
      end_game new_state
  | SpellsWord ->
      Printf.printf "Spelled a word! '%s' is a word!\n" next_string;
      end_game new_state
  | Legal -> turn new_state

and end_game game_state =
  let { side_to_move; human_player; _ } = game_state in
  let () =
    match side_to_move = human_player with
    | true -> Printf.printf "You win!\n"
    | false -> Printf.printf "Computer wins!\n"
  in
  ()

let ask_play_again () =
  Printf.printf "Play again? (Y for yes / any other input to quit)\n";
  let input = read_line () in
  match input with "y" | "Y" -> Play | _ -> Quit

(* Read wordlist file and build evaluated tree. *)
let read_lines file = In_channel.with_open_bin file In_channel.input_lines
let solution () = tree_from_words (read_lines "ghost_words.txt") |> evaluate

let rec loop_game solution action =
  match action with
  | Play ->
      ask_for_side solution;
      loop_game solution (ask_play_again ())
  | Quit -> ()

let main _ =
  Printf.printf "Welcome to GHOST.\n";
  let solution = solution () in
  loop_game solution Play;
  Printf.printf "Goodbye!\n"

let _ = main ()
