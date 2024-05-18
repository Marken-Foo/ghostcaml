open Ghost.Build_tree

type move_outcome = Legal | Illegal | SpellsWord

let move_outcome (c : char) (curr_node : game_value WordTree.t) =
  match WordTree.next_node c curr_node with
  | None -> Illegal
  | Some { value = Won _; _ } -> SpellsWord
  | Some { value = Winning _ | Losing _; _ } -> Legal

type game_state = { solution : game_value WordTree.t; curr_string : string }

let next_game_state (state : game_state) (c : char) =
  { state with curr_string = state.curr_string ^ String.make 1 c }

let random_from_list list = List.nth list (Random.int (List.length list))

let decide_computer_next_move (curr_node : game_value WordTree.t) =
  match curr_node.value with
  | Won _ -> assert false
  | Winning { winning_moves; _ } -> random_from_list winning_moves
  | Losing _ -> (
      let valid_moves = curr_node.valid_moves |> CharMap.to_list in
      let not_immediately_losing =
        CharMap.filter
          (fun _ (n : game_value WordTree.t) ->
            match n.value with Won _ -> false | Winning _ | Losing _ -> true)
          curr_node.valid_moves
      in
      match CharMap.is_empty not_immediately_losing with
      | true -> random_from_list valid_moves |> fst
      | false ->
          random_from_list (not_immediately_losing |> CharMap.to_list) |> fst)

(* Read wordlist file and build evaluated tree. *)
let read_lines file = In_channel.with_open_bin file In_channel.input_lines
let solution () = tree_from_words (read_lines "ghost_words.txt") |> evaluate

let is_alpha ch =
  let c = int_of_char ch in
  (c >= 65 && c <= 90) || (c >= 97 && c <= 122)

let rec load_game _ =
  let start_state = { solution = solution (); curr_string = "" } in
  welcome start_state

and welcome game_state =
  Printf.printf "Welcome to GHOST.\n";
  ask_for_side game_state

and ask_for_side game_state =
  Printf.printf "Do you want to go first or second? (enter 1 or 2)\n";
  let input = read_line () in
  match input with
  | "1" -> player_turn { game_state with curr_string = "" }
  | "2" -> computer_turn { game_state with curr_string = "" }
  | _ ->
      Printf.printf "Invalid input. Please enter '1' or '2'.\n";
      ask_for_side game_state

and player_turn game_state =
  let { solution; curr_string } = game_state in
  let _ = Printf.printf "Current string: %s\nEnter a letter:\n" curr_string in
  let input = read_line () |> String.to_seq |> List.of_seq in
  let c, is_single_letter =
    match input with c :: [] -> (c, is_alpha c) | _ -> (char_of_int 0, false)
  in
  match is_single_letter with
  | false ->
      Printf.printf "Please enter a single letter.\n";
      player_turn game_state
  | true -> (
      let next_string = curr_string ^ String.make 1 c in
      let new_state = next_game_state game_state c in
      let outcome =
        WordTree.find_node game_state.curr_string solution
        |> Option.map (move_outcome c)
        |> Option.value ~default:Illegal
      in
      match outcome with
      | Illegal ->
          Printf.printf "Illegal move! No word begins with '%s'!\n" next_string;
          computer_win new_state
      | SpellsWord ->
          Printf.printf "Spelled a word! '%s' is a word!\n" next_string;
          computer_win new_state
      | Legal -> computer_turn new_state)

and computer_turn game_state =
  let { solution; curr_string } = game_state in
  let curr_node = WordTree.find_node curr_string solution in
  let c =
    Option.fold ~none:(char_of_int 0) ~some:decide_computer_next_move curr_node
  in
  Printf.printf "Computer plays a letter: %c\n" c;
  let outcome =
    WordTree.find_node game_state.curr_string solution
    |> Option.map (move_outcome c)
    |> Option.value ~default:Illegal
  in
  let next_string = curr_string ^ String.make 1 c in
  let new_state = next_game_state game_state c in
  match outcome with
  | Illegal ->
      Printf.printf "Illegal move! No word begins with '%s'!\n" next_string;
      player_win new_state
  | SpellsWord ->
      Printf.printf "Spelled a word! '%s' is a word!\n" next_string;
      player_win new_state
  | Legal -> player_turn new_state

and computer_win game_state =
  Printf.printf "Computer wins!\n";
  play_again game_state

and player_win game_state =
  Printf.printf "You win!\n";
  play_again game_state

and play_again game_state =
  Printf.printf "Play again? (Y for yes / any other input to quit)\n";
  let input = read_line () in
  match input with
  | "y" | "Y" -> ask_for_side { game_state with curr_string = "" }
  | _ ->
      Printf.printf "Goodbye!\n";
      exit 0

let _ = load_game ()
