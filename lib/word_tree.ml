module CharMap = Map.Make (Char)

(** A tree where each child node is indexed by a letter.
   A word (or prefix) can be represented by a path from the root. *)

type 'a t = { valid_moves : 'a t CharMap.t; value : 'a }

(** Tree catamorphism *)
let rec cata f_node (node : 'a t) : 'r =
  let recurse = cata f_node in
  f_node node (node.valid_moves |> CharMap.map recurse)

let rec foldl f_node acc (node : 'a t) : 'r =
  let recurse = foldl f_node in
  let new_acc = f_node acc node in
  CharMap.to_list node.valid_moves
  |> List.map snd
  |> List.fold_left recurse new_acc

let next_node c node = CharMap.find_opt c node.valid_moves

let rec _find_node (word : char list) node =
  match word with
  | [] -> Some node
  | c :: cs -> (
      match next_node c node with None -> None | Some n -> _find_node cs n)

let find_node (word : string) node =
  _find_node (word |> String.to_seq |> List.of_seq) node
