(* https://www.kith.org/words/2022/03/19/word-lists-for-writing-computer-word-games/ *)
(* Take SCOWL (http://wordlist.aspell.net/scowl-readme/), *)
(* Concat the english-10, -20, -35, and possibly -40 *)
(* Filter away words containing apostrophes and accents *)
(* For ghost, remove words shorter than 4 letters. *)

let read_lines file = In_channel.with_open_bin file In_channel.input_lines

let word_files =
  [
    {|./scowl/english-words.10|};
    {|./scowl/english-words.20|};
    {|./scowl/english-words.35|};
    {|./scowl/english-words.40|};
  ]

let is_alpha ch =
  let c = int_of_char ch in
  (c >= 65 && c <= 90) || (c >= 97 && c <= 122)

let words =
  List.concat_map read_lines word_files
  |> List.filter (fun word ->
         let chars = String.to_seq word in
         chars |> Seq.for_all is_alpha && chars |> Seq.length >= 4)
  |> List.sort String.compare

let write_lines file lines =
  let out_lines oc = Out_channel.output_string oc (String.concat "\n" lines) in
  Out_channel.with_open_bin file out_lines

let () = write_lines "ghost_words.txt" words
