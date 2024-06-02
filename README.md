# ghostcaml

The word game [Ghost](https://en.wikipedia.org/wiki/Ghost_(game)) implemented as an OCaml command line program.

`dune build` to build, `dune exec ghost` to run. The word list used has already been built as `ghost_words.txt` (generated from the [SCOWL](http://wordlist.aspell.net/) word lists by the script `make_word_list.ml`).

## Rules

Players take turns saying letters, building up a string. The player who completes a word loses. In this version, only words of four or more letters count (so the game will last at least four turns).

If a player believes the current string cannot be continued to form any word, they may "challenge" the other player. If there is indeed no valid continuation, the challenge is successful and they win, otherwise they lose.

## Opponent

The computer opponent is perfect, but you may choose to go first or second. Good luck!
