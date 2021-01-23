open Ocello_ai__Othello

let main =
  cpu_battle_main (default_gen_map ()) choice_min_max_move choice_random Fst;;
