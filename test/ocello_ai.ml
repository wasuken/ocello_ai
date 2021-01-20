open Ocello_ai__Othello

let map = default_gen_map

let%test _ =
  let fst = Fst in
  let scd = Snd in
  let g = get in
  g 3 3 map = fst &&
    g 4 4 map = fst &&
      g 3 4 map = scd &&
        g 4 3 map = scd

(* let%test _ =
 *   let cp = can_put in
 *   let g x y map = dim_get x y map false in
 *   let cpm = cp map Fst in
 *   g 2 4 cpm &&
 *     g 3 5 cpm &&
 *       g 4 2 cpm &&
 *         g 5 3 cpm *)

let%test _ =
  let user = Fst in
  let m = default_gen_map in
  let moves = can_put_ptr user m in
  let (x,y) = List.hd moves in
  let set_map = set x y user m in
  let _ = print_pretty_map set_map in
  true
