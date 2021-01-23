open Ocello_ai__Othello

let map = default_gen_map ()

let%test _ =
  let fst = Fst in
  let scd = Snd in
  let g = get in
  g 3 3 map = fst &&
    g 4 4 map = fst &&
      g 3 4 map = scd &&
        g 4 3 map = scd

(* let%test _ =
 *   let user = Fst in
 *   let m = default_gen_map () in
 *   let moves = can_put_ptr user m in
 *   let (x,y) = List.hd moves in
 *   let set_map = set x y user m in
 *   let _ = print_pretty_map set_map in
 *   true *)

let%test _ =
  let m = default_gen_map () in
  let _ = set 0 0 Fst m in
  let _ = set 7 7 Snd m in
  (* let _ = print_pretty_map m in
   * let _ = print_pretty_map (default_gen_map ()) in *)
  Array.mapi
    (fun i line ->
      Array.mapi (fun j x -> x = (get i j map)) line)
    m |> Array.for_all (fun x -> Array.for_all (fun y -> y) x)
  |> not

let%test _ =
  let m = default_gen_map ()
          |> set_eval 2 3 Snd in
  let exp_m = [|
      [| Emp; Emp; Emp; Emp; Emp; Emp; Emp; Emp; |];
      [| Emp; Emp; Emp; Emp; Emp; Emp; Emp; Emp; |];
      [| Emp; Emp; Emp; Emp; Emp; Emp; Emp; Emp; |];
      [| Emp; Emp; Snd; Snd; Snd; Emp; Emp; Emp; |];
      [| Emp; Emp; Emp; Snd; Fst; Emp; Emp; Emp; |];
      [| Emp; Emp; Emp; Emp; Emp; Emp; Emp; Emp; |];
      [| Emp; Emp; Emp; Emp; Emp; Emp; Emp; Emp; |];
      [| Emp; Emp; Emp; Emp; Emp; Emp; Emp; Emp; |];
    |] in
  m = exp_m;;
