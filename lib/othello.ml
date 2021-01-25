let width = 8;;
let height = 8;;

(* 参考: https://aidiary.hatenablog.com/entry/20050121/1274149979 *)
let eval_array = [|
    [| 120; -20; 20;  5;  5; 20; -20; 120; |];
    [| -20; -40; -5; -5; -5; -5; -40; -20; |];
    [|  20;  -5; 15;  3;  3; 15;  -5;  20; |];
    [|   5;  -5;  3;  3;  3;  3;  -5;   5; |];
    [|   5;  -5;  3;  3;  3;  3;  -5;   5; |];
    [|  20;  -5; 15;  3;  3; 15;  -5;  20; |];
    [| -20; -40; -5; -5; -5; -5; -40; -20; |];
    [| 120; -20; 20;  5;  5; 20; -20; 120; |];
  |];;

let dim_deep_copy dary =
  Array.map (fun x -> Array.map (fun y -> y) x) dary

let shuffle d =
  let nd = List.map (fun c ->
               (Random.bits (Random.self_init ()), c)) d in
  let sond = List.sort compare nd in
  List.map snd sond;;

type panel = Fst | Snd | Emp | Out;;

let panel_to_string v =
  match v with
  | Fst -> "Fst"
  | Snd -> "Snd"
  | Emp -> "Emp"
  | Out -> "Out";;

let print_dim m f =
  Array.iter (fun x ->
      print_endline
        (Array.fold_left
           (fun ac y ->
             ac ^ (f y))
           ""
           x)
    )
    m;;

let print_map m =
  let ts v = panel_to_string v in
  print_dim m ts;;

let print_pretty_map m =
  let ts v = match v with
    | Fst -> "X"
    | Snd -> "O"
    | Emp -> " "
    | _ -> "?"
  in
  print_dim m ts;;

let dim_get x y map df =
  if x >= 0 && x < width && y >= 0 && y < height
  then Array.get (Array.get map y) x
  else df;;

let get x y map =
  dim_get x y map Out;;

let set x y p map =
  if x >= 0 && x < width && y >= 0 && y < height
  then
    let line = Array.get map y in
    let _ = Array.set line x p in
    Array.set map y line;
    map
  else
    map;;

let gen_map x y v =
  Array.make_matrix x y v;;

let default_gen_map () =
  let map = gen_map width height Emp in
  set 3 3 Fst map
  |> set 4 4 Fst
  |> set 3 4 Snd
  |> set 4 3 Snd;;

let enemy_panel user =
  if user == Fst
  then Snd
  else
    if user == Snd
    then Fst
    else Emp;;

let around_area x y map =
  let indexes = [| -1; 0; 1; |] in
  Array.map
    (fun i ->
      Array.map
        (fun j -> get (x+i) (y+j) map)
        indexes)
    indexes;;

let rec search x y v_x v_y user map =
  let en = enemy_panel user in
  let cur = get (x+v_x) (y+v_y) map in
  if cur == en
  then search (x+v_x) (y+v_y) v_x v_y user map
  else
    cur == user;;

let around_search x y user map =
  let indexes = [| -1; 0; 1; |] in
  let first_panel_enemy a b = get (x+a) (y+b) map == enemy_panel user in
  Array.exists
    (fun x -> x)
    (Array.concat
       (Array.to_list
          (Array.map
             (fun i ->
               Array.map
                 (fun j -> (not (i = 0 && j = 0)) &&
                             first_panel_enemy i j &&
                               search x y i j user map)
                 indexes)
             indexes)));;

let rec search_turn_over_line x y v_x v_y user map =
  let en = enemy_panel user in
  let cur = get x y map in
  if cur == en
  then Some(x,y) :: search_turn_over_line (x+v_x) (y+v_y) v_x v_y user map
  else
    if cur == user
    then []
    else [None];;

let turn_over_line x y v_x v_y user map =
  let rst = search_turn_over_line (x+v_x) (y+v_y) v_x v_y user map in
  let m = ref map in
  if List.length rst <= 0
  then !m
  else
    match List.hd (List.rev rst) with
    | Some(_) -> List.iter
                   (fun v -> match v with
                             | Some(x,y) -> m := (set x y user !m)
                             | _ -> ())
                   rst;
                 !m
    | _ -> !m;;

let around_turn_over_line x y user map =
  let indexes = [| -1; 0; 1; |] in
  let m = ref map in
  let _ = Array.iter
            (fun i ->
              Array.iter
                (fun j -> if x == 0 && y == 0
                          then ()
                          else m := turn_over_line x y i j user !m)
                indexes)
            indexes in
  !m;;

let can_put user map f =
  let en = enemy_panel user in
  let exists area =
    Array.exists
      (fun z -> enemy_panel z <> Emp && z == en)
      (Array.concat (Array.to_list area)) in
  Array.mapi
    (fun i line ->
      Array.mapi
        (fun j _ ->
          f i j ((get i j map) = Emp &&
                   exists (around_area i j map) &&
                     around_search i j user map))
        line)
    map;;

let can_put_ptr user map =
  let f i j b = if b then (i, j) else (-1, -1) in
  can_put user map f
  |> Array.to_list
  |> Array.concat
  |> Array.to_list
  |> List.filter (fun (i, _) -> i > -1);;

let is_finish map =
  Array.for_all (fun line -> Array.for_all (fun x -> x <> Emp) line) map;;

let count_stone_in_map stone map =
  Array.fold_left
    (fun asm line ->
      asm + (Array.fold_left
               (fun a x -> a + (if x = stone then 1 else 0))
               0
               line)
    )
    0
    map;;

let score_closure map =
  let total_fst = count_stone_in_map Fst map in
  let total_snd = count_stone_in_map Snd map in
  let total_f winner f = f total_fst total_snd winner in
  if total_fst > total_snd
  then total_f Fst
  else
    if total_fst < total_snd
    then total_f Snd
    else total_f Emp;;

let set_eval x y user map =
  let set_map = set x y user map in
  around_turn_over_line x y user set_map;;

let board_eval user map =
  Array.mapi
    (fun i x ->
      (Array.mapi
         (fun j y -> if y = user
                     then eval_array.(i).(j)
                     else 0)
         x |> Array.fold_left (fun asm z -> asm + z) 0))
    map
  |> Array.fold_left (fun asm z -> asm + z) 0

let choice_random user map =
  let moves = can_put_ptr user map in
  if List.length moves > 0
  then Some(shuffle moves |> List.hd)
  else None

(* 二手先しか読まないやつ。 *)
(* n手先読めるような関数を新たに作るか、これを修正したい。 *)
let choice_min_max_move user map =
  let moves = can_put_ptr user map in
  if List.length moves <= 0
  then None
  else
    let en = enemy_panel user in
    let (pair, _) = List.map
                      (fun (x,y) ->
                        ((x,y), List.fold_left
                                  (* ここらへんおかしい *)
                                  (fun acm (i,j) ->
                                    acm + board_eval en (set_eval i j user map))
                                  0
                                  (can_put_ptr en (set_eval x y user map)))
                      )
                      moves
                    |> List.sort (fun (_, ascore) (_, bscore) -> Int.compare ascore bscore)
                    |> List.hd in
    Some(pair);;

let rec emp c =
  if c > 0
  then " " ^ emp (c-1)
  else "";;

let choice_min_max_move_cnt cnt user map =
  let moves = can_put_ptr user map in
  if List.length moves <= 0
  then None
  else
    let rec f u m c =
      let mvs = can_put_ptr u m in
      if c > 0 && List.length mvs > 0
      then
        let choice l = List.hd (if user = u then List.rev l else l) in
        let en = enemy_panel u in
        let pairs = List.map
                      (fun (x,y) ->
                        let sum = f en (set_eval x y u m) (c-1) in
                        sum
                      )
                      mvs in
        List.sort (fun a b -> Int.compare a b) pairs |> choice
      else
        board_eval u m
    in
    let first_f u m c =
      let mvs = can_put_ptr u m in
      let en = enemy_panel u in
      let rst = List.map
                        (fun (x,y) ->
                          ((x,y), f en (set_eval x y u m) (c-1))
                        )
                        mvs
                      |> List.sort
                           (fun (_, ascore) (_, bscore) ->
                             Int.compare ascore bscore) in
      let (pair, _) = List.hd rst in
      (* let (x,y) = pair in *)
      (* let _ = Printf.printf "[choose] %d, %d => %d\n" x y score in *)
      Some(pair) in
    first_f user map cnt;;

let cpu_choice user map choice_f =
  match choice_f user (dim_deep_copy map) with
  | Some(x,y) ->
     set_eval x y user map
  | None -> map;;

let rec cpu_battle_main map user_choice_f enemy_choice_f turn =
  (* let _ = print_endline ((panel_to_string turn) ^ ">") in
   * let _ = print_pretty_map map in *)
  if is_finish map
  then
    let _ = score_closure map (fun fst snd wu ->
                Printf.printf "winner:%s fst_cnt=%d, snd_cnt=%d\n"
                  (panel_to_string wu) fst snd) in
    print_endline "finish."
  else
    let moves = can_put_ptr turn map in
    let enemy = enemy_panel turn in
    if (List.length moves) + (List.length (can_put_ptr enemy map)) <= 0
    then print_endline "no moves"
    else
      let choice_f = if turn = Fst
                     then user_choice_f
                     else enemy_choice_f in
      let m = cpu_choice turn map choice_f in
      cpu_battle_main m user_choice_f enemy_choice_f enemy;;
