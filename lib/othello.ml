let width = 8;;
let height = 8;;

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

let default_gen_map =
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
        (fun j x ->
          f i j (x == Emp &&
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

let min_max_move user map =
  let moves = can_put_ptr user map in
  moves;;

let shuffle d =
  let nd = List.map (fun c -> (Random.bits (), c)) d in
  let sond = List.sort compare nd in
  List.map snd sond;;

let rec cpu_battle map user =
  let ps = panel_to_string user in
  let _ = print_endline (ps ^ ">") in
  let moves = can_put_ptr user map in
  let _ = List.iter (fun (x,y) -> Printf.printf "%d-%d\n" x y) moves in
  let en = enemy_panel user in
  if is_finish map
  then print_endline "finish."
  else
    if (List.length moves) + (List.length (can_put_ptr en map)) <= 0
    then print_endline "no moves"
    else
      (* let _ = Unix.sleep 3 in *)
      if List.length moves <= 0
      then cpu_battle map en
      else
        let move = shuffle moves |> List.hd in
        let (x,y) = move in
        let set_map = set x y user map in
        let umap = around_turn_over_line x y user set_map in
        let _ = Printf.printf "%s => %d-%d\n" ps x y in
        let _ = print_pretty_map umap in
        cpu_battle umap en;;

let cpu_battle_start =
  let map = default_gen_map in
  let _ = print_pretty_map map in
  cpu_battle map Fst;;
