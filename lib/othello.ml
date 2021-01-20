let width = 8
let height = 8

type panel = Fst | Snd | Emp | Out

let panel_to_string v =
  match v with
  | Fst -> "Fst"
  | Snd -> "Snd"
  | Emp -> "Emp"
  | Out -> "Out"

type player = First | Second

let dim_get x y map df =
  if x >= 0 && x < width && y >= 0 && y < height
  then Array.get (Array.get map y) x
  else df

let get x y map =
  dim_get x y map Out

let set x y p map =
  if x >= 0 && x < width && y >= 0 && y < height
  then
    let line = Array.get map y in
    let _ = Array.set line x p in
    Array.set map y line;
    map
  else
    map

let gen_map x y v =
  Array.make_matrix x y v

let default_gen_map =
  let map = gen_map width height Emp in
  set 3 3 Fst map
  |> set 4 4 Fst
  |> set 3 4 Snd
  |> set 4 3 Snd

let enemy_panel user =
  if user == Fst
  then Snd
  else
    if user == Snd
    then Fst
    else Emp

let around_area x y map =
  let indexes = [| -1; 0; 1; |] in
  Array.map
    (fun i ->
      Array.map
        (fun j -> get (x+i) (y+j) map)
        indexes)
    indexes

let rec search x y v_x v_y user map =
  let en = enemy_panel user in
  let cur = get (x+v_x) (y+v_y) map in
  if cur == en
  then search (x+v_x) (y+v_y) v_x v_y user map
  else
    cur == user

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
               indexes)))

let rec search_turn_over_line x y v_x v_y user map =
  let en = enemy_panel user in
  let cur = get (x+v_x) (y+v_y) map in
  if cur == en
  then Some(x,y) :: search_turn_over_line (x+v_x) (y+v_y) v_x v_y user map
  else
    if cur == Emp
    then []
    else [None]

let turn_over_line x y v_x v_y user map =
  let rst = search_turn_over_line x y v_x v_y user map in
  let m = ref map in
  match List.hd (List.rev rst) with
  | Some(_) -> List.iter
                 (fun v -> match v with
                           | Some(x,y) -> m := (set x y user !m)
                           | _ -> ())
                 (rst |> List.rev |> List.tl |> List.rev);
               !m
  | _ -> !m

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
  !m

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
               exists (around_area j i map) &&
                 around_search j i user map))
        line)
    map

let can_put_ptr user map =
  let f i j b = if b then (i, j) else (-1, -1) in
  can_put user map f
  |> Array.to_list
  |> Array.concat
  |> Array.to_list
  |> List.filter (fun (i, _) -> i > -1)

let is_finish map =
  Array.for_all (fun line -> Array.for_all (fun x -> x <> Emp) line) map

let count_stone_in_map stone map =
  Array.fold_left
    (fun asm line ->
      asm + (Array.fold_left
               (fun a x -> a + (if x = stone then 1 else 0))
               0
               line)
    )
    0
    map

let score_closure map =
  let total_fst = count_stone_in_map Fst map in
  let total_snd = count_stone_in_map Snd map in
  let total_f winner f = f total_fst total_snd winner in
  if total_fst > total_snd
  then total_f Fst
  else
    if total_fst < total_snd
    then total_f Snd
    else total_f Emp

let min_max_move user map =
  let moves = can_put_ptr user map in
  moves
