let map = Ocello_ai__Middle.default_gen_map

let print_dim m f =
  Array.iter (fun x ->
      print_endline
        (Array.fold_left
           (fun ac y ->
             ac ^ (f y))
           ""
           x)
    )
    m

let print_map m =
  let ts v = Ocello_ai__Middle.panel_to_string v in
  print_dim m ts

let%test _ =
  let fst = Ocello_ai__Middle.Fst in
  let scd = Ocello_ai__Middle.Snd in
  let g = Ocello_ai__Middle.get in
  g 3 3 map = fst &&
    g 4 4 map = fst &&
      g 3 4 map = scd &&
        g 4 3 map = scd

let%test _ =
  let cp = Ocello_ai__Middle.can_put in
  let g x y map = Ocello_ai__Middle.dim_get x y map false in
  let cpm = cp map Ocello_ai__Middle.Fst in
  g 2 4 cpm &&
    g 3 5 cpm &&
      g 4 2 cpm &&
        g 5 3 cpm
