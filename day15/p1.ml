open Base

let respond nums history until =
  let i = ref (List.length nums + 1) in
  let current = ref 0 in
  let get = Hashtbl.find history in
  let set key data = Hashtbl.set history ~key ~data in
  while !i < until do
    (current :=
       match get !current with
       | None ->
           set !current !i;
           0
       | Some j ->
           set !current !i;
           !i - j);
    i := !i + 1
  done;
  !current

let solve () =
  let open CCList in
  let input = [ 0; 3; 1; 6; 7; 5 ] in
  let history = Hashtbl.Poly.create ~growth_allowed:true ~size:1_000 () in
  iteri (fun i key -> Hashtbl.set history ~key ~data:(i + 1)) input;
  respond input history 30000000
