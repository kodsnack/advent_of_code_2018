#use "./ext.ml";;

module S = Set.Make(struct type t = char let compare = compare end)
module M = Map.Make(struct type t = char let compare = compare end)

let parse line =
  Scanf.sscanf
    line
    "Step %c must be finished before step %c can begin."
    (fun dep step -> dep,step)

let add_dep d s = M.update d (function None -> Some s | Some steps -> Some (S.union steps s))
let as_graph =
  List.fold_left (fun g (d,s) ->
    g |> (add_dep d (S.singleton s)) |> (add_dep s S.empty)
  ) M.empty

let dependent_steps graph steps =
  S.fold (fun step deps ->
    graph |> M.find step |> S.union deps
  ) steps S.empty

let part1 edges =
  let graph = as_graph edges in
  let dependent_steps = dependent_steps graph in

  let rec walk order steps =
    if steps |> S.is_empty then
      order |> List.rev
    else
      let available = S.diff steps (dependent_steps steps) in
      let next = available |> S.min_elt in
      steps |> S.remove next |> walk (next :: order)
  in
  M.fold (fun dep _ deps -> deps |> S.add dep) graph S.empty
  |> walk []
  |> String.from_chars

let part2 base_time number_of_workers edges =
  let duration step = base_time + Char.code step - Char.code 'A' + 1 in
  let graph = as_graph edges in
  let dependent_steps = dependent_steps graph in
  let by_time (_,a) (_,b) = a - b in

  let rec loop current_time active steps =
    let available =
      active
      |> List.map fst
      |> List.fold_left (fun steps s -> steps |> S.add s) (dependent_steps steps)
      |> S.diff steps
    in
    if not (S.is_empty available) && List.length active < number_of_workers then
      let next = available |> S.min_elt in
      let active' = (next,current_time + duration next)::active |> List.sort by_time in
      loop current_time active' steps
    else
      match active with
      | [] -> current_time
      | (completed,current_time')::active' ->
        loop current_time' active' (steps |> S.remove completed)
  in

  M.fold (fun dep _ deps -> deps |> S.add dep) graph S.empty
  |> loop 0 []

let () =
  let steps = [
    "Step C must be finished before step A can begin.";
    "Step C must be finished before step F can begin.";
    "Step A must be finished before step B can begin.";
    "Step A must be finished before step D can begin.";
    "Step B must be finished before step E can begin.";
    "Step D must be finished before step E can begin.";
    "Step F must be finished before step E can begin.";
  ] |> List.map parse in
  assert (steps |> part1 = "CABDFE");
  assert (steps |> part2 0 2 = 15)

let () = File.open_in "./day07.input" (fun ch ->
  let steps = Seq.of_lines ch |> Seq.map parse |> List.of_seq in
  let () = steps |> part1 |> Printf.printf "part1: %s\n%!" in
  let () = steps |> part2 60 5 |> Printf.printf "part2: %d\n%!" in
  ()
)
