#use "./ext.ml";;

let example_state = "#..#.#..##......###...###"

let example_rules = [
  "...## => #";
  "..#.. => #";
  ".#... => #";
  ".#.#. => #";
  ".#.## => #";
  ".##.. => #";
  ".#### => #";
  "#.#.# => #";
  "#.### => #";
  "##.#. => #";
  "##.## => #";
  "###.. => #";
  "###.# => #";
  "####. => #";
]

module S = Set.Make(struct type t = int let compare a b = a - b end)

let parse_state line =
  line
  |> String.to_chars
  |> List.filter_mapi (fun i -> function '#' -> Some i | _ -> None)
  |> List.fold_left (fun plants pos -> S.add pos plants) S.empty

let parse_rule line =
  try
    Scanf.sscanf line "%[.#] => #" (fun pattern ->
      Some (fun plants pos ->
        Range.fold 0 4 (fun all_match i ->
          let has_plant = S.mem (pos + i - 2) plants in
          let should_have_plant = pattern.[i] = '#' in
          let is_match =
            should_have_plant && has_plant
            || (not (should_have_plant || has_plant))
          in all_match && is_match
        ) true
      )
    )
  with _ -> None

let parse_rules = List.filter_map parse_rule

(*
 * 432101234
 * ....#....
 *   ..#..
 *     #....
 *   ..#..
 * ....#
 *)
let evolve rules plants =
  let min = S.min_elt plants - 4 in
  let max = S.max_elt plants + 4 in
  Range.fold min max (fun plants' pos ->
    if List.exists (fun rule -> rule plants pos) rules then
      S.add pos plants'
    else
      plants'
  ) S.empty

let checksum plants = S.fold ( + ) plants 0

let part1 rules state = Range.fold 1 20 (fun plants _ -> evolve rules plants) state |> checksum

let part2 rules plants =
  let exception Done of int in
  try
    Range.fold 1 50_000_000_000 (fun (plants,diff) i ->
      let plants' = evolve rules plants in
      let cs = checksum plants in
      let cs' = checksum plants' in
      let diff' = cs' - cs in
      if diff' = diff then raise (Done (cs' + diff' * (50_000_000_000 - i)));
      plants', diff'
    ) (plants, checksum plants)
    |> ignore; assert false
  with Done cs -> cs

let () =
  let rules = example_rules |> parse_rules in
  let state = example_state |> parse_state in
  let () = assert (part1 rules state = 325) in
  ()

let () = File.open_in "./day12.input" (fun ch ->
  match Seq.of_lines ch |> List.of_seq with
  | state :: _ :: rules ->
    let state = state |> parse_state in
    let rules = rules |> parse_rules in
    let () = part1 rules state |> Printf.printf "part1: %d\n%!" in
    let () = part2 rules state |> Printf.printf "part2: %d\n%!" in
    ()
  | _ -> assert false
)
