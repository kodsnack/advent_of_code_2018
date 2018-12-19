#use "./ext.ml";;
#load "str.cma";;

type node = {
  meta : int list;
  children : node list;
}

let as_tree nums =
  let rec node = function
    | [] | [_] -> assert false
    | n::m::nums ->
      let children,nums' =
        ([],nums)
        |> Range.fold 1 n (fun (acc,nums) _ ->
          let child,nums = node nums in
          child::acc,nums
        )
      in
      let meta,nums'' = ([],nums') |> metadata m in
      { meta; children = List.rev children; },nums''
  and metadata m = function
    | acc,md::nums when m > 0 ->
      (md :: acc,nums) |> metadata (m - 1)
    | acc,nums -> acc |> List.rev,nums
  in nums |> node |> fst

let parse = Str.(split (regexp " ")) >> List.map int_of_string >> as_tree

let part1 tree =
  let rec walk acc { meta; children; } =
    children |> List.fold_left walk (meta |> List.fold_left ( + ) acc)
  in
  tree |> walk 0

let part2 tree =
  let rec walk = function
    | { meta; children = [] } ->
      meta |> List.fold_left ( + ) 0
    | { meta; children } ->
      let child i = try Some (List.nth children (i - 1)) with _ -> None in
      meta
      |> List.filter_map (child >> Option.map walk)
      |> List.fold_left ( + ) 0
  in
  tree |> walk

let () =
  let example = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2" in
  let tree = example |> parse in
  let () = assert (tree |> part1 = 138) in
  let () = assert (tree |> part2 = 66) in
  ()

let () = File.open_in "./day08.input" (fun ch ->
  let tree = Seq.of_lines ch |> List.of_seq |> List.hd |> parse in
  let () = tree |> part1 |> Printf.printf "part1: %d\n%!" in
  let () = tree |> part2 |> Printf.printf "part2: %d\n%!" in
  ()
)
