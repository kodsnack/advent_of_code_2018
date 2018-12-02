#use "./ext.ml";;

module M = Map.Make(Char)

let inc m ch =
  match M.find_opt ch m with
  | None -> M.add ch 1 m
  | Some n -> M.add ch (n + 1) m

let count s =
  s
  |> String.to_chars
  |> List.fold_left inc M.empty

let with_freq m = M.exists (fun _ n -> n = m)

let part1 ids =
  let freqs = ids |> List.map count in
  let pairs = List.(freqs |> filter (with_freq 2) |> length) in
  let triplets = List.(freqs |> filter (with_freq 3) |> length) in
  pairs * triplets

let _test1 =
  let checksum = [
    "abcdef";
    "bababc";
    "abbcde";
    "abcccd";
    "aabcdd";
    "abcdee";
    "ababab";
  ] |> part1 in
  assert (checksum = 12)


let diff_by_one (s1,s2) =
  let rec check diff i =
    if i = -1 then true
    else if s1.[i] <> s2.[i] then
      if diff then false
      else check true (i - 1)
    else check diff (i - 1)
  in check false (String.length s1 - 1)

let common (s1,s2) =
  let rec pick acc = function
    | [], ch2 -> acc @ ch2
    | ch1, [] -> acc @ ch1
    | c1 :: ch1, c2 :: ch2 ->
      pick (if c1 = c2 then (c1 :: acc) else acc) (ch1,ch2)
  in String.(
    pick [] (to_chars s1, to_chars s2)
    |> List.rev
    |> from_chars
  )


let part2 ids =
  ids
  |> List.unique_pairs
  |> List.find diff_by_one
  |> common


let _test2 =
  assert (diff_by_one ("fghij","fguij"));
  assert (common ("fghij","fguij") = "fgij");
  let chars = [
    "abcde";
    "fghij";
    "klmno";
    "pqrst";
    "fguij";
    "axcye";
    "wvxyz";
  ] |> part2 in
  assert (chars = "fgij")

let () = File.open_in "./day02.input" (fun ch ->
  let ids = Seq.of_lines ch |> List.of_seq in

  let () = Printf.printf "part1: %d\n%!" @@ part1 ids in
  let () = Printf.printf "part2: %s\n%!" @@ part2 ids in

  ()
)
