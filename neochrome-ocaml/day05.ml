#use "./ext.ml";;
#load "str.cma";;

let same a b = Char.(
  abs (code a - code b) = code 'a' - code 'A'
)

let () =
  assert (same 'a' 'A');
  assert (same 'S' 's');
  assert (not (same 'r' 'S'));
  assert (not (same 't' 't'))

let compress s =
  let rec loop chars =
    let l = List.length chars in
    let chars' = eliminate [] chars in
    if List.length chars' < l then
      loop chars'
    else
      chars'
  and eliminate acc chars =
    match chars with
    | [] -> acc |> List.rev
    | [x] -> x :: acc |> List.rev
    | a :: b :: chars' when same a b -> eliminate acc chars'
    | a :: b :: chars' -> eliminate (a :: acc) (b :: chars')
  in
  String.(s |> to_chars |> loop |> from_chars)

let part1 input = input |> compress |> String.length

let part2 input =
  let alphabet = Char.(List.init (code 'z' - code 'a') (fun i -> code 'a' + i |> chr |> escaped)) in
  let prep c = Str.(global_replace (regexp_case_fold c) "") in
  alphabet
  |> List.map (fun c -> prep c input |> compress |> String.length)
  |> List.min

let () =
  assert ("dabAcCaCBAcCcaDA" |> compress = "dabCBAcaDA");
  assert ("dabAcCaCBAcCcaDA" |> part1 = 10);
  assert ("dabAcCaCBAcCcaDA" |> part2 = 4)

let () = File.open_in "./day05.input" (fun ch ->
  let input = Seq.of_lines ch |> List.of_seq |> List.hd in
  let () = input |> part1 |> Printf.printf "part1: %d\n%!" in
  let () = input |> part2 |> Printf.printf "part2: %d\n%!" in
  ()
)
