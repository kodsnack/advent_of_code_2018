#use "./ext.ml";;

let () =
  File.open_in "./day01.input" (fun ch ->
    let changes = Seq.of_lines ch |> Seq.map int_of_string |> List.of_seq in

    let _part1 =
      changes
      |> List.fold_left ((+)) 0
      |> Printf.printf "part1: %d\n%!"
    in

    let module S = Set.Make(struct
      type t = int
      let compare = compare
    end) in

    let rec part2 seen current changes' =
      if S.mem current seen then
        Printf.printf "part2: %d\n%!" current
      else
        match changes' with
        | [] -> part2 seen current changes
        | c :: changes' ->
          let current' = current + c in
          part2 (S.add current seen) current' changes'
    in
    let () = part2 S.empty 0 changes in
    ()
  )
