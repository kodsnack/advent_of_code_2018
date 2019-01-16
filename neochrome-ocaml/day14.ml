#use "./ext.ml";;

let as_digits =
  let rec build acc n =
    if n < 10 then n :: acc
    else build (n mod 10 :: acc) (n / 10)
  in build [] >> Array.of_list

let improve input =
  let scores = Array.make 50_000_000 0 in
  let e1 = ref 0 and e2 = ref 1 in
  scores.(!e1) <- 3;
  scores.(!e2) <- 7;
  let next = ref 2 in
  let rounds = ref 0 in
  let part1 = ref 0 in
  let part2 = ref 0 in
  let part2_target = input |> as_digits in
  let part2_length = Array.length part2_target in

  let part2_check s =
    let rec check s t =
      if t = 0 then Some s
      else if scores.(s - 1) <> part2_target.(t - 1) then None
      else check (s - 1) (t - 1)
    in check s part2_length
  in

  let part1_done = ref false in
  let part2_done = ref false in
  while not (!part1_done && !part2_done) do
    incr rounds;
    (* "improve" recipes *)
    let s1,s2 = scores.(!e1),scores.(!e2) in
    let score = s1 + s2 in
    if score < 10 then begin
      scores.(!next) <- score;
      incr next;
    end else begin
      scores.(!next) <- 1;
      scores.(!next + 1) <- score mod 10;
      next := !next + 2;
    end;
    e1 := (1 + !e1 + s1) mod !next;
    e2 := (1 + !e2 + s2) mod !next;
    (* check for part1 *)
    if not !part1_done && !rounds = input + 10 - 1 then begin
      let final_scores = Array.sub scores input 10 in
      let pot = ref 1 in
      for i = 10 downto 1 do
        part1 := !part1 + final_scores.(i - 1) * !pot;
        pot := !pot * 10;
      done;
      part1_done := true;
    end;
    (* check for part2 *)
    if not !part2_done && !next > part2_length then begin
      match part2_check (!next - 1) with
      | None ->
        begin match part2_check (!next - 2) with
        | None -> ()
        | Some at -> part2 := at; part2_done := true
        end
      | Some at -> part2 := at; part2_done := true
    end;
  done;
  !part1,!part2

let () =
  let () = assert (let pt1,_ = improve 9 in pt1 = 5158916779) in
  let () = assert (let _,pt2 = improve 51589 in pt2 = 9) in
  let () = assert (let _,pt2 = improve 92510 in pt2 = 18) in
  let () = assert (let _,pt2 = improve 59414 in pt2 = 2018) in
  let input = 681901 in
  let () = improve input |> fun (part1,part2) ->
    Printf.printf "part1: %d\npart2: %d\n%!" part1 part2
  in
  ()
