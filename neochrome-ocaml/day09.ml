#use "./ext.ml";;

module Ring = struct

  type t = { marble : int; mutable head : bool; mutable cw : t; mutable ccw : t }

  let init marble =
    let rec e = { marble; head = true; cw = e; ccw = e } in e

  let insert marble head =
    let e = { marble; head = true; cw = head.cw; ccw = head } in
    head.head <- false;
    head.cw.ccw <- e;
    head.cw <- e;
    e

  let remove head =
    head.ccw.cw <- head.cw;
    head.cw.ccw <- head.ccw;
    head.cw.head <- true;
    head.cw,head.marble

  let head head = head.marble

  let iter_cw f start =
    let rec step = function
      | { head = true; _ } -> ()
      | { marble; cw; _ } -> f marble; step cw
    in f start.marble; step start.cw

  let rec rot_cw n head =
    if n <= 0 then head
    else begin
      head.head <- false;
      head.cw.head <- true;
      rot_cw (n - 1) head.cw;
    end

  let rec rot_ccw n head =
    if n <= 0 then head
    else begin
      head.head <- false;
      head.ccw.head <- true;
      rot_ccw (n - 1) head.ccw;
    end

end

module M = Map.Make(struct type t = int let compare = compare end)

let play players marbles =
  let rec place player marble scores ring =
    if marble > marbles then scores
    else if marble mod 23 = 0 then
      let ring',removed =
        ring
        |> Ring.rot_ccw 7
        |> Ring.remove
      in
      let score = marble + removed in
      let scores' =
        scores
        |> M.update
          (player + 1)
          (function None -> Some score | Some s -> Some (s + score))
      in
      place ((player + 1) mod players) (marble + 1) scores' ring'
    else
      ring
      |> Ring.rot_cw 1
      |> Ring.insert marble
      |> place ((player + 1) mod players) (marble + 1) scores
  in
  place 0 1 M.empty (Ring.init 0)

let part1 players marbles =
  play players marbles
  |> M.maxf (fun _ score -> score)
  |> snd

let part2 players marbles = part1 players (marbles * 100)

let () =
  let () = assert (part1 9 25 = 32) in
  let () = assert (part1 10 1618 = 8317) in
  let () = assert (part1 13 7999 = 146373) in
  let () = assert (part1 17 1104 = 2764) in
  let () = assert (part1 21 6111 = 54718) in
  let () = assert (part1 30 5807 = 37305) in
  ()

let () =
  let players,marbles = 411,72059 in
  let () = part1 players marbles |> Printf.printf "part1: %d\n%!" in
  let () = part2 players marbles |> Printf.printf "part2: %d\n%!" in
  ()
