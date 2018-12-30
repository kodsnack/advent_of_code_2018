#use "./ext.ml";;

module M = Map.Make(struct type t = int*int let compare = compare end)

(* https://en.wikipedia.org/wiki/Summed-area_table *)
(* 1 1 1    1 2 3
 * 1 1 1 -> 2 4 6
 * 1 1 1    3 6 9
 *)
let sum_areas power =
  Range.fold 1 300 (fun areas y ->
    Range.fold 1 300 (fun areas x ->
      let sum =
        power x y
        + M.find_default (x,y - 1) 0 areas
        + M.find_default (x - 1,y) 0 areas
        - M.find_default (x - 1,y - 1) 0 areas
      in
      areas |> M.add (x,y) sum
    ) areas
  ) M.empty

let power_square areas size x y =
  let a = M.find_default (x-1,y-1) 0 areas in
  let b = M.find_default (x+size-1,y-1) 0 areas in
  let c = M.find_default (x-1,y+size-1) 0 areas in
  let d = M.find_default (x+size-1,y+size-1) 0 areas in
  d - b - c + a

let power serial x y =
  let rack_id = x + 10 in
  let p = (rack_id * y + serial) * rack_id in
  let h = p / 100 mod 10 in
  h - 5

let part1 power_square =
  Range.fold 1 298 (fun mp y ->
    Range.fold 1 298 (fun ((p,_) as mp) x ->
      let p' = power_square 3 x y in
      if p' > p then (p',(x,y)) else mp
    ) mp
  ) (0,(1,1))

let part2 power_square =
  Range.fold 1 300 (fun mp size ->
    Range.fold 1 (300 - size + 1) (fun ((p,_,_) as mp) y ->
      Range.fold 1 (300 - size + 1) (fun ((p,_,_) as mp) x ->
        let p' = power_square size x y in
        if p' > p then (p',size,(x,y)) else mp
      ) mp
    ) mp
  ) (0,1,(1,1))

let () =
  let () = assert (power 8 3 5 = 4) in
  let () = assert (power 57 122 79 = -5) in
  let () = assert (power 39 217 196 = 0) in
  let () = assert (power 71 101 153 = 4) in

  let area18 = sum_areas (power 18) in
  let area42 = sum_areas (power 42) in

  let () = assert (power_square area18 3 33 45 = 29) in
  let () = assert (power_square area42 3 21 61 = 30) in
  let () = assert (power_square area18 16 90 269 = 113) in
  let () = assert (power_square area42 12 232 251 = 119) in

  let () = assert (part1 (power_square area18) = (29,(33,45))) in
  let () = assert (part1 (power_square area42) = (30,(21,61))) in

  (* let () = assert (part2 (power_square area18) = (113,16,(90,269))) in *)
  ()

let () =
  let input = 8868 in
  let areas = sum_areas (power input) in
  let () = part1 (power_square areas) |> fun (p,(x,y)) -> Printf.printf "part1: %d,%d -> %d\n%!" x y p in
  let () = part2 (power_square areas) |> fun (p,s,(x,y)) -> Printf.printf "part2: %d,%d,%d -> %d\n%!" x y s p in
  ()
