#use "./ext.ml";;

type rect = { id : int; x1 : int; y1 : int; x2 : int; y2 : int; }

let parse line = Scanf.sscanf line "#%d @ %d,%d: %dx%d" (fun id x y w h -> {
  id; x1 = x; y1 = y; x2 = x + w - 1; y2 = y + h - 1;
})

module M = Map.Make(struct
  type t = int * int
  let compare = compare
end)

let inc loc = M.update loc (function None -> Some 1 | Some n -> Some (n+1))

let claim fabric { x1;y1;x2;y2 } =
  let rec row fabric y =
    if y > y2 then fabric
    else col fabric y x1
  and col fabric y x =
    if x > x2 then row fabric (y+1)
    else col (inc (x,y) fabric) y (x+1)
  in row fabric y1

let overlapping fabric =
  M.fold (fun _ n o -> if n > 1 then o + 1 else o) fabric 0

let part1 claims =
  claims |> List.fold_left claim M.empty |> overlapping

let intersects a b =
  (a.x1 >= b.x1 && a.x1 <= b.x2 || b.x1 >= a.x1 && b.x1 <= a.x2)
  && (a.y1 >= b.y1 && a.y1 <= b.y2 || b.y1 >= a.y1 && b.y1 <= a.y2)

let part2 claims =
  let rec find prev = function
    | [] -> None
    | claim :: next ->
      if List.exists (intersects claim) @@ prev @ next then
        find (claim :: prev) next
      else Some claim
  in claims |> find [] |> function
    | None -> assert false
    | Some { id } -> id

let _tests =
  let claims = [
      "#1 @ 1,3: 4x4";
      "#2 @ 3,1: 4x4";
      "#3 @ 5,5: 2x2";
    ]
    |> List.map parse
  in
  assert (part1 claims = 4);
  assert (part2 claims = 3)


let () = File.open_in "./day03.input" (fun ch ->
  let claims = Seq.of_lines ch
    |> Seq.map parse
    |> List.of_seq
  in

  let () = claims |> part1 |> Printf.printf "part1: %d\n%!" in
  let () = claims |> part2 |> Printf.printf "part2: %d\n%!" in
  ()
)
