#use "./ext.ml";;

type coord = { id : int; x : int; y : int; }
let coord_of_string id line = Scanf.sscanf line "%d, %d" (fun x y -> { id; x; y })
let distance x1 y1 x2 y2 = abs (x1 - x2) + abs (y1 - y2)

module M = Map.Make(struct type t = int let compare = compare end)
module S = Set.Make(struct type t = int let compare = compare end)

let closest_to x y coords =
  List.fold_left (fun found c ->
    let d' = distance x y c.x c.y in
    match found with
    | None -> Some (d', Some c.id)
    | Some (d,_) when d' < d -> Some (d', Some c.id)
    | Some (d,_) when d' = d -> Some (d, None)
    | _ -> found
  ) None coords
  |> function
    | None | Some (_, None) -> None
    | Some (_, Some id) -> Some id

let () =
  let coords = [
    {id=0;x=0;y=0};
    {id=1;x=2;y=2}
  ] in
  [
    0,0,Some 0; 1,0,Some 0; 2,0,None;
    0,1,Some 0; 1,1,None; 2,1,Some 1;
    0,2,None; 1,2,Some 1; 2,2,Some 1;
  ] |> List.iter (fun (x,y,id) -> assert (coords |> closest_to x y = id))

let bounds = function
  | [] -> assert false
  | {x;y} :: coords ->
    let rec search x1 y1 x2 y2 = function
      | [] -> x1,y1,x2,y2
      | {x;y} :: coords -> search (min x1 x) (min y1 y) (max x2 x) (max y2 y) coords
    in search x y x y coords

let part1 coords =
  let x1,y1,x2,y2 = bounds coords in
  let on_border x y = x = x1 || x = x2 || y = y1 || y = y2 in
  let areas,borders =
    Range.fold y1 y2 (fun (areas,borders) y ->
      Range.fold x1 x2 (fun (areas,borders) x ->
        match coords |> closest_to x y with
        | None -> (areas,borders)
        | Some id ->
          let areas' = areas |> M.update id (function
            | None -> Some 1
            | Some a -> Some (a + 1)
          ) in
          let borders' = if on_border x y then borders |> S.add id else borders in
          (areas',borders')
      ) (areas,borders)
    ) (M.empty,S.empty)
  in
  areas
  |> M.filter (fun id _ -> not (borders |> S.mem id))
  |> M.maxf (fun _ a -> a)
  |> snd

let part2 within coords =
  let x1,y1,x2,y2 = bounds coords in
  Range.fold y1 y2 (fun area y ->
    Range.fold x1 x2 (fun area x ->
      let d =
        coords
        |> List.map (fun { x=tx; y=ty } -> distance x y tx ty)
        |> List.fold_left ( + ) 0
      in
      if d < within then area + 1 else area
    ) area
  ) 0

let _test =
  let coords = [
      "1, 1";
      "1, 6";
      "8, 3";
      "3, 4";
      "5, 5";
      "8, 9";
    ] |> List.mapi coord_of_string
  in
  assert (coords |> part1 = 17);
  assert (coords |> part2 32 = 16)

let () = File.open_in "./day06.input" (fun ch ->
  let coords = Seq.of_lines ch |> List.of_seq |> List.mapi coord_of_string in
  let () = coords |> part1 |> Printf.printf "part1: %d\n%!" in
  let () = coords |> part2 10_000 |> Printf.printf "part2: %d\n%!" in
  ()
)
