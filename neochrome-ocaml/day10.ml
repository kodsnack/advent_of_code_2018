#use "./ext.ml";;

type light = { x : int; y : int; vx : int; vy : int; }

let parse line =
  Scanf.sscanf
    line
    "position=< %d , %d> velocity=< %d, %d >"
    (fun x y vx vy -> { x; y; vx; vy; })

module M = Map.Make(struct type t = int*int let compare = compare end)
module S = Set.Make(struct type t = int*int let compare = compare end)

let integrate ({ x;y;vx;vy; } as light) = { light with x = x + vx; y = y + vy }

let bounds = function
  | [] -> assert false
  | {x;y} :: lights ->
    let rec search x1 y1 x2 y2 = function
      | [] -> x1,y1,x2,y2
      | {x;y} :: coords -> search (min x1 x) (min y1 y) (max x2 x) (max y2 y) coords
    in search x y x y lights

let as_lookup =
  List.fold_left (fun lookup { x; y; } -> S.add (x,y) lookup) S.empty

let render lights =
  let x1,y1,x2,y2 = bounds lights in
  let lookup = as_lookup lights in
  let () = Range.iter y1 y2 (fun y ->
    let () = Range.iter x1 x2 (fun x ->
      let () = (if S.mem (x,y) lookup then '#' else '.') |> print_char in
      ()
    ) in
    let () = print_newline () in
    ()
  ) in
  ()

let rec wait height time lights =
  let _,y1,_,y2 = bounds lights in
  if (y2 - y1) / height = 0 then lights,time
  else lights |> List.map integrate |> wait height (time + 1)

let () = File.open_in "./day10.example" (fun ch ->
  let lights = Seq.of_lines ch |> Seq.map parse |> List.of_seq in
  let () = lights |> wait 8 0 |> fun (lights,time) ->
    assert (time = 3);
    render lights
  in
  ()
)

let () = File.open_in "./day10.input" (fun ch ->
  let lights = Seq.of_lines ch |> Seq.map parse |> List.of_seq in
  let () = lights |> wait 10 0 |> fun (lights,time) ->
    print_endline "part1:";
    render lights;
    Printf.printf "part2: %d\n" time
  in
  ()
)
