#use "./ext.ml";;

type direction = Up | Down | Left | Right
type cart = { x : int; y : int; dir : direction; intersections : int }
let initial_cart = { x = 0; y = 0; dir = Up; intersections = 0 }

let straight ({ x; y;} as cart) =
  match cart.dir with
  | Up -> { cart with y = y - 1 }
  | Left -> { cart with x = x - 1 }
  | Down -> { cart with y = y + 1 }
  | Right -> { cart with x = x + 1 }
let turn_left cart = { cart with dir = match cart.dir with Up -> Left | Left -> Down | Down -> Right | Right -> Up }
let turn_right cart = { cart with dir = match cart.dir with Up -> Right | Right -> Down | Down -> Left | Left -> Up }

let pass_intersection cart = { cart with intersections = cart.intersections + 1 }

let move tracks ({ x; y; dir; intersections; } as cart) =
  try
    match tracks.(y).[x] with
    | '-' | '|' | '<' | '>' | 'v' | '^' -> cart |> straight
    | '/' ->
      begin match dir with
      | Up | Down -> cart |> turn_right |> straight
      | Left | Right -> cart |> turn_left |> straight
      end
    | '\\' ->
      begin match dir with
      | Up | Down -> cart |> turn_left |> straight
      | Left | Right -> cart |> turn_right |> straight
      end
    | '+' ->
      begin match intersections mod 3 with
      | 0 -> cart |> turn_left |> straight
      | 1 -> cart |> straight
      | 2 -> cart |> turn_right |> straight
      | _ -> failwith "intersection madness"
      end |> pass_intersection
    | _ -> failwith "off track"
  with Invalid_argument _ -> failwith "off track"

module M = Map.Make(struct
  type t = int * int
  let compare (ax,ay) (bx,by) = match ay - by with 0 -> ax - bx | n -> n
end)

let find_carts tracks =
  tracks
  |> Array.fold_left (fun (carts,y) row ->
    row
    |> String.fold (fun (carts,x) track ->
      (match track with
      | '>' -> carts |> M.add (x,y) { initial_cart with x;y; dir = Right }
      | '<' -> carts |> M.add (x,y) { initial_cart with x;y; dir = Left }
      | '^' -> carts |> M.add (x,y) { initial_cart with x;y; dir = Up }
      | 'v' -> carts |> M.add (x,y) { initial_cart with x;y; dir = Down }
      | _ -> carts
      ),(x+1)
    ) (carts,0) |> fst,(y+1)
  ) (M.empty,0)
  |> fst

let simulate tracks carts =
  let rec tick first carts =
    match M.cardinal carts with
    | 0 -> Option.value first,None
    | 1 -> let {x;y} = M.choose carts |> snd in Option.value first,(Some (x,y))
    | _ -> carts |> M.bindings |> step first carts
  and step first carts = function
    | [] -> tick first carts
    | (pos,cart)::left when M.mem pos carts ->
      let carts' = carts |> M.remove pos in
      let cart' = cart |> move tracks in
      let pos' = cart'.x,cart'.y in
      if M.mem pos' carts then
        step (first |> Option.orSome pos') (carts' |> M.remove pos') left
      else
        step first (carts' |> M.add pos' cart') left
    | _::left -> step first carts left
  in tick None carts

let () = File.open_in "./day13.example1" (fun ch ->
  let tracks = Seq.of_lines ch |> Array.of_seq in
  let carts = find_carts tracks in
  let part1,_ = simulate tracks carts in
  let () = assert (part1 = (7,3)) in
  ()
)

let () = File.open_in "./day13.example2" (fun ch ->
  let tracks = Seq.of_lines ch |> Array.of_seq in
  let carts = find_carts tracks in
  let _,part2 = simulate tracks carts in
  let () = assert (part2 = Some (6,4)) in
  ()
)

let () = File.open_in "./day13.input" (fun ch ->
  let tracks = Seq.of_lines ch |> Array.of_seq in
  let carts = find_carts tracks in
  let part1,part2 = simulate tracks carts in
  let () = part1 |> fun (x,y) -> Printf.printf "part1: %d,%d\n%!" x y in
  let () = part2 |> Option.value |> fun (x,y) -> Printf.printf "part2: %d,%d\n%!" x y in
  ()
)
