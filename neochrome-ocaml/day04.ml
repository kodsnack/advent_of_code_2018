#use "./ext.ml";;

type state =
  | Guard of int
  | Sleep
  | Wakeup

type event = {
  date : string;
  hour : int;
  minute : int;
  state : state;
}

let by_time a b =
  match compare a.date b.date with
  | 0 ->
    begin match compare a.hour b.hour with
    | 0 -> compare a.minute b.minute
    | n -> n
    end
  | n -> n

let parse fmt map line = try Some (Scanf.sscanf line fmt map) with _ -> None

let rec choose parsers line =
  match parsers with
  | [] -> failwith ("can't handle: " ^ line)
  | parse :: parsers ->
    begin match parse line with
    | None -> choose parsers line
    | Some result -> result
    end

let parsers = [
  parse "[%s %d:%d] Guard #%d begins shift" (fun date hour minute id -> { date; hour; minute; state = Guard id });
  parse "[%s %d:%d] wakes up" (fun date hour minute -> { date; hour; minute; state = Wakeup });
  parse "[%s %d:%d] falls asleep" (fun date hour minute -> { date; hour; minute; state = Sleep });
]

module IntMap = Map.Make(struct type t = int let compare = compare end)

let inc id = IntMap.update id (function None -> Some 1 | Some n -> Some (n + 1))

let rec add_sleep minute stop sleep =
  if minute = stop then sleep
  else (inc minute sleep) |> add_sleep (minute + 1) stop

let rec collect_sleep guards guard from = function
  | [] -> guards
  | event :: events ->
    begin match event.state with
    | Guard guard' -> collect_sleep guards guard' from events
    | Sleep -> collect_sleep guards guard event.minute events
    | Wakeup ->
      let sleep =
        match guards |> IntMap.find_opt guard with
        | None -> IntMap.empty
        | Some sleep -> sleep
        |> add_sleep from event.minute
      in
      let guards' = IntMap.update guard (function _ -> Some sleep) guards in
      collect_sleep guards' guard from events
    end

let strategy1 guards = IntMap.(
  let guard,sleep =
    guards |> maxf (fun _ sleep ->
      fold (fun _ n acc -> acc + n) sleep 0
    )
  in
  sleep |> maxf (fun _ n -> n) |> fun (minute,_) -> minute * guard
)

let strategy2 guards = IntMap.(
  guards
  |> map (fun sleep -> sleep |> maxf (fun _ n -> n))
  |> maxf (fun _ (_,n) -> n)
  |> fun (guard,(minute,_)) -> minute * guard
)

let () = File.open_in "./day04.input" (fun ch ->
  let guards =
    Seq.of_lines ch
    |> Seq.map (choose parsers)
    |> List.of_seq
    |> List.sort by_time
    |> collect_sleep IntMap.empty 0 0
  in
  let () = guards |> strategy1 |> Printf.printf "part1: %d\n%!" in
  let () = guards |> strategy2 |> Printf.printf "part2: %d\n%!" in
  ()
)
