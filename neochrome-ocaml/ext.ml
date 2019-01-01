let bind f g x = g (f x)
let (>>) = bind

module Option = struct

  let is_none = function None -> true | _ -> false
  let is_some = function None -> false | _ -> true

  let value = function
    | None -> raise (Invalid_argument "Must not be None")
    | Some x -> x

  let value_default default = function
    | None -> default
    | Some x -> x

  let map f = function
    | None -> None
    | Some x -> Some (f x)

end

module File = struct

  let open_in filename fn =
    let ch = open_in filename in
    try
      let res = fn ch in
      close_in ch;
      res
    with e ->
      close_in ch;
      raise e

end

module Seq = struct
  include Seq

  let of_lines ch =
    let rec next () =
      try Cons (input_line ch, next)
      with End_of_file -> Nil
    in next
end

module String = struct
  include String

  let to_chars s =
    let rec build n l =
      if n = 0 then l
      else build (n - 1) (s.[n - 1] :: l)
    in
    build (String.length s) []
  ;;

  let from_chars chars =
    String.init (List.length chars) (fun i -> List.nth chars i)

end

module List = struct
  include List

  let unique_pairs xs =
    let rec take acc = function
      | [] -> acc
      | curr :: next ->
        let acc' = next |> List.map (fun x -> curr, x) in
        take (acc' @ acc) next
    in take [] xs

  let reject f = filter (fun x -> not (f x))

  let min =
    let rec search = function
      | [] -> failwith "min requires a non-empty list"
      | [x] -> x
      | x :: xs -> Pervasives.min x (search xs)
    in search

  let max =
    let rec search = function
      | [] -> failwith "a non-empty list is required"
      | [x] -> x
      | x :: xs -> Pervasives.max x (search xs)
    in search

  let minmax cmp xs =
    let min a b = if cmp a b < 0 then a else b in
    let max a b = if cmp b a < 0 then a else b in
    let rec search mi ma = function
      | [] -> mi,ma
      | x :: xs -> search (min mi x) (max ma x) xs
    in match xs with
    | [] -> failwith "a non-empty list is required"
    | x :: xs -> search x x xs

  let minf f xs =
    let minf a b = if f a < f b then a else b in
    let rec search mx = function
      | [] -> mx
      | x :: xs -> search (minf mx x) xs
    in
    match xs with
    | [] -> failwith "a non-empty list is required"
    | x :: xs -> search x xs

  let maxf f xs =
    let maxf a b = if f a > f b then a else b in
    let rec search mx = function
      | [] -> mx
      | x :: xs -> search (maxf mx x) xs
    in
    match xs with
    | [] -> failwith "a non-empty list is required"
    | x :: xs -> search x xs

  let remove x xs =
    let rec loop xs' = function
      | [] -> xs' |> List.rev
      | y :: ys when y = x -> loop xs' ys
      | y :: ys -> loop (y :: xs') ys
    in loop [] xs

  let filter_map f xs =
    xs
    |> map f
    |> filter Option.is_some
    |> map Option.value

  let filter_mapi f xs =
    xs
    |> mapi f
    |> filter Option.is_some
    |> map Option.value

end

module Map = struct
  module type S = sig
    include Map.S
    val maxf: (key -> 'a -> 'b) -> 'a t -> key * 'a
    val find_default : key -> 'a -> 'a t -> 'a
  end

  module Make (Ord: Map.OrderedType) : S with type key = Ord.t = struct
    module Map = Map.Make(Ord)
    include Map

    let maxf f m =
      fold (fun key value acc ->
        let a' = f key value in
        match acc with
        | Some (a,_) when a' <= a -> acc
        | _ -> Some (a', (key,value))
      ) m None
      |> function
        | None -> failwith "no values"
        | Some (_,max) -> max

    let find_default k d m =
      match find_opt k m with
      | None -> d
      | Some x -> x

  end
end

module Range = struct

  let rec iter b e f =
    if b > e then ()
    else begin f (b); iter (b + 1) e f end

  let rec fold b e f a =
    if b > e then a
    else fold (b + 1) e f (f a b)

end
