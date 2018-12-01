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
  ;;

end

module Seq = struct
  include Seq

  let of_lines ch =
    let rec next () =
      try Cons (input_line ch, next)
      with End_of_file -> Nil
    in next
end
