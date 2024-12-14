type instance = int * int list [@@deriving show]
type int_list = int list [@@deriving show]

let compose f g = function x -> g (f x)
let ( o ) = compose
let sum = List.fold_left (+) 0

let parse (in_file: string): instance list = 
  In_channel.with_open_text in_file In_channel.input_lines
  |> List.filter (function s -> s != "")
  |> List.map (function line -> 
      let splitted = String.split_on_char ':' line in
      let target = List.nth splitted 0 |> int_of_string in
      let materials = List.nth splitted 1 |> String.trim |> String.split_on_char ' ' |> List.map int_of_string in
      (target, materials)
    )

let rec decideOne ((target, elems): instance) : bool = 
  match elems with 
    | [] -> raise (Failure "no way")
    | [x] -> x = target
    | x::rest -> decideOne (target - x, rest) || (target mod x = 0 && decideOne (target / x, rest))

let part1 (insts: instance list): int = 
  List.map (function (target, elems) -> (target, List.rev elems)) insts
  |> List.map decideOne
  |> List.combine (List.map fst insts)
  |> List.map (function (target, x) -> if x then target else 0)
  |> sum

let () = Printf.printf "part1 test: %d\n" (part1 (parse "test.txt"))
let () = Printf.printf "part1 real: %d\n" (part1 (parse "input.txt"))