(* increasing with step size 1, 2, or 3 *)
let rec allIncr (l: int list) = match l with
  | [] -> true
  | _::[] -> true
  | a::(b::_ as rest) -> b > a && (b - a) <= 3 && allIncr rest

(* decreasing with step size 1, 2, or 3 *)
let rec allDecr (l: int list) = match l with
  | [] -> true
  | _::[] -> true
  | a::(b::_ as rest) -> b < a && (a - b) <= 3 && allDecr rest

let rec allIncrDampened (l: int list) = match l with
  | [] -> true
  | _::[] -> true
  | _::_::[] -> true
  | a::b::c::rs -> match (b > a && (b - a) <= 3, c > a && (c - a) <= 3) with 
    | (true, true) -> allIncr (a::c::rs) || allIncrDampened (b::c::rs)
    | (true, false) -> allIncrDampened (b::c::rs)
    | (false, true) -> allIncr (a::c::rs)
    | (false, false) -> false

let allIncrDampened' (l: int list) = match l with
  | [] -> true
| _::rest -> allIncrDampened l || allIncr rest

let rec allDecrDampened (l: int list) = match l with
  | [] -> true
  | _::[] -> true
  | _::_::[] -> true
  | a::b::c::rs -> match (a > b && (a - b) <= 3, a > c && (a - c) <= 3) with 
    | (true, true) -> allDecr (a::c::rs) || allDecrDampened (b::c::rs)
    | (true, false) -> allDecrDampened (b::c::rs)
    | (false, true) -> allDecr (a::c::rs)
    | (false, false) -> false

let allDecrDampened' (l: int list) = match l with
  | [] -> true
| _::rest -> allDecrDampened l || allDecr rest

let sum = List.fold_left (+) 0

let parse in_file = 
  In_channel.with_open_text in_file In_channel.input_lines
  |> List.filter ((!=) "")
  |> List.map (function x -> String.split_on_char ' ' x |> List.map int_of_string)

let part1 input =
  sum (List.map (function report -> 
    if allIncr report || allDecr report then 1 else 0
  ) input) 

let part2 input =
  sum (List.map (function report -> 
    if allIncrDampened' report || allDecrDampened' report then 1 else 0
  ) input) 

let () = Printf.printf "part1 test: %d\n" (part1 (parse "test.txt"))
let () = Printf.printf "part1 real: %d\n" (part1 (parse "input.txt"))
let () = Printf.printf "part2 test: %d\n" (part2 (parse "test.txt"))
let () = Printf.printf "part2 real: %d\n" (part2 (parse "input.txt"))