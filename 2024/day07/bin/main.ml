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

let rec num_digits x = if x < 10 then 1 else 1 + num_digits (x / 10)
let rec pow_base10 e = if e = 0 then 1 else 10 * pow_base10 (e-1)
let zeros_on_least_sig x num_zeros = (x / (pow_base10 num_zeros)) * (pow_base10 num_zeros)
let isolate_least_sig x num_digits = x - (zeros_on_least_sig x num_digits)

(* 
target = 1234 = 12 || 34
curr elem = 34
target' = 12
*)
let rec decideOne2 ((target, elems): instance) : bool = 
  match elems with 
    | [] -> raise (Failure "no way")
    | [x] -> x = target
    | x::rest -> 
        decideOne2 (target - x, rest) || 
        (target mod x = 0 && decideOne2 (target / x, rest)) ||
        let x_num_digits = num_digits x in 
        let target_num_digits = num_digits target in 
        let target_isolated_least_sig_digits = isolate_least_sig target x_num_digits in
        let target' = (zeros_on_least_sig target x_num_digits / pow_base10 x_num_digits) in
        target_num_digits > x_num_digits && target_isolated_least_sig_digits = x && decideOne2 (target', rest)

let part2 (insts: instance list): int = 
  List.map (function (target, elems) -> (target, List.rev elems)) insts
  |> List.map decideOne2
  |> List.combine (List.map fst insts)
  |> List.map (function (target, x) -> if x then target else 0)
  |> sum

let () = Printf.printf "part1 test: %d\n" (part1 (parse "test.txt"))
let () = Printf.printf "part1 real: %d\n" (part1 (parse "input.txt"))
let () = Printf.printf "part2 test: %d\n" (part2 (parse "test.txt"))
let () = Printf.printf "part2 real: %d\n" (part2 (parse "input.txt"))