type int_int_list = (int * int) list [@@deriving show]

let compose f g = function x -> g (f x)
let ( o ) = compose

type letter = X | M | A | S
let to_letter (s: char): letter = match s with
  | 'X' -> X
  | 'M' -> M
  | 'A' -> A
  | 'S' -> S
  | _ -> raise (Failure "no")

let parse (in_file: string): letter array array = 
  In_channel.with_open_text in_file In_channel.input_lines
  |> List.map (function line -> String.to_seq line |> List.of_seq |> List.map to_letter |> Array.of_list)
  |> Array.of_list

let sum = List.fold_left (+) 0
let part1 (input: letter array array): int = 
  let height = Array.length input in
  let width = Array.length input.(0) in
  let locCount i j =
    [
      (try if input.(i+0).(j) = X && input.(i+1).(j) = M && input.(i+2).(j) = A && input.(i+3).(j) = S then 1 else 0 with Invalid_argument _ -> 0);
      (try if input.(i-0).(j) = X && input.(i-1).(j) = M && input.(i-2).(j) = A && input.(i-3).(j) = S then 1 else 0 with Invalid_argument _ -> 0);
      (try if input.(i).(j+0) = X && input.(i).(j+1) = M && input.(i).(j+2) = A && input.(i).(j+3) = S then 1 else 0 with Invalid_argument _ -> 0);
      (try if input.(i).(j-0) = X && input.(i).(j-1) = M && input.(i).(j-2) = A && input.(i).(j-3) = S then 1 else 0 with Invalid_argument _ -> 0);
      (try if input.(i+0).(j+0) = X && input.(i+1).(j+1) = M && input.(i+2).(j+2) = A && input.(i+3).(j+3) = S then 1 else 0 with Invalid_argument _ -> 0);
      (try if input.(i-0).(j-0) = X && input.(i-1).(j-1) = M && input.(i-2).(j-2) = A && input.(i-3).(j-3) = S then 1 else 0 with Invalid_argument _ -> 0);
      (try if input.(i-0).(j+0) = X && input.(i-1).(j+1) = M && input.(i-2).(j+2) = A && input.(i-3).(j+3) = S then 1 else 0 with Invalid_argument _ -> 0);
      (try if input.(i+0).(j-0) = X && input.(i+1).(j-1) = M && input.(i+2).(j-2) = A && input.(i+3).(j-3) = S then 1 else 0 with Invalid_argument _ -> 0);
    ] |> sum
  in 
  List.init height (function i -> 
    List.init width (function j -> 
      locCount i j
    ) |> sum
  ) |> sum

let part2 (input: letter array array): int = 
  let height = Array.length input in
  let width = Array.length input.(0) in
  let locCount i j =
    [
      (try if 
        input.(i-1).(j-1) = M            &&            input.(i-1).(j+1) = M 
                               && input.(i).(j) = A && 
        input.(i+1).(j-1) = S            &&            input.(i+1).(j+1) = S
      then 1 else 0 with Invalid_argument _ -> 0);
      (try if 
        input.(i-1).(j-1) = M            &&            input.(i-1).(j+1) = S
                               && input.(i).(j) = A && 
        input.(i+1).(j-1) = M            &&            input.(i+1).(j+1) = S
      then 1 else 0 with Invalid_argument _ -> 0);
      (try if 
        input.(i-1).(j-1) = S            &&            input.(i-1).(j+1) = S 
                               && input.(i).(j) = A && 
        input.(i+1).(j-1) = M            &&            input.(i+1).(j+1) = M
      then 1 else 0 with Invalid_argument _ -> 0);
      (try if 
        input.(i-1).(j-1) = S            &&            input.(i-1).(j+1) = M 
                               && input.(i).(j) = A && 
        input.(i+1).(j-1) = S            &&            input.(i+1).(j+1) = M
      then 1 else 0 with Invalid_argument _ -> 0);
    ] |> sum
  in 
  List.init height (function i -> 
    List.init width (function j -> 
      locCount i j
    ) |> sum
  ) |> sum

  
let () = Printf.printf "part1 test: %d\n" (part1 (parse "test.txt"))
let () = Printf.printf "part1 real: %d\n" (part1 (parse "input.txt"))
let () = Printf.printf "part2 test: %d\n" (part2 (parse "test.txt"))
let () = Printf.printf "part2 real: %d\n" (part2 (parse "input.txt"))