type int_int_list = (int * int) list [@@deriving show]

let parse (in_file: string): string list = 
  In_channel.with_open_text in_file In_channel.input_lines

let sum = List.fold_left (+) 0
let uncurry f (x, y) = f x y

let procLine (line: string): int = 
  let r = Str.regexp {|mul(\([0-9]+\),\([0-9]+\))|} in
  let rec match_all start acc =
    try
      let _ = Str.search_forward r line start in
      let a = Str.matched_group 1 line in
      let b = Str.matched_group 2 line in
      match_all (Str.match_end ()) ((int_of_string a, int_of_string b)::acc)
    with Not_found ->
      List.rev acc
  in
  match_all 0 [] |> List.map (uncurry ( * )) |> sum

let part1 (input: string list): int = sum (List.map procLine input)



type instr = Mult of int * int | Do | Dont
let procLine2 (line: string): int = 
  let r = Str.regexp {|mul(\([0-9]+\),\([0-9]+\))\|do()\|don't()|} in
  let rec match_all start acc =
    try
      let _ = Str.search_forward r line start in
      let s = Str.matched_group 0 line in
      if s = "do()" then match_all (Str.match_end ()) (Do::acc) else
      if s = "don't()" then match_all (Str.match_end ()) (Dont::acc) else
      let a = Str.matched_group 1 line in
      let b = Str.matched_group 2 line in
      match_all (Str.match_end ()) ((Mult(int_of_string a, int_of_string b))::acc)
    with Not_found ->
      List.rev acc
  in
  match_all 0 [] |> List.fold_left (function (state, acc) -> function instr -> 
    match (state, instr) with
      | (_, Do) -> (true, acc)
      | (_, Dont) -> (false, acc)
      | (false, _) -> (false, acc)
      | (true, Mult(a,b)) -> (true, acc + a * b)
  ) (true, 0) |> snd

let part2 (input: string list): int = procLine2 (String.concat "<newline>" input)


  
  
let () = Printf.printf "part1 test: %d\n" (part1 (parse "test.txt"))
let () = Printf.printf "part1 real: %d\n" (part1 (parse "input.txt"))
let () = Printf.printf "part2 test: %d\n" (part2 (parse "test2.txt"))
let () = Printf.printf "part2 real: %d\n" (part2 (parse "input.txt"))