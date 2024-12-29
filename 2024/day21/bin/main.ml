let parse (in_file: string): int list = 
  In_channel.with_open_text in_file In_channel.input_lines
  |> List.filter (function s -> s != "")
  |> List.map int_of_string

let mix a b = a lxor b

let prune a = a mod 16777216

let sim_one (s: int): int =
  let s = prune (mix (s * 64) s) in
  let s = prune (mix (s / 32) s) in
  let s = prune (mix (s * 2048) s) in
  s

let rec sim_n_steps (n_steps: int) (s: int): int = 
  match n_steps with 
  | 0 -> s
  | _ -> sim_n_steps (n_steps - 1) (sim_one s)

let sum = List.fold_left (+) 0

let part1 (input: int list): int = 
  List.map (sim_n_steps 2000) input |> sum

let () = Printf.printf "part1 test: %d\n" (part1 (parse "test.txt"))
let () = Printf.printf "part1 real: %d\n" (part1 (parse "input.txt"))
