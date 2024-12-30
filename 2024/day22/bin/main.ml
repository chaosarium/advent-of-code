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

module Int4Tuple =
  struct
    type t = int * int * int * int
    let compare (a0,b0,c0,d0) (a1,b1,c1,d1) =
      match Stdlib.compare a0 a1 with
          0 -> (match Stdlib.compare b0 b1 with
                    0 -> (match Stdlib.compare c0 c1 with
                              0 -> Stdlib.compare d0 d1
                            | c -> c)
                  | c -> c)
        | c -> c
  end

module Int4Map = Map.Make(Int4Tuple)

let possible_delta = [-9;-8;-7;-6;-5;-4;-3;-2;-1;0;1;2;3;4;5;6;7;8;9]

let least_sig x = x mod 10

let max_steps = 2000

(* 
                             current step
                             v
  step1    step2    step3    step4
  ^-- window -----------^    ^-- head_n_steps left --...
                    ^ delta4 ^
           ^ delta3 ^
  ^ delta2 ^
*)
let rec p2_sim_all' (seeds: int list) (head_n_steps: int) (acc: int) (window: int * int * int) (sell_condition: int * int * int * int): int = 
  match seeds with
    | [] -> acc
    | s::ss -> match head_n_steps with
      | 0 -> p2_sim_all' ss max_steps acc window sell_condition
      | _ -> (
        let (delta1, delta2, delta3) = window in
        let s' = sim_one s in
        let curr_price = least_sig s' in
        let prev_price = least_sig s in
        let delta4 = curr_price - prev_price in
        let curr_condition = (delta1, delta2, delta3, delta4) in
        if curr_condition <> sell_condition then
          p2_sim_all' (s'::ss) (head_n_steps-1) acc (delta2, delta3, delta4) sell_condition
        else
          p2_sim_all' ss max_steps (acc + curr_price) (delta2, delta3, delta4) sell_condition
      )

let p2_sim_all (seeds: int list) ((cond1, cond2, cond3, cond4) as sell_condition: int * int * int * int): int = 
  match seeds with
    | [] -> 0
    | s::ss -> (
      let s1 = sim_one s in
      let s2 = sim_one s1 in
      let s3 = sim_one s2 in
      let delta1 = (least_sig s1) - (least_sig s) in
      let delta2 = (least_sig s2) - (least_sig s1) in
      let delta3 = (least_sig s3) - (least_sig s2) in
      p2_sim_all' (s3::ss) (max_steps-3) 0 (delta1, delta2, delta3) sell_condition
    )

let all_sell_conditions = 
  let possible_delta_seq = List.to_seq possible_delta in
  let pairs = Seq.product possible_delta_seq possible_delta_seq in
  Seq.product pairs pairs |> Seq.map (fun ((a, b), (c, d)) -> (a, b, c, d))

(* 
  meh let's just brute force part2 
  for future self: first prune search space by first making a set of possible sell conditions with one pass
  then filter before mapping the passes
*)
let part2 (input: int list): int = 
  all_sell_conditions 
  |> Seq.fold_left (
    fun (acc, i) -> fun ((cond1, cond2, cond3, cond4) as sell_condition) -> 
    let bananas = p2_sim_all input sell_condition in
    let _ = if bananas > acc then 
      Printf.printf "    more bananas (%d) with try %d/130321 try (%d, %d, %d, %d)\n%!" bananas i cond1 cond2 cond3 cond4
    else () in
    (max acc bananas, i + 1)
  ) (0, 0) 
  |> fst

let () = Printf.printf "part1 test: %d\n%!" (part1 (parse "test.txt"))
let () = Printf.printf "part1 real: %d\n%!" (part1 (parse "input.txt"))

let () = Printf.printf "part2 test: %d\n%!" (part2 (parse "test.txt"))
let () = Printf.printf "part2 test2: %d\n%!" (part2 (parse "test2.txt"))
let () = Printf.printf "part2 real: %d\n%!" (part2 (parse "input.txt"))
