type int_int_list = (int * int) list [@@deriving show]

let compose f g = function x -> g (f x)
let ( o ) = compose
let sum = List.fold_left (+) 0

let parse (in_file: string): char array array = 
  In_channel.with_open_text in_file In_channel.input_lines
  |> List.filter (function s -> s != "")
  |> List.map (function line -> String.to_seq line |> List.of_seq |> Array.of_list)
  |> Array.of_list


(* union find *)

type node = { 
  size: int; 
  parent: int; 
} [@@deriving show]

type forest = node array [@@deriving show]

let init_uf (size: int): forest = 
  Array.init size (function i -> {size = 1; parent = i})

let rec find (f: forest) (id: int): int = let n = f.(id) in 
  if n.parent = id then id else (
    let root = find f n.parent in 
    let _ = f.(id) <- {f.(id) with parent = root} in 
    root
  )

let rec link (f: forest) (id1: int) (id2: int) : unit = 
  if f.(id1).size >= f.(id2).size then 
    (f.(id2) <- {f.(id2) with parent = id1};
      f.(id1) <- {f.(id1) with size = f.(id1).size + f.(id2).size})
  else 
    link f id2 id1

let union (f: forest) (id1: int) (id2: int) : unit = 
  let repr1 = (find f id1) in
  let repr2 = (find f id2) in
  if repr1 = repr2 then () else link f repr1 repr2


(* id encoding *)

type coord = int * int [@@deriving show]
type direction = U | D | L | R [@@deriving show]
type thing = Cell of coord | Wall of coord * direction [@@deriving show]

let enc (height: int) (width: int) (x: thing) = match x with
  | Cell (i, j) -> (width * i + j ) * 5
  | Wall ((i, j), L) -> (width * i + j ) * 5 + 1
  | Wall ((i, j), U) -> (width * i + j ) * 5 + 2
  | Wall ((i, j), R) -> (width * i + j ) * 5 + 3
  | Wall ((i, j), D) -> (width * i + j ) * 5 + 4

let dec (height: int) (width: int) (id: int) = match id mod 5 with 
  | 0 -> Cell (id / width, id mod width)
  | 1 -> let id = id - 1 in Wall ((id / width, id mod width), L)
  | 2 -> let id = id - 2 in Wall ((id / width, id mod width), U)
  | 3 -> let id = id - 3 in Wall ((id / width, id mod width), R)
  | 4 -> let id = id - 4 in Wall ((id / width, id mod width), D)
  | _ -> raise (Failure "impossible")

(* algorithm *)

let part1 (input: char array array): int = 
  let height = Array.length input in
  let width = Array.length input.(0) in
  let f = init_uf (5*width*height) in 
  let enc = enc height width in
  let dec = dec height width in
  let checkWall i j = 
    (
      if (i = 0)          || (input.(i-1).(j)) != (input.(i).(j)) then union f (enc (Cell(i,j))) (enc (Wall((i,j), U)));
      if (i = height - 1) || (input.(i+1).(j)) != (input.(i).(j)) then union f (enc (Cell(i,j))) (enc (Wall((i,j), D)));
      if (j = 0)          || (input.(i).(j-1)) != (input.(i).(j)) then union f (enc (Cell(i,j))) (enc (Wall((i,j), L)));
      if (j = width - 1)  || (input.(i).(j+1)) != (input.(i).(j)) then union f (enc (Cell(i,j))) (enc (Wall((i,j), R)));
    )
  in
  let checkNeightbour i j =
    (
      if (i != 0)          && (input.(i-1).(j)) = (input.(i).(j)) then union f (enc (Cell(i,j))) (enc (Cell(i-1,j)));
      if (i != height - 1) && (input.(i+1).(j)) = (input.(i).(j)) then union f (enc (Cell(i,j))) (enc (Cell(i+1,j)));
      if (j != 0)          && (input.(i).(j-1)) = (input.(i).(j)) then union f (enc (Cell(i,j))) (enc (Cell(i,j-1)));
      if (j != width - 1)  && (input.(i).(j+1)) = (input.(i).(j)) then union f (enc (Cell(i,j))) (enc (Cell(i,j+1)));
    )
  in
  let _ = 
    List.init height (function i -> 
      List.init width (function j -> 
        (checkWall i j; checkNeightbour i j; ())
      )
    )
  in
  let stats = Hashtbl.create height in
  let _ =
    List.init height (function i -> 
      List.init width (function j -> 
        (
          (let repr = find f (enc (Cell(i,j))) in 
          if f.(repr).size <= 1 then () else
          match Hashtbl.find_opt stats repr with 
            | None -> Hashtbl.add stats repr (1, 0)
            | Some (area, parameter) -> Hashtbl.replace stats repr (area+1, parameter));
          
          (let _ = List.map (function dir -> 
            let repr = find f (enc (Wall((i,j), dir))) in 
            if f.(repr).size <= 1 then () else
            match Hashtbl.find_opt stats repr with 
              | None -> Hashtbl.add stats repr (0, 1)
              | Some (area, parameter) -> Hashtbl.replace stats repr (area, parameter+1)
            ) [L;R;U;D] in ());
          
          ()
        )
      )
    )
  in
  let 
    folder id (area, parameter) acc = acc + area * parameter
  in
  Hashtbl.fold folder stats 0
  
let () = Printf.printf "part1 test: %d\n" (part1 (parse "test.txt"))
let () = Printf.printf "part1 test2: %d\n" (part1 (parse "test2.txt"))
let () = Printf.printf "part1 test3: %d\n" (part1 (parse "test3.txt"))
let () = Printf.printf "part1 test4: %d\n" (part1 (parse "test4.txt"))
let () = Printf.printf "part1 real: %d\n" (part1 (parse "input.txt"))
(* let () = Printf.printf "part2 test: %d\n" (part2 (parse "test.txt"))
let () = Printf.printf "part2 real: %d\n" (part2 (parse "input.txt")) *)