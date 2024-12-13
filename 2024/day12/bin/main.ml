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
type thing = Cell of coord | Wall of coord * direction | Corner of coord * direction [@@deriving show]
(* 
  L  U  U
  L [X] R
  D  D  R
*)

let components_per_cell = 9

let enc (height: int) (width: int) (x: thing) = match x with
  | Cell (i, j) -> (width * i + j ) * components_per_cell
  | Wall ((i, j), L) -> (width * i + j ) * components_per_cell + 1
  | Wall ((i, j), U) -> (width * i + j ) * components_per_cell + 2
  | Wall ((i, j), R) -> (width * i + j ) * components_per_cell + 3
  | Wall ((i, j), D) -> (width * i + j ) * components_per_cell + 4
  | Corner ((i, j), L) -> (width * i + j ) * components_per_cell + 5
  | Corner ((i, j), U) -> (width * i + j ) * components_per_cell + 6
  | Corner ((i, j), R) -> (width * i + j ) * components_per_cell + 7
  | Corner ((i, j), D) -> (width * i + j ) * components_per_cell + 8

let dec (height: int) (width: int) (id: int) = match id mod components_per_cell with 
  | 0 -> Cell (id / width, id mod width)
  | 1 -> let id = id - 1 in Wall ((id / width, id mod width), L)
  | 2 -> let id = id - 2 in Wall ((id / width, id mod width), U)
  | 3 -> let id = id - 3 in Wall ((id / width, id mod width), R)
  | 4 -> let id = id - 4 in Wall ((id / width, id mod width), D)
  | 5 -> let id = id - 5 in Corner ((id / width, id mod width), L)
  | 6 -> let id = id - 6 in Corner ((id / width, id mod width), U)
  | 7 -> let id = id - 7 in Corner ((id / width, id mod width), R)
  | 8 -> let id = id - 8 in Corner ((id / width, id mod width), D)
  | _ -> raise (Failure "impossible")

(* algorithm *)

(* returns id |-> num cells, num walls, num corners *)
let mk_stats (input: char array array): (int, int * int * int) Hashtbl.t = 
  let height = Array.length input in
  let width = Array.length input.(0) in
  let f = init_uf (components_per_cell * width * height) in 
  let enc = enc height width in
  let dec = dec height width in
  let union' thing1 thing2 = 
    union f (enc thing1) (enc thing2) 
  in
  let checkWallAndCorner i j = 
    let diffU = (i = 0)          || (input.(i-1).(j)) != (input.(i).(j))
    and diffD = (i = height - 1) || (input.(i+1).(j)) != (input.(i).(j))
    and diffL = (j = 0)          || (input.(i).(j-1)) != (input.(i).(j))
    and diffR = (j = width - 1)  || (input.(i).(j+1)) != (input.(i).(j))
    in
    (
      if diffU then union' (Cell(i,j)) (Wall((i,j), U));
      if diffD then union' (Cell(i,j)) (Wall((i,j), D));
      if diffL then union' (Cell(i,j)) (Wall((i,j), L));
      if diffR then union' (Cell(i,j)) (Wall((i,j), R));
      if diffL && diffU then (union' (Cell(i,j)) (Corner((i,j), L)));
      if diffU && diffR then (union' (Cell(i,j)) (Corner((i,j), U)));
      if diffR && diffD then (union' (Cell(i,j)) (Corner((i,j), R)));
      if diffD && diffL then (union' (Cell(i,j)) (Corner((i,j), D)));
    )
  in
  let checkNeightbour i j =
    let sameU = (i != 0)          && (input.(i-1).(j)) = (input.(i).(j))
    and sameD = (i != height - 1) && (input.(i+1).(j)) = (input.(i).(j))
    and sameL = (j != 0)          && (input.(i).(j-1)) = (input.(i).(j))
    and sameR = (j != width - 1)  && (input.(i).(j+1)) = (input.(i).(j))
    in
    (
      if sameU then union' (Cell(i,j)) (Cell(i-1,j));
      if sameD then union' (Cell(i,j)) (Cell(i+1,j));
      if sameL then union' (Cell(i,j)) (Cell(i,j-1));
      if sameR then union' (Cell(i,j)) (Cell(i,j+1));
      if sameL && sameU && (input.(i).(j)) != (input.(i-1).(j-1)) then (union' (Cell(i,j)) (Corner((i,j), L)));
      if sameU && sameR && (input.(i).(j)) != (input.(i-1).(j+1)) then (union' (Cell(i,j)) (Corner((i,j), U)));
      if sameR && sameD && (input.(i).(j)) != (input.(i+1).(j+1)) then (union' (Cell(i,j)) (Corner((i,j), R)));
      if sameD && sameL && (input.(i).(j)) != (input.(i+1).(j-1)) then (union' (Cell(i,j)) (Corner((i,j), D)));
    )
  in
  let _ = 
    List.init height (function i -> 
      List.init width (function j -> 
        (checkWallAndCorner i j; checkNeightbour i j; ())
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
            | None -> Hashtbl.add stats repr (1, 0, 0)
            | Some (area, parameter, corners) -> Hashtbl.replace stats repr (area+1, parameter, corners));
          
          (let _ = List.map (function dir -> 
            let repr = find f (enc (Wall((i,j), dir))) in 
            if f.(repr).size <= 1 then () else
            match Hashtbl.find_opt stats repr with 
              | None -> Hashtbl.add stats repr (0, 1, 0)
              | Some (area, parameter, corners) -> Hashtbl.replace stats repr (area, parameter+1, corners)
            ) [L;R;U;D] in ());
          
          (let _ = List.map (function dir -> 
            let repr = find f (enc (Corner((i,j), dir))) in 
            if f.(repr).size <= 1 then () else
            match Hashtbl.find_opt stats repr with 
              | None -> Hashtbl.add stats repr (0, 0, 1)
              | Some (area, parameter, corners) -> Hashtbl.replace stats repr (area, parameter, corners+1)
            ) [L;R;U;D] in ());
          
          ()
        )
      )
    )
  in
  stats

let part1 input = 
  let stats = mk_stats input in 
  let 
    folder id (area, parameter, corners) acc = acc + area * parameter
  in
    Hashtbl.fold folder stats 0

let part2 input = 
  let stats = mk_stats input in 
  let 
    folder id (area, parameter, corners) acc = acc + area * corners
  in
    Hashtbl.fold folder stats 0
  
let () = Printf.printf "part1 test: %d\n" (part1 (parse "test.txt"))
let () = Printf.printf "part1 test2: %d\n" (part1 (parse "test2.txt"))
let () = Printf.printf "part1 test3: %d\n" (part1 (parse "test3.txt"))
let () = Printf.printf "part1 test4: %d\n" (part1 (parse "test4.txt"))
let () = Printf.printf "part1 real: %d\n" (part1 (parse "input.txt"))
let () = Printf.printf "\n"
let () = Printf.printf "part2 test: %d\n" (part2 (parse "test.txt"))
let () = Printf.printf "part2 test2: %d\n" (part2 (parse "test2.txt"))
let () = Printf.printf "part2 test3: %d\n" (part2 (parse "test3.txt"))
let () = Printf.printf "part2 test4: %d\n" (part2 (parse "test4.txt"))
let () = Printf.printf "part2 real: %d\n" (part2 (parse "input.txt"))