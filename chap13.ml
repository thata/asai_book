let rec map f lst =
  match lst with
  | [] -> []
  | first :: rest -> (f first) :: (map f rest)

let map_sqrt lst = map sqrt lst
let test_map_sqrt = map_sqrt [] = []
let test_map_sqrt = map_sqrt [1.; 4.; 9.] = [1.; 2.; 3.]

let square n = n * n
let map_square lst = map square lst
let test_map_square = map_square [] = []
let test_map_square = map_square [1; 2; 3] = [1; 4; 9]

(* 問題 13.4 *)
let time2 x = x * 2
let add3 x = x + 3
let compose f g =
  let h x = f (g x)
  in h
let test = (compose time2 add3) 4

(* 問題 13.5 *)
let twice f =
  let g x = f (f x)
  in g

let twice_twice = twice twice
let test = (twice_twice add3) 10 = 22
