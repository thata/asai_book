(* リストに 0 が含まれているか調べる *)
let rec contain_zero lst =
  match lst with
  | [] -> false
  | 0 :: rest -> true
  | _ :: rest -> contain_zero rest

let contain_zero_test1 = contain_zero [] = false
let contain_zero_test2 = contain_zero [ 0 ] = true
let contain_zero_test3 = contain_zero [ 1; 0 ] = true
let contain_zero_test4 = contain_zero [ 1; 2; 3 ] = false

(* リストの含まれる整数の合計を計算する *)
let rec sum lst = match lst with [] -> 0 | head :: rest -> head + sum rest
let sum_test1 = sum [] = 0
let sum_test2 = sum [ 0 ] = 0
let sum_test3 = sum [ 1 ] = 1
let sum_test4 = sum [ 1; 2 ] = 3
let sum_test5 = sum [ 1; 2; 3; 4; 5 ] = 15

(* リストの長さを返す *)
let rec length lst = match lst with [] -> 0 | head :: rest -> 1 + length rest
let length_test1 = length [] = 0
let length_test2 = length [ 1 ] = 1
let length_test3 = length [ 1; 2; 3 ] = 3

(* リスト中の偶数の要素を返す *)
let rec even lst =
  match lst with
  | [] -> []
  | head :: rest -> if (head mod 2) = 0 then head :: even rest else even rest

let even_test1 = even [] = []
let even_test2 = even [ 2 ] = [ 2 ]
let even_test3 = even [ 1; 2; 3; 4 ] = [ 2; 4 ]


type gakusei_t = { name: string; tensuu: int; seiseki: string }
let gakusei1 = { name = "alice"; tensuu = 90; seiseki = "A" }
let gakusei2 = { name = "bob"; tensuu = 70; seiseki = "B" }
let gakusei3 = { name = "carol"; tensuu = 50; seiseki = "C" }
let gakusei4 = { name = "dave"; tensuu = 100; seiseki = "A" }

(* 成績Aの生徒の人数を数える *)
let rec count_A lst =
  match lst with
  | [] -> 0
  | { name = n; tensuu = t; seiseki = s } :: rest
    -> if s = "A" then 1 + count_A rest
                  else count_A rest

let count_a_test1 = count_A [] = 0
let count_a_test2 = count_A [gakusei1] = 1
let count_a_test3 = count_A [gakusei2] = 0
let count_a_test4 = count_A [gakusei1; gakusei2; gakusei3] = 1
let count_a_test5 = count_A [gakusei1; gakusei2; gakusei3; gakusei4] = 2
