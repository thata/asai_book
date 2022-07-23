(* 配列内の接頭語の先頭に整数 n を追加する（？？？） *)
let rec add_to_each n lst =
  match lst with
  | [] -> []
  | first :: rest -> (n :: first) :: add_to_each n rest

let test1 = add_to_each 1 [] = []
let test2 = add_to_each 1 [ [ 2 ] ] = [ [ 1; 2 ] ]
let test3 = add_to_each 1 [ [ 2 ]; [ 2; 3 ] ] = [ [ 1; 2 ]; [ 1; 2; 3 ] ]

(* 受け取った lst の接頭語のリストを求める（？？？） *)
let rec prefix lst =
  match lst with
  | [] -> []
  | first :: rest -> [ first ] :: add_to_each first (prefix rest)

let test4 = prefix [] = []
let test5 = prefix [ 1 ] = [ [ 1 ] ]
let test6 = prefix [ 1; 2 ] = [ [ 1 ]; [ 1; 2 ] ]

let test7 =
  prefix [ 1; 2; 3; 4 ] = [ [ 1 ]; [ 1; 2 ]; [ 1; 2; 3 ]; [ 1; 2; 3; 4 ] ]


(* 渡されたリストの中の整数のうち最小値を返す *)
let rec minimum lst =
  match lst with
  | [] -> max_int
  | first :: rest ->
      let min = minimum rest in
      if first < min then first else min

let test8 = minimum [ 1 ] = 1
let test9 = minimum [ 1; 2 ] = 1
let test10 = minimum [ 5; 4; 3; 2 ] = 2

type gakusei_t = { name : string; tensuu : int; seiseki : string }

let gakusei1 = { name = "alice"; tensuu = 90; seiseki = "A" }
let gakusei2 = { name = "bob"; tensuu = 70; seiseki = "B" }
let gakusei3 = { name = "carol"; tensuu = 50; seiseki = "C" }
let gakusei4 = { name = "dave"; tensuu = 100; seiseki = "A" }
let gakusei5 = { name = "eve"; tensuu = 10; seiseki = "D" }

(* A, B, C, Dの各成績の学生の人数を集計する *)
let rec shukei lst =
  match lst with
  | [] -> (0, 0, 0, 0)
  | { name = n; tensuu = t; seiseki = s } :: rest -> (
      let a, b, c, d = shukei rest in
      match s with
      | "A" -> (a + 1, b, c, d)
      | "B" -> (a, b + 1, c, d)
      | "C" -> (a, b, c + 1, d)
      | "D" | _ -> (a, b, c, d + 1))

let test11 = shukei [] = (0, 0, 0, 0)
let test12 = shukei [ gakusei1 ] = (1, 0, 0, 0)
let test13 = shukei [ gakusei1; gakusei4 ] = (2, 0, 0, 0)
let test14 = shukei [ gakusei1; gakusei4; gakusei2 ] = (2, 1, 0, 0)

let test15 =
  shukei [ gakusei1; gakusei4; gakusei2; gakusei3; gakusei5 ] = (2, 1, 1, 1)

(* ふたつのリストを結合して返す *)
let rec append lst1 lst2 =
  match lst1 with [] -> lst2 | first :: rest -> first :: append rest lst2

let test16 = append [] [] = []
let test17 = append [ 1 ] [] = [ 1 ]
let test18 = append [] [ 2 ] = [ 2 ]
let test19 = append [ 1 ] [ 2 ] = [ 1; 2 ]
let test20 = append [ 1; 2 ] [ 3; 4 ] = [ 1; 2; 3; 4 ]

(* ふたつの昇順に並んだリストをマージして返す *)
let rec merge lst1 lst2 =
  match (lst1, lst2) with
  | ([], []) -> []
  | (first1 :: rest1, []) -> lst1
  | ([], first2 :: rest2) -> lst2
  | (first1 :: rest1, first2 :: rest2)
    -> if first1 < first2 then first1 :: merge rest1 lst2
       else first2 :: merge lst1 rest2

let test = merge [] [] = []
let test = merge [] [1] = [1]
let test = merge [2] [] = [2]
let test = merge [2] [1] = [1; 2]
let test = merge [1] [2] = [1; 2]
let test = merge [2; 4; 6] [1; 3; 5; 7] = [1; 2; 3; 4; 5; 6; 7]
let test = merge [] [1; 3; 5; 7] = [1; 3; 5; 7]
let test = merge [2; 4; 6] [] = [2; 4; 6]


(* quick sort*)
let rec quick_sort lst =
  let take_greater n lst = List.filter (fun x -> x >= n) lst in
  let take_less n lst = List.filter (fun x -> x < n) lst in
  match lst with
  | [] -> []
  | first :: rest ->
      quick_sort (take_less first rest)
      @ [ first ]
      @ quick_sort (take_greater first rest)

let qs_test1 = quick_sort [] = []
let qs_test1 = quick_sort [ 1 ] = [ 1 ]
let qs_test1 = quick_sort [ 2; 3; 1 ] = [ 1; 2; 3 ]
let qs_test1 =
  quick_sort [ 2; 5; 8; 4; 1; 3; 9; 6; 7 ] = [ 1; 2; 3; 4; 5; 6; 7; 8; 9 ]

(* 問題 10.1 *)
(* 昇順に並んでいる整数にリストに整数 n を照準となる位置に挿入する *)
let rec insert lst n =
  match lst with
  | [] -> [n]
  | first :: rest
    ->
      if n < first then n :: first :: rest
      else first :: (insert rest n)

let test_insert = insert [] 1 = [1]
let test_insert = insert [2; 3] 1 = [1; 2; 3]
let test_insert = insert [2; 3] 4 = [2; 3; 4]
let test_insert = insert [2; 4; 6; 8; 12; 14] 10 = [2; 4; 6; 8; 10; 12; 14]

(* 問題 10.2 *)
let rec ins_sort lst =
  match lst with
  | [] -> []
  | first :: rest -> insert (ins_sort rest) first

let test_ins_sort = ins_sort [] = []
let test_ins_sort = ins_sort [1] = [1]
let test_ins_sort = ins_sort [8; 5; 3; 1] = [1; 3; 5; 8]
