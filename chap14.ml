(* リスト中の偶数の要素を返す *)
let rec even lst =
  let p x = (x mod 2) = 0 in
  List.filter p lst

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
  let p {seiseki} = seiseki = "A" in
  List.length (List.filter p lst)

let count_a_test1 = count_A [] = 0
let count_a_test2 = count_A [gakusei1] = 1
let count_a_test3 = count_A [gakusei2] = 0
let count_a_test4 = count_A [gakusei1; gakusei2; gakusei3] = 1
let count_a_test5 = count_A [gakusei1; gakusei2; gakusei3; gakusei4] = 2

(* my_fold_right *)
let rec my_fold_right f lst init =
  match lst with
    | [] -> init
    | first :: rest -> f first (my_fold_right f rest init)

(* sum *)
let sum lst =
  my_fold_right (+) lst 0
let sum_test = sum [] = 0
let sum_test = sum [1; 3; 5; 7; 9] = 25

(* length *)
let length lst =
  my_fold_right (fun _ y -> y + 1) lst 0
let length_test = length [] = 0
let length_test = length [1; 3; 5; 7; 9] = 5


type gakusei_t = { name: string; tensuu: int; seiseki: string }
let gakusei1 = { name = "alice"; tensuu = 90; seiseki = "A" }
let gakusei2 = { name = "bob"; tensuu = 70; seiseki = "B" }
let gakusei3 = { name = "carol"; tensuu = 50; seiseki = "C" }
let gakusei4 = { name = "dave"; tensuu = 100; seiseki = "A" }
let gakusei_list = [gakusei1; gakusei2; gakusei3]
let test = List.map (fun g -> g.name) gakusei_list

(* 完全数 *)
let perfect n =
  List.filter (fun x -> perfectp x) (enumerate n)

let rec enumerate n =
  if n = 0 then []
  else n :: (enumerate (n - 1))

let divisors n = List.filter (fun x -> (n mod x) = 0) (enumerate n)

let perfectp n =
  let sum = List.fold_right (+) (divisors n) 0 in
  n = (sum - n)

let test = perfect 1000
