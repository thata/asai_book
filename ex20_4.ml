#use "ex20_3.ml"

(* 目的: 木とキーを受け取ったら、そのキーに対応する値を返す *)
let rec search tree key =
  match tree with
      Empty -> raise Not_found
    | Node (t1, k, v, _, t2) ->
        if k = key then v
        else if key > k then search t2 key
        else search t1 key

let t0 = Empty
let t1 = insert t0 1 "1"
let t2 = insert t1 2 "10"
let t3 = insert t2 3 "11"
let t4 = insert t3 4 "100"
let t5 = insert t4 5 "101"
let t6 = insert t5 6 "110"
let t7 = insert t6 7 "111"
let test = try search t0 1 with Not_found -> 999 = 999
let test = search t1 1 = "1"
let test = search t3 2 = "10"
let test = search t3 1 = "1"
let test = search t3 3 = "11"
let test = search t7 7  = "111"
