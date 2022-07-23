(* 目的: 鶴の匹数 n に応じた鶴の足の数を計算する *)
(* tsuru_no_ashi n = int -> int *)
let tsuru_no_ashi n = n * 2

(* 目的: 亀の匹数 n に応じた亀の足の数を計算する *)
(* kame_no_ashi n = int -> int *)
let kame_no_ashi n = n * 4

(* 目的: 鶴の匹数 t と亀の匹数 k に応じた鶴亀の足の数を計算する *)
(* tsurukame_no_ashi t k = int -> int -> int *)
let tsurukame_no_ashi t k = (tsuru_no_ashi t) + (kame_no_ashi k)

(* 目的: 鶴亀の匹数 n と鶴亀の足の数 a から鶴の数を計算する *)
(* tsurukame n a = int -> int -> int *)
let rec tsurukame n a =
  if (tsurukame_no_ashi n 0) = a then
    n
  else
    tsurukame (n - 1) (a - (kame_no_ashi 1))

let test1 = (tsuru_no_ashi 0) = 0
let test2 = tsuru_no_ashi 1 = 2
let test3 = tsuru_no_ashi 2 = 4
let test4 = kame_no_ashi 0 = 0
let test5 = kame_no_ashi 1 = 4
let test6 = kame_no_ashi 2 = 8
let test7 = tsurukame_no_ashi 0 0 = 0
let test8 = tsurukame_no_ashi 1 0 = 2
let test9 = tsurukame_no_ashi 0 1 = 4
let test10 = tsurukame_no_ashi 1 1 = 6
let test11 = tsurukame_no_ashi 2 2 = 12
let test12 = tsurukame 0 0 = 0
let test13 = tsurukame 1 2 = 1
let test14 = tsurukame 1 4 = 0
let test15 = tsurukame 2 6 = 1
let test16 = tsurukame 3 6 = 3
let test17 = tsurukame 3 10 = 1
let test18 = tsurukame 3 12 = 0
