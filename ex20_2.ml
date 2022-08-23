#use "ex20_1.ml"

(* 目的: 赤黒木のバランスをとる。偏っていない場合はそのままの木を返す *)
let balance t =
  match t with
      Node (a, xk, xv, Black, Node (b, yk, yv, Red, Node (c, zk, zv, Red, d)))
    | Node (a, xk, xv, Black, Node (Node (b, yk, yv, Red, c), zk, zv, Red, d))
    | Node (Node (a, xk, xv, Red, Node (b, yk, yv, Red, c)), zk, zv, Black, d)
    | Node (Node (Node (a, xk, xv, Red, b), yk, yv, Red, c), zk, zv, Black, d)
        -> Node (Node (a, xk, xv, Black, b), yk, yv, Red, Node (c, zk, zv, Black, d))
    | _
        -> t

(* let t0 = Empty
let test = balance t0 = Empty
let t1 = (Node (Empty, "x", 10, Black, Empty))
let test =
  balance t1 = Node (Empty, "x", 10, Black, Empty)
let t2 = (Node (Empty, "x", 10, Black, (Node (Empty, "b", 20, Red, Empty))))
let test =
  balance t2 = Node (Empty, "x", 10, Black, (Node (Empty, "b", 20, Red, Empty)))
let t3 = Node (Empty, "x", 10, Black, (Node (Empty, "b", 20, Red, Empty)))
let test =
  balance t3 = Node (Empty, "x", 10, Black, (Node (Empty, "b", 20, Red, Empty)))
let t4 = Node (Empty, "x", 10, Black, (Node (Empty, "b", 20, Red, (Node (Empty, "c", 30, Red, Empty)))))
let test =
  balance t4 = Node (Node (Empty, "x", 10, Black, Empty), "y", 20, Red, Node (Empty, "z", 30, Black, Empty))
let t5 = Node (Empty, "x", 10, Black, (Node ((Node (Empty, "y", 20, Red, Empty)), "z", 30, Red, Empty)))
let test =
  balance t5 = Node (Node (Empty, "x", 10, Black, Empty), "y", 20, Red, Node (Empty, "z", 30, Black, Empty))
let t6 = Node ((Node (Empty, "x", 10, Red, (Node (Empty, "y", 20, Red, Empty)))), "z", 30, Black, Empty)
let test =
  balance t6 = Node (Node (Empty, "x", 10, Black, Empty), "y", 20, Red, Node (Empty, "z", 30, Black, Empty))
let t7 = Node ((Node ((Node (Empty, "x", 10, Red, Empty), "y", 20, Red, Empty))), "z", 30, Black, Empty)
let test =
  balance t7 = Node (Node (Empty, "x", 10, Black, Empty), "y", 20, Red, Node (Empty, "z", 30, Black, Empty))

(* X, Y, Z にぶら下がる a, b, c, d のテストも行う *)
let ta = Node (Empty, "a", 999, Black, Empty)
let tb = Node (Empty, "b", 999, Black, Empty)
let tc = Node (Empty, "c", 999, Black, Empty)
let td = Node (Empty, "d", 999, Black, Empty)
let t8 = Node ((Node ((Node (ta, "x", 10, Red, tb), "y", 20, Red, tc))), "z", 30, Black, td)
let test =
  balance t8 = Node (Node (ta, "x", 10, Black, tb), "y", 20, Red, Node (tc, "z", 30, Black, td))
let t9 = Node ((Node (ta, "x", 10, Red, (Node (tb, "y", 20, Red, tc)))), "z", 30, Black, td)
let test =
  balance t9 = Node (Node (ta, "x", 10, Black, tb), "y", 20, Red, Node (tc, "z", 30, Black, td))

let t10 = Node (ta, "x", 10, Black, (Node ((Node (tb, "y", 20, Red, tc)), "z", 30, Red, td)))
let test =
  balance t10 = Node (Node (ta, "x", 10, Black, tb), "y", 20, Red, Node (tc, "z", 30, Black, td))
let t11 = Node (ta, "x", 10, Black, (Node (tb, "y", 20, Red, (Node (tc, "z", 30, Red, td)))))
let test =
  balance t11 = Node (Node (ta, "x", 10, Black, tb), "y", 20, Red, Node (tc, "z", 30, Black, td)) *)
