#use "ex20_2.ml"

(* 目的: 木とキーと値を受け取ったら、その木へキーと値を挿入した赤黒木を返す *)
let insert t k v =
  let turnB t = match t with
      Node (t1, k, v, c, t2) -> Node (t1, k, v, Black, t2)
    | _ -> assert false in
  let rec ins t k v = match t with
      Empty -> Node (Empty, k, v, Red, Empty)
    | Node (t1, k0, v0, c0, t2)
        ->
          if k < k0 then balance (Node ((ins t1 k v), k0, v0, c0, t2))
          else if k > k0 then balance (Node (t1, k0, v0, c0, (ins t2 k v)))
          else Node (t1, k, v, c0, t2) in
  turnB (ins t k v)

let t0 = Empty
let t1 = insert t0 1 "dummy"
let test = t1 = Node (Empty, 1, "dummy", Black, Empty)
let t2 = insert t1 2 "dummy"
let test = t2 = Node (Empty, 1, "dummy", Black, Node (Empty, 2, "dummy", Red, Empty))
let t3 = insert t2 3 "dummy"
let test = t3 = Node (Node (Empty, 1, "dummy", Black, Empty), 2, "dummy", Black, Node (Empty, 3, "dummy", Black, Empty))
let t4 = insert t3 4 "dummy"
let test = t4 = Node (Node (Empty, 1, "dummy", Black, Empty), 2, "dummy", Black, Node (Empty, 3, "dummy", Black, Node (Empty, 4, "dummy", Red, Empty)))
let t5 = insert t4 5 "dummy"
let test = t5 = Node (Node (Empty, 1, "dummy", Black, Empty), 2, "dummy", Black, Node (Node (Empty, 3, "dummy", Black, Empty), 4, "dummy", Red, Node (Empty, 5, "dummy", Black, Empty)))
let t6 = insert t5 6 "dummy"
let test = t6 = Node (Node (Empty, 1, "dummy", Black, Empty), 2, "dummy", Black, Node (Node (Empty, 3, "dummy", Black, Empty), 4, "dummy", Red, Node (Empty, 5, "dummy", Black, Node (Empty, 6, "dummy", Red, Empty))))
let t7 = insert t6 7 "dummy"
let test = t7 = Node (Node (Node (Empty, 1, "dummy", Black, Empty), 2, "dummy", Black, Node (Empty, 3, "dummy", Black, Empty)), 4, "dummy", Black, Node (Node (Empty, 5, "dummy", Black, Empty), 6, "dummy", Black, Node (Empty, 7, "dummy", Black, Empty)))
let t8 = insert t7 4 "DUMMY"
let test = t8 = Node (Node (Node (Empty, 1, "dummy", Black, Empty), 2, "dummy", Black, Node (Empty, 3, "dummy", Black, Empty)), 4, "DUMMY", Black, Node (Node (Empty, 5, "dummy", Black, Empty), 6, "dummy", Black, Node (Empty, 7, "dummy", Black, Empty)))
let t9 = insert t8 0 "dummy"
let test = t9 = Node (Node (Node (Node (Empty, 0, "dummy", Red, Empty), 1, "dummy", Black, Empty), 2, "dummy", Black, Node (Empty, 3, "dummy", Black, Empty)), 4, "DUMMY", Black, Node (Node (Empty, 5, "dummy", Black, Empty), 6, "dummy", Black, Node (Empty, 7, "dummy", Black, Empty)))
let t10 = insert t9 (-1) "dummy"
let test = t10 = Node (Node (Node (Node (Empty, -1, "dummy", Black, Empty), 0, "dummy", Red, Node (Empty, 1, "dummy", Black, Empty)), 2, "dummy", Black, Node (Empty, 3, "dummy", Black, Empty)), 4, "DUMMY", Black, Node (Node (Empty, 5, "dummy", Black, Empty), 6, "dummy", Black, Node (Empty, 7, "dummy", Black, Empty)))
