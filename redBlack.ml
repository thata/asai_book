type color_t = Red | Black

type ('a, 'b) t = Empty
                        | Node of ('a, 'b) t * 'a * 'b * color_t * ('a, 'b) t

let empty = Empty

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

(* 目的: 木とキーを受け取ったら、そのキーに対応する値を返す *)
let rec search tree key =
  match tree with
      Empty -> raise Not_found
    | Node (t1, k, v, _, t2) ->
        if k = key then v
        else if key > k then search t2 key
        else search t1 key
