(* ２分探索木を表すモジュール *)
module Tree = struct
  type ('a, 'b) t = Empty
                  | Node of ('a, 'b) t * 'a * 'b * ('a, 'b) t

  (* 空の木 *)
  let empty = Empty

  (* 目的: tree にキー k で値が v なノードを挿入した木を返す *)
  (* insert : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t *)
  let rec insert tree k v = match tree with
      Empty -> Node (Empty, k, v, Empty)
    | Node (t1, k0, v0, t2) ->
        if k = k0 then Node (t1, k, v, t2)
        else
          if k < k0 then Node ((insert t1 k v), k0, v0, t2)
          else Node (t1, k0, v0, (insert t2 k v))

  (* 目的: tree からキー k に対応する値 v を検索して返す *)
  (* 見つからなければ Not_found 例外を投げる *)
  let rec search tree k = match tree with
      Empty -> raise Not_found
    | Node (t1, k0, v0, t2) ->
        if k = k0 then v0
        else
          if k < k0 then search t1 k
          else search t2 k
end

open Tree

(* insert のテスト *)

let t0 = empty
let test = t0 = Empty
let t1 = insert empty "i" 10
let test = t1 = Node (Empty, "i", 10, Empty)
let t2 = insert empty "i" 11
let test = t2 = Node (Empty, "i", 11, Empty)
let t3 = insert t2 "e" 20
let test = t3 = Node (Node (Empty, "e", 20, Empty), "i", 11, Empty)
let t4 = insert t3 "k" 30
let test = t4 = Node (Node (Empty, "e", 20, Empty), "i", 11, Node (Empty, "k", 30, Empty))
let t5 = insert t4 "g" 40
let test = t5 = Node (Node (Empty, "e", 20, Node (Empty, "g", 40, Empty)), "i", 11, Node (Empty, "k", 30, Empty))
let t6 = insert t5 "a" 50
let test = t6 = Node (Node (Node (Empty, "a", 50, Empty), "e", 20, Node (Empty, "g", 40, Empty)), "i", 11, Node (Empty, "k", 30, Empty))

(* search のテスト *)

let test = (try search empty "a" with Not_found -> 9999) = 9999
let test = search t1 "i" = 10
let test = search t6 "k" = 30
let test = search t6 "g" = 40
let test = search t6 "a" = 50
let test = (try search t6 "z" with Not_found -> 9999) = 9999
