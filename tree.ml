(* ２分探索木モジュール *)

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
