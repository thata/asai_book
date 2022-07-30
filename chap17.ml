(* 問題 17.1 *)
type nengou_t =
    Meiji of int
  | Taisho of int
  | Showa of int
  | Heisei of int
  | Reiwa of int

let to_seireki nengou = match nengou with
    Meiji (n) -> n + 1867
  | Taisho (n) -> n + 1911
  | Showa (n) -> n + 1925
  | Heisei (n) -> n + 1988
  | Reiwa (n) -> n + 2018

let nenrei tanjou genzai =
  let tanjou_seireki = to_seireki tanjou in
  let genzai_seireki = to_seireki genzai in
  genzai_seireki - tanjou_seireki

let test = nenrei (Meiji 10) (Meiji 10) = 0
let test = nenrei (Heisei 26) (Reiwa 4) = 8

(* 問題 17.2 *)
type year_t =
    January of int
  | February of int
  | March of int
  | April of int
  | May of int
  | June of int
  | July of int
  | August of int
  | September of int
  | October of int
  | November of int
  | December of int

(* 問題 17.3 *)
type seiza_t =
    Aries (* 牡羊座 *)
  | Taurus (* 牡牛座 *)
  | Gemini (* 双子座 *)
  | Cancer (* 蟹座 *)
  | Leo (* 獅子座 *)
  | Virgo (* 乙女座 *)
  | Libra (* 天秤座 *)
  | Scorpius (* 蠍座 *)
  | Sagittarius (* 射手座 *)
  | Capricornus (* 山羊座 *)
  | Aquarius (* 水瓶座 *)
  | Pisces (* 魚座 *)

(* 問題 17.4 *)

(* seiza : year_t -> seiza_t *)
let seiza year =
  match year with
    | January (n) -> if n <= 19 then Capricornus else Aquarius
    | February (n) -> if n <= 18 then Aquarius else Pisces
    | March (n) -> if n <= 20 then Pisces else Aries
    | April (n) -> if n <= 19 then Aries else Taurus
    | May (n) -> if n <= 20 then Taurus else Gemini
    | June (n) -> if n <= 21 then Gemini else Cancer
    | July (n) -> if n <= 22 then Cancer else Leo
    | August (n) -> if n <= 22 then Leo else Virgo
    | September (n) -> if n <= 22 then Virgo else Libra
    | October (n) -> if n <= 23 then Libra else Scorpius
    | November (n) -> if n <= 22 then Scorpius else Sagittarius
    | December (n) -> if n <= 21 then Sagittarius else Capricornus

let test = seiza (April 19) = Aries
let test = seiza (April 20) = Taurus
let test = seiza (May 20) = Taurus
let test = seiza (May 21) = Gemini


(* 内部に整数しか持てない木 *)
(* type tree_t =
    Empty
  | Leaf of int
  | Node of tree_t * int * tree_t *)

(* 多相の木を表す型 *)
type 'a tree_t =
    Empty
  | Leaf of 'a
  | Node of 'a tree_t * 'a * 'a tree_t

let test = Empty
let test = Leaf (10)
let test = Node (Empty, 7, Leaf 3)

(* 問題 17.5 *)

(* 目的: 渡された木の値をすべて2倍の値にして返す *)
let rec tree_double tree =
  match tree with
      Empty -> Empty
    | Leaf (n) -> Leaf (n * 2)
    | Node (t1, n, t2) -> Node ((tree_double t1), n * 2, (tree_double t2))

let test = tree_double Empty = Empty
let test = tree_double (Leaf 0) = Leaf (0)
let test = tree_double (Node (Empty, 0, Empty)) = Node (Empty, 0, Empty)
let test = tree_double (Leaf 10) = Leaf (20)
let test = tree_double (Node (Empty, 10, Empty)) = Node (Empty, 20, Empty)
let test = tree_double (Node (Leaf (5), 10, Empty)) = Node (Leaf (10), 20, Empty)
let test = tree_double (Node (Leaf (5), 10, Node (Leaf 10, 20, Leaf 30))) = Node (Leaf (10), 20, Node (Leaf 20, 40, Leaf 60))


(* 問題 17.6 *)

(* 目的: int -> int 型の関数 f と tree_t を受け取ったら、Leaf や Node の値すべてに f を適用した木を返す *)
let rec tree_map f tree =
  match tree with
      Empty -> Empty
    | Leaf (n) -> Leaf (f n)
    | Node (t1, n, t2) -> Node ((tree_map f t1), f n, (tree_map f t2))

let test = tree_map (fun x -> x * x) Empty = Empty
let test = tree_map (fun x -> x * x) (Leaf 0) = Leaf (0)
let test = tree_map (fun x -> x * x) (Node (Empty, 0, Empty)) = Node (Empty, 0, Empty)
let test = tree_map (fun x -> x * x) (Leaf 10) = Leaf (100)
let test = tree_map (fun x -> x * x) (Node (Empty, 10, Empty)) = Node (Empty, 100, Empty)
let test = tree_map (fun x -> x * x) (Node (Leaf (5), 10, Empty)) = Node (Leaf (25), 100, Empty)
let test = tree_map (fun x -> x * x) (Node (Leaf (5), 10, Node (Leaf 10, 20, Leaf 30))) = Node (Leaf (25), 100, Node (Leaf 100, 400, Leaf 900))

(* 問題 17.7 *)

let rec tree_length tree =
  match tree with
      Empty -> 0
    | Leaf (n) -> 1
    | Node (t1, n, t2) -> 1 + (tree_length t1) + (tree_length t2)

let test = tree_length Empty = 0
let test = tree_length (Leaf 0) = 1
let test = tree_length (Node (Empty, 0, Empty)) = 1
let test = tree_length (Leaf 10) = 1
let test = tree_length (Node (Leaf (5), 10, Empty)) = 2
let test = tree_length (Node (Leaf (5), 10, Node (Leaf 10, 20, Leaf 30))) = 5



(* 目的: data が二分探索木に含まれているかどうかを返す *)
let rec search tree data =
  match tree with
      Empty -> false
    | Leaf (n) -> n = data
    | Node (t1, n, t2) ->
        if n = data then true
        else if data < n then search t1 data
        else search t2 data

let tree1 = Empty
let tree2 = Leaf (3)
let tree3 = Node (Leaf (1), 2, Leaf (3))
let tree4 = Node (Empty, 7, Leaf (9))
let tree5 = Node (tree3, 6, tree4)

(* テスト *)
let test = search tree1 3 = false
let test = search tree2 3 = true
let test = search tree2 4 = false
let test = search tree5 6 = true
let test = search tree5 2 = true
let test = search tree5 1 = true
let test = search tree5 4 = false
let test = search tree5 7 = true
let test = search tree5 8 = false


(* 目的: 二分木に新しい要素 data を追加する *)
let rec insert_tree tree data =
  match tree with
      Empty -> Leaf (data)
    | Leaf (n) ->
        if n = data then Leaf (n)
        else if n > data then Node (Leaf (data), n, Empty)
        else Node (Empty, n, Leaf (data))
    | Node (t1, n, t2) ->
        if n = 0 then Node (t1, n, t2)
        else if n > data then Node ((insert_tree t1 data), n, t2)
        else Node (t1, n, (insert_tree t2 data))

let test = insert_tree Empty 3 = Leaf (3)
let test = insert_tree (Leaf (3)) 2 = Node ((Leaf 2), 3, Empty)
let test = insert_tree (Leaf (3)) 3 = Leaf (3)
let test = insert_tree (Leaf (3)) 4 = Node (Empty, 3, Leaf (4))
let test = insert_tree tree5 4 = Node (Node (Leaf (1), 2, Node (Empty, 3, Leaf (4))), 6, Node (Empty, 7, Leaf (9)))

(* 説明: 二分木内の値の合計を返す *)
let rec sum_tree tree =
  match tree with
      Empty -> 0
    | Leaf (n) -> n
    | Node (t1, n, t2) -> (sum_tree t1) + n + (sum_tree t2)
