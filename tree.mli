(* キーが 'a 値が 'b の木 *)
type ('a, 'b) t

(* 使い方: empty *)
(* 空の木を返す *)
val empty : ('a, 'b) t

(* 使い方: insert tree key value *)
(* 木 tree にキー key と値 value を挿入した木を返す *)
val insert : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t

(* 使い方: search tree key *)
(* 木 tree の中からキー key に対応する値を返す *)
val search : ('a, 'b) t -> 'a -> 'b
