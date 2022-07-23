(* 駅名 *)
type ekimei_t = {
  kanji: string;
  kana: string;
  romaji: string;
  shozoku: string
}

(* 駅間 *)
type ekikan_t = {
  kiten: string; (* 起点の駅名 *)
  shuten: string; (* 終点の駅名 *)
  keiyu: string; (* 経由する路線名 *)
  kyori: float; (* 距離（km、実数）*)
  jikan: int (* 所要時間 （分、整数）*)
}

(* 渡された ekimei_t の「路線名、駅名（かな）」を返す *)
(* let hyouji ekimei = ekimei_t -> string *)
let hyouji ekimei = match ekimei with
  { kanji = kj; kana = kn; shozoku = s } -> s ^ "、" ^ kj ^ "（" ^ kn ^ "）"

let eki1 = {kanji = "茗荷谷"; kana = "みょうがだに"; romaji = "MYOGADANI"; shozoku = "丸ノ内線"};;

let hyouji_test1 = hyouji eki1 = "丸ノ内線、茗荷谷（みょうがだに）"
