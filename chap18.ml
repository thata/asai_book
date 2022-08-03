let yaoya_list = [("トマト", 300); ("たまねぎ", 200);
                  ("にんじん", 150); ("ほうれん草", 200)]

(* 目的: item の値段を返す *)
let rec price item yaoya_list = match yaoya_list with
    [] -> None
  | (namae, nedan) :: rest ->
      if item = namae then Some (nedan)
      else price item rest

let test = price "トマト" yaoya_list = Some (300)
let test = price "ほうれん草" yaoya_list = Some (200)
let test = price "つけ麺" yaoya_list = None


(* yasai_list を買った時の合計額を返す *)
let rec total_price yasai_list yaoya_list = match yasai_list with
    [] -> 0
  | first :: rest ->
      match price first yaoya_list with
          None -> total_price rest yaoya_list
        | Some (p) -> p + (total_price rest yaoya_list)

let test = total_price [] yaoya_list = 0
let test = total_price ["トマト"] yaoya_list = 300
let test = total_price ["なめこ"] yaoya_list = 0
let test = total_price ["トマト"; "なめこ"] yaoya_list = 300
let test = total_price ["トマト"; "なめこ"; "ほうれん草"] yaoya_list = 500


(* 売り切れを示す例外 *)
exception Urikire

(* 目的: itemの値段を返す *)
(* 見つからない時は Urikire 例外を投げる *)
(* price : string -> (string * int) list -> int *)
let rec price item yaoya_list = match yaoya_list with
    [] -> raise Urikire
  | (namae, nedan) :: rest ->
      if item = namae then nedan
      else price item rest

(* 目的: yasai_list を買った時の値段の合計を調べる *)
(* total_price: string list -> (string * int) list -> int *)
let total_price yasai_list yaoya_list =
  let rec _total_price yasai_list yaoya_list = match yasai_list with
      [] -> 0
    | first :: rest -> price first yaoya_list + (_total_price rest yaoya_list)
  in
  try _total_price yasai_list yaoya_list with
      Urikire -> 0

let test = total_price [] yaoya_list = 0
let test = total_price ["トマト"] yaoya_list = 300
let test = total_price ["なめこ"] yaoya_list = 0
let test = total_price ["トマト"; "なめこ"] yaoya_list = 0
let test = total_price ["トマト"; "ほうれん草"] yaoya_list = 500
