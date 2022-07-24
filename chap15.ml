let arr = [5; 4; 9; 8; 2; 3]
let p = match arr with first :: _ -> first | _ -> failwith "err"

let rec qsort lst =
  match lst with
  | [] -> []
  | first :: rest ->
    let pivot = first in
    let a = List.filter (fun x -> x < pivot) rest in
    let b = List.filter (fun x -> x >= pivot) rest in
    (qsort a) @ [pivot] @ (qsort b)
