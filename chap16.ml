(* 問題 16.1 *)

(* 目的: 整数のリストを受け取り、それまでの数の合計からなるリストを返す *)
let sum_list lst =
  let rec sum_list_with_acc lst acc =
    match lst with
    | [] -> []
    | first :: rest ->
        let sum = first + acc in
        sum :: sum_list_with_acc rest sum
  in
  sum_list_with_acc lst 0

let test = sum_list [ 3 ] = [ 3 ]
let test = sum_list [ 3; 2 ] = [ 3; 5 ]
let test = sum_list [ 3; 2; 1; 4 ] = [ 3; 5; 6; 10 ]

let rec reverse lst =
  let rec rev lst result =
    match lst with [] -> result | first :: rest -> rev rest (first :: result)
  in rev lst []

let test = reverse [] = []
let test = reverse [ 1; 2; 3 ] = [ 3; 2; 1 ]

(* 問題 16.2 *)
let rec fold_left f init lst =
  match lst with
    [] -> init
  | first :: rest -> fold_left f (f init first) rest

let test = fold_left (fun sum x -> sum + x) 0 [1; 2; 3] = 6
let test = fold_left (fun result x -> x :: result) [] [1; 2; 3] = [3; 2; 1]
