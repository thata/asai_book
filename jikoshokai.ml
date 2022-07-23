(* jikoshokai.ml *)
let jikoshokai name =
  "ワタシ ノ ナマエハ " ^ name ^ "。コンゴトモ ヨロシク。"

let () =
  print_string (jikoshokai "chikuwax");
  print_newline ()
