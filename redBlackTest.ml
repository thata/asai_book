(* open RedBlack *)

let () =
  let tree = RedBlack.insert RedBlack.empty "a" 10 in
  let n = RedBlack.search tree "a" in
  print_int n;
  print_newline ()
