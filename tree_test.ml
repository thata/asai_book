open Tree

let () =
  let tree = insert empty "a" 10 in
  let n = search tree "a" in
  print_int n;
  print_newline ()
