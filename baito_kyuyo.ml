(* baito_kyuyo.ml *)
let baito_kyuyo n j =
  let jikyu = 850 + (n * 100) in
  jikyu * j

let () =
  assert ((baito_kyuyo 0 0) = 0);
  assert ((baito_kyuyo 0 1) = 850);
  assert ((baito_kyuyo 1 1) = 950);
  assert ((baito_kyuyo 1 2) = 1900);
  print_string "ok";
  print_newline ()
