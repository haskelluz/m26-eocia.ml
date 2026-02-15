(* open Alcotest
open Eociaml.Interpreter *)

(* let eval' = eval Env.empty
let test_lit () = assert (eval' (Lit 42) = 42)

let test_binapp_add () =
  (* (+ 32 10) => 42 *)
  assert (eval' (BinApp (Add, Lit 32, Lit 10)) = 42)
;;

let test_binapp_nested () =
  (* (+ (+ 1 2) (- 42 3)) => 42 *)
  let input = BinApp (Add, BinApp (Add, Lit 1, Lit 2), BinApp (Sub, Lit 42, Lit 3)) in
  assert (eval' input = 42)
;;

let test_unapp_neg () =
  (* (- (- 42)) => 42 *)
  assert (eval' (UnApp (Neg, UnApp (Neg, Lit 42))) = 42)
;;

let test_let_simple () =
  (* (let [x 42] x) => 42 *)
  assert (eval' (Let ("x", Lit 42, Var "x")) = 42)
;;

let test_let_nested () =
  (* (let [x 32] (let [y 10] (+ x y))) => 42 *)
  let input = Let ("x", Lit 32, Let ("y", Lit 10, BinApp (Add, Var "x", Var "y"))) in
  assert (eval' input = 42)
;;

let test_let_shadowing () =
  (* (let [x 1] (let [x 42] x)) => 42 *)
  let input = Let ("x", Lit 1, Let ("x", Lit 42, Var "x")) in
  assert (eval' input = 42)
;;

let test_let_shadowing_outer () =
  (* (let [x 32] (+ (let [x 10] x) x)) => 42 *)
  let input = Let ("x", Lit 32, BinApp (Add, Let ("x", Lit 10, Var "x"), Var "x")) in
  assert (eval' input = 42)
;;

let () =
  run
    "Interpreter"
    [ ( "eval"
      , [ test_case "lit" `Quick test_lit
        ; test_case "binapp add" `Quick test_binapp_add
        ; test_case "binapp nested" `Quick test_binapp_nested
        ; test_case "unapp neg" `Quick test_unapp_neg
        ; test_case "let simple" `Quick test_let_simple
        ; test_case "let nested" `Quick test_let_nested
        ; test_case "let shadowing" `Quick test_let_shadowing
        ; test_case "let shadowing outer" `Quick test_let_shadowing_outer
        ] )
    ]
;; *)
