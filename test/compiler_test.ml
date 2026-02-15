(* open Alcotest *)
(* open Eociaml *)

(* let test_uniquify_shadowing () =
  let gensym = Compiler.make_gensym () in
  let input = `Let ("x", `Lit 32, `BinApp (`Add, `Let ("x", `Lit 10, `Var "x"), `Var "x")) in
  let expected = `Let ("x.1", `Lit 32, `BinApp (`Add, `Let ("x.2", `Lit 10, `Var "x.2"), `Var "x.1")) in
  assert (Compiler.uniquify ~gensym input = expected)
;;

let test_uniquify_shadowing_2 () =
  let gensym = Compiler.make_gensym () in
  let input = `Let ("x", `Let ("x", `Lit 4, `BinApp (`Add, `Var "x", `Lit 1)), `BinApp (`Add, `Var "x", `Lit 2)) in
  let expected = `Let ("x.1", `Let ("x.2", `Lit 4, `BinApp (`Add, `Var "x.2", `Lit 1)), `BinApp (`Add, `Var "x.1", `Lit 2)) in
  assert (Compiler.uniquify ~gensym input = expected)
;;

let test_rco_nested_binapp () =
  let gensym = Compiler.make_gensym () in
  (* (+ (+ 1 2) 3) *)
  let input = `BinApp (`Add, `BinApp (`Add, `Lit 1, `Lit 2), `Lit 3) in
  (* (let [tmp.1 (+ 1 2)] (+ tmp.1 3)) *)
  let expected = `Let ("tmp.1", `BinApp (`Add, `Lit 1, `Lit 2), `BinApp (`Add, `Var "tmp.1", `Lit 3)) in
  assert (Compiler.remove_complex_operands ~gensym input = expected)
;;

let test_rco_simple_binapp () =
  let gensym = Compiler.make_gensym () in
  let input = `BinApp (`Add, `Lit 1, `Lit 2) in
  assert (Compiler.remove_complex_operands ~gensym input = input)
;;

let test_rco_unapp_complex () =
  let gensym = Compiler.make_gensym () in
  (* (- (+ 1 2)) *)
  let input = `UnApp (`Neg, `BinApp (`Add, `Lit 1, `Lit 2)) in
  (* (let [tmp.1 (+ 1 2)] (- tmp.1)) *)
  let expected = `Let ("tmp.1", `BinApp (`Add, `Lit 1, `Lit 2), `UnApp (`Neg, `Var "tmp.1")) in
  assert (Compiler.remove_complex_operands ~gensym input = expected)
;;

let test_rco_both_complex () =
  let gensym = Compiler.make_gensym () in
  (* (+ (read) (read)) *)
  let input = `BinApp (`Add, `NulApp `Read, `NulApp `Read) in
  (* (let [tmp.1 (read)] (let [tmp.2 (read)] (+ tmp.1 tmp.2))) *)
  let expected = `Let ("tmp.1", `NulApp `Read, `Let ("tmp.2", `NulApp `Read, `BinApp (`Add, `Var "tmp.1", `Var "tmp.2"))) in
  assert (Compiler.remove_complex_operands ~gensym input = expected)
;;

let test_rco_let_unchanged () =
  let gensym = Compiler.make_gensym () in
  let input = `Let ("a", `Lit 42, `Let ("b", `Var "a", `Var "b")) in
  let expected = `Let ("a", `Atom (`Lit 42), `Let ("b", `Atom (`Var "a"), `Atom (`Var "b"))) in
  assert (Compiler.remove_complex_operands ~gensym input = expected)
;;

let test_explicate_control () =
  let gensym = Compiler.make_gensym () in
  let input = `Let ("y", `Let ("x", `Lit 20, `BinApp (`Add, `Var "x", `Let ("x", `Lit 22, `Var "x"))), `Var "y") in
  let expected =
    `Seq
      ( `Assign ("x.2", `Atom (`Lit 20))
      , `Seq
          ( `Assign ("x.3", `Atom (`Lit 22))
          , `Seq (`Assign ("y.1", `BinApp (`Add, `Var "x.2", `Var "x.3")), `Return (`Atom (`Var "y.1"))) ) )
  in
  assert (input |> Compiler.uniquify ~gensym |> Compiler.remove_complex_operands ~gensym |> Compiler.explicate_control = expected)
;;

let test_select_return_lit () =
  let input = `Return (`Atom (`Lit 42)) in
  let expected = [ `movq (`Imm 42, `Reg `rax) ] in
  assert (Compiler.select_instructions input = expected)
;;

let test_select_return_add () =
  (* return (+ 1 2) *)
  let input = `Return (`BinApp (`Add, `Lit 1, `Lit 2)) in
  (* movq $1, %rax; addq $2, %rax *)
  let expected = [ `movq (`Imm 1, `Reg `rax); `addq (`Imm 2, `Reg `rax) ] in
  assert (Compiler.select_instructions input = expected)
;;

let test_select_assign_and_return () =
  (* x = 32; return x *)
  let input = `Seq (`Assign ("x", `Atom (`Lit 32)), `Return (`Atom (`Var "x"))) in
  (* movq $32, x; movq x, %rax *)
  let expected = [ `movq (`Imm 32, `Var "x"); `movq (`Var "x", `Reg `rax) ] in
  assert (Compiler.select_instructions input = expected)
;;

let test_select_neg () =
  (* return (- 42) *)
  let input = `Return (`UnApp (`Neg, `Lit 42)) in
  (* movq $42, %rax; negq %rax *)
  let expected = [ `movq (`Imm 42, `Reg `rax); `negq (`Reg `rax) ] in
  assert (Compiler.select_instructions input = expected)
;;

let test_select_read () =
  (* return (read) *)
  let input = `Return (`NulApp `Read) in
  (* callq read_int; movq %rax, %rax *)
  let expected = [ `callq "read_int"; `movq (`Reg `rax, `Reg `rax) ] in
  assert (Compiler.select_instructions input = expected)
;;

let test_select_sub () =
  (* return (- 10 3) *)
  let input = `Return (`BinApp (`Sub, `Lit 10, `Lit 3)) in
  (* movq $10, %rax; subq $3, %rax *)
  let expected = [ `movq (`Imm 10, `Reg `rax); `subq (`Imm 3, `Reg `rax) ] in
  assert (Compiler.select_instructions input = expected)
;;

let test_assign_homes_book_example () =
  (* (let ([a 42]) (let ([b a]) b)) after select_instructions *)
  (* movq $42, a; movq a, b; movq b, %rax *)
  let input = [ `movq (`Imm 42, `Var "a"); `movq (`Var "a", `Var "b"); `movq (`Var "b", `Reg `rax) ] in
  (* movq $42, -8(%rbp); movq -8(%rbp), -16(%rbp); movq -16(%rbp), %rax *)
  let expected =
    [ `movq (`Imm 42, `Deref (-8, `Reg `rbp))
    ; `movq (`Deref (-8, `Reg `rbp), `Deref (-16, `Reg `rbp))
    ; `movq (`Deref (-16, `Reg `rbp), `Reg `rax)
    ]
  in
  let instrs, frame_size = Compiler.assign_homes input in
  assert (instrs = expected);
  assert (frame_size = 16)
;;

let test_assign_homes_single_var () =
  let input = [ `movq (`Imm 10, `Var "x"); `negq (`Var "x"); `movq (`Var "x", `Reg `rax) ] in
  let expected =
    [ `movq (`Imm 10, `Deref (-8, `Reg `rbp)); `negq (`Deref (-8, `Reg `rbp)); `movq (`Deref (-8, `Reg `rbp), `Reg `rax) ]
  in
  let instrs, frame_size = Compiler.assign_homes input in
  assert (instrs = expected);
  assert (frame_size = 8)
;;

let test_patch_mem_to_mem () =
  (* movq -8(%rbp), -16(%rbp) => movq -8(%rbp), %rax; movq %rax, -16(%rbp) *)
  let input = [ `movq (`Deref (-8, `Reg `rbp), `Deref (-16, `Reg `rbp)) ] in
  let expected = [ `movq (`Deref (-8, `Reg `rbp), `Reg `rax); `movq (`Reg `rax, `Deref (-16, `Reg `rbp)) ] in
  assert (Compiler.patch_instructions input = expected)
;;

let test_patch_no_change () =
  (* Instructions without mem-to-mem pass through unchanged *)
  let input = [ `movq (`Imm 42, `Deref (-8, `Reg `rbp)); `movq (`Deref (-8, `Reg `rbp), `Reg `rax) ] in
  assert (Compiler.patch_instructions input = input)
;;

let test_patch_book_example () =
  (* Full book example after assign_homes *)
  let input =
    [ `movq (`Imm 42, `Deref (-8, `Reg `rbp))
    ; `movq (`Deref (-8, `Reg `rbp), `Deref (-16, `Reg `rbp))
    ; `movq (`Deref (-16, `Reg `rbp), `Reg `rax)
    ]
  in
  let expected =
    [ `movq (`Imm 42, `Deref (-8, `Reg `rbp))
    ; `movq (`Deref (-8, `Reg `rbp), `Reg `rax)
    ; `movq (`Reg `rax, `Deref (-16, `Reg `rbp))
    ; `movq (`Deref (-16, `Reg `rbp), `Reg `rax)
    ]
  in
  assert (Compiler.patch_instructions input = expected)
;;

let test_prelude_and_conclusion_basic () =
  let body = [ `movq (`Imm 42, `Reg `rax) ] in
  let result = Compiler.prelude_and_conclusion ~frame_size:8 body in
  (* frame_size 8 -> aligned to 16 *)
  let expected =
    [ "main", [ `pushq (`Reg `rbp); `movq (`Reg `rsp, `Reg `rbp); `subq (`Imm 16, `Reg `rsp); `jmp "start" ]
    ; "start", [ `movq (`Imm 42, `Reg `rax); `jmp "conclusion" ]
    ; "conclusion", [ `addq (`Imm 16, `Reg `rsp); `popq (`Reg `rbp); `retq ]
    ]
  in
  assert (result = expected)
;;

let test_prelude_and_conclusion_no_stack () =
  let body = [ `movq (`Imm 42, `Reg `rax) ] in
  let result = Compiler.prelude_and_conclusion ~frame_size:0 body in
  let expected =
    [ "main", [ `pushq (`Reg `rbp); `movq (`Reg `rsp, `Reg `rbp); `jmp "start" ]
    ; "start", [ `movq (`Imm 42, `Reg `rax); `jmp "conclusion" ]
    ; "conclusion", [ `popq (`Reg `rbp); `retq ]
    ]
  in
  assert (result = expected)
;;

let () =
  run
    "Compiler"
    [ ( "uniquify"
      , [ test_case "shadowing" `Quick test_uniquify_shadowing; test_case "shadowing 2" `Quick test_uniquify_shadowing_2 ] )
    ; ( "rco"
      , [ test_case "nested binapp" `Quick test_rco_nested_binapp
        ; test_case "simple binapp" `Quick test_rco_simple_binapp
        ; test_case "unapp complex" `Quick test_rco_unapp_complex
        ; test_case "both complex" `Quick test_rco_both_complex
        ; test_case "let unchanged" `Quick test_rco_let_unchanged
        ] )
    ; "explicate_control", [ test_case "cvar" `Quick test_explicate_control ]
    ; ( "select_instructions"
      , [ test_case "return lit" `Quick test_select_return_lit
        ; test_case "return add" `Quick test_select_return_add
        ; test_case "assign and return" `Quick test_select_assign_and_return
        ; test_case "neg" `Quick test_select_neg
        ; test_case "read" `Quick test_select_read
        ; test_case "sub" `Quick test_select_sub
        ] )
    ; ( "assign_homes"
      , [ test_case "book example" `Quick test_assign_homes_book_example
        ; test_case "single var" `Quick test_assign_homes_single_var
        ] )
    ; ( "patch_instructions"
      , [ test_case "mem to mem" `Quick test_patch_mem_to_mem
        ; test_case "no change" `Quick test_patch_no_change
        ; test_case "book example" `Quick test_patch_book_example
        ] )
    ; ( "prelude_and_conclusion"
      , [ test_case "basic" `Quick test_prelude_and_conclusion_basic
        ; test_case "no stack" `Quick test_prelude_and_conclusion_no_stack
        ] )
    ]
;; *)
