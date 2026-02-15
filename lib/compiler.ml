let make_gensym () =
  let counter = ref 0 in
  fun prefix ->
    incr counter;
    prefix ^ string_of_int !counter
;;

module Env = Map.Make (String)

let uniquify gensym expr =
  let rec loop env expr =
    match expr with
    | `Lit n -> `Lit n
    | `NulApp op -> `NulApp op
    | `UnApp (op, a) -> `UnApp (op, loop env a)
    | `BinApp (op, a, b) -> `BinApp (op, loop env a, loop env b)
    | `Let (name, value, body) ->
      let name2 = gensym (name ^ ".") in
      `Let (name2, loop env value, loop (Env.add name name2 env) body)
    | `Var name -> `Var (Env.find name env)
  in
  loop Env.empty expr
;;

let remove_complex_operands gensym expr =
  let wrap binds expr = List.fold_right (fun (name, value) acc -> `Let (name, value, acc)) binds expr in
  let rec rco_atom expr =
    match expr with
    | `Lit n -> [], `Lit n
    | `Var n -> [], `Var n
    | `Let (name, value, body) ->
      let binds, atom = rco_atom body in
      (name, rco_expr value) :: binds, atom
    | other ->
      let tmp = gensym "tmp." in
      [ tmp, rco_expr other ], `Var tmp
  and rco_expr expr =
    match expr with
    | `Lit n -> `Atom (`Lit n)
    | `Var n -> `Atom (`Var n)
    | `Let (name, value, body) -> `Let (name, rco_expr value, rco_expr body)
    | `NulApp op -> `NulApp op
    | `UnApp (op, a) ->
      let binds, a2 = rco_atom a in
      wrap binds (`UnApp (op, a2))
    | `BinApp (op, a, b) ->
      let binds_a, a2 = rco_atom a in
      let binds_b, b2 = rco_atom b in
      wrap (binds_a @ binds_b) (`BinApp (op, a2, b2))
  in
  rco_expr expr
;;

let rec explicate_control expr =
  let rec explicate_assign name expr cont =
    match expr with
    | `Atom atom -> `Seq (`Assign (name, `Atom atom), cont)
    | `Let (name2, value2, body) -> explicate_assign name2 value2 (explicate_assign name body cont)
    | `NulApp op -> `Seq (`Assign (name, `NulApp op), cont)
    | `UnApp (op, a) -> `Seq (`Assign (name, `UnApp (op, a)), cont)
    | `BinApp (op, a, b) -> `Seq (`Assign (name, `BinApp (op, a, b)), cont)
  in
  match expr with
  | `Atom atom -> `Return (`Atom atom)
  | `Let (name, value, body) -> explicate_assign name value (explicate_control body)
  | `NulApp op -> `Return (`NulApp op)
  | `UnApp (op, a) -> `Return (`UnApp (op, a))
  | `BinApp (op, a, b) -> `Return (`BinApp (op, a, b))
;;

let select_instructions cvar =
  let select_arg atom =
    match atom with
    | `Lit n -> `Imm n
    | `Var n -> `Var n
  in
  let select_expr dest expr =
    match expr with
    | `Atom a -> [ `movq (select_arg a, dest) ]
    | `NulApp `Read -> [ `callq "read_int"; `movq (`Reg `rax, dest) ]
    | `UnApp (`Neg, a) -> [ `movq (select_arg a, dest); `negq dest ]
    | `BinApp (`Add, a, b) -> [ `movq (select_arg a, dest); `addq (select_arg b, dest) ]
    | `BinApp (`Sub, a, b) -> [ `movq (select_arg a, dest); `subq (select_arg b, dest) ]
  in
  let rec select_tail cvar =
    match cvar with
    | `Return expr -> select_expr (`Reg `rax) expr
    | `Seq (`Assign (name, value), cont) -> select_expr (`Var name) value @ select_tail cont
  in
  select_tail cvar
;;

let compile expr =
  let gensym = make_gensym () in
  let lvarmon = expr |> uniquify gensym |> remove_complex_operands gensym in
  let cvar = explicate_control lvarmon in
  let x86var = select_instructions cvar in
  x86var
;;
