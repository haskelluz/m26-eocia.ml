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

let compile expr =
  let gensym = make_gensym () in
  uniquify gensym expr
;;
