module Env = Map.Make(String)

(* call by value *)
let rec interp env expr =
  match expr with
  | `Lit n -> n
  | `NulApp `Read -> read_int ()
  | `UnApp (`Neg, a) -> -(interp env a)
  | `BinApp (`Add, a, b) -> (interp env a) + (interp env b)
  | `BinApp (`Sub, a, b) -> (interp env a) - (interp env b)
  | `Let (name, value, body) ->
    interp (Env.add name (interp env value) env) body
  | `Var name -> Env.find name env
  ;;

let ex = `Let ("x", `NulApp `Read, `BinApp (`Add, `Var "x", `Var "x"))
