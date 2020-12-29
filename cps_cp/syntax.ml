


type t = Number of int
       | Plus of t * t
       | Var of string
       | Fun of string * t
       | App of t * t
       | Control of string * t
       | Prompt of t




(* Syntax.to_string : Syntax.t -> string *)
let rec to_string exp = match exp with
    Number (n) -> string_of_int n
  | Plus (t1, t2) -> to_string t1 ^ "+" ^ to_string t2
  | Var (x) -> x
  | Fun (x, t) ->
        "(fun " ^ x ^ " -> " ^ to_string t ^ ")"
  | App (t1, t2) ->
    "(" ^ to_string t1 ^ "@ " ^ to_string t2 ^ ")"
  | Control (x, t) ->
    "(control " ^ x ^ " -> " ^ to_string t ^ ")"
  | Prompt (t) ->
    "(prompt -> " ^ to_string t ^ ")"



(* Syntax.print : Syntax.t -> unit *)
let print exp =
  let str = to_string exp in
  print_string str
