type t = TNum of int
       | TPlus of t * t
       | TVar of string
       | TFun of string * t
       | TApp of t * t
       | TId
       | TTrail of t
       | TIdk of t * t
       | TAppend of t * t
       | TCons of t * t
       | TLet of string * t * t
                 
       (* | TMatch of t * t * string * t *)




(* Syntax.to_string : Syntax.t -> string *)
let rec to_string exp = match exp with
    TNum (n) -> string_of_int n
  | TPlus (n1, n2) -> to_string n1^ "+" ^to_string n2
  | TVar (x) -> x
  | TFun (x, t) ->
        "(fun " ^ x ^ " -> " ^ to_string t ^ ")"
  | TApp (t1, t2) ->
    "(" ^ to_string t1 ^ " " ^ to_string t2 ^ ")"
  | TId -> "TId"
  | TTrail(t) -> to_string t
  | TIdk (t1, t2) -> "(idk "^to_string t1^" "^to_string t2^")"
  | TAppend (t1, t2) -> to_string t1 ^" @ "^to_string t2
  | TCons (t1, t2) -> to_string t1 ^" :: "^to_string t2
  | TLet (v, t1, t2) -> "let "^v^" = "^to_string t1^" in "^to_string t2
    
  (* | TMatch (a, b, k, c) ->
   *   " (match " ^ to_string a ^ " with TId -> " ^ to_string b ^ " | " ^ k ^ " -> " ^ to_string c^") " *)





(* Syntax.print : Syntax.t -> unit *)
let print exp =
  let str = to_string exp in
  print_string str
