open Syntax
open Target

(* DS version *)
let idt = TId

(* let rec cons cont trail = match trail with
 *     Nil -> cont
 *   | Trail(k) -> fun v -> fun t' -> cont v (Trail (cons k  t'))
 * 
 * let at trail trail2 = match trail with
 *     Nil -> trail2
 *   | Trail(k) -> Trail(cons k trail2) *)

  
(* let idk v t =
 *   let k = Gensym.f "k" in
 *   TMatch(t, v, k, TApp(TVar(k), TApp(v, TId))) *)

let idk = fun v -> fun t -> TIdk(v,t)


(* let rec cons k t =
 *   (\* let v = Gensym.f "v" in *\)
 *   let k' = Gensym.f "k'" in
 *   (\* let t' = Gensym.f "t'" in *\)
 *   TMatch(t,  k,  k',
 *          TVar("k v (k' :: t'"))
 * let at t t' =
 *   (\* let k = Gensym.f "k" in *\)
 *   TMatch(t, t', "k",  (TVar ("k' :: t'"))) *)
  
exception Term_error
let rec term t env  = match t with
    Number (n) -> fun k -> fun t -> k (TNum (n)) t
  | Plus (t1, t2) ->
    let v = Gensym.f "v" in
    let t'' = Gensym.f "t''" in
    let env2 = Env.add (Env.add env "v" (TVar (v))) "t''" (TVar (t'')) in
    fun k -> fun t -> (term t1 env2)
      (fun v1 -> fun t1 -> (term t2 env2)
          (fun v2 -> fun t2  -> TApp(TPlus(v1, v2),  TApp(TFun(v, TFun(t'', k (TVar (v)) (TVar (t'')))), t2))) t1)  t
  | Var (x) ->   fun k -> fun t -> k (Env.get env x) t
      
  | Fun (x, e) ->
    let x = Gensym.f "x" in
    let k' = Gensym.f "k'" in
    let t' = Gensym.f "t'" in
    let env2 = Env.add (Env.add (Env.add env "x" (TVar (x))) "k'" (TVar (k'))) "t'" (TVar (t')) in
    fun k -> fun t -> k (TFun (x, TFun(k', TFun(t', (term e env2)
                                      (fun v -> fun t'' -> TApp((TVar (k')), TApp(v, t''))) (TVar (t')))))) t
        
  | App (t1, t2) ->
    let v = Gensym.f "v" in
    let t'' = Gensym.f "t''" in
    let env2 = Env.add (Env.add env "v" (TVar (v))) "t''" (TVar (t'')) in
    fun k -> fun t -> (term t1 env2)
      (fun v1 -> fun t1 -> (term t2 env2)
          (fun v2 -> fun t2  -> TApp(v1, TApp(v2, TApp(TFun(v, TFun(t'', k (TVar (v)) (TVar (t'')))), t2)))) t1)  t

(* k=TFun(v, TFun(t'', TApp(k, TApp((TVar (v)), (TVar (t'')))))) *)
  | Control (c, e) ->
    let x= Gensym.f "x" in
    let k' = Gensym.f "k'" in
    let t' = Gensym.f "t'" in
    let v = Gensym.f "v" in
    let t'' = Gensym.f "t''" in
    let env2 = Env.add (Env.add (Env.add (Env.add (Env.add env "x" (TVar (x))) "k'" (TVar (k')))
                                   "t'" (TVar (t'))) "v" (TVar (v))) "t''" (TVar (t'')) in
    fun k -> fun t ->
      TLet(x, TFun(v, TFun(k', TFun(t',
                                    TLet(t'', TAppend(t,TCons((TVar(k')),(TVar(t')))), k (TVar(v)) (TVar(t'')))))),
           (term e (Env.add env2 c (TVar(x))) idk idt))
      
 
  | Prompt (e) ->
    let v = Gensym.f "v" in
    let env2 = Env.add env "v" (TVar(v)) in
    fun k -> fun t ->
      TLet(v, (term e env2) idk idt, k (TVar(v)) t)       
                    




(* Eval.f : Syntax.t -> Syntax.t *)
let f program  = term program Env.empty_env idk idt

