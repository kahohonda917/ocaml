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
    fun k -> fun t -> (term t1 env)
      (fun v1 -> fun t1 -> (term t2 env)
          (fun v2 -> fun t2  ->
             TLet(v, TPlus(v1, v2), k (TVar(v)) t2)) t1) t
  (* TApp(TPlus(v1, v2),  TApp(TFun(v, TFun(t'', k (TVar (v)) (TVar (t'')))), t2))) t1)  t *)
      
  | Var (x) ->   fun k -> fun t -> k (Env.get env x) t
      
  | Fun (x, e) ->
    let x = Gensym.f "x" in
    let k' = Gensym.f "k'" in
    let t' = Gensym.f "t'" in
    fun k -> fun t -> k (TFun (x, TFun(k', TFun(t', (term e env)
                                      (fun v -> fun t'' -> TApp(TApp((TVar (k')),v), t'')) (TVar (t')))))) t
        
  | App (t1, t2) ->
    let v = Gensym.f "v" in
    fun k -> fun t -> (term t1 env)
      (fun v1 -> fun t1 -> (term t2 env)
          (fun v2 -> fun t2  ->
             TLet(v, TApp(v1, v2), k (TVar(v)) t2)) t1) t


(* k=TFun(v, TFun(t'', TApp(k, TApp((TVar (v)), (TVar (t'')))))) *)
  | Control (c, e) ->
    let x= Gensym.f "x" in
    let k' = Gensym.f "k'" in
    let t' = Gensym.f "t'" in
    let v = Gensym.f "v" in
    let t'' = Gensym.f "t''" in
    fun k -> fun t ->
      TLet(x, TFun(v, TFun(k', TFun(t',
                                    TLet(t'', TAppend(t,TCons((TVar(k')),(TVar(t')))), k (TVar(v)) (TVar(t'')))))),
           (term e (Env.add env c (TVar(x))) idk idt))
      
 
  | Prompt (e) ->
    let v = Gensym.f "v" in
    fun k -> fun t ->
      TLet(v, (term e env) idk idt, k (TVar(v)) t)       
                    




(* Eval.f : Syntax.t -> Syntax.t *)
let f program  = term program Env.empty_env idk idt

