type log_exp =
    Const of bool
  | Var of string
  | Neg of log_exp
  | Disj of log_exp * log_exp
  | Conj of log_exp * log_exp
  | Cond of log_exp * log_exp
  | BiCond of log_exp * log_exp;;


  
let rec eval ctx = function
    Const b -> b
  | Var s -> List.assoc s ctx
  | Neg e -> not (eval ctx e)
  | Disj (e1, e2) -> (eval ctx e1) || (eval ctx e2)
  | Conj (e1, e2) -> (eval ctx e1) && (eval ctx e2)
  | Cond (e1, e2) -> (not (eval ctx e1)) || (eval ctx e2)
  | BiCond (e1, e2) -> (eval ctx e1) = (eval ctx e2);;
  
type oper = Not;;

type biOper = Or | And | If | Iff;;

type prop =
    C of bool
  | V of string
  | Op of oper * prop
  | BiOp of biOper * prop * prop;;
  
let rec prop_of_log_exp = function
    Const b -> C b
  | Var s -> V s
  | Neg e -> Op (Not, prop_of_log_exp e)
  | Disj (e1, e2) -> BiOp (Or, prop_of_log_exp e1, prop_of_log_exp e2)
  | Conj (e1, e2) -> BiOp (And, prop_of_log_exp e1, prop_of_log_exp e2)
  | Cond (e1, e2) -> BiOp (If, prop_of_log_exp e1, prop_of_log_exp e2)
  | BiCond (e1, e2) -> BiOp (Iff, prop_of_log_exp e1, prop_of_log_exp e2);;
  
let rec log_exp_of_prop = function
    C b -> Const b
  | V s -> Var s
  | Op (Not, e) -> Neg (log_exp_of_prop e)
  | BiOp (Or, e1, e2) -> Disj (log_exp_of_prop e1, log_exp_of_prop e2)
  | BiOp (And, e1, e2) -> Conj (log_exp_of_prop e1, log_exp_of_prop e2)
  | BiOp (If, e1, e2) -> Cond (log_exp_of_prop e1, log_exp_of_prop e2)
  | BiOp (Iff, e1, e2) -> BiCond (log_exp_of_prop e1, log_exp_of_prop e2);;
  
let opval = function
  Not -> not;;
  
let biopval = function
    Or -> (||)
  | And -> (&&)
  | If -> (||)
  | Iff -> (=);;

let rec peval ctx = function
    C b -> b
  | V s -> List.assoc s ctx
  | Op (Not, e) -> not(peval ctx e)
  | BiOp (Or, e1, e2) -> (peval ctx e1) || (peval ctx e2)
  | BiOp (And, e1, e2) -> (peval ctx e1) && (peval ctx e2)
  | BiOp (If, e1, e2) -> (not (peval ctx e1)) || (peval ctx e2)
  | BiOp (Iff, e1, e2) -> (peval ctx e1) = (peval ctx e2);;

let rec remove_all vl = function   
        [] -> []
        |h :: t -> if (vl = h) then remove_all vl t else h::(remove_all vl t)
;;

let rec ldif lst1 lst2 = match lst2 with
        [] -> lst1
        |h::t -> ldif(remove_all h lst1) t
;;

let rec lprod lst1 lst2 = match lst1 with
    [] -> []
    |h::t -> 
        let rec helP hd lst2 = match lst2 with
                [] -> []
                |h::t -> (hd, h)::( helP hd t)
        in (helP h lst2) @ (lprod t lst2)
;;

let variables prp = 
  let rec helP prp lst = match prp with
    C b -> lst
  | V s -> if not(List.mem s lst) then List.rev_append lst [s] else lst
  | Op (Not, e) -> helP e lst
  | BiOp (Or, e1, e2) -> List.rev_append (ldif(helP e1 lst) (helP e2 lst)) (helP e2 lst)
  | BiOp (And, e1, e2) -> List.rev_append (ldif(helP e1 lst) (helP e2 lst)) (helP e2 lst)
  | BiOp (If, e1, e2) -> List.rev_append (ldif(helP e1 lst) (helP e2 lst)) (helP e2 lst)
  | BiOp (Iff, e1, e2) -> List.rev_append (ldif(helP e1 lst) (helP e2 lst)) (helP e2 lst)
  in helP prp [];;
  
let giveValues lst n cas = match n,cas with
    1,4 -> lprod lst [true]
  | 2,4 -> lprod lst [false]
  | 3,4 -> List.rev_append (lprod [(List.nth lst 0)] [true]) (lprod [(List.nth lst 1)] [false])
  | 4,4 -> List.rev_append (lprod [(List.nth lst 0)] [false]) (lprod [(List.nth lst 1)] [true])
  | 1,_ -> lprod lst [true]
  | 2,_ -> lprod lst [false]
  | 3,_ -> List.rev_append (List.rev_append (lprod [(List.nth lst 0)] [true]) (lprod [(List.nth lst 1)] [false])) (lprod [(List.nth lst 2)] [true])
  | 4,_ -> List.rev_append (List.rev_append (lprod [(List.nth lst 0)] [true]) (lprod [(List.nth lst 1)] [false])) (lprod [(List.nth lst 2)] [false])
  | 5,_ -> List.rev_append (List.rev_append (lprod [(List.nth lst 0)] [true]) (lprod [(List.nth lst 1)] [true])) (lprod [(List.nth lst 2)] [true])
  | 6,_ -> List.rev_append (List.rev_append (lprod [(List.nth lst 0)] [true]) (lprod [(List.nth lst 1)] [true])) (lprod [(List.nth lst 2)] [false])
  | 7,_ -> List.rev_append (List.rev_append (lprod [(List.nth lst 0)] [false]) (lprod [(List.nth lst 1)] [false])) (lprod [(List.nth lst 2)] [true])
  | 8,_ -> List.rev_append (List.rev_append (lprod [(List.nth lst 0)] [false]) (lprod [(List.nth lst 1)] [true])) (lprod [(List.nth lst 2)] [false])
  | _,_ -> List.rev_append (List.rev_append (lprod [(List.nth lst 0)] [false]) (lprod [(List.nth lst 1)] [true])) (lprod [(List.nth lst 2)] [true]);;

let is_tau prp = 
  let lst = variables prp in
  let cas = (List.length lst * List.length lst) in
    let rec helP n sol=
     if (n = 0 || not(sol)) then sol
     else helP (n-1) (sol && (peval (giveValues lst n cas) prp))
     in helP cas true;;



