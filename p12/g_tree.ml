type 'a g_tree = Gt of 'a * 'a g_tree list;;

let rec size = function 
        Gt (_,[]) -> 1
        | Gt (r,h::t) -> size h + size (Gt (r,t));;
   
let rec height = function
        Gt (_, []) -> 1
        | Gt(_, _::t) -> let add = List.fold_left (+) 0 in
        2 + add (List.map height t);;
                
let rec leaves = function
        Gt (r,[]) -> [r]
        | Gt (r,lst) -> List.flatten (List.map leaves lst);;
        
let rec mirror = function
        Gt (r,[]) -> Gt (r, [])
        | Gt (r,h::t) -> Gt (r,List.rev (List.map mirror (h::t)));;

let rec preorder = function
  Gt(r,lst) -> r::List.concat (List.map preorder lst);;   

let rec postorder = function
        Gt (r, []) -> [r]
        | Gt(r, h::t) -> List.append (postorder h) (postorder (Gt(r,t))) ;;
