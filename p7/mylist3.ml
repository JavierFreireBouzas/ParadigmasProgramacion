let rec remove vl = function
        [] -> [] 
        |h :: t -> if (vl = h) then t else h::(remove vl t)
;;

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

let rec divide = function
        h1::h2::t -> let lst1, lst2 = divide t
                in h1::lst1, h2::lst2
        | lst -> lst, []
;;
