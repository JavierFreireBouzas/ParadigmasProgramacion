let rec remove vl = function
        [] -> [] 
        |h :: t -> if (vl = h) then t else h::(remove vl t)
;;

let rec remove_all vl = function   
        [] -> []
        |h :: t -> if (vl = h) then remove_all vl t else h::(remove_all vl t)
;;
	
let rec ldif l1 l2 = match l2 with
        [] -> l1
        |h::t -> ldif(remove_all h l1) t
;;

let rec lprod l1 l2 = match l1 with
    [] -> []
    |h::t -> 
        let rec helP hd l2 = match l2 with
                [] -> []
                |h::t -> (hd, h)::( helP hd t)
        in (helP h l2) @ (lprod t l2)
;;


(*MAL - REVISAR!!!!!!*)
let divide l =
        if l = [] then []
        else 
        let rec helP l anw1 anw2 ctr = match l with
                []->(anw1, anw2)
                |h::t ->
                if ctr mod 2 = 0 then helP (t anw1 (anw2::h) (ctr+1)) else helP (t (anw1::h) anw2 (ctr+1))
        in helP (l [] [] 0)
;;
