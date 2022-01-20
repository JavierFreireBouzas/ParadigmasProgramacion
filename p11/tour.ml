let inside m n (x,y) = (x>=1 && x<=m && y>=1 && y<=n);;

let salto(c1, c2) =[(c1+2, c2+1);(c1+1, c2+2);(c1-2, c2-1);(c1-1, c2-2);(c1+1, c2-2);(c1+2,c2-1);(c1-1, c2+2);(c1-2,c2+1)];;

let possibleMovements m n path pI =
        let saltos = salto pI in 
        let rec helP saltos possible = match saltos with
                [] -> List.filter (inside m n) possible
                |h::t -> if List.mem h path then helP t possible else helP t (h::possible)
        in helP saltos []
;;

let tour m n pI pF =
        let rec helP path next =
                if List.hd path = pF then List.rev path
                else match next with
                [] -> raise(Not_found)
                |h::t-> try helP (h::path) (possibleMovements m n path h) 
                        with Not_found -> helP path t
        in helP [pI] (possibleMovements m n [] pI)
;;
