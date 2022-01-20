let bigTree n =
        let rec helP n lst = 
        if n <= 0 then G_tree.Gt(1,lst) else helP (n-4) (List.rev_append lst [G_tree.Gt(3, [G_tree.Gt(89, [G_tree.Gt(37, [G_tree.Gt(89, [])])])])])
        in helP n [];;

let breadth_first_t tree=
        let rec helP tree lst= match tree with
        G_tree.Gt (x, []) -> List.rev_append lst [x]
        | G_tree.Gt (x, (G_tree.Gt (y, t2))::t1) -> helP (G_tree.Gt (y, List.rev_append(List.rev_append t1 []) t2)) (List.rev_append [x] lst)
        in helP tree [];;

let t = bigTree 10000;;
