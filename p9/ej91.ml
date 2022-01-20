let to0from n =
        let rec helP lst idx =
                if idx > n then lst
                else helP (idx::lst)(idx+1)
        in helP [] 0
;;
	
let fromto m n =
        let rec helP lst idx =
                if idx < m then lst
                else helP (idx::lst)(idx-1)
        in helP [] n
;;
	
let from1to n =
        let rec helP lst idx =
                if idx < 1 then lst
                else helP (idx::lst)(idx-1)
        in helP [] n
;;
	
let map f lst=
        let rec helP f lst con = match lst with
                [] -> List.rev con
                |h::t -> helP f t ((f h)::con)
        in helP f lst []
;; 

let power x y =
        if y < 0 then invalid_arg "power"
        else
                let rec helP x y res =
                        if y = 0 then res
                        else helP x (y-1) (res*x)
                in helP x y 1
;;

let incseg l=
        let rec helP l res nl = match l with
                [] -> List.rev nl
                |h::t -> helP t (h+res) ((h+res)::nl)
        in helP l 0 []
;;

let remove x l =
        let rec helP x l nl = match l with
                [] -> List.rev nl
                |h::t -> if h = x then helP x t nl
                else helP x t (h::nl)
        in helP x l []
;;

let divide l = 
        let rec helP l l1 l2 = match l with
                [] -> List.rev l1, List.rev l2
                |h1::[] -> List.rev (h1::l1), List.rev l2
                |h1::h2::t -> helP t (h1::l1) (h2::l2)
        in helP l [] []
;;

let compress l =
        let rec helP l nl = match l with
                [] -> List.rev nl
                |h1::[] -> List.rev (h1::nl)
                |h1::h2::t -> if h1 = h2 then helP (h2::t) (nl) else helP (h2::t) (h1::nl)
        in helP l []
;;
