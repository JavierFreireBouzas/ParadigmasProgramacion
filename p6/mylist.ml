(*hd*)
let hd = function
	h::_ -> h
	|[] -> raise(Failure "hd")
;;

(*tl*)
let tl = function
	_::t -> t
	|[] -> raise(Failure "tl")
;;

(*length*)
let rec length = function
	[] -> 0
	| _::t -> 1 + length t
;;

(*compare_lengths*)
let rec compare_lengths lt1 lt2 = match lt1, lt2 with
	[],[] -> 0
	| [], _ -> -1
	| _,[] -> 1
	| _::lt1, _::lt2 -> compare_lengths lt1 lt2
;;

(*nth*)
let rec nth lt nt = match lt,nt with
    | [], _ -> raise (Failure "nth")
    | _, n when n < 0 -> raise (Invalid_argument "nth")
    | l::_, 0 -> l
    | l::ls, n -> nth ls (nt-1)
;;

(*append*)
let rec append ls1 ls2 = match ls1 with
	[]->ls2
	|h::t -> h :: append t ls2
;;

(*find*)
let rec find fu lt = match lt with
     |[] -> raise (Failure "Not found")
     |h :: t -> if fu h = true then h else find fu t
;;

(*for_all*)
let rec for_all fu lt = match lt with
     |[] -> true
     |h :: t -> fu h && for_all fu t
;;

(*exists*)
let rec exists fu lt = match lt with
     |[] -> false
     |h :: t -> fu h || exists fu t
;;

(*mem*)
let rec mem x = function
	[] -> false
	|h::t -> x = h || mem x t
;;

(*rev*)
let rec rev = function
	[] -> []
	|h::t -> rev t @ [h]
;;

(*filter*)
let rec filter fu lt = 
	let rec helP fu lt newList = match lt with
     |[] -> newList
     |h :: t -> 
     if fu h then helP fu t (h::newList)
     else helP fu t newList
 in helP fu (rev lt) []
;;

(*find_all*)
let rec find_all fu lt = 
	let rec helP fu lt newList = match lt with
     |[] -> newList
     |h :: t -> 
     if fu h then helP fu t (h::newList)
     else helP fu t newList
 in helP fu (rev lt) []
;;

(*partition*)
let rec partition fu lt = 
	let rec helP fu lt newLt1 newLt2 = match lt with
     |[] -> (rev newLt1, rev newLt2)
     |h :: t -> 
     if fu h then helP fu t (h::newLt1) newLt2
     else helP fu t newLt1 (h::newLt2)
 in helP fu lt [] []
;;

(*split*)
let rec split = function
	|[] -> ([],[])
	|(h1,h2)::t -> let (newLt1, newLt2) = split t in (h1::newLt1, h2::newLt2)
;;

(*combine*)
let rec combine l1 l2= match l1, l2 with
	|[], [] -> []
	|[], _ -> raise (Invalid_argument "combine")
	|_ , [] -> raise (Invalid_argument "combine")
	|h1::t1, h2::t2 -> (h1, h2)::combine t1 t2
;;

(*init*)
let init len f =
	if len < 0 then raise (Invalid_argument "init")
	else let rec helP len f ind =
	if ind >= len then []
	else (f ind):: (helP len f (ind+1))
	in helP len f 0
;;

(*rev_append*)
let rec rev_append lt1 lt2 = match lt1 with
	[] -> lt2
	|h::t -> rev_append t(h::lt2)
;;

(*concat*)
let rec concat = function
	[] -> []
	|h::t -> append h (concat t)
;;


(*flatten*)
let rec flatten = function
	[] -> []
	|h::t -> append h (flatten t)
;;


(*map*)
let rec map f = function
	[] -> []
	|h::t -> f h :: map f t
;;

(*rev_map*)
let rev_map f l =
	let rec helP l it1 = match l with
		[] -> it1
		| h::t -> helP t (f(h)::it1)
	in helP l []
;;


(*map2*)
let rec map2 f l1 l2 =
	if(length l1 != length l2) then raise (Invalid_argument"map2")
	else if (length l1 = 0) then []
	else (f(hd l1)(hd l2))::map2 f (tl l1)(tl l2)
;;


(*fold_left*)
let rec fold_left f a = function
	[] -> a
	|h::t -> fold_left f(f a h) t
;;


(*fold_right*)
let rec fold_right f l b = match l with
	[] -> b
	|h::t -> f h (fold_right f t b)
;;

