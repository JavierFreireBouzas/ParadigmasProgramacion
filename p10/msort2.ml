(*val divide : 'a list -> 'a list * 'a list = <fun>*)
let rec divide l = match l with
h1::h2::t -> let t1, t2 = divide t in (h1::t1, h2::t2)
| _ -> l, [];;


(*val merge : ('a -> 'a -> bool) -> 'a list * 'a list -> 'a list = <fun>*)
let rec merge f = function
        [], l | l, [] -> l
        | h1::t1, h2::t2 -> if (f h1) h2 then h1 :: merge f (t1, h2::t2) else h2 :: merge  f (h1::t1, t2)
;;


(*val msort1 : ('a -> 'a -> bool) -> 'a list -> 'a list = <fun>*)
let rec msort1 f l= match l with
        [] | _::[] -> l
        | _ -> let l1, l2 = divide l in
        merge f (msort1 f l1, msort1 f l2);;
        
(*PREGUNTA: ¿Puede provocar algún problema la no terminalidad de divide o merge?*)
(*Sí, en caso de listas de gran longitud, provocará Stack Overflow*)
        
(*Definición de l2 (que contendrá 1000000 valores aleatorios (con valores del 0 al 10000)*)
let initList n f =
        let rec helP (h, t) =
                if h = 0 then t
                else helP(h-1, f h::t)
        in helP(n, [])
;;

let l2= initList 1000000 (function fn -> Random.int 10000);;

(*Defina de modo recursivo terminal funciones divide' y merge' que cumplan el mismo
cometido que divide y merge, respectivamente. Realice una implementación, msort2, de la
ordenación por fusión utilizando divide' y merge'*)

let divide' l =
        let rec helP d1 d2 = function
                [] -> (List.rev d1, List.rev d2)
                |h::[]->(List.rev(h::d1), List.rev d2)
                |h1::h2::t -> helP (h1::d1) (h2::d2) t
        in helP [] [] l
;;

let merge' f (lst1, lst2)= 
        let rec helP (l1, l2) sol = match l1,l2 with
                [], l | l, [] -> List.rev_append sol l
                | h1::t1, h2::t2 -> if (f h1) h2 then helP (t1, h2::t2) (h1::sol) else helP (h1::t1, t2) (h2::sol)
        in helP (lst1, lst2) []
;;

let rec msort2 f l= match l with
        [] | _::[] -> l
        | _ -> let l1, l2 = divide' l in
        merge' f (msort2 f l1, msort2 f l2)
;;

(*Ejemplo de criterio de ordenación empleado para probar cada uno de los algoritmos*)
let crOrd2 a b = if a>b then false else true;;

(*Caso de una lista ordenada*)
let list1 = [1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16;17;18;19;20];;
(*Tiempos:
        -qsort2: 1.100000000020529e-05
        -msort1: 8.99999999992573407e-06
        -msort2: 1.2000000000345068e-05
msort1 es más rápido que qsort2, que a su vez es más rápido que msort2*)
        
(*Caso de una lista ordenada en sentido inverso*)
let list2 = [20;19;18;17;16;15;14;13;12;11;10;9;8;7;6;5;4;3;2;1];;
(*Tiempos:
        -qsort2: 1.50000000003203127e-05
        -msort1: 8.99999999992573407e-06
        -msort2: 1.2000000000345068e-05
msort1 es más rápido que msort2, que a su vez es más rápido que qsort2*)
        
(*Caso de una lista desordenada*)
let list3 = [23;55;2;45;2;1;45;89;67;45;32;84;78;3;4;2;7;53;52;98];;
(*Tiempos:
        -qsort2: 8.99999999992573407e-06
        -msort1: 7.99999999978595611e-06
        -msort2: 1.19999999999009788e-05
msort1 es más rápido que qsort2, que a su vez es más rápido que msort2*)


(*Función que usamos para medir los tiempos de ejecución*)
let crono f x y=
        let t = Sys.time() in
        let _= f x y in
        Sys.time() -.t
;;


