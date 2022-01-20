let rec qsort1 ord = function
        [] -> []
        | h::t -> let after, before = List.partition (ord h) t in
        qsort1 ord before @ h :: qsort1 ord after
;;
        
(*PREGUNTA: ¿En qué casos no será bueno el rendimiento de esta implementación?*)
(*RESPUESTA: En aquellos casos en los que se presente una lista no balanceada*)


let rec qsort2 ord =
        let append' l1 l2 = List.rev_append (List.rev l1) l2 in
        function
        [] -> []
        | h::t -> let after, before = List.partition (ord h) t in
        append' (qsort2 ord before) (h :: qsort2 ord after)
;;
(*PREGUNTA: ¿Tiene qsort2 alguna ventaja sobre qsort1?*)
(*RESPUESTA: Si. Entre otras, usamos las funciones terminales List.rev_append y List.rev, lo que
nos permite aplicar el algoritmo para listas de mayor tamaño que en el caso de qsort1*)

(*PREGUNTA: ¿Permite qsort2 ordenar listas que no podrían ordenarse con qsort1?*)
(*RESPUESTA: Sí. Es el caso de listas de gran tamaño*)


(*Definición de l1 (que contendrá 1000000 valores aleatorios (con valores del 0 al 10000)*)
let initList n f =
        let rec helP (h, t) =
                if h = 0 then t
                else helP(h-1, f h::t)
        in helP(n, [])
;;

let l1= initList 1000000 (function fn -> Random.int 10000);;

(*Ejemplo de criterio de ordenación empleado para probar cada uno de los algoritmos*)
let crOrd2 a b = if a>b then false else true;;

(*PREGUNTA: ¿Tiene qsort2 alguna desventaja sobre qsort1?*)
(*RESPUESTA: En el caso de listas ordenadas, qsort2 es más lento que qsort1*)

(*Caso de una lista ordenada*)
let list1 = [1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16;17;18;19;20];;
(*Tiempos:
        -qsort1: 5.09999999991350705e-05
        -qsort2: 5.49999999961414687e-05
PENALIZACIÓN: Un 107.8% (tiempo usado de qsort2 respecto de qsort1)*)
        
(*Caso de una lista ordenada en sentido inverso*)
let list2 = [20;19;18;17;16;15;14;13;12;11;10;9;8;7;6;5;4;3;2;1];;
(*Tiempos:
        -qsort1: 6.39999999947349352e-05
        -qsort2: 7.50000000024897417e-05
PENALIZACIÓN: Un 117.2% (tiempo usado de qsort2 respecto de qsort1)*)
        
(*Caso de una lista desordenada*)
let list3 = [23;55;2;45;2;1;45;89;67;45;32;84;78;3;4;2;7;53;52;98];;
(*Tiempos:
        -qsort1: 3.69999999989545358e-05
        -qsort2: 3.39999999994233804e-05*)
        
(*Función que usamos para medir los tiempos de ejecución*)
let crono f x y=
        let t = Sys.time() in
        let _= f x y in
        Sys.time() -.t
;;

