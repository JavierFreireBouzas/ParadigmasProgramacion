let e1 = (function pi -> pi *. (pi +. 1.)) (2. *. asin 1.);;

let e2 = (function lg2 -> let log2 = function x -> log x /. lg2 in log2 (float (1024 * 1024)))(log 2.);;

let e3 = (function pi_2 -> function r -> pi_2 *. r)(4. *. asin 1.);;

let e4 = (function sqr -> let pi = 2. *. asin 1. in function r -> pi *. sqr r)(function x -> x *. x);;

let abs n = (function true -> n | false -> -n)(n >=0);;

let par n = (function true -> true | false -> false)(n mod 2 == 0);;

let saluda s = (function true -> print_endline("Hola!") | false -> ())((compare s "Hola") == 0);;

let f n = (function true -> "es par" | false -> "es impar")(n mod 2 == 0);;

let f n = (function true -> "múltiplo de 2" | false -> (function true -> "múltiplo de 3" | false -> "impar")(n mod 3 == 0))(n mod 2 == 0);;
