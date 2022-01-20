let f n = if n mod 2 = 0 then n / 2 else 3 * n + 1;;

let rec orbit n = 
if (n <> 1) then print_string(string_of_int(n) ^", ") else print_endline("1");
if (n > 0 && n <> 1) then orbit(f(n));
;;

let rec length = function
1 -> 0
| n -> length (f n) + 1
;;

let rec top = function
1 -> 1
| n ->  max n (top(f n))
;;

let rec length'n'top n =
if (n = 1) then (0,1) 
else 
let x = length'n'top(f(n)) in (fst(x) + 1, max n (snd(x)))
;;
