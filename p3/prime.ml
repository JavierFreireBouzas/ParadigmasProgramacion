let is_prime n =
let rec check_from i =
i >= n ||
(n mod i <> 0 && check_from (i+1))
in check_from 2;;

let is_prime2 n=
if(((n == 2) || (n == 3) || (n == 5) || (n == 7) || (n == 11)) || ((n mod 2 != 0) && (n mod 3 != 0) && (n mod 5 != 0) && (n mod 7 != 0) && (n mod 11 != 0))) then true else false;;

let rec next_prime n = 
if is_prime (n+1) then n+1 else next_prime (n+1);;

let rec last_prime_to n = 
if is_prime (n) then n else last_prime_to (n-1);;
