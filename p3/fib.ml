let rec fib n = 
 if n <= 1 then n
 else fib (n-1) + fib (n-2);;

let printFib x =
print_endline(string_of_int(fib(x)));;

let rec recFib n =
 if n <= int_of_string Sys.argv.(1) then printFib (n);
 if n <= int_of_string Sys.argv.(1) then recFib(n+1);;

recFib(0);;
