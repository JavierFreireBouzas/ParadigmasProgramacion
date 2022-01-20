let rec fact = function
0 -> 1
| n -> if n < 0 then raise(Stack_overflow) else n * fact (n - 1);;



let facto =
	try 
		print_int(fact(int_of_string Sys.argv.(1)));
		print_endline("")
	with
	        Invalid_argument(_) -> print_endline("fact: número de argumentos inválido")
		|Stack_overflow -> print_endline("fact: argumento inválido")
		|Failure(_) -> print_endline("fact: argumento inválido")
