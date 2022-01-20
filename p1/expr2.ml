(*Un valor u de tipo int a partir de una expresión que contenga, al menos, 4 operadores infijos*)
let u = 3 + 5 * 7 - 4 / 2;;

(*Un valor v de tipo float a partir de una expresión que incluya una función predefinida*)
let v = sqrt(4.);;

(*Un valor w de tipo char a partir de una expresión que incluya una sub-expresión de tipo int*)
let w = char_of_int 65;;

(*Un valor x de tipo bool a partir de una expresión que incluya una o más funciones u operadores*)
let x = sqrt(4.) = sqrt(2.);;

(*Un valor y de tipo string a partir de una expresión que contenga una frase if-then-else*)
let y = if 3=3 then "3 is equal to 3" else "3 is not equal to 3";;
