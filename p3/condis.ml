false && (2 / 0 > 0);;
(*- : bool = false*)
(*Al analizar el primer argumento, que es false, devuelve false. De este modo, no analiza el segundo argumento*)

(*true && (2 / 0 > 0);;*)
(*Devuelve una excepcion. Ya que el primer elemento es verdadero, analiza el segundo, en el que se encuentra con una division entre 0*)

true || (2 / 0 > 0);;
(*- : bool = true*)
(*Al analizar el primer argumento, que es true, devuelve true. De este modo, no analiza el segundo argumento*)

(*false || (2 / 0 > 0);;*)
(*Devuelve una excepcion. Ya que el primer elemento es falso, analiza el segundo, en el que se encuentra con una división entre 0*)

let con b1 b2 = b1 && b2;;
(*val con : bool -> bool -> bool = <fun>*)

let dis b1 b2 = b1 || b2;;
(*val dis : bool -> bool -> bool = <fun>*)

(*con (1 < 0) (2 / 0 > 0);;*)
(*Devuelve una excepcion. En este caso, al introducir en la funcion "con" un argumento en el que se realiza un division por 0, el sistema devuelve una excepcion*)

(1 < 0) && (2 / 0 > 0);;
(*- : bool = false*)
(*Al analizar el primer argumento, que es false, devuelve false. De este modo, no analiza el segundo argumento*)

(*dis (1 > 0) (2 / 0 > 0);;*)
(*Devuelve una excepcion. En este caso, al introducir en la funcion "dis" un argumento en el que se realiza un division por 0, el sistema devuelve una excepcion*)

(1 > 0) || (2 / 0 > 0);;
(*- : bool = true*)
(*Al analizar el primer argumento, que es true, devuelve true. De este modo, no analiza el segundo argumento*)
