let rec power x y =
if y == 0 then 1
else x * power x (y-1);;

let rec power' x y =
if y == 0 then 1
else 
 if (y mod 2 == 0) then power' (x*x) (y/2)
 else
 x * power' (x*x) (y/2);;

(*power' debería ser mejor en terminos de eficiencia ya que realiza un menor número de iteracciones que en el caso de power. A pesar de estar operando en int, la ganancia obtenida merece la pena pues es una mejora bastante substancial en lo que respecta al número de iteracciones de la funcion recursiva*)
 
let rec powerf x n =
if n == 0 then 1.0
else x *. powerf x (n - 1);;
