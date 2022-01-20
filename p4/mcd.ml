let rec mcd (x,y) =
 if (x>y) then mcd(y, x) else if (y mod x == 0) then x else mcd(y mod x, x);;
 (*Establecemos: x numero menor, y numero mayor*)
