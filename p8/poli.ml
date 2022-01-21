let f : 'a -> 'a = fun x->x ;;

let h : 'a * 'b -> 'a = fun (x,y) -> x;;

let i : 'a * 'b -> 'b = fun (x,y) -> y;;

let j: 'a -> 'a list = fun x -> [x];;
