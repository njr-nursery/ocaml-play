type numeral = Zero | Succ of numeral ;;

let rec add a b = match (a, b) with
    |  (x, Zero) -> a   
    |  (Zero, y) -> a   
    |  (x, Succ y) -> add (Succ x) y
    ;;

let main =  ((add (Succ (Succ Zero)) Zero)        = (Succ (Succ Zero)))
         && ((add (Succ (Succ Zero)) (Succ Zero)) = Succ (Succ (Succ Zero)))
         ;;

print_string (if main then "Passed\n" else "Failed\n")
