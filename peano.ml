type numeral = Zero | Succ of numeral

let rec add a b = match (a, b) with
    | (x, Zero)   -> a
    | (Zero, y)   -> a
    | (x, Succ y) -> add (Succ x) y

let rec numeral2Int n = match n with
    | Zero   -> 0
    | Succ n -> 1 + numeral2Int n

let rec int2numeral i = match i with
    | 0 -> Zero
    | n -> Succ (int2numeral (n - 1))

let main =  ((add (Succ (Succ Zero)) Zero)        = (Succ (Succ Zero)))
         && ((add (Succ (Succ Zero)) (Succ Zero)) = Succ (Succ (Succ Zero)))
         && (numeral2Int Zero = 0)
         && (numeral2Int (Succ (Succ Zero)) = 2)
         && (numeral2Int (add (Succ (Succ Zero)) (Succ Zero)) = 3)
         && (int2numeral 0 = Zero)
         && (int2numeral 3 = Succ (Succ (Succ Zero)))
         ;;

print_string (if main then "Passed\n" else "Failed\n")
