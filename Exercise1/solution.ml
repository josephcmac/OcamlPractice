let rec gcd_aux a b =
  if b = 0 then
    a
  else
    if a >= b then
      gcd_aux (a mod b) b
    else
      gcd_aux b a;;

let gcd a b = gcd_aux (abs a) (abs b);;

let rec relPrimePair_aux c d n depth =
  if (gcd c d = 1) || (depth = 0) then
    (c, d)
  else
    relPrimePair_aux c (d + n) n (depth-1);;

let relPrimePair c d n = relPrimePair_aux c d n 10000;;


let (x, y) = relPrimePair 16 12 9 ;;

print_string ((string_of_int x) ^  (" ")  ^ (string_of_int y) ^ "\n" ) ;;

