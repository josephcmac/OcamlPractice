let rec extended_gcd_aux old_r r old_s s old_t t =
  if r = 0 then
    (old_r, r, old_s, s, old_t, t)
  else
    let quotient = old_r / r in
      extended_gcd_aux r (old_r - quotient*r) s (old_s - quotient*s) t (old_t - quotient*t);;

let extended_gcd a b = extended_gcd_aux a b 1 0 0 1;;

let a = 462382 and b = 4354352 in
  let (d, r, x, s, y, t) = extended_gcd a b in 
    print_string ( (string_of_int a) ^ "*" ^ (string_of_int x) ^ " + " ^ (string_of_int b) ^ "*" ^ (string_of_int y) ^ "\n")


