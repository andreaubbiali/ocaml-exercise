(* 
gcd( n , m ) =
  while m != 0
    if n > m
      n ← n − m
    else
      m ← m − n
  return n
*)

let rec gccd n m = 
  if m == 0 then n 
  else
    if n > m then gccd (n-m) (m)
    else gccd (n) (m-n);;


let main() =  Printf.printf "%d\n" (gccd 5 4); (*expected=1*)
            Printf.printf "%d\n" (gccd 200 4); (*expected=4*)
            Printf.printf "%d\n" (gccd 200 3); (*expected=1*)
            Printf.printf "%d\n" (gccd 5 10);; (*expected=5*)
      
main();;