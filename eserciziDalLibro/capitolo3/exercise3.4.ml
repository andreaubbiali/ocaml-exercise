(* 
gcd( n , m ) =
  while m != 0
    if n > m
      n ← n − m
    else
      m ← m − n
  return n
*)

let rec (%%) m n = 
  if m != 0 then 
    if n > m then (n-m) %% m
    else n %% (m-n)
  else n;;

let main() =  Printf.printf "%d\n" (5 %% 4); (*expected=1*)
              Printf.printf "%d\n" (200 %% 4); (*expected=4*)
              Printf.printf "%d\n" (200 %% 3); (*expected=1*)
              Printf.printf "%d\n" (5 %% 10);; (*expected=5*)

main();;