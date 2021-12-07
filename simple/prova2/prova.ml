
let compose ~f ~g x = f(g x);;

let composee = compose ~g:(fun x -> x**3.);;

Printf.printf "risultato: %f \n" (compose ~f:(fun x -> x-.1.) ~g:(fun x -> x**3.) 2.);;

Printf.printf "risultato: %f \n" (composee ~f:(fun x -> x-.2.) 2.);;

Printf.printf "risultato: %f \n" (composee ~f:(fun x -> x-.3.)  2.);;