
let trialdivision a = 
    Printf.printf "Trial-Division's Primality Test";
    let rec calc = function 
        n when (n > a / 2)          -> true
    |   n                           -> if (a mod n) == 0 then false else calc (n+1)
    in calc 2

let lucaslehmer a = 
    Printf.printf "Lucas-Lehmer's Primality Test";
    false;;

let rec createRandom max = match (Random.int64 (Int64.of_int max)) with
    0L | 1L       -> createRandom max
|   n           -> n

let esponential a b = 
    let af = Int64.to_float a and
    bf = Float.of_int b in
    Int64.of_float (af**bf);;

(*number of loops decided by me (p/2)+5. The plus 5 is needed for little numbers*)
let littlefermat p = 
    Printf.printf "Little Fermat's Primality Test";
    let loops = (p / 2) + 5 in
    let rec calc index a = match ((esponential a (p-(1))) == Int64.of_int (1 mod p)) with
        false                       -> false
    |   true when (index == 0)      -> true
    |   true                        -> calc (index-1) (createRandom p)
    in calc loops (createRandom p)


let is_prime = function
    1 | 0                                   -> true
|   n when n <= 10000                       -> trialdivision n
|   n when n > 10000 && n <= 524287         -> lucaslehmer n
|   n                                       -> littlefermat n