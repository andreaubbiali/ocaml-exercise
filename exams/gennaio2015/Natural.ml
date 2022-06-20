open NaturalI;;

module Natural =
    struct

        type natural = Z | Succ of natural;;
        exception NegativeNumber;;
        exception DivisionByZero;;

        let rec eval = function 
            Z           -> 0
        |   Succ num    -> 1 + (eval num);;

        let convert num = 
            let rec conv = function
                    0   -> Z
                |   n   -> Succ(conv (n-1))
            in conv num;;

        let (>:) a b = if (eval(a) > eval(b)) then 
                            true
                        else
                            false;;

        let ( + ) a b = 
            let rec calc res = function
                Z               -> res
            |   Succ num        -> calc (Succ(res)) num
            in calc a b;;

        let ( - ) a b = if (a > b) then
                            Z
                        else
                            raise NegativeNumber;;

        let ( * ) a b = Succ(Z);;

        let ( / ) a b = if (eval(b) != 0) then
                            Z
                        else
                            raise DivisionByZero;;

    end;;
    

module N = (Natural: NaturalI);;
