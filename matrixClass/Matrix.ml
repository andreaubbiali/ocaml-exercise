module Matrix = struct 

    type t = (int array) array;;
    exception OperationException;;
    exception NotAMatrix;;
    exception MatrixNotEqual;;

    let lenArr v = (Array.length v)-1;;
    let lenRow v pos = (Array.length v.(pos))-1;;
    let lenFstRow v = lenRow v 0;;

    let isMatrix a =
        let lenFst = lenFstRow a in
        let rec check = function
            0   -> true 
        |   n   -> if (lenRow a n) == lenFst then check (n-1) else raise NotAMatrix
        in check (lenArr a);;

    let sameLen a b = 
        ((lenArr a) == (lenArr b)) && ((lenFstRow a) == (lenFstRow b));;

    let makeOp f rowNum colNum =
            let rec exec c r = match (c, r) with
                (cc, rr) when cc == colNum && rr == rowNum      -> f cc rr
            |   (cc, rr) when rr == rowNum                      -> f cc rr; exec (cc+1) 0
            |   (cc, rr)                                        -> f cc rr; exec cc (rr+1)
            in exec 0 0;;

    let eq a b col row =  if not(a.(col).(row) == b.(col).(row)) then
                            raise MatrixNotEqual;;
    let cp a res col row = res.(col).(row) <- a.(col).(row);;
    let (+) a b res col row = res.(col).(row) <- (a.(col).(row) + b.(col).(row));;
    let scalarMult a num res col row = res.(col).(row) <- (a.(col).(row) * num);;
    let ( * ) a b res col row = res.(col).(row) <- (a.(col).(row) * b.(col).(row));;
    let printCell a rowNum col row = match row with
        n when n == rowNum      -> Printf.printf "%d\n" a.(col).(row)
    |   _                       -> Printf.printf "%d " a.(col).(row);;

    let equivalence a b = if (isMatrix a) && (isMatrix b) && (sameLen a b) then
                            try
                                makeOp (fun col row -> eq a b col row) 3 3;
                                true
                            with MatrixNotEqual ->
                                false
                        else
                            false;;

    let copy a = if (isMatrix a) then
                    let res = Array.make_matrix (Array.length a) (Array.length a.(0)) 0 in
                    makeOp (fun col row -> cp a res col row) 3 3;
                    res
                else
                    raise OperationException;;

    let sum a b = if (isMatrix a) && (isMatrix b) && (sameLen a b) then 
                    let res = Array.make_matrix (Array.length a) (Array.length a.(0)) 0 in
                    makeOp (fun col row -> (+) a b res col row) 3 3;
                    res
                else
                    raise OperationException;;

    let scalarMultiplication a num =  if (isMatrix a) then 
                    let res = Array.make_matrix (Array.length a) (Array.length a.(0)) 0 in
                    makeOp (fun col row -> scalarMult a num res col row) 3 3;
                    res
                else
                    raise OperationException;;

    let multiplication a b =  if (isMatrix a) && (isMatrix b) && (sameLen a b) then 
                    let res = Array.make_matrix (Array.length a) (Array.length a.(0)) 0 in
                    makeOp (fun col row -> ( * ) a b res col row) 3 3;
                    res
                else
                    raise OperationException;;

    let print m = if (isMatrix m) then
                    makeOp (fun col row -> printCell m 3 col row) 3 3;; 

end;;