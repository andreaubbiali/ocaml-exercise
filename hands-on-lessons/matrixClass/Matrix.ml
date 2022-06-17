module Matrix = struct 

    type t = (int array) array;;
    exception OperationException;;
    exception NotAMatrix;;

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

    let makeOp f a b res =
        let rowNum = lenFstRow a in
        let colNum = lenArr a in
            let rec exec c r = match (c, r) with
                (cc, rr) when cc == colNum && rr == rowNum      -> f a b res cc rr; res
            |   (cc, rr) when rr == rowNum                      -> f a b res cc rr; exec (cc+1) 0
            |   (cc, rr)                                        -> f a b res cc rr; exec cc (rr+1)
            in exec 0 0;;

    let (+) a b res col row = res.(col).(row) <- (a.(col).(row) + b.(col).(row));;
    let ( * ) a b res col row = res.(col).(row) <- (a.(col).(row) * b.(col).(row));;
(*
    let (+) a b = 
        let rowNum = lenFstRow a in
        let colNum = lenArr a in
        let res = Array.make_matrix (colNum+1) (rowNum+1) 0 in
            let rec sumMatrix c r = match (c, r) with
                (cc, rr) when cc == colNum && rr == rowNum      -> res.(cc).(rr) <- (a.(cc).(rr) + b.(cc).(rr)); res
            |   (cc, rr) when rr == rowNum                      -> res.(cc).(rr) <- (a.(cc).(rr) + b.(cc).(rr)); sumMatrix (cc+1) 0
            |   (cc, rr)                                        -> res.(cc).(rr) <- (a.(cc).(rr) + b.(cc).(rr)); sumMatrix cc (rr+1)
            in sumMatrix 0 0;;
*)

    let sum a b = if (isMatrix a) && (isMatrix b) && (sameLen a b) then 
                    let res = Array.make_matrix (Array.length a) (Array.length a.(0)) 0 in
                    makeOp (+) a b res
                else
                    raise OperationException;;

    let multiplication a b =  if (isMatrix a) && (isMatrix b) && (sameLen a b) then 
                    let res = Array.make_matrix (Array.length a) (Array.length a.(0)) 0 in
                    makeOp ( * ) a b res
                else
                    raise OperationException;;

    let printRow arr = 
        let rec p = function 
            0   -> Printf.printf "%d\n" arr.(0)
        |   n   -> Printf.printf "%d " arr.(n); p (n-1)
        in p (lenArr arr);;

    let print arr = 
        let rec print = function
            0   -> printRow arr.(0)
        |   n   -> printRow arr.(n); print (n-1)
        in print (lenArr arr);;

end;;