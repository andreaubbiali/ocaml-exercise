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

    let summable a b = 
        ((lenArr a) == (lenArr b)) && ((lenFstRow a) == (lenFstRow b));;

    let sumArray 

    let sum a b = if (isMatrix a) && (isMatrix b) && (summable a b) then 
                    let rec sumRow
                else
                    raise OperationException;;

    let printRow arr = 
        let rec p = function 
            0   -> Printf.printf "%d\n" arr.(0)
        |   n   -> Printf.printf "%d " arr.(n); p (n-1)
        in p (lenArr arr);;

    let printMatrix arr = 
        let rec print = function
            0   -> printRow arr.(0)
        |   n   -> printRow arr.(n); print (n-1)
        in print (lenArr arr);;

end;;