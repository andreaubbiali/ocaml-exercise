module Matrix = 
    struct
        type matrix = int list list

        (* esercizio 1 *)
        let zeroes n m = 
            List.init n (fun x -> List.init m (fun x -> 0));;

        (* esercizio 2 *)
        let identity n m =
            List.init n (fun x -> List.init m (fun y -> if (x<>y) then 0 else 1));;

        (* esercizio 3 *)
        let init_square_matrix n =
            List.init n (fun x -> List.init n (fun y -> x+y));;



        (** pretty printing functions **)
        let pp_print_elem ppf e = Format.fprintf ppf "%02d" e ;;
        
        let pp_comma ppf () = Format.fprintf ppf ", " ;;
        let pp_comma_nl ppf () = Format.fprintf ppf ",\n " ;;
        
        let pp_print_generic ppf sep p lst =
        Format.fprintf ppf "%a" Format.(pp_print_list ~pp_sep:sep p) lst ;;
        
        let pp_print_row ppf lst =
        let pp_print_row ppf lst = pp_print_generic ppf pp_comma pp_print_elem lst
        in Format.fprintf ppf "[%a]" pp_print_row lst ;;
        
        let pp_print_matrix ppf tbl =
        let pp_print_matrix ppf tbl = pp_print_generic ppf pp_comma_nl pp_print_row tbl
        in Format.fprintf ppf "[%a]" pp_print_matrix tbl ;;

    end;;

    