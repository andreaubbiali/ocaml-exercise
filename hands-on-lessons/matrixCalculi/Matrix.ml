module Matrix = 
  struct
    type matrix = int list list

    let zeroes n m = List.init n (fun x -> List.init m (fun y -> 0))

    let identity n = List.init n (fun x -> List.init n (fun y -> if (x=y) then 1 else 0))



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