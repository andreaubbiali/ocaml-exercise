module type Matrix = 
  sig
    type matrix
    val zeroes : int -> int -> matrix
    val identity : int -> matrix
    
    val pp_print_matrix : Format.formatter -> matrix -> unit
  end;;