type 'a set = 'a list
type 'a choice = Element of 'a | Empty
val empty : 'a list
val add : 'a -> 'a list -> 'a list
val choose : 'a -> 'a list -> bool