(* 
  A dictionary is a data structure that represents a map from keys to values.
  A dictionary has three operations.
  • empty : dictionary
  • add : dictionary -> key -> value -> dictionary
  • find : dictionary -> key -> value
  The value empty is an empty dictionary; 
  The expression add (dict key value) takes an existing dictionary dict and augments it with a new binding key -> value;
  The expression find (dict key) fetches the value in the dictionary dict associated with the key .

  One way to implement a dictionary is to represent it as a function from keys to
  values. Let’s assume we are building a dictionary where the key type is string , the
  value type is int , and the empty dictionary maps every key to zero. This dictionary
  can be implemented abstractly as follows, where we write → for the map from keys to
  values.

  empty = key → 0
  add(dict, key, v) = key'= 
                          → v             if key' = key
                          → dict(key)     otherwise
  find(dict, key) = dict(key)

  1.  Implement the dictionary in OCaml.
  2.  Suppose we have constructed several dictionaries as follows.
    
    let dict1 = add empty "x" 1
    let dict2 = add dict1 "y" 2
    let dict3 = add dict2 "x" 3
    let dict4 = add dict3 "y" 4

  What are the values associated with "x" and "y" in each of the four dictionaries?

*)

let empty = [];;

(* create a tupla *)
let tupla k v = (k, v);;

(* add a tupla into dictionary *)
let add dict key v = dict @ [(tupla key v)];;

(* find a key into the dictionary *)
let find dict key = 
  let rec f = function
      h::tl  -> if (fst h) == key then snd h else f tl
    | []    -> "Not founded"
  in dict;;

let dict1 = add empty "x" 1;;
let dict2 = add dict1 "y" 2;;
let dict3 = add dict2 "x" 3;;
let dict4 = add dict3 "y" 4;;