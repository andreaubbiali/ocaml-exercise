(* 
A social network is a social structure made of individuals (or organizations) called nodes, which are tied (connected) by one or more specific types of
interdependency, such as friendship, kinship, financial exchange, dislike, sexual relationships, or relationships of beliefs, knowledge or prestige.

A graph is an abstract representation of a set of objects where some pairs of the objects are connected by links. The interconnected objects are 
represented by mathematical abstractions called vertices, and the links that connect some pairs of vertices are called edges.

The exercise consists of:

  1-  implementing the social network as a graph, i.e., to define the graph data structure with the operations you consider necessary to implement a social network
  2-  implementing an operation that visits in a convenient way all the elements of the graph
  3-  testing it against a dummy social network.

ATTENZIONE ALL'ORDINE IN CUI PONI I MODULI PERCHÈ ALTRIMENTI NON COMPILA

*)

module type PersonADT = sig
  type person = { id : string; mutable name : string; }
  val create: string -> string -> person
  val get_id: person -> string
  val get_name: person -> string
end;;


module FriendShip(Pers: PersonADT) = struct

  let create_friend p1 p2 = (Pers.get_id p1, Pers.get_id p2);;

  let are_friend l p1 p2=
    let rec check = function 
        h::tl       -> if (h = (Pers.get_id p1 , Pers.get_id p2)) || (h = (Pers.get_id p2, Pers.get_id p1)) then
                         true 
                        else
                         false
      | []          -> false
    in check l;;

  let add_friend l p1 p2 = if (are_friend l p1 p2) == false then
                              create_friend p1 p2::l
                            else
                              l;;
  
end;;

module Person = struct
  type person = {
    id: string;
    mutable name: string
  }

  let create id name = {id = id; name = name};;

  let get_id p = p.id;;
  
  let get_name p = p.name;;

end;;

let p1 = Person.create "111" "Andrea";;
let p2 = Person.create "222" "Luca";;
let p3 = Person.create "333" "Giovanni";;

module F = FriendShip(Person);;

let friend_list = ref [(F.create_friend p1 p2)];;


friend_list := F.add_friend !friend_list p1 p3;;


let main() = 
  Printf.printf "are friend?   %b\n" prova;
  Printf.printf "are friend?   %b\n" prova2;
  Printf.printf "are friend?   %b\n" prova3;

  ;;

main();;