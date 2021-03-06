(** Arbre binaire de recherche *)

type 'a binTree = 
    | Leaf
    | Node of 'a * 'a binTree * 'a binTree


let feuille = Leaf
let h1 = Node(6,Leaf,Leaf)
let h2 = Node(4,Node(3,Leaf,Leaf),h1)

let rec lookup x = function
  | Leaf -> false
  | Node(v,g,d) -> v = x || if x > v then lookup x d else lookup x g

let rec insertNode (tr : 'a binTree) (el : 'a) : 'a binTree =
    match tr with
        Leaf -> Node(el,Leaf,Leaf)
        | Node(e,g,d) as n -> if el = e then n else
                if el > e then Node(e,g,insertNode d el)
                else Node(e,insertNode g el,d)

let ex = Node(7,Node(6,Leaf,Leaf),Node(12,Node(10,Node(9,Leaf,Leaf),Node(11,Leaf,Leaf)),Node(14,Leaf,Leaf)))

(** fonction utilitaire pour aider a 
recuperer le plus petit predecesseur plus grand d'un noeud *)
let rec minGreaterPredecessorHelper (tr : 'a binTree) : 'a = 
    match tr with
        Leaf -> raise Not_found
        | Node(e,Leaf,_) -> e
        | Node(e,g,d) -> minGreaterPredecessorHelper g


(** cas 1 : supprimer un noeud sans enfants*)
(** cas 2 : supprimer un noeud avec un enfant *)
(** cas 3 : supprimer un noeud avec deux enfants *)

let rec deleteNode (tr: 'a binTree) (el : 'a) : 'a binTree = 
    match tr with
        Leaf -> raise Not_found
        (** cas 1 *)
        | Node(e,Leaf,Leaf) -> if el = e then Leaf else deleteNode Leaf el
        (** cas 2 *)
        | Node (e,g,Leaf) -> if el = e then g
                             else if el > e then Node(e,g,deleteNode Leaf el)
                                  else  Node(e,deleteNode g el,Leaf)
        (** cas 2 *)
        | Node (e,Leaf,d) -> if el = e then d
                             else if el > e then Node(e,Leaf,deleteNode d el)
                                  else  Node(e,deleteNode Leaf el,d)
        (** cas 3 on prend le sous arbre droit, et on cherche son plus petit element, ça remplace
            le noeud supprimé, et on oublie pas de supprimer ce plus petit element de l'arbre pour pas
            avoir de doublon *)
        | Node (e,g,d) -> if el = e then let minPred = minGreaterPredecessorHelper d in
                          Node(minPred,g, deleteNode d minPred)
                          else if el > e then Node(e,g,deleteNode d el)
                               else  Node(e,deleteNode g el,d)

