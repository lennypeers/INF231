(*
231 PROJET
 *)


(* 2 multi-elements *)

type nat = int (* >= 0 *) ;;

type 'a multielement = 'a * nat (* ’a ≡ 𝛼 : réservoir des valeurs *) ;;

(* 3 multi-ensembles *)

type 'a multiensemble = (* ’a ≡ 𝛼 : réservoir des valeurs *)
  | V                                       (* V mis pour « vide » *)
  | A of 'a multielement * 'a multiensemble (* A mis pour « ajout » *) ;;

(* 3.2 Implémentation des fonctions spécifiées dans la section 3.1. *)

(* Q1: *)

let rec cardinal (ens : 'a multiensemble) : int =
  match ens with
  | V -> 0
  | A((_,a), tail) -> a + cardinal tail ;;

(* Q2: *)

let rec nbocc (elt : 'a) (ens : 'a multiensemble) : int =
  match ens with
  | V -> 0
  | A((head,n), tail) ->
        if head = elt
        then n  (* stops at the first element found *)
        else nbocc elt tail   ;;

(* Q3: *)

let appartient (elt : 'a) (ens : 'a multiensemble) : bool = nbocc elt ens <> 0 ;;

(* Q4: *)

let rec inclus (ens1 : 'a multiensemble) (ens2 : 'a multiensemble) : bool =
  match ens1 with
  | V -> true
  | A((_,0), tail) -> inclus tail ens2
  | A((head,n), tail) -> (appartient head ens2) && (n <= (nbocc head ens2)) && (inclus tail ens2) ;;

(* Q5: *)

let rec ajoute (elt,occ:'a multielement) (ens: 'a multiensemble) : 'a multiensemble =
  match ens with
  | V ->  if occ <> 0 (* if the occurence is 0, the function does nothing *)
          then A((elt,occ),V)
          else V
  | A((head,n) ,tail) ->
          if head = elt
          then A((elt,n+occ), tail)
          else A((head,n), ajoute (elt,occ) tail) ;;

(* Q6: *)

let rec supprime (x,n:'a multielement) (ens: 'a multiensemble) : 'a multiensemble =
  match ens with
  | V -> V
  | A((head,occ), tail) ->
     if head = x
     then if max 0 (occ - n) = 0
          then tail     (* remove the element if the occurence is 0, more aesthetic *)
          else A((head,occ - n), tail)
     else A((head,occ), supprime (x,n) tail) ;;

(* Q7: *)

let egaux (ens1 : 'a multiensemble) (ens2 : 'a multiensemble) : bool =
  inclus ens1 ens2 && inclus ens2 ens1 ;;

(* Q8: *)

let rec intersection (ens1: 'a multiensemble) (ens2: 'a multiensemble) : 'a multiensemble =
  match ens1 with
  | V -> V
  | A ((x,n), tail) ->
      if n <> 0 && appartient x ens2
      then A ((x, min (n)(nbocc x ens2)) , intersection tail ens2 )
      else intersection tail ens2 ;;

(* Q9: *)

let rec difference (ens1: 'a multiensemble) (ens2: 'a multiensemble) : 'a multiensemble =
  match ens1 with
  | V -> V
  | A((elt,occ), tail) ->
      if occ <> 0 && not (appartient elt ens2)
      then ajoute (elt,occ) (difference tail ens2)
      else ajoute (elt,max 0 (occ- nbocc elt ens2)) (difference tail ens2);;

(* Q10: *)

let un_dans (ens: 'a multiensemble) : 'a =
  let rec ieme (ens: 'a multiensemble) (index : int)=
    match ens with
      | A((elem,occurence),tail) ->
          if index = 0
          then elem
          else if occurence = 1
               then ieme (tail) (index - 1)
               else ieme (A((elem,occurence-1), tail)) (index-1)
      | _ -> failwith "ensemble vide" (* case that will never happen,
                                      just avoiding the non exhaustive matching*)
  in let ( random : int ) = Random.int(cardinal ens)
in ieme (ens) (random);;


(* Tests *)

(* cardinal *)
assert(cardinal ( A(('a',3), A(('b',2), A(('c',1), V )))) = 6) ;;
assert(cardinal V = 0) ;;

(* occurences *)
assert(nbocc 'c' ( A(('a',3), A(('b',2), A(('c',1), V )))) = 1) ;;
assert(nbocc 'a' ( A(('a',3), A(('b',2), A(('c',1), V )))) = 3) ;;
assert(nbocc 'b' V = 0) ;;

(* appartenance *)
assert(appartient 'c' ( A(('a',3), A(('b',2), A(('c',1), V )))) = true) ;;
assert(appartient 'z' ( A(('a',3), A(('b',2), A(('c',1), V )))) = false) ;;
assert(appartient 'a' V = false) ;;

(* inclusion *)
assert(inclus ( A(('a',3), V )) ( A(('a',3), V )) = true) ;;
assert(inclus ( A(('a',3), V )) ( A(('a',1), V )) = false) ;;
assert(inclus ( A(('a',3), V )) ( A(('a',3), A(('b',4), V ))) = true) ;;
assert(inclus V  ( A(('a',3), V )) = true) ;;
assert(inclus ( A(('a',3), V ))  V = false) ;;

let ens = A(('a',3), A(('b',2), A(('c',1), V ))) ;;
let ens1 = A(('a',3), A(('b',2), V )) ;;
let ens2 = A(('a',3), A(('b',1), A(('c',2), V ))) ;;

(* ajout *)
assert(ajoute ('c',1) ens1 = ens ) ;;
assert(ajoute ('c',1) V = A(('c',1), V )) ;;

(* suppression *)
assert(supprime ('c',1) V = V) ;;
assert(supprime ('c',1) ens = ens1) ;;
assert(supprime ('c',1) (A(('c',1), V)) = V) ;;

(* egalite *)
assert(egaux ens ens1 = false) ;;
assert(egaux ens ens = true) ;;
assert(egaux V ens = false) ;;

(* intersection *)
assert(intersection ens ens2 = A(('a',3), A(('b',1), A(('c',1), V )))) ;;
assert(intersection (A((1,3), V)) (A((1,1), V)) = A((1,1), V)) ;;
assert(intersection V V = V) ;;

(* difference *)
assert((difference ens ens2) = A(('b',1), V)) ;;
assert(difference (A((5,2), A((3,3), V))) (A((5,2), A((3,1), V))) = A((3,2), V)) ;;

(* element aleatoire *)
assert(let x = un_dans ens in (appartient x ens) = true) ;;





(* end *)
