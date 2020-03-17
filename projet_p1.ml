(*
231 PROJECT
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
  | A((_,a),tail) -> a + cardinal (tail) ;;

(* Q2: *)

let rec nbocc (elt : 'a) (ens : 'a multiensemble) : int =
  match ens with
  | V -> 0
  | A((elt2,n),ens2) ->
    if elt2 = elt
    then n  (* stops at the first element found *)
    else nbocc elt ens2   ;;

(* Q3: *)

let rec appartient (elt : 'a) (ens : 'a multiensemble) : bool =
  match ens with
  | V -> false
  | A((elt2,n),tail) -> (elt2 = elt && n <> 0) || (appartient elt tail) ;; (*n peut etre egal a zero*)

(* Q4: *)

let rec inclus (ens1 : 'a multiensemble) (ens2 : 'a multiensemble) : bool =
  match ens1 with
  | V -> true
  | A((_,0),tail) -> inclus tail ens2
  | A((elt,n),tail) -> (appartient elt ens2) && (n <= (nbocc elt ens2)) && (inclus tail ens2) ;;

(* Q5: *)

let rec ajoute (elt,occ:'a multielement) (ens: 'a multiensemble) : 'a multiensemble =
  if occ = 0 (* do nothing if occ = 0, avoid all the recursive calls*)
  then ens
  else match ens with
        | V -> A((elt,occ),V)
        | A((elt2,n),tail) ->
          if elt2 = elt
          then A((elt,n+occ),tail)
          else A((elt2,n),(ajoute (elt,occ) (tail))) ;;

(* Q6: *)

let rec supprime (x,n:'a multielement) (ens: 'a multiensemble) : 'a multiensemble =
  match ens with
  | V -> V
  | A((x2,n2),tail) ->
     if x2 = x
     then if max (0) (n2 - n) = 0
          then tail      (* remove the element if the occurence is 0 *)
          else A((x2,n2 - n),tail)
     else A((x2,n2),supprime (x,n)(tail)) ;;

(* Q7: *)

let egaux (ens1 : 'a multiensemble) (ens2 : 'a multiensemble) : bool =
  inclus ens1 ens2 && inclus ens2 ens1 ;;

(* Q8: *)

let rec intersection (ens1: 'a multiensemble) (ens2: 'a multiensemble) : 'a multiensemble =
  match ens1 with
  | V -> V
  | A ((x,n),tail) ->
      if x <> 0 && appartient x tail
      then A ( (x,min (n)(nbocc x ens2)) , intersection tail ens2 )
      else intersection tail ens2 ;;

(* Q9: *)

let rec difference (ens1: 'a multiensemble) (ens2: 'a multiensemble) : 'a multiensemble =
  match ens1 with
  | V -> V
  | A((elt,occ),tail) ->
      if occ <> 0 && not (appartient elt ens2)
      then ajoute (elt,occ) (difference tail ens2)
      else ajoute (elt,max 0 (occ- nbocc elt ens2)) (difference tail ens2);;

(* Q10: *)

let un_dans (ens: 'a multiensemble) : nat =
  let rec ieme (ens: 'a multiensemble) (index : int) (random : int)=
    match ens with
      | A((elem,occurence),tail) ->
          if index = random
          then elem
          else if occurence = 1
               then ieme (tail) (index+1) (random)
               else ieme (A((elem,occurence-1),tail)) (index+1) (random)
      | V -> failwith "ensemble vide" (* case that will never happen,
                                      just avoiding the non exhaustive matching*)
    and  ( random : int ) = Random.int(cardinal ens)
  in ieme (ens) (0) (random);;






(* end *)
