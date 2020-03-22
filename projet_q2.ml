(* 4 Reusinage *)

(* Q2 Reusinage du code de la question 3:*)

type nat = int (* >= 0 *) ;;
type 'a multielement = 'a * nat ;;
type 'a multiensemble = 'a multielement list ;;

(* 1 *)

let rec cardinal (ens : 'a multiensemble) : int =
  match ens with
  | [] -> 0
  | (_,occ)::tail -> occ + cardinal (tail) ;;

(* 2 *)

let rec nbocc (elt : 'a) (ens : 'a multiensemble) : int =
  match ens with
  | [] -> 0
  | (elt2,n)::tail ->
    if elt2 = elt
    then n
    else nbocc elt tail  (*stops at the first occurence*) ;;

(* 3 *)

let rec appartient (elt : 'a) (ens : 'a multiensemble) : bool =
  match ens with
  | [] -> false
  | (elt2,n)::tail -> (elt2 = elt && n <> 0) || (appartient elt tail) ;; (*n peut etre egal a zero*)

(* 4 *)

let rec inclus (ens1 : 'a multiensemble) (ens2 : 'a multiensemble) : bool =
  match ens1 with
  | [] -> true
  | (_,0)::tail -> inclus tail ens2
  | (elt,n)::tail -> (appartient elt ens2) && (n <= (nbocc elt ens2)) && (inclus tail ens2) ;;

(* 5 *)

let rec ajoute (elt,occ:'a multielement) (ens: 'a multiensemble) : 'a multiensemble =
  if occ = 0
  then ens
  else match ens with
        | [] -> (elt,occ)::[]
        | (elt2,n)::tail ->
          if elt2 = elt
          then (elt,n+occ)::tail
          else (elt2,n)::(ajoute (elt,occ)(tail)) ;;

(* 6 *)

let rec supprime (x,n:'a multielement) (ens: 'a multiensemble) : 'a multiensemble =
  match ens with
  | [] -> []
  | (x2,n2)::tail ->
     if x2 = x
     then if max (0) (n2 - n) = 0
          then tail      (* remove the element if the occurence is 0 *)
          else (x2,n2 - n)::tail
     else (x2,n2)::supprime (x,n)(tail) ;;

(* 7 *)

let egaux (ens1 : 'a multiensemble) (ens2 : 'a multiensemble) : bool =
  inclus ens1 ens2 && inclus ens2 ens1 ;;

(* 8 *)

let rec intersection (ens1: 'a multiensemble) (ens2: 'a multiensemble) : 'a multiensemble =
  match ens1 with
  | [] -> []
  | (x,n)::tail ->
      if x <> 0 && appartient x ens2
      then (x,min(n)(nbocc x ens2))::intersection tail ens2
      else intersection tail ens2 ;;

(* 9 *)

let rec difference (ens1: 'a multiensemble) (ens2: 'a multiensemble) : 'a multiensemble =
  match ens1 with
  | [] -> []
  | (elt,occ)::tail ->
      if occ <> 0 && not (appartient elt ens2)
      then ajoute (elt,occ) (difference tail ens2)
      else (supprime (elt,nbocc elt ens2) [(elt,occ)]) @ difference tail ens2 ;;

(* 10 *)

let un_dans (ens: 'a multiensemble) : 'a =
  let rec ieme (ens: 'a multiensemble) (index : int) =
    match ens with
      | (elem,occurence)::tail ->
          if index = 0
          then elem
          else if occurence = 1
               then ieme (tail) (index-1)
               else ieme ((elem,occurence-1)::tail) (index-1)
      | _ -> failwith "ensemble vide" (* case that will never happen, just avoiding
                                        the error message of the interpretor *)
  in let ( random : int ) = Random.int(cardinal ens)
  in ieme (ens) (random);;

(* Q3: Reusinage avec l'ordre superieur *)








(* end *)
