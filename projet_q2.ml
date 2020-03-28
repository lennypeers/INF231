(*
 * INF231 
 *
 * Thien Dat Phan
 * Ny Aina Pedersen
 *)

(* 4 Reusinage *)

(* Q2 Reusinage du code de la question 3:*)

type nat = int ;; (* >= 0 *) 
type 'a multielement = 'a * nat ;;
type 'a multiensemble = 'a multielement list ;;

(* 1 *)

let rec cardinal (ens : 'a multiensemble) : int =
  match ens with
  | [] -> 0
  | (_,occ)::tail -> occ + cardinal tail ;;

(* 2 *)

let rec nbocc (elt : 'a) (ens : 'a multiensemble) : int =
  match ens with
  | [] -> 0
  | (elt2,n)::tail ->
    if elt2 = elt
    then n
    else nbocc elt tail  (*stops at the first occurence*) ;;

(* 3 *)

let rec appartient (elt : 'a) (ens : 'a multiensemble) : bool = nbocc elt ens <> 0 ;;

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
         else (x2,n2)::supprime (x,n) tail ;;

(* 7 *)

let egaux (ens1 : 'a multiensemble) (ens2 : 'a multiensemble) : bool =
  inclus ens1 ens2 && inclus ens2 ens1 ;;

(* 8 *)

let rec intersection (ens1: 'a multiensemble) (ens2: 'a multiensemble) : 'a multiensemble =
  match ens1 with
  | [] -> []
  | (x,n)::tail ->
      if n <> 0 && appartient x ens2
      then (x,min (n) (nbocc x ens2))::intersection tail ens2
      else intersection tail ens2 ;;

(* 9 *)

let rec difference (ens1: 'a multiensemble) (ens2: 'a multiensemble) : 'a multiensemble =
  match ens1 with
  | [] -> []
  | (elt,occ)::tail ->
      if occ <> 0 && not (appartient elt ens2)
      then  (elt,occ)::(difference tail ens2)
      else (supprime (elt,nbocc elt ens2) [(elt,occ)]) @ difference tail ens2 ;;

(* 10 *)

let un_dans (ens: 'a multiensemble) : 'a =
  let rec ieme (ens: 'a multiensemble) (index : int) =
    match ens with
      | (elem,occurence)::tail ->
          if index < occurence
          then elem
          else ieme tail (index - occurence)
      | _ -> failwith "ensemble vide" (* case that will never happen, just avoiding
                                        the error message of the interpretor *)
  in let ( random : int ) = Random.int(cardinal ens)
  in ieme (ens) (random);;

(* Tests *)

(* cardinal *)
assert (cardinal [('a',5);('b',5);('c',5)] = 15) ;;
assert (cardinal [] = 0) ;;

(* occurences *)
assert (nbocc 'a' [('a',5);('b',5);('c',5)] = 5) ;;
assert (nbocc 'c' [('a',5);('b',5);('c',5)] = 5) ;;
assert (nbocc 'z' [('a',5);('b',5);('c',5)] = 0) ;;
assert (nbocc 'z' [] = 0) ;;

(* appartenance *)
assert (appartient 'a' [] = false) ;;
assert (appartient 'c' [('a',5);('b',5);('c',5)] = true) ;;
assert (appartient 'z' [('a',5);('b',5);('c',5)] = false) ;;

(* inclusion *)
assert (inclus [] [] = true) ;;
assert (inclus [] [(5,3)] = true) ;;
assert (inclus [(5,3)] [] = false) ;;
assert (inclus [('a',2);('b',2)] [('a',2);('b',2)] = true) ;;
assert (inclus [('a',2);('b',2)] [('a',2);('b',2);('c',2)] = true) ;;
assert (inclus [('a',2);('b',2)] [('a',2);('b',1)] = false) ;;

(* ajout *)
assert (ajoute ('a',5) [] = [('a',5)]) ;;
assert (ajoute ('a',0) [] = []) ;;
assert (ajoute ('a',1) [('a',1)] = [('a',2)]) ;;
assert (ajoute ('a',1) [('a',1);('b',1)] = [('a',2);('b',1)]) ;;

(* suppression *)
assert (supprime ('a',5) [] = []) ;;
assert (supprime ('a',0) ['b',5] = ['b',5]) ;;
assert (supprime ('a',1) [('a',1)] = []) ;;
assert (supprime ('a',1) [('a',1);('b',1)] = ['b',1]) ;;

(* egalite *)
assert (egaux [] [] = true) ;;
assert (egaux [] [(5,5)] = false) ;;
assert (egaux [(5,5)] [(5,5)] = true) ;;
assert (egaux [(5,5)] [(5,6)] = false) ;;

(* intersection *)
assert (intersection [('a',2);'b',2] [] = []) ;;
assert (intersection [('a',2);'b',2] [('a',1)] = [('a',1)]) ;;
assert (intersection [] [] = []) ;;

(* difference *)
assert (difference [('a',1);('b',1)] [] = [('a',1);('b',1)]) ;;
assert (difference [(5,5)] [(5,3)] = [(5,2)]) ;;

(* element aleatoire *)
let ens = [('a',5);('b',5);('c',5)] in
assert (let x = un_dans ens in appartient x ens) ;;



(* end *)
