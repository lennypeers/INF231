(*
 * INF231 
 *
 * Thien Dat Phan
 * Pedersen Ny Aina
 *)

type nat = int (* >= 0 *) ;;

type 'a multielement = 'a * nat ;;

type 'a multiensemble = 'a multielement list;;
  
(* a *)

let cardinal (set: 'a multiensemble) : int = 
    List.fold_left (fun acc (_,occ)  -> acc + occ) 0 set ;;

(* b *)

let nbocc (elt: 'a) (set: 'a multiensemble) : int =
    List.fold_left (fun acc (x,occ) -> if (x = elt) then occ else acc) 0 set ;;

(* c *)

let appartient (elt: 'a) (set: 'a multiensemble) : bool =
    List.exists (fun (x,_) -> x = elt) set ;;

(* d *)

let inclus (set1: 'a multiensemble) (set2: 'a multiensemble) : bool = 
    List.for_all (fun (elt,occ) -> (appartient elt set2) && (occ <= nbocc elt set2)) set1 ;;

(* e *)

let ajoute (elt,occ: 'a multielement) (set: 'a multiensemble) : 'a multiensemble =
    if (appartient elt set || occ = 0)
    then List.map (function | (x,n) when (x = elt) -> (x,n +occ) | (x,n) -> (x,n)) set 
    else set @ [(elt,occ)] ;;

(* f *)

let supprime (elt,occ: 'a multielement) (set: 'a multiensemble) : 'a multiensemble =
    if (occ >= nbocc elt set)
    then List.filter (fun (x,_) -> x <> elt) set
    else List.map (function | (x,n) when (x = elt) -> (x,n -occ) | (x,n) -> (x,n)) set ;;

(* g *)

let intersection (set1: 'a multiensemble) (set2: 'a multiensemble) : 'a multiensemble =
    List.fold_left (fun acc (x,occ) -> if (appartient x set2) 
                                       then ajoute (x, min occ (nbocc x set2)) acc 
                                       else acc )    [] set1 ;;

(* g *)

let difference (set1: 'a multiensemble) (set2: 'a multiensemble) : 'a multiensemble =
    List.fold_left (fun acc (elt,occ) -> (acc @ supprime (elt, nbocc elt set2) [elt,occ])) [] set1 ;;

(* non modified functions *)

let egaux (set1: 'a multiensemble) (set2: 'a multiensemble) : bool =
    inclus set1 set2 && inclus set2 set1 ;;

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
