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


(* Q4 *)

type couleur = 
    | Bleu
    | Rouge
    | Jaune 
    | Noir ;;

type valeur = int (* restreint à l'intervalle [1,13] *) ;;

type tuile = 
    | Joker 
    | T of valeur * couleur ;;


(* Q5 *)

type combinaison = tuile list ;; (* avec l'ordre des tuiles dans un ordre 
                                    croissant et de même couleur ou constant
                                    avec des couleurs différentes. *)

type table = combinaison list ;;

type pose = combinaison list ;;


(* Q6 *) 

type main = tuile multiensemble ;;

type pioche = tuile multiensemble ;;

let cst_PIOCHE_INIT : pioche = 

  [(T (1, Bleu), 2); (T (2, Bleu), 2); (T (3, Bleu), 2); (T (4, Bleu), 2);
   (T (5, Bleu), 2); (T (6, Bleu), 2); (T (7, Bleu), 2); (T (8, Bleu), 2);
   (T (9, Bleu), 2); (T (10, Bleu), 2); (T (11, Bleu), 2); (T (12, Bleu), 2);
   (T (13, Bleu), 2); (T (1, Rouge), 2); (T (2, Rouge), 2); (T (3, Rouge), 2);
   (T (4, Rouge), 2); (T (5, Rouge), 2); (T (6, Rouge), 2); (T (7, Rouge), 2);
   (T (8, Rouge), 2); (T (9, Rouge), 2); (T (10, Rouge), 2);
   (T (11, Rouge), 2); (T (12, Rouge), 2); (T (13, Rouge), 2);
   (T (1, Jaune), 2); (T (2, Jaune), 2); (T (3, Jaune), 2); (T (4, Jaune), 2);
   (T (5, Jaune), 2); (T (6, Jaune), 2); (T (7, Jaune), 2); (T (8, Jaune), 2);
   (T (9, Jaune), 2); (T (10, Jaune), 2); (T (11, Jaune), 2);
   (T (12, Jaune), 2); (T (13, Jaune), 2); (T (1, Noir), 2); (T (2, Noir), 2);
   (T (3, Noir), 2); (T (4, Noir), 2); (T (5, Noir), 2); (T (6, Noir), 2);
   (T (7, Noir), 2); (T (8, Noir), 2); (T (9, Noir), 2); (T (10, Noir), 2);
   (T (11, Noir), 2); (T (12, Noir), 2); (T (13, Noir), 2); (Joker,2)] ;;


(* Q7 *)

let en_ordre (ens: tuile multiensemble) : tuile multiensemble =
    let comp_ordre (a,_: tuile multielement) (b,_: tuile multielement) : bool =
        match (a,b) with
            | (_,Joker) -> true
            | (T(n1,couleur1),T(n2,couleur2)) -> (couleur1 < couleur2) || (couleur1 = couleur2 && n1 <= n2)
            | _ -> false in
    let rec insertion x = function | [] -> x::[]
                                   | head::tail -> if comp_ordre x head
                                                   then x::head::tail
                                                   else head::(insertion x tail) in
    let rec tri = function | [] -> []
                           | head::tail -> insertion head (tri tail)  
    in tri ens ;;

(* 6.4 Les joueurs *)

type joueur = J1 | J2 ;;

type statut = joueur * bool * main ;;


(* 6.5 Etat d'une partie *)

type etat = ( statut * statut ) * table * pioche * joueur ;;


(* 6.5.1 Etat initial *)

(* Q8 *)

let extraire (n:int) (p:pioche) : main * pioche =
    let rec extraction (n:int) (p:pioche) (temp:main) =
        if n = 0
        then en_ordre temp, en_ordre p
        else let elem = un_dans p in extraction (n-1) (supprime (elem,1) p) (ajoute (elem,1) temp) in
    extraction n p [] ;;


let distrib () : main * main * pioche =
  let (main1,pioche1) = extraire 14 cst_PIOCHE_INIT in 
  let (main2,pioche2)= extraire 14 pioche1 in 
  (main1,main2,pioche2) ;;


let init_partie () : etat =
  let (main1,main2,pioche) = distrib () in
  (((J1,false,main1),(J2,false,main2)),[],pioche,J1) ;;


(* 6.5.2 Accès aux informations d'un état *)

(* 1 *)

let joueur_courant (etat:etat) : joueur = 
    let (_,_,_,joueur) = etat in joueur ;;

let joueur_suivant (etat:etat) : joueur =
    let courant = joueur_courant etat in if courant = J1 
                                         then J2
                                         else J1 ;;
    
(* 2 *)

let la_table (etat:etat) : table =
    let (_,table,_,_) = etat in table ;;

(* 3 *)

let la_pioche (etat:etat) : pioche = 
    let (_,_,pioche,_) = etat in pioche ;;

(* 4 *)

let le_statut (joueur:joueur) (etat:etat) : statut = 
    let ((j1,j2),_,_,_) = etat in if joueur = J1 
                                  then j1 
                                  else j2 ;;

(* 5 *)

let la_main (joueur:joueur) (etat:etat) : main = 
    let (_,_,main) = le_statut joueur etat in main ;;


(* Q10 *)

(* fonction est_suite *)

(* An intermediate function that compares a tuile to the value in the accumulator:
 * the accumulator is a tupple containing the length of the combinaison, the previous value, 
 * the color of the sequence, a bool that says if we already matched a tuile that is not 
 * a Joker and the status of the current sequence (is it a suite or not ?) *)

let f_suite (len,valeur,couleur,est_debut,statut : int * valeur * couleur * bool * bool) (tuile : tuile) : int * valeur * couleur * bool * bool = 
    match tuile with
    | Joker -> 
       if est_debut
       then (len +1, valeur +1, couleur, true, true)    
       else (len +1, valeur +1, couleur, false, statut && ( valeur + 1 <= 13 ))
    | T(valeur2,couleur2) -> 
       if est_debut
       then (len +1, valeur2, couleur2, false, (valeur < valeur2) && (valeur2 <= 13) )
       else (len +1, valeur +1 , couleur, false, statut && (couleur2 = couleur) && (valeur2 = valeur + 1) && ( valeur2 <= 13 )) ;;

let est_suite (comb: combinaison) : bool = 
    let len,_,_,_,statut = List.fold_left f_suite (0, 0, Jaune, true, true) comb 
    in len >= 3 && statut ;;

(* fonction est_groupe *)

(* An intermediate function that compares a tuile to the value in the accumulator:
 * the accumulator is a tupple containing the length of the combinaison, a list of 
 * previous colors, the previous number, 
 * a bool that says if we already matched a tuile that is not a Joker
 * and the status of the current sequence (is it a groupe or not ?) *)

let f_groupe (len,coul_list,num,est_debut,statut : int * couleur multiensemble * valeur * bool * bool) (tuile : tuile) : int * couleur multiensemble * valeur * bool * bool =
    match tuile with 
    | Joker -> 
        (len +1, coul_list, num, est_debut, statut)
    | T(valeur,couleur) ->
        if est_debut
        then (len +1, (couleur,1)::[], valeur, false, true)
        else (len +1, (couleur,1)::coul_list, num, false, statut && (not (appartient couleur coul_list)) && (valeur = num)) ;;

let est_groupe (comb: combinaison) : bool =
    let len,_,_,_,statut = List.fold_left f_groupe (0, [], 0, true, true) comb
    in (len = 3 || len = 4) && statut ;;

(* fonction de verification des combinaisons *)

let combinaison_valide (comb: combinaison) : bool =
    est_suite comb || est_groupe comb ;;

let combinaisons_valides (comb_list: combinaison list) : bool =
    comb_list <> [] && List.fold_left (fun x y -> x && combinaison_valide y ) true comb_list ;; 

(* Q11 : Calcul des points *)

let points_suite (comb : combinaison) : int =
    let len,valeur,_,_,_ = List.fold_left f_suite (0, 0, Noir, true, true) comb 
    in len * ( 2 * valeur - len + 1 ) / 2 ;;

let points_groupe (comb: combinaison) : int =
    let len,_,valeur,_,_ = List.fold_left f_groupe (0, [], 0, true, true) comb in
    valeur * len ;;

let points_pose (pose : pose) : int =
    let f_pose = fun (acc : int) (x : combinaison) ->
        acc + (
        if (est_groupe x) && (est_suite x)
        then max (points_groupe x) (points_suite x)
        else
            if est_groupe x
            then  points_groupe x
            else  points_suite x ) in
    List.fold_left f_pose 0 pose ;;

(* Q12 : tableVmens *)

let tableVmens (table: table) : tuile multiensemble =
    en_ordre (List.fold_left (fun acc x -> ajoute (x,1) acc) [] (List.flatten table) );;


(* Q13 *)

let premier_coup_ok (m0: main) (p :pose) (m1: main) : bool =
    (combinaisons_valides p) && (difference m0 m1 = tableVmens p) && (points_pose p >= 30) ;;

let coup_ok (t0: table) (m0: main) (t1: table) (m1: main) : bool =
    (combinaisons_valides t1) && (en_ordre (difference m0 m1) = en_ordre (difference (tableVmens t1) (tableVmens t0)))
     && (cardinal m0 > cardinal m1) ;;

(* Q14 *)

(* Q15 *)

(* 
 * Algorithm n2 : testing all the possible cases.
 * In this algorithmn, we focus on the combinations of 3 tuiles.
 *)

(* permutation_test: function that checks if the permutation of a 
 *                   tupple of three tuiles can verify a predicate *) 

let permutation_test (test: combinaison -> bool) =
    function  | t1,t2,t3 when test [t1;t2;t3] -> [t1;t2;t3] 
              | t1,t2,t3 when test [t1;t3;t2] -> [t1;t3;t2] 
              | t1,t2,t3 when test [t2;t1;t3] -> [t2;t1;t3] 
              | t1,t2,t3 when test [t2;t3;t1] -> [t2;t3;t1] 
              | t1,t2,t3 when test [t3;t2;t1] -> [t3;t2;t1] 
              | t1,t2,t3 when test [t3;t1;t2] -> [t3;t1;t2] 
              |  _ -> [] ;;

(* comparator: function that, for a given sort conditions, and a given predicate, slices into
 *             a sorted list and returns the minimal combinaison that satisfies the predicate. *)

let comparator (f_sort: tuile -> tuile -> int) (f_comp: combinaison -> bool) (main: main) : combinaison =

    (* conversion of the main to a tuile list while squeezing it (remove the unnecessary copies) *)
    let number_joker,main = List.fold_left (fun (number_joker,acc) x -> match x with
                                            | Joker,n -> n,acc
                                            | tuile,_ -> number_joker,tuile::acc ) (0,[]) main in
    (* sorting the list *)
    let main = List.sort (f_sort) main in

    (* slice with no joker *)
    let rec slice_no_joker = function | t1::t2::t3::tail -> if f_comp [t1;t2;t3] 
                                                            then [t1;t2;t3]
                                                            else slice_no_joker (t2::t3::tail)
                                      | _ -> [] in

    (* slice with one joker *)
    let rec slice_one_joker = function | t1::t2::tail -> let ret = permutation_test f_comp (t1,t2,Joker) in
                                                             if ret <> []
                                                             then ret
                                                             else slice_one_joker (t2::tail) 
                                       | _ -> [] in


    (* slice with two jokers *)
    let slice_two_jokers = function | t1::_ -> permutation_test f_comp (t1,Joker,Joker)
                                    | _ -> [] in

    (* returning the combinaison with smallest number of joker *)
    match (main,number_joker) with
        | (main,_) when (slice_no_joker main <> []) -> slice_no_joker main 
        | (main,n) when n >= 1 && (slice_one_joker main <> []) -> slice_one_joker main
        | (main,2) when (slice_two_jokers main <> []) -> slice_two_jokers main 
        | _ -> [] ;;

let extraction_suite = comparator (fun x y -> match x,y with
                                     | T(n1, couleur1),T(n2, couleur2) 
                                       when (couleur1 < couleur2) || (couleur1 = couleur2 && n1 <= n2) -> -1
                                     | _ -> 1 ) (est_suite) ;;

let extraction_groupe = comparator (fun x y -> match x,y with
                                     | T(n1, _),T(n2, _) when n1 > n2 -> 1
                                     | T(n1, _),T(n2, _) when n1 = n2 -> 0
                                     | _ -> -1 ) (est_groupe) ;;


(* Q16 *)

let piocher (etat: etat) : etat =
  let (((j1 ,b1, m1), (j2,b2,m2)), table, pioche, joueur) = etat in 
  if pioche = [] 
  then etat
  else 
    let new_card = un_dans pioche in 
    let pioche = supprime (new_card,1) pioche in
    match joueur with
    | J1 ->
        let m1 = en_ordre (ajoute (new_card,1) m1)         
        in (((j1,b1,m1), (j2,b2,m2)), table, pioche, J2)
    | J2->
        let m2 = en_ordre (ajoute (new_card,1) m2)
        in (((j1,b1,m1), (j2,b2,m2)), table, pioche, J1) ;;

let remove_from_hand (liste: tuile multiensemble) (main: main) =
    List.fold_right (fun elem x -> supprime elem x) liste main ;;

let jouer_1_coup (etat: etat) (table: table) : etat = 
    let (((j1 ,b1, m1), (j2,b2,m2)), table1, pioche, joueur) = etat in 
    match joueur with 
    | J1 -> let diff = difference (tableVmens table) (tableVmens table1) in
            let new_main = remove_from_hand diff m1 in 
                 if b1 = false || not (coup_ok table1 m1 table new_main) 
                 then etat
                 else ((j1, b1, new_main), (j2, b2, m2)), table, pioche, J2 
    | J2 -> let diff = difference (tableVmens table) (tableVmens table1) in
            let new_main = remove_from_hand diff m2 in 
                 if b2 = false || not (coup_ok table1 m2 table new_main) 
                 then etat
                 else ((j1, b1, m1), (j2, b2, new_main)), table, pioche, J1 ;;

let jouer_1er_coup (etat: etat) (pose: pose) : etat =
    let (((j1 ,b1, m1), (j2,b2,m2)), table, pioche, joueur) = etat in 
    let new_table = pose @ table in
    match joueur with
    | J1 -> let new_main = remove_from_hand (tableVmens pose) m1 in
                if b1 = true || not (premier_coup_ok m1 pose new_main)
                then etat
                else ((j1, true, new_main),(j2, b2, m2)), new_table, pioche, J2
    | J2 -> let new_main = remove_from_hand (tableVmens pose) m2 in
                if b2 = true || not (premier_coup_ok m2 pose new_main)
                then etat
                else ((j1, b1, m1),(j2, true, new_main)), new_table, pioche, J1 ;;

    

(* end *)  
(* Some functions to make the interface *)

(*#require "ANSITerminal"*)

open ANSITerminal ;;

(* somme shortcut for colors *)

let bleu : style list = [Foreground(Blue)] and
    rouge : style list = [Foreground(Red)] and 
    jaune : style list = [Foreground(Yellow)] and
    noir : style list = [Foreground(White) ; Background(Black)] and
    joker : style list= [Foreground(Cyan)] and
    normal : style list = [] ;;

let tuile2string = function | Joker -> " [J] ",joker
                            | T(n,Bleu) -> " [" ^ string_of_int(n) ^ "] ",bleu
                            | T(n,Jaune) -> " [" ^ string_of_int(n) ^ "] ",jaune
                            | T(n,Noir) -> " [" ^ string_of_int(n) ^ "] ",noir
                            | T(n,Rouge) -> " [" ^ string_of_int(n) ^ "] ",rouge ;;

let print_tuile (tuile: tuile) : unit =
    let str,style = tuile2string tuile in
    print_string (style) (str) ;;

let print_mens (set: tuile multiensemble) =
    (* conversion from mens to tuile list *)
    let set = List.fold_left (fun acc (x,n) -> match n with 
                                            | 1 -> acc @ [x]
                                            | _ -> acc @ [x;x] ) [] set
    in List.fold_left (fun acc x -> print_tuile x ; acc) () set;
     print_string normal "\n";;

let print_comb (comb: combinaison) =
    List.fold_left (fun acc x -> print_tuile x ; acc) () comb ;
    print_string normal "\n" ;;

let print_table (table: table) =
    List.fold_left (fun acc x -> print_comb x ; print_string normal "\n"; acc) () table ;
    print_string normal "\n" ;;

(*  main program *)

let () = print_string normal "La partie va commencer...\nTappez c pour clean l'écran\nTappez p pour piocher\n" 

let etat = init_partie () ;;

let rec lire_combinaison () : combinaison =
    let inp = read_line () in
        match inp with
        | "s" -> []
        | "1N" -> T(1,Noir)::lire_combinaison ()
        | "2N" -> T(2,Noir)::lire_combinaison ()
        | "3N" -> T(3,Noir)::lire_combinaison ()
        | "4N" -> T(4,Noir)::lire_combinaison ()
        | "5N" -> T(5,Noir)::lire_combinaison ()
        | "6N" -> T(6,Noir)::lire_combinaison ()
        | "7N" -> T(7,Noir)::lire_combinaison ()
        | "8N" -> T(8,Noir)::lire_combinaison ()
        | "9N" -> T(9,Noir)::lire_combinaison ()
        | "10N" -> T(10,Noir)::lire_combinaison ()
        | "11N" -> T(11,Noir)::lire_combinaison ()
        | "12N" -> T(12,Noir)::lire_combinaison ()
        | "13N" -> T(13,Noir)::lire_combinaison ()
        | "1J" -> T(1,Jaune)::lire_combinaison ()
        | "2J" -> T(2,Jaune)::lire_combinaison ()
        | "3J" -> T(3,Jaune)::lire_combinaison ()
        | "4J" -> T(4,Jaune)::lire_combinaison ()
        | "5J" -> T(5,Jaune)::lire_combinaison ()
        | "6J" -> T(6,Jaune)::lire_combinaison ()
        | "7J" -> T(7,Jaune)::lire_combinaison ()
        | "8J" -> T(8,Jaune)::lire_combinaison ()
        | "9J" -> T(9,Jaune)::lire_combinaison ()
        | "10J" -> T(10,Jaune)::lire_combinaison ()
        | "11J" -> T(11,Jaune)::lire_combinaison ()
        | "12J" -> T(12,Jaune)::lire_combinaison ()
        | "13J" -> T(13,Jaune)::lire_combinaison ()
        | "1R" -> T(1,Rouge)::lire_combinaison ()
        | "2R" -> T(2,Rouge)::lire_combinaison ()
        | "3R" -> T(3,Rouge)::lire_combinaison ()
        | "4R" -> T(4,Rouge)::lire_combinaison ()
        | "5R" -> T(5,Rouge)::lire_combinaison ()
        | "6R" -> T(6,Rouge)::lire_combinaison ()
        | "7R" -> T(7,Rouge)::lire_combinaison ()
        | "8R" -> T(8,Rouge)::lire_combinaison ()
        | "9R" -> T(9,Rouge)::lire_combinaison ()
        | "10R" -> T(10,Rouge)::lire_combinaison ()
        | "11R" -> T(11,Rouge)::lire_combinaison ()
        | "12R" -> T(12,Rouge)::lire_combinaison ()
        | "13R" -> T(13,Rouge)::lire_combinaison ()
        | "1B" -> T(1,Bleu)::lire_combinaison ()
        | "2B" -> T(2,Bleu)::lire_combinaison ()
        | "3B" -> T(3,Bleu)::lire_combinaison ()
        | "4B" -> T(4,Bleu)::lire_combinaison ()
        | "5B" -> T(5,Bleu)::lire_combinaison ()
        | "6B" -> T(6,Bleu)::lire_combinaison ()
        | "7B" -> T(7,Bleu)::lire_combinaison ()
        | "8B" -> T(8,Bleu)::lire_combinaison ()
        | "9B" -> T(9,Bleu)::lire_combinaison ()
        | "10B" -> T(10,Bleu)::lire_combinaison ()
        | "11B" -> T(11,Bleu)::lire_combinaison ()
        | "12B" -> T(12,Bleu)::lire_combinaison ()
        | "13B" -> T(13,Bleu)::lire_combinaison ()
        | "J" -> Joker::lire_combinaison ()
        | _ -> lire_combinaison () ;;

let lire_table () : table = 
    print_string normal "Combien de combinaisons contient la table?\n";
    let n = read_int () in
    let rec lire (n: int) : table =
        if n = 0
        then []
        else (lire_combinaison ())::(lire (n -1)) 
    in lire n ;;

let est_premier_coup (etat: etat) : bool =
    let _,ret,_ = le_statut (joueur_courant etat) etat in
    not ret ;;
 
let rec loop (etat: etat) : etat =
    if (la_pioche etat = [] || (la_main J1 etat = []) || (la_main J2 etat = []))
    then etat 
    else 
        begin
            erase Above ;
            print_string normal "Voici la table:\n";
            print_table (la_table etat) ;
            print_string normal "Voici votre main:\n";
            print_mens (la_main (joueur_courant etat) etat) ;
            print_string normal "Vous choisissez de piocher/poser ?\n";
        let input = read_line () in
         match input with 
         | "piocher" -> loop (piocher etat)
         | "poser" -> if (est_premier_coup etat)
                      then loop (jouer_1er_coup etat (lire_table ()))
                      else loop (jouer_1_coup etat (lire_table())) 
         | "q" -> print_string normal "Au revoir \n"; exit 0
         | _ -> loop (etat) 
        end ;; 
        
let etat = loop(etat) ;; 


