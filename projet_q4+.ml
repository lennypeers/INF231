

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
    let slice_two_jokers = function | t1::tail -> permutation_test f_comp (t1,Joker,Joker)
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
