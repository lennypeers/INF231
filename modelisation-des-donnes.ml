

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

let en_ordre (ens:tuile multiensemble) : tuile multiensemble =

    let comp_ordre (a,occ1: tuile multielement) (b,occ2: tuile multielement) : bool =
        match (a,b) with
            | (_,Joker) -> true
            | (T(n1,Bleu),T(n2,couleur)) -> (couleur <> Bleu) || n1 < n2
            | (T(n1,Rouge),T(n2,couleur)) -> couleur = Jaune || couleur = Noir || (couleur = Rouge && n1 < n2) 
            | (T(n1,Jaune),T(n2,couleur)) -> couleur = Noir || (couleur = Jaune && n1 < n2)
            | (T(n1,Noir),T(n2,couleur)) -> couleur = Noir && n1 < n2
            | _ -> false in

    let rec ordonne (ens: tuile multiensemble) (temp: tuile multiensemble) : tuile multiensemble =
        match ens with 
            | [] -> temp
            | head1::tail1 -> match temp with
                                | [] -> ordonne tail1 [head1]
                                | head2::tail2 -> if (comp_ordre head1 head2)
                                                  then ordonne tail1 (head1::head2::tail2)
                                                  else ordonne ([head1]@tail1@[head2]) tail2
    in ordonne ens [] ;;


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

(* Function that returns the number of jokers in the head of the list
 * and a new list without these jokers in the head. *)

let rec remove_first_joker (li : tuile list) : int * tuile list = 
    match li with
    | [] -> 0,[]
    | T(n,c)::tail -> 0,T(n,c)::tail
    | Joker::tail -> let n,l = remove_first_joker tail in (n + 1, l);;

(* fonction est_suite *)

(* An intermediate function that compares a tuile to the value in the accumulator:
 * the accumulator is a tupple containing the number of jokers, the previous value, the color of the 
 * sequence and the status of the current sequence (is it a suite or not ?) *)

let f_suite (nombre_joker,valeur,couleur,statut : int * valeur * couleur * bool) (tuile : tuile) : int * valeur * couleur * bool = 
    match tuile with
    | Joker -> 
        ( nombre_joker, valeur +1, couleur, statut && ( valeur + 1 <= 14 ) )
    | T(valeur2,couleur2) -> 
        (nombre_joker, valeur +1 , couleur, statut && (nombre_joker < valeur2) && (couleur2 == couleur) && (valeur2 == valeur + 1) && ( valeur2 <= 14 )) ;;

let est_suite (comb: combinaison) : bool = 
    List.length comb >= 3 && 
    let n,liste_simplifiee = remove_first_joker comb in 
    match List.hd liste_simplifiee with 
        | Joker -> failwith "case that will never happen" 
        | T(valeur,couleur) ->
    let _,_,_,statut = List.fold_left f_suite (n,valeur - 1,couleur,true) liste_simplifiee 
    in statut ;;

(* fonction est_groupe *)

(* An intermediate function that compares a tuile to the value in the accumulator:
 * the accumulator is a tupple containing a list of previous colors, the previous number, 
 * and the status of the current sequence (is it a groupe or not ?) *)

let f_groupe (coul_list,num,statut : couleur multiensemble * int * bool) (tuile : tuile) : couleur multiensemble * int * bool =
    match tuile with 
    | Joker -> 
        (coul_list, num, statut)
    | T(valeur,couleur) ->
        ((couleur,1)::coul_list, num, statut && (not (appartient couleur coul_list)) && (valeur == num)) ;;

let est_groupe (comb: combinaison) : bool =
    (List.length comb = 3 || List.length comb = 4) &&
    let _,list_simplifiee = remove_first_joker comb in
    match List.hd list_simplifiee with
        | Joker -> failwith "case that will never happen"
        | T(valeur,_) ->
    let _,_,statut = List.fold_left f_groupe ([], valeur, true) list_simplifiee 
    in statut ;;

(* fonction de verification des combinaisons *)

let combinaison_valide (comb: combinaison) : bool =
    est_suite comb || est_groupe comb ;;

let combinaisons_valides (comb_list: combinaison list) : bool =
    comb_list <> [] && List.fold_left (fun x y -> x && combinaison_valide y ) true comb_list ;; 

(* Q11 : Calcul des points *)

let points_suite (comb : combinaison) : int =
    let n,liste_simplifiee = remove_first_joker comb in 
    match List.hd liste_simplifiee with 
        | Joker -> failwith "case that will never happen" 
        | T(valeur,couleur) ->
    let _,valeur,_,_ = List.fold_left f_suite (n,valeur - 1,couleur,true) liste_simplifiee 
    in List.length comb * ( 2 * valeur - List.length comb + 1 ) / 2 ;;

let points_groupe (comb: combinaison) : int =
    let len = List.length comb in
    let rec f (comb: combinaison):int=
        match comb with
            | Joker::b -> f b
            | T(valeur,couleur)::_ -> valeur * len
            | _ -> failwith "never happen"
    in f comb ;;

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

let premier_coup_ok (m1: main) (p :pose) (m2: main) : bool =
    difference m1 m2 == poseVmens p && points_pose p >= 30 ;;

let coup_ok (t0: table) (m0: main) (t1: table) (m1: main) : bool=
    en_ordre (difference m0 m1) == en_ordre (difference (tableVmens t0) (tableVmens t1)) ;;
 
(* end *)  
