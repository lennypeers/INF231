

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



















(* end *) 
