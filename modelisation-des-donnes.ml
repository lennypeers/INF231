

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








(* end *) 
