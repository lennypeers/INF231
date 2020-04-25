(* Some functions to make the interface *)

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

let print_mens (set: tuile multiensemble) : unit =
    (* conversion from mens to tuile list *)
    let set = List.fold_left (fun acc (x,n) -> match n with 
                                            | 1 -> acc @ [x]
                                            | _ -> acc @ [x;x] ) [] set
    in List.fold_left (fun acc x -> print_tuile x ; acc) () set;
     print_string normal "\n";;

let print_comb (comb: combinaison) : unit =
    List.fold_left (fun acc x -> print_tuile x ; acc) () comb ;
    print_string normal "\n" ;;

let print_table (table: table) : unit =
    List.fold_left (fun acc x -> print_comb x ; print_string normal "\n"; acc) () table ;
    print_string normal "\n" ;;

