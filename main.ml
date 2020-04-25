(*  main program *)

let clean () : unit =
    set_cursor 1 1;
    erase Below ;;

let help () : unit = 
    print_string normal "- To enter a tuile, type the number, followed by the first letter of its color\n" ;
    print_string normal "- To enter a combination, type the sequence of tuiles\n" ;
    print_string normal "- For instance, the combination T(1,Bleu) T(1,Jaune) T(1,Noir) T(1,Rouge) Joker is:\n" ;
    print_string [Foreground(Green)] "           1B 1J 1N 1R J\n" ;
    print_string normal "- To enter a table or a pose, type the combinations, line by line.\n" ;
    print_string normal "- To quit the game, type q\n\n\n" ;
    print_string [Foreground(Red)] "Hit enter to play!\n" ;
    let _ = read_line () 
    in () ;
;;

let welcome () : unit = 
    clean () ;
    print_string [cyan] "A terminal based Rummikub\n" ;
    print_string [cyan] "Powered by ANSITerminal\n\n" ;
    help () ;;

let string2tuiles (inp: string) : tuile list =
        match inp with
        | "1N" -> [T(1,Noir)]
        | "2N" -> [T(2,Noir)]
        | "3N" -> [T(3,Noir)]
        | "4N" -> [T(4,Noir)]
        | "5N" -> [T(5,Noir)]
        | "6N" -> [T(6,Noir)]
        | "7N" -> [T(7,Noir)]
        | "8N" -> [T(8,Noir)]
        | "9N" -> [T(9,Noir)]
        | "10N" -> [T(10,Noir)]
        | "11N" -> [T(11,Noir)]
        | "12N" -> [T(12,Noir)]
        | "13N" -> [T(13,Noir)]
        | "1J" -> [T(1,Jaune)]
        | "2J" -> [T(2,Jaune)]
        | "3J" -> [T(3,Jaune)]
        | "4J" -> [T(4,Jaune)]
        | "5J" -> [T(5,Jaune)]
        | "6J" -> [T(6,Jaune)]
        | "7J" -> [T(7,Jaune)]
        | "8J" -> [T(8,Jaune)]
        | "9J" -> [T(9,Jaune)]
        | "10J" -> [T(10,Jaune)]
        | "11J" -> [T(11,Jaune)]
        | "12J" -> [T(12,Jaune)]
        | "13J" -> [T(13,Jaune)]
        | "1R" -> [T(1,Rouge)]
        | "2R" -> [T(2,Rouge)]
        | "3R" -> [T(3,Rouge)]
        | "4R" -> [T(4,Rouge)]
        | "5R" -> [T(5,Rouge)]
        | "6R" -> [T(6,Rouge)]
        | "7R" -> [T(7,Rouge)]
        | "8R" -> [T(8,Rouge)]
        | "9R" -> [T(9,Rouge)]
        | "10R" -> [T(10,Rouge)]
        | "11R" -> [T(11,Rouge)]
        | "12R" -> [T(12,Rouge)]
        | "13R" -> [T(13,Rouge)]
        | "1B" -> [T(1,Bleu)]
        | "2B" -> [T(2,Bleu)]
        | "3B" -> [T(3,Bleu)]
        | "4B" -> [T(4,Bleu)]
        | "5B" -> [T(5,Bleu)]
        | "6B" -> [T(6,Bleu)]
        | "7B" -> [T(7,Bleu)]
        | "8B" -> [T(8,Bleu)]
        | "9B" -> [T(9,Bleu)]
        | "10B" -> [T(10,Bleu)]
        | "11B" -> [T(11,Bleu)]
        | "12B" -> [T(12,Bleu)]
        | "13B" -> [T(13,Bleu)]
        | "J" -> [Joker]
        | _ -> [] ;;

let lire_combinaison () : combinaison =
    let inp = String.split_on_char ' ' (read_line()) 
    in List.fold_left (fun acc x -> acc @ (string2tuiles x)) [] inp ;;


let lire_table () : table = 
    print_string normal "\nCombien de combinaisons contient la table?\n";
    let n = read_int () in
    let rec lire (n: int) : table =
        if n = 0
        then []
        else
            (lire_combinaison ())::(lire (n -1)) 
    in lire n ;;

let est_premier_coup (etat: etat) : bool =
    let _,ret,_ = le_statut (joueur_courant etat) etat in
    not ret ;;

let ask (joueur: joueur) : unit =
        print_string [red] (if (joueur = J1) then "\nJ1:   " else "\nJ2:   ");
        print_string [red] "p" ;
        print_string normal "iocher/" ;
        print_string [red] "r" ;
        print_string normal "eorganiser/";
        print_string [red] "a" ;
        print_string normal "jouter/" ;
        print_string [red] "q" ;
        print_string normal "uitter/" ;
        print_string [red] "h" ;
        print_string normal  "elp ?\n" ;;

let rec loop (etat: etat) : etat =
    if (la_pioche etat = [] || (la_main J1 etat = []) || (la_main J2 etat = []))
    then etat 
    else 
        begin
            clean () ;
            print_string normal "Voici la table:\n";
            print_table (la_table etat) ;
            print_string normal "Voici votre main:\n";
            print_mens (la_main (joueur_courant etat) etat) ;
            ask (joueur_courant etat) ;
        let input = read_line () in
         match input with 
         | "piocher" | "p" -> loop (piocher etat)
         | "reorganiser" | "r" -> if (est_premier_coup etat)
                                  then loop (jouer_1er_coup etat (lire_table()))
                                  else loop (jouer_1_coup etat (lire_table())) 
         | "ajouter" | "a" -> if (est_premier_coup etat)
                              then loop (jouer_1er_coup etat (lire_table()))
                              else loop (jouer_1_coup etat (la_table etat @ lire_table ()))
         | "quitter" | "q" -> print_string [red] "\nAu revoir \n"; exit 0
         | "help" | "h" -> begin
                                clean () ;
                                help () ;
                                loop (etat) ;
                           end
         | _ -> loop (etat) 
        end ;; 
       
Random.self_init () ;; (* random seed *)

let () = welcome () ;;

let etat = init_partie () ;;

let etat = loop(etat) ;; 


