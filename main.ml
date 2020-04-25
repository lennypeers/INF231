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
    print_string normal "\nCombien de combinaisons contient la table?\n";
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
            set_cursor 1 1;
            erase Below ;
            print_string normal "Voici la table:\n";
            print_table (la_table etat) ;
            print_string normal "Voici votre main:\n";
            print_mens (la_main (joueur_courant etat) etat) ;
            print_string normal "\nVous choisissez de piocher/poser ?\n";
        let input = read_line () in
         match input with 
         | "piocher" -> loop (piocher etat)
         | "poser" -> if (est_premier_coup etat)
                      then loop (jouer_1er_coup etat (lire_table ()))
                      else loop (jouer_1_coup etat (lire_table())) 
         | "q" -> print_string normal "\nAu revoir \n"; exit 0
         | _ -> loop (etat) 
        end ;; 
        
let etat = loop(etat) ;; 


