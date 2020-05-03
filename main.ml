(*  main program *)

let clean () : unit =
    set_cursor 1 1;
    erase Below ;;

let help () : unit = 
    print_string normal "- To enter a tuile, type the number, followed by the first letter (lower or uppercase) of its color\n" ;
    print_string normal "- To enter a combination, type the sequence of tuiles\n" ;
    print_string normal "- For instance, the combination T(1,Bleu) T(1,Jaune) T(1,Noir) T(1,Rouge) Joker is:\n" ;
    print_string [Foreground(Green)] "           1B 1J 1N 1R J" ;
    print_string normal "    or also    " ;
    print_string [Foreground(Green)] "1b 1j 1n 1r j\n" ;
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

let string2tuile (inp: string) : tuile =
        match inp with
        | "1N" | "1n"-> T(1,Noir) 
        | "2N" | "2n"-> T(2,Noir) 
        | "3N" | "3n"-> T(3,Noir) 
        | "4N" | "4n"-> T(4,Noir) 
        | "5N" | "5n"-> T(5,Noir) 
        | "6N" | "6n"-> T(6,Noir) 
        | "7N" | "7n"-> T(7,Noir) 
        | "8N" | "8n"-> T(8,Noir) 
        | "9N" | "9n"-> T(9,Noir) 
        | "10N" | "10n" -> T(10,Noir) 
        | "11N" | "11n" -> T(11,Noir) 
        | "12N" | "12n" -> T(12,Noir) 
        | "13N" | "13n" -> T(13,Noir) 
        | "1J" | "1j"-> T(1,Jaune) 
        | "2J" | "2j"-> T(2,Jaune) 
        | "3J" | "3j"-> T(3,Jaune) 
        | "4J" | "4j"-> T(4,Jaune) 
        | "5J" | "5j"-> T(5,Jaune) 
        | "6J" | "6j"-> T(6,Jaune) 
        | "7J" | "7j"-> T(7,Jaune) 
        | "8J" | "8j"-> T(8,Jaune) 
        | "9J" | "9j"-> T(9,Jaune) 
        | "10J" | "10j" -> T(10,Jaune) 
        | "11J" | "11j" -> T(11,Jaune) 
        | "12J" | "12j" -> T(12,Jaune) 
        | "13J" | "13j" -> T(13,Jaune) 
        | "1R" | "1r"-> T(1,Rouge) 
        | "2R" | "2r"-> T(2,Rouge) 
        | "3R" | "3r"-> T(3,Rouge) 
        | "4R" | "4r"-> T(4,Rouge) 
        | "5R" | "5r"-> T(5,Rouge) 
        | "6R" | "6r"-> T(6,Rouge) 
        | "7R" | "7r"-> T(7,Rouge) 
        | "8R" | "8r"-> T(8,Rouge) 
        | "9R" | "9r"-> T(9,Rouge) 
        | "10R" | "10r" -> T(10,Rouge) 
        | "11R" | "11r" -> T(11,Rouge) 
        | "12R" | "12r" -> T(12,Rouge) 
        | "13R" | "13r" -> T(13,Rouge) 
        | "1B" | "1b"-> T(1,Bleu) 
        | "2B" | "2b"-> T(2,Bleu) 
        | "3B" | "3b"-> T(3,Bleu) 
        | "4B" | "4b"-> T(4,Bleu) 
        | "5B" | "5b"-> T(5,Bleu) 
        | "6B" | "6b"-> T(6,Bleu) 
        | "7B" | "7b"-> T(7,Bleu) 
        | "8B" | "8b"-> T(8,Bleu) 
        | "9B" | "9b"-> T(9,Bleu) 
        | "10B" | "10b" -> T(10,Bleu) 
        | "11B" | "11b" -> T(11,Bleu) 
        | "12B" | "12b" -> T(12,Bleu) 
        | "13B" | "13b" -> T(13,Bleu) 
        | "J" | "j" -> Joker 
        | _ -> failwith "error" ;;

let lire_combinaison () : combinaison =
    let inp = String.split_on_char ' ' (read_line()) 
    in List.fold_right (fun x acc -> try (string2tuile x) :: acc with _ -> acc ) inp [] ;;


let lire_table () : table = 
    let rec readint () : int =
        print_string normal "How many combination do you want to put?\n";
        try read_int ()
        with _ -> readint ()
    in let n = readint () in
    let rec lire (n: int) : table =
        if n = 0
        then []
        else
            (lire_combinaison ())::(lire (n -1)) 
    in lire n ;;

let rec read_single_tile () : tuile =
    let inp = read_line () in
    try string2tuile inp
    with _ -> read_single_tile () ;;

let est_premier_coup (etat: etat) : bool =
    let _,ret,_ = le_statut (joueur_courant etat) etat in
    not ret ;;

let ask (joueur: joueur) : unit =
        print_string [red] (if (joueur = J1) then "\nJ1:   " else "\nJ2:   ");
        print_string [red] "d" ;
        print_string normal "raw / " ;
        print_string [red] "r" ;
        print_string normal "eorganize / add a ";
        print_string [red] "c" ;
        print_string normal "ombination or a " ;
        print_string [red] "t" ;
        print_string normal "ile / " ;
        print_string [red] "q" ;
        print_string normal "uit / " ;
        print_string [red] "s" ;
        print_string normal "ort / h" ;
        print_string [red] "i" ;
        print_string normal "nt / " ;
        print_string [red] "h" ;
        print_string normal  "elp ?\n" ;;


let en_ordre_groupe (ens: tuile multiensemble) : tuile multiensemble =
  let comp_ordre (a,_: tuile multielement) (b,_: tuile multielement) : bool =
    match (a,b) with
    | (_,Joker) -> true
    | (T(n1,_),T(n2,_)) ->  n1 <= n2
    | _ -> false in
  let rec insertion x = function | [] -> x::[]
                                 | head::tail -> if comp_ordre x head
                                     then x::head::tail
                                     else head::(insertion x tail) in
  let rec tri = function | [] -> []
                         | head::tail -> insertion head (tri tail)  
  in tri ens ;;

let hint (etat: etat) : unit =
    print_string normal "\nExisting combination: ";
    (let proposition = extraction_suite (la_main (joueur_courant etat) etat) in
    if proposition <> [] 
    then print_comb proposition 
    else let proposition = extraction_groupe (la_main (joueur_courant etat) etat) in
        if proposition <> []
        then print_comb proposition
        else print_string rouge "nothing found\n") ;
    print_string rouge "\nHit enter to continue\n";
    let _ = read_line () in () ;;



let rec loop (etat: etat) (sorting) : etat =
    if (la_pioche etat = [] || (la_main J1 etat = []) || (la_main J2 etat = []))
    then etat 
    else 
        begin
            clean () ;
            print_string normal (string_of_int (cardinal (la_pioche etat)) ^ " tile(s) left\n\n") ;
            print_string normal "The table:\n";
            print_table (la_table etat) ;
            print_string normal "Your hand:\n";
            print_mens (sorting (la_main (joueur_courant etat) etat)) ;
            ask (joueur_courant etat) ;
            let input = read_line () in
                 match input with 
                 | "draw" | "d" -> loop (piocher etat) sorting
                 | "reorganize" | "r" -> if (est_premier_coup etat)
                                         then loop (jouer_1er_coup etat (lire_table())) sorting
                                         else loop (jouer_1_coup etat (lire_table())) sorting 
                 | "c" -> if (est_premier_coup etat)
                          then loop (jouer_1er_coup etat (lire_table())) sorting
                          else loop (jouer_1_coup etat (la_table etat @ lire_table ())) sorting
                 | "quit" | "q" -> print_string [red] "\nSee you !\n"; exit 0
                 | "help" | "h" -> begin
                                        clean () ;
                                        help () ;
                                        loop etat sorting;
                                   end
                 | "tuile" | "t" -> begin 
                                        print_string normal "Enter the tile you want to add: ";
                                        loop (jouer_1_coup etat (ajouter_tuile (read_single_tile()) (la_table etat)) ) sorting
                                    end
                 | "sort" | "s" -> loop etat (if sorting == en_ordre_groupe then en_ordre else en_ordre_groupe)
                 | "hint" | "i" -> begin
                                    hint(etat);
                                    loop etat sorting
                                   end
                 | _ -> loop etat sorting
        end ;; 
       
Random.self_init () ;; (* random seed *)

let () = welcome () ;;

let etat = init_partie () ;;

let etat = loop etat en_ordre ;; 


