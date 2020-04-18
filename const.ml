(* Some constant to test the functions 
 *
 * Name format: 
     * type_number_presenceOfJoker
 * *)


let g1n = [T(12,Bleu) ; T(12,Rouge) ; T(12,Jaune) ; T(12,Noir)] ;;
(* 48 pts *)
let g2n = [T(11,Bleu) ; T(11,Rouge) ; T(11,Jaune) ; T(11,Noir)] ;;
(* 44 pts *)
let g3n = [T(10,Bleu) ; T(10,Rouge) ; T(10,Jaune)] ;;
(* 30 pts *)
let g4n = [T(5,Bleu) ; T(5,Rouge) ; T(5,Noir)] ;;
(* 15 pts *)
let g1y = [Joker ; T(10,Noir) ; Joker] ;;
(* 30 pts *)
let g2y = [Joker ; Joker ; T(13,Jaune)] ;;
(* 39 pts *)
let g3y = [T(1,Bleu) ; Joker ; Joker] ;;
(* 3 pts *)


let s1n = [T(10,Rouge) ; T(11,Rouge) ; T(12,Rouge) ; T(13,Rouge)] ;;
(* 46 pts *)
let s2n = [T(9,Jaune) ; T(10,Jaune) ; T(11,Jaune) ; T(12,Jaune)] ;;
(* 42 pts *)
let s3n = [T(8,Bleu) ; T(9,Bleu) ; T(10,Bleu)] ;;
(* 27 pts *)
let s4n = [T(7,Noir) ; T(8,Noir) ; T(9,Noir)] ;;
(* 24 pts *)
let s1y = [T(10,Rouge) ; Joker ; T(12,Rouge) ; T(13,Rouge)] ;;
(* 46 pts *)
let s2y = [T(9,Jaune) ; T(10,Jaune) ; Joker ; T(12,Jaune)] ;;
(* 42 pts *)
let s3y = [Joker ; Joker ; T(10,Bleu)] ;;
(* 27 pts /!\ it is also a groupe *)

