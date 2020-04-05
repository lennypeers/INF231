type nat = int (* >= 0 *) ;;
type 'a multielement = 'a * nat ;;

type 'a multiensemble = 'a multielement list;;
  

let x=('m',3)::('a',1)::('c',4)::('v',1)::('b',2)::[]
let y=('m',3)::('a',1)::('c',4)::('v',1)::('b',2)::[]
                                                   

let cardinal (s:'a multiensemble) : int = 
  List.fold_left (fun z (_,y)  -> y + z) 0 s ;;

let nbocc (s:'a multiensemble) (e:'a) : int =
  List.fold_left (fun z (x,y) -> if (x = e) then y + z else z ) 0 s ;;

let appartient (s:'a multiensemble) (e:'a) : bool =
  List.fold_left (fun z (x,y) -> x = e || z ) false s ;;

let inclus (s1:'a multiensemble) (s2:'a multiensemble) : bool =
  List.fold_left (fun z (x,y) -> appartient s2 x && z) true s1 ;;

let ajout(s:'a multiensemble)((a,b):'a multielement):'a multiensemble=
  if appartient s a 
  then List.fold_right (fun (x,y) z -> if x=a then (x,y+b)::z else z) s []
  else ((a,b)::[])@s;;

(* end *)
