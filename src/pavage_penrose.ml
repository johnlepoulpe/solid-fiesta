#load "graphics.cma";;
      
open Graphics;;
 
open_graph " 800x600-0+0";;
       
let golden_ratio = (1. +. sqrt 5.) /. 2. ;;

type triangle = Obtuse | Acute ;;

let draw points triangle =
  if triangle = Obtuse then set_color yellow
  else set_color blue;
  (* let point1x =  and point1y =  in *)
  (* let point2x = points.(1).(0) and point2y = points.(1).(1) in *)
  (* let point3x = points.(2).(0) and point3y = points.(2).(1) in *)
  moveto points.(0).(0) points.(0).(1); lineto points.(1).(0) points.(1).(1);
  lineto points.(2).(0) points.(2).(1); lineto points.(0).(0) points.(0).(1);
  fill_poly
	  
(* let rec divide generation points triangle = *)
(*   if generation = 0 then *)
(*     draw points triangle *)
(*   else begin *)
(*       if triangle = obtuse then *)
(* 	newpoint1 = (points.(0).(0) - *)
