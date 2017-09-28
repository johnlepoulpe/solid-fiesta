#load "graphics.cma";;
open Graphics;;
open_graph " 800x600-0+0";;

(* CONSTANTS *)
  
let golden_ratio = (1. +. sqrt 5.) /. 2. ;;
let size = 500.;;
  
type triangle = Obtuse | Acute ;;

(*POSTING FUNCTIONS*)
  
let move (a,b) = moveto (int_of_float a) (int_of_float b);;
let line (a,b) = lineto (int_of_float a) (int_of_float b);;
let iof_array tab = Array.map (fun (x,y) -> (int_of_float x, int_of_float y)) tab;;
 
let draw points triangle =
  (if triangle = Obtuse then set_color red
   else set_color blue);
   fill_poly (iof_array points);
   move points.(0);
   set_color black;
   line points.(1);  
   line points.(2);
   line points.(0);;

(* FIRST VERSION *)
  
let rec divide generation points triangle =
  if generation = 0 then
    draw points triangle
  else begin
      let (p1x,p1y) = points.(0) and (p2x,p2y) = points.(1) and (p3x,p3y) = points.(2) in
      if triangle = Obtuse then
	(let newpoint = (p2x +.(p3x-.p2x)/.(1.+.golden_ratio), p2y +.(p3y-.p2y)/.(1.+.golden_ratio)) in
	divide (generation -1) [|newpoint; points.(0); points.(1)|] Obtuse;
	divide (generation -1) [|points.(2); points.(0); newpoint|] Acute)
      else 
	(let  newpoint1 = (p1x +.(p2x-.p1x)/.(1.+.golden_ratio), p1y +.(p2y-.p1y)/.(1.+.golden_ratio)) in
	let newpoint2 = (p3x +.(p1x-.p3x)/.(1.+.golden_ratio), p3y +.(p1y-.p3y)/.(1.+.golden_ratio)) in
	divide (generation -1) [|newpoint1; newpoint2; points.(0)|] Obtuse;
	divide (generation -1) [|points.(1); points.(2); newpoint2|] Acute;
	divide (generation -1) [|points.(1); newpoint2 ; newpoint1|] Acute)
    end;;

(* TESTING FUNCTIONS*)
  
let test n =
  match n with
  | 0 -> divide 8 [|(size*.golden_ratio/.2., size*.sqrt(1.-.golden_ratio**2. /. 4.));
		    (size*. golden_ratio,0.);
		    (0.,0.)|] Obtuse
  | 1 -> divide 8 [|(size*. sqrt (golden_ratio**2. -. 0.25), size*.0.5);
		    (0.,0.);
		    (0., size)|] Acute
;;
  
