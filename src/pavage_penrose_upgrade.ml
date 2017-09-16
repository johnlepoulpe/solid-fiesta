#load "graphics.cma";;
open Graphics;;
open_graph " 1500x1000-0+0";;
       
let golden_ratio = (1. +. sqrt 5.) /. 2. ;;
let taille = 800.;;
  
type triangle = Obtuse | Acute ;;
let move (a,b) = moveto (int_of_float a) (int_of_float b);;
let line (a,b) = lineto (int_of_float a) (int_of_float b);;
let iof_array tab = Array.map (fun (x,y) -> (int_of_float x, int_of_float y)) tab;;
let homothety tab factor = Array.map (fun (x,y) -> (x*.factor,y*.factor)) tab;;
 
let draw points triangle =
  set_color black;
  move points.(0); line points.(1);  
  line points.(2); line points.(0);
  (if triangle = Obtuse then set_color red
  else set_color blue);
  fill_poly (iof_array points);; 
  

let a = taille *. (sqrt (golden_ratio*.golden_ratio -. 0.25));;

  
let rec divide generation points triangle (* init_generation *)=
  if generation = 0 then
    draw points triangle (* init_generation *)
  else begin
      let (p1x,p1y) = points.(0) and (p2x,p2y) = points.(1) and (p3x,p3y) = points.(2) in
      if triangle = Obtuse then
	(let newpoint = (p2x +.(p3x-.p2x)/.(1.+.golden_ratio), p2y +.(p3y-.p2y)/.(1.+.golden_ratio)) in
	 divide (generation -1) [|newpoint; points.(0); points.(1)|] Obtuse (* init_generation *);
	 divide (generation -1) [|points.(2);points.(0); newpoint|] Acute (* init_generation*) ) 
      else 
	(let  newpoint1 = (p1x +.(p2x-.p1x)/.(1.+.golden_ratio), p1y +.(p2y-.p1y)/.(1.+.golden_ratio)) in
	let newpoint2 = (p3x +.(p1x-.p3x)/.(1.+.golden_ratio), p3y +.(p1y-.p3y)/.(1.+.golden_ratio)) in
	divide (generation -1) [|newpoint1; newpoint2; points.(0)|] Obtuse (* init_generation *); (* *)
	divide (generation -1) [|points.(1); newpoint2; points.(2)|] Acute (* init_generation *); (* *)
	divide (generation -1) [|points.(1); newpoint2; newpoint1|] Acute (* init_generation *))
    end;;

 (* divide 3 [|(taille*.golden_ratio/.2., taille*.sqrt(1.-.golden_ratio*.golden_ratio /. 4.)); (taille*. golden_ratio,0.); (0.,0.)|] Obtuse ;; *)

divide 5 [|(taille*. sqrt (golden_ratio *.golden_ratio -. 0.25), taille*.0.5); (0.,0.); (0., taille)|] Acute;;
