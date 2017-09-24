#load "graphics.cma";;
#load "unix.cma";;
     
open Graphics;;
open_graph " 1500x1000-0+0";;

(*CONSTANTS*)
  
let width = 1500.;;
let height = 1000.;; 
   
let golden_ratio = (1. +. sqrt 5.) /. 2. ;;
let size = 800.;;
    
type triangle = Obtuse | Acute ;;
type triangle_liste = ((float * float) array * triangle) list;;
  
(*POSTING FUNCTIONS*)
let move (a,b) = moveto (int_of_float a) (int_of_float b);;
let line (a,b) = lineto (int_of_float a) (int_of_float b);;
let iof_array tab = Array.map (fun (x,y) -> (int_of_float x, int_of_float y)) tab;;
let homothety tab factor = Array.map (fun (x,y) -> (x*.factor,y*.factor)) tab;;
  
let draw points triangle =
  (if triangle = Obtuse then set_color red
   else set_color blue);
  fill_poly (iof_array (homothety points 1.));;
 
let in_bounds pts_triangle =
  let [|(p1x,p1y); (p2x,p2y); (p3x,p3y)|] = pts_triangle in
  (p1x >= 0. && p1x <= width && p1y >= 0. && p1y <= height &&
   p2x >= 0. && p2x <= width && p2y >= 0. && p2y <= height &&
   p3x >= 0. && p3x <= width && p3y >= 0. && p3y <= height);;


(*FIRST VERSION: Depth-first search*)

(*Draw each line only once*)
  
let divide generation points triangle_type fill=
  let rec aux_divide gen pts triangle =
    if gen = 0 then (if fill then draw pts triangle else ())
    else begin
	(* We will use the following convention: we will list the corners of a triangle starting with the different one and turning clockwise*)
	let (p1x,p1y) = pts.(0) and (p2x,p2y) = pts.(1) and (p3x,p3y) = pts.(2) in
	if triangle = Obtuse then
 	  (let newpoint = (p2x +.(p3x-.p2x)/.(1.+.golden_ratio), p2y +.(p3y-.p2y)/.(1.+.golden_ratio)) in
 	   aux_divide (gen -1) [|newpoint; pts.(0); pts.(1)|] Obtuse;
 	   aux_divide (gen -1) [|pts.(2);pts.(0); newpoint|] Acute;
 	   set_color black;
 	   move pts.(0);
 	   line newpoint)
	else
 	  (let  newpoint1 = (p1x +.(p2x-.p1x)/.(1.+.golden_ratio), p1y +.(p2y-.p1y)/.(1.+.golden_ratio)) in
 	   let newpoint2 = (p3x +.(p1x-.p3x)/.(1.+.golden_ratio), p3y +.(p1y-.p3y)/.(1.+.golden_ratio)) in
 	   aux_divide (gen -1) [|newpoint1; newpoint2; pts.(0)|] Obtuse;
 	   aux_divide (gen -1) [|pts.(1); pts.(2); newpoint2|] Acute;
 	   aux_divide (gen -1) [|pts.(1); newpoint2; newpoint1|] Acute;
 	   set_color black;
  	   move newpoint1;
  	   line newpoint2;
  	   line pts.(1))
      end
  in
  aux_divide generation points triangle_type;
  move points.(0);
  line points.(1);
  line points.(2);
  line points.(0)
;;
  
(*SECOND VERSION: Breadth-first search*)

  (* Depth-first search prevent us from posting each generation one after the other*)
  (* One of the main problem is that the triangles have to be filled before thy are drawn to avoid covering their edges.*)
  
let divide2 generation points triangle fill=
  move points.(0);
  line points.(1);
  line points.(2);
  line points.(0);
  let rec triangle_division u v =
    match u with
    | [] -> v
    | u1::us -> (let ([|(p1x,p1y);(p2x,p2y);(p3x,p3y)|],triangle_type) = u1 in
 		 if triangle_type = Obtuse then
 		   (let newpoint = (p2x +.(p3x-.p2x)/.(1.+.golden_ratio), p2y +.(p3y-.p2y)/.(1.+.golden_ratio)) in
 		    set_color black;
 		    move (p1x,p1y);
 		    line newpoint;
 		    let new_triangle1 = [|newpoint; (p1x,p1y); (p2x,p2y)|] and new_triangle2 = [|(p3x,p3y); (p1x,p1y); newpoint|] in
  		    triangle_division us ((new_triangle1, Obtuse)::(new_triangle2, Acute)::v))
 		 else 
 		   (let  newpoint1 = (p1x +.(p2x-.p1x)/.(1.+.golden_ratio), p1y +.(p2y-.p1y)/.(1.+.golden_ratio)) in
 		    let newpoint2 = (p3x +.(p1x-.p3x)/.(1.+.golden_ratio), p3y +.(p1y-.p3y)/.(1.+.golden_ratio)) in
 		    set_color black;
 		    move newpoint1;
 		    line newpoint2;
 		    line (p2x,p2y);
 		    let new_triangle1 = [|newpoint1; newpoint2; (p1x,p1y)|] in 
 		    let new_triangle2 = [|(p2x,p2y); (p3x,p3y); newpoint2|] in
 		    let new_triangle3 = [|(p2x,p2y); newpoint2; newpoint1|] in
 		    triangle_division us ((new_triangle1, Obtuse)::
 			      (new_triangle2, Acute)::
 				(new_triangle3, Acute)::v)))
  in
   let rec one_step gen triangle_list =
     if gen = 0 then (if fill
		      then (List.map (fun (tab, triangle_type) -> draw tab triangle_type) triangle_list)
		      else [()] (*Caml is expecting a Unit list*))
     else let v = triangle_division triangle_list [] in Unix.sleep 1;
							one_step (gen - 1) v
   in
   one_step generation [(points, triangle)];
   divide generation points triangle false 
;;

(*THIRD VERSION*)
  
(*TESTING*)

divide2 7 [|(size*. sqrt (golden_ratio *.golden_ratio -. 0.25), size*.0.5); (0.,0.); (0., size)|] Acute true;;
(* divide 5 [|(size*. sqrt (golden_ratio *.golden_ratio -. 0.25), size*.0.5); (0.,0.); (0., size)|] Acute true;; *)
  
  
   (* TODO: - homothetie *)
   (* 	- fill the whole screen *)
