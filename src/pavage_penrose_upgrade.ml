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

(*FIRST VERSION: Depth-fisrt search*)

  (* Draw each line only once *)

let rec divide generation points triangle fill=
  if generation = 0 then (if fill then draw points triangle else ())
  else begin
      (* We will use the following convention: we will list the corners of a triangle starting with the different one and turning clockwise*)
      let (p1x,p1y) = points.(0) and (p2x,p2y) = points.(1) and (p3x,p3y) = points.(2) in
      if triangle = Obtuse then
	(let newpoint = (p2x +.(p3x-.p2x)/.(1.+.golden_ratio), p2y +.(p3y-.p2y)/.(1.+.golden_ratio)) in
	 divide (generation -1) [|newpoint; points.(0); points.(1)|] Obtuse fill;
	 divide (generation -1) [|points.(2);points.(0); newpoint|] Acute fill;
	 set_color black;
	 move points.(0);
	 line newpoint)
      else
	(let  newpoint1 = (p1x +.(p2x-.p1x)/.(1.+.golden_ratio), p1y +.(p2y-.p1y)/.(1.+.golden_ratio)) in
	 let newpoint2 = (p3x +.(p1x-.p3x)/.(1.+.golden_ratio), p3y +.(p1y-.p3y)/.(1.+.golden_ratio)) in
	 divide (generation -1) [|newpoint1; newpoint2; points.(0)|] Obtuse fill;
	 divide (generation -1) [|points.(1); points.(2); newpoint2|] Acute fill;
	 divide (generation -1) [|points.(1); newpoint2; newpoint1|] Acute fill;
	 set_color black;
	 move newpoint1;
	 line newpoint2;
	 line points.(1))
    end
;;

(*SECOND VERSION: Breadth-first search*)

  (* Depth-first search prevent us from posting each generation one after the other*)
  (* One of the main problemis that the triangles have to be filled before thy are drawn to avoid covering their edges.*)
  
let divide2 generation points triangle =
  move points.(0); line points.(1); line points.(2); line points.(0);
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
  let rec one_step generation u =
    if generation = 0 then List.map (fun (array, triangle_type) -> draw array triangle_type) u
    else let v = triangle_division u [] in Unix.sleep 1; one_step (generation - 1) v
  in
  one_step generation [(points, triangle)];
  divide generation points triangle false
;;

(*THIRD VERSION: repeted depth-first search*)
  
  (* To implement the homothety, we cant't use the last version because each generation have to be drawn in one go.*)
  (* We will also post each generations using the fisrt version. It will be twice longer but the asymptotic theorical complexity will be the same.*)


let divide3 generation points triangle =
  move points.(0); line points.(1); line points.(2); line points.(0);
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
  let rec one_step generation u =
    if generation = 0 then List.map (fun (array, triangle_type) -> draw array triangle_type) u
    else let v = triangle_division u [] in Unix.sleep 1; one_step (generation - 1) v
  in
  one_step generation [(points, triangle)];
  divide generation points triangle false
	 
(*TESTING FUNCTIONS*)

(* divide 5 [|(size*. sqrt (golden_ratio *.golden_ratio -. 0.25), size*.0.5); (0.,0.); (0., size)|] Acute true;; *)

(* divide2 7 [|(size*. sqrt (golden_ratio *.golden_ratio -. 0.25), size*.0.5); (0.,0.); (0., size)|] Acute;; *)
  
divide2 7 [|( width+.height*.sqrt(golden_ratio*.golden_ratio-. 0.25),height/.2.); (0., -.width*.sqrt(4.*.golden_ratio*.golden_ratio-.1.)); (0., height+.width*.sqrt(4.*.golden_ratio*.golden_ratio-.1.))|] Acute;;
																      
(* TODO: *) 
  (*    - homothetie *)
  (* 	- fill the whole screen *)
  (*    - puissance float *)
