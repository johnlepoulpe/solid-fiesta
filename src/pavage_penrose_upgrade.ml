#load "graphics.cma";;
#load "unix.cma";;
    
open Graphics;;
open_graph " 1500x1000-0+0";;
let width = 1500.;;
let height = 1000.;; 
  
let golden_ratio = (1. +. sqrt 5.) /. 2. ;;
let size = 800.;;
  
type triangle = Obtuse | Acute ;;
let move (a,b) = moveto (int_of_float a) (int_of_float b);;
let line (a,b) = lineto (int_of_float a) (int_of_float b);;
let iof_array tab = Array.map (fun (x,y) -> (int_of_float x, int_of_float y)) tab;;
let homothety tab factor = Array.map (fun (x,y) -> (x*.factor,y*.factor)) tab;;
 
let draw points triangle =
  (if triangle = Obtuse then set_color red
  else set_color blue);
  fill_poly (iof_array points);;
  
let a = taille *. (sqrt (golden_ratio*.golden_ratio -. 0.25));;
  
let rec divide generation points triangle =
  if generation = 0 then draw points triangle
  else begin
      let (p1x,p1y) = points.(0) and (p2x,p2y) = points.(1) and (p3x,p3y) = points.(2) in
      if triangle = Obtuse then
	(let newpoint = (p2x +.(p3x-.p2x)/.(1.+.golden_ratio), p2y +.(p3y-.p2y)/.(1.+.golden_ratio)) in
	 divide (generation -1) [|newpoint; points.(0); points.(1)|] Obtuse;
	 divide (generation -1) [|points.(2);points.(0); newpoint|] Acute;
	 set_color black; move points.(0); line newpoint)
      else 
	(let  newpoint1 = (p1x +.(p2x-.p1x)/.(1.+.golden_ratio), p1y +.(p2y-.p1y)/.(1.+.golden_ratio)) in
	 let newpoint2 = (p3x +.(p1x-.p3x)/.(1.+.golden_ratio), p3y +.(p1y-.p3y)/.(1.+.golden_ratio)) in
	 divide (generation -1) [|newpoint1; newpoint2; points.(0)|] Obtuse; 	     divide (generation -1) [|points.(1); points.(2); newpoint2|] Acute;
	 divide (generation -1) [|points.(1); newpoint2; newpoint1|] Acute;
	 set_color black; move newpoint1;
	 line newpoint2; line points.(1))
    end;;

(* divide 5 [|(taille*. sqrt (golden_ratio *.golden_ratio -. 0.25), taille*.0.5); (0.,0.); (0., taille)|] Acute;; *)

type triangle_liste = ((float * float) array * triangle) list;;

let divide2 generation points triangle =
  move points.(0); line points.(1); line points.(2); line points.(0);
  let rec aux u v =
    match u with
    | [] -> v
    | u1::us -> (let ([|(p1x,p1y);(p2x,p2y);(p3x,p3y)|],triangle_type) = u1 in
		 if triangle_type = Obtuse then
		   (let newpoint = (p2x +.(p3x-.p2x)/.(1.+.golden_ratio), p2y +.(p3y-.p2y)/.(1.+.golden_ratio)) in
		    set_color black; move (p1x,p1y); line newpoint;
		     aux us (([|newpoint; (p1x,p1y); (p2x,p2y)|], Obtuse)::([|(p3x,p3y); (p1x,p1y); newpoint|], Acute)::v))
		 else 
		   (let  newpoint1 = (p1x +.(p2x-.p1x)/.(1.+.golden_ratio), p1y +.(p2y-.p1y)/.(1.+.golden_ratio)) in
		    let newpoint2 = (p3x +.(p1x-.p3x)/.(1.+.golden_ratio), p3y +.(p1y-.p3y)/.(1.+.golden_ratio)) in
		    set_color black; move newpoint1;
		    line newpoint2; line (p2x,p2y);
		    aux us (([|newpoint1; newpoint2; (p1x,p1y)|], Obtuse)::([|(p2x,p2y); (p3x,p3y); newpoint2|], Acute)::([|(p2x,p2y); newpoint2; newpoint1|], Acute)::v)))
  in
  let rec aux2 generation u =
    if generation = 0 then List.map (fun (tab, triangle_type) -> draw tab triangle_type) u
    else let v = aux u [] in Unix.sleep 1; aux2 (generation - 1) v
  in
  aux2 generation [(points, triangle)];();;

divide2 10 [|(taille*. sqrt (golden_ratio *.golden_ratio -. 0.25), taille*.0.5); (0.,0.); (0., taille)|] Acute;;
 
