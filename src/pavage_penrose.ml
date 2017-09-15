#load "graphics.cma";;
open Graphics;;
open_graph " 800x600-0+0";;
       
let golden_ratio = (1. +. sqrt 5.) /. 2. ;;
let taille = 500.;;
  
type triangle = Obtuse | Acute ;;
let move a b = moveto (int_of_float a) (int_of_float b);;
let line a b = lineto (int_of_float a) (int_of_float b);;
let int_of_float_array tab =
  let int_tab = Array.create (Array.length tab) (0,0) in
  for i= 0 to Array.length tab do
    let (x,y) = tab.(i) in int_tab.(i) <- (int_of_float x, int_of_float y) 
  done; int_tab;;
  
									   
  
let draw points triangle =
  (if triangle = Obtuse then set_color red
  else set_color blue);
  let (p1x,p1y) = points.(0) and (p2x,p2y) = points.(1) and (p3x,p3y) = points.(2) in
  move p1x p1y; line p2x p2y;
  line p3x p3y; line p1x p1y; fill_poly (int_of_float_array points);;
  

let a = taille *. (sqrt (golden_ratio*.golden_ratio -. 0.25));;

  
let rec divide generation points triangle =
  if generation = 0 then
    draw points triangle
  else begin
      let (p1x,p1y) = points.(0) and (p2x,p2y) = points.(1) and (p3x,p3y) = points.(2) in
      if triangle = Obtuse then
	(let newpoint = (p3x +.(p1x-.p3x)/.(1.+.golden_ratio), p3y +.(p1y-.p3y)/.(1.+.golden_ratio)) in
	divide (generation -1) [|points.(0); points.(1); newpoint|] Acute;
	divide (generation -1) [|points.(2); newpoint; points.(1)|] Obtuse)
      else 
	(let  newpoint1 = (p1x +.(p2x-.p1x)/.(1.+.golden_ratio), p1x +.(p2x-.p2x)/.(1.+.golden_ratio)) in
	let newpoint2 = (p2x +.(p1x-.p2x)/.(1.+.golden_ratio), p2y +.(p1y-.p2y)/.(1.+.golden_ratio)) in
	divide (generation -1) [|points.(0); points.(1); newpoint2|] Obtuse;
	divide (generation -1) [|points.(1); points.(2); newpoint2|] Acute;
	divide (generation -1) [|points.(1); newpoint2 ; newpoint1|] Acute)
    end;;

divide 0 [|(0.,0.); (500.*.golden_ratio/.2., 500.*.sqrt(1.-.golden_ratio*.golden_ratio /. 4.)); (500.*. golden_ratio,0.)|] Acute ;;
