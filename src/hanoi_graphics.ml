
(*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*)

(*DRAWING FUNCTIONS*)

#load "unix.cma";;
#load "graphics.cma";;
open Unix;;
open Graphics;;

let base_width = 240;;
let base_height = 20;;

(*draws the 3 black rods*)
let draw_rods () =
  clear_graph ();
  set_color black;
  fill_poly [|(145, 0); (155, 0); (155, 400); (145, 400)|];
  fill_poly [|(395, 0); (405, 0); (405, 400); (395, 400)|];
  fill_poly [|(645, 0); (655, 0); (655, 400); (645, 400)|];;

(*draws the disc carrying the number num on the specified rod at the specified height*)
let draw_disc total_discs num rod disc_height =
  let red_width = 200 / total_discs in

  (*cannot handle 40 discs or more because of the height reduction*)
  let upd_height =
    if total_discs > 20 then base_height - (total_discs - 20)
    else base_height in

  let width = base_width - (red_width * (total_discs - num - 1)) in
  let pos = rod * 250 + 150 in
  
  let real_height = upd_height * disc_height in
  set_color blue;
  fill_poly [|(pos - width / 2, real_height);
	      (pos + width / 2, real_height);
	      (pos + width / 2, real_height + upd_height);
	      (pos - width / 2, real_height + upd_height)|];;

(*draws the current situation of the game*)
let draw_situation total_discs rods =
  let rec draw_rod u rod curr =
    match u with
    | [] -> ()
    | x :: xs ->
       (
	 draw_rod xs rod (curr+1);
	 draw_disc total_discs x rod curr;
       ) in
  for i=0 to 2 do
    draw_rod (mirror rods.(i)) i 0
  done;
  sleepf 0.02;;

(*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*)

(*SECONDARY FUNCTIONS AND TYPE DEFINITIONS*)

(*stack structure definition*)
let empty_stack = [];;
let push x stack = x :: stack;;
let pop stack =
  match stack with
  | [] -> failwith "empty stack pop"
  | x :: xs -> xs;;
let top stack =
  match stack with
  | [] -> failwith "empty stack top"
  | x :: xs -> x;;

(*counter declaration*)
let c = ref 0;;
let init () = c := 0;;
let step () = c := !c + 1;;
let get () = !c;;

(*prints movement message in interpreter*)
let print_mov origin destination =
  print_string ("I move a disc from rod " ^
		   (string_of_int origin) ^
		   " to rod " ^
		   (string_of_int destination));
  print_newline();;

(*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*)

(*AUXILIARY FUNCTIONS*)

(*returns a list of the type [0; ...;n-1]*) 
let rec init_rod n rod =
  if n < 0 then rod
  else init_rod (n-1) (n :: rod);;

(*reverses a list in a linear time*)
let mirror u =
  let rec aux u v =
    match u with
    | [] -> v
    | x :: xs -> aux xs (x::v) in
  aux u [];;

(*if a and b are two distinct elements of {0, 1, 2}; choose a b returns the third element*)
let choose a b =
  if a = b then failwith "a and b are equal"
  else if a = 0 then
    if b = 1 then 2
    else 1
  else if a = 1 then
    if b = 0 then 2
    else 0
  else if a = 2 then
    if b = 0 then 1
    else 0
  else failwith "a or b are not between 0 and 2";;

(*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*)

(*CORE FUNCTIONS*)

(*moves a single disc from orig_rod to dest_rod*)
let move_disc total_discs rods orig_rod dest_rod =
  let disc = top rods.(orig_rod) in
  draw_rods ();
  
  rods.(orig_rod) <- pop rods.(orig_rod);
  rods.(dest_rod) <- push disc rods.(dest_rod);
  
  draw_situation total_discs rods;;
      
(*moves num_discs discs from orig_rod to dest_rod*)
let rec move total_discs rods num_discs orig_rod dest_rod =
  if num_discs = 0 then ()
  else if num_discs = 1 then
   (
      move_disc total_discs rods orig_rod dest_rod;
      
      step ();
    )
  else
    (
      let temp_rod = choose orig_rod dest_rod in
      move total_discs rods (num_discs - 1) orig_rod temp_rod;
      move_disc total_discs rods orig_rod dest_rod;
      
      step ();
      
      move total_discs rods (num_discs - 1) temp_rod dest_rod;
    );;

(*main function*)
let hanoi total_discs =
  init ();
  
  close_graph ();
  open_graph " 800x600-0+0";
  
  let rods = [|[]; []; []|] in rods.(0) <- init_rod (total_discs-1) [];

  draw_rods ();
  draw_situation total_discs rods;
  
  move total_discs rods (total_discs-1) 0 1;
  move_disc total_discs rods 0 2;
  
  step ();
  
  move total_discs rods (total_discs-1) 1 2;
      
  print_int (get ());
  print_newline ();
  sleepf 2.5;
  close_graph ();;

(*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*)

(*DEBUG FUNCTIONS*)

let rec print_int_list u =
  match u with
  | [] -> print_char '\t';
  | [x] -> print_int x; print_char '\t';
  | x :: xs -> print_int x; print_int_list xs;;

(*prints an array of int list, made to print rods during execution*)
let print t =
  let len = Array.length t in
  for i=0 to (len-1) do
    print_int_list t.(i);
  done;
  print_newline ();;

(*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*)
