(*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*)

(*MODULES AND VARIABLES*)

#load "unix.cma";;
#load "graphics.cma";;
open Unix;;
open Graphics;;

let win_width = 1280;;
let win_height = 1024;;

let rod_base_width = win_width / 80;;
let rod_base_height = 2 * win_height / 3;;

let sleep_time_float = 0.02;;

(*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*)

(*DRAWING FUNCTIONS*)

let get_rod_spacing total_rods = win_width / total_rods;;

let draw_rod midx width height =
  set_color black;
  fill_poly [|(midx-width/2, 0); (midx+width/2, 0);
	      (midx+width/2, height); (midx-width/2, height)|]
;;

(*draws the 3 black rods*)
let draw_rods total_rods =
  (*reduces rod width where there are a lot of rods*)
  let upd_width = rod_base_width * 3 / total_rods in
  let spacing = get_rod_spacing total_rods in
  (*determines the x position of the first rod*)
  let first_pos =
    if total_rods mod 2 = 0 then
      (win_width-spacing)/2 mod spacing
    else
      (win_width/2) mod spacing in
  
  clear_graph ();
  
  for i=0 to total_rods-1 do
    draw_rod (first_pos + spacing * i) upd_width rod_base_height
  done
;;

(*draws the disc carrying the number disc_num on the specified rod at the specified height*)
let draw_disc total_discs total_rods rod disc_height disc_num =
  let spacing = get_rod_spacing total_rods in
  let disc_base_width = spacing - 4 in
  let disc_base_height = win_height / 30 in
  (*reduction factor of the discs' width*)
  let red_width = (9 * disc_base_width) / (10 * total_discs)  in

  (*cannot handle 40 discs or more because of the height reduction*)
  let upd_height =
    if total_discs > 20 then disc_base_height - (total_discs - 20)
    else disc_base_height in

  let width =
    disc_base_width - (red_width * (total_discs - disc_num)) in
  let first_pos =
    if total_rods mod 2 = 0 then
      (win_width-spacing)/2 mod spacing
    else
      (win_width/2) mod spacing in
  let pos = first_pos + spacing * rod in

  (*calculates the height of the bottom of a disc*)
  let real_height = upd_height * disc_height in
  set_color blue;
  fill_poly [|(pos - width / 2, real_height);
	      (pos + width / 2, real_height);
	      (pos + width / 2, real_height + upd_height);
	      (pos - width / 2, real_height + upd_height)|]
;;

(*draws the current situation of the game*)
let draw_situation total_discs total_rods rods =
  (*draws the discs on a particular rod*)
  let rec draw_rod rod_rest rod_num curr =
    match rod_rest with
    | [] -> ()
    | x :: xs ->
       (
	 draw_rod xs rod_num (curr+1);
	 draw_disc total_discs total_rods rod_num curr x;
       ) in
  for i=0 to total_rods-1 do
    draw_rod (mirror rods.(i)) i 0;
  done;
  sleepf sleep_time_float
;;

(*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*)

(*TYPE DEFINITIONS*)

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

(*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*)

(*AUXILIARY FUNCTIONS*)

(*prints movement message in interpreter*)
let print_mov origin destination =
  print_string ("I move a disc from rod " ^
		   (string_of_int origin) ^
		   " to rod " ^
		   (string_of_int destination));
  print_newline()
;;

(*returns a list of the type [0; ...;n-1]*) 
let rec init_rod n rod =
  if n == 0 then rod
  else init_rod (n-1) (n :: rod)
;;

(*reverses a list in a linear time*)
let mirror u =
  let rec aux u v =
    match u with
    | [] -> v
    | x :: xs -> aux xs (x::v) in
  aux u [];;

(*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*)

(*CORE FUNCTIONS*)

(*moves a single disc from orig_rod to dest_rod*)
let move_disc total_discs total_rods rods orig_rod dest_rod =
  let disc = top rods.(orig_rod) in
  draw_rods total_rods;
  
  rods.(orig_rod) <- pop rods.(orig_rod);
  rods.(dest_rod) <- push disc rods.(dest_rod);
  
  draw_situation total_discs total_rods rods;
;;
      
(*moves num_discs discs from orig_rod to dest_rod*)
let rec move total_discs total_rods rods num_discs
    orig_rod dest_rod temp_rod =
  if num_discs = 0 then ()
  else if num_discs = 1 then
    (
      move_disc total_discs total_rods rods orig_rod dest_rod;
      
      print_mov orig_rod dest_rod;
     
      step ();
    )
  else
    (
      move total_discs total_rods rods
	(num_discs - 1) orig_rod temp_rod dest_rod;
      move_disc total_discs total_rods rods orig_rod dest_rod;
      
      print_mov orig_rod dest_rod;
      step ();
      
      move total_discs total_rods rods
	(num_discs - 1) temp_rod dest_rod orig_rod;
    )
;;

(*main function*)
let hanoi total_discs total_rods =
  init ();

  let win_format =
    " " ^ (string_of_int win_width) ^ "x" ^
      (string_of_int win_height) ^ "-0+0" in
  close_graph ();
  open_graph win_format;
  
  let rods =
    Array.make total_rods [] in rods.(0) <- init_rod total_discs [];
  
  (*-> disc_count equals to the number of discs to be stored in
    the intermediate rods.
    -> disc_rods is the remainder of the division, those discs can
    be distributed freely between the intermediate rods *)
  let disc_count = (total_discs - 1) / (total_rods - 2) and
      disc_rest = (total_discs - 1) mod (total_rods - 2) in

  draw_rods total_rods;
  draw_situation total_discs total_rods rods;
  
  (*FIRST STEP:
    distributes all of the discs except for the largest between
    the intermediate rod using the last one as a buffer*)
  for i=1 to total_rods-2 do
    if i = 1 then
      move total_discs total_rods rods
	(disc_count + disc_rest) 0 i (total_rods - 1)
    else
      move total_discs total_rods rods
	disc_count 0 i (total_rods - 1)
  done;

  (*SECOND STEP: moves the largest disc to the last rod*)
  move_disc total_discs total_rods rods 0 (total_rods - 1);
  
  print_mov 0 (total_rods - 1);
  step ();

  (*THIRD STEP:
    stacks the discs stocked in the intermediate
    rods on top of the largest one in the last rod*)
  for i = total_rods-2 downto 1 do
    if i = 1 then
      move total_discs total_rods rods
	(disc_count + disc_rest) i (total_rods - 1) 0
    else
      move total_discs total_rods rods
	disc_count i (total_rods - 1) 0
  done;

  print_int (get ());
  print_newline ();
  sleepf 2.5;
  close_graph ();;
;;

(*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*)
