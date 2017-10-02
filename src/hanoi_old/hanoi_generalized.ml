(*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*)

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
  print_newline()
;;

(*returns a list of the type [1; ...;n]*) 
let rec init_rod n rod =
  if n == 0 then rod
  else init_rod (n-1) (n :: rod)
;;


(*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*)

(*CORE FUNCTIONS*)

(*moves a single disc from orig_rod to dest_rod*)
let move_disc rods orig_rod dest_rod =
  let disc = top rods.(orig_rod) in
  rods.(orig_rod) <- pop rods.(orig_rod);
  rods.(dest_rod) <- push disc rods.(dest_rod)
;;
      
(*moves num_discs discs from orig_rod to dest_rod*)
let rec move rods num_discs orig_rod dest_rod temp_rod =
  if num_discs = 0 then ()
  else if num_discs = 1 then
    (
      move_disc rods orig_rod dest_rod;
      
      print_mov orig_rod dest_rod;
      step ();
    )
  else
    (
      (*moves num_discs-1 discs to temp_rod*)
      move rods (num_discs - 1) orig_rod temp_rod dest_rod;
      (*moves the last disc to dest_rod*)
      move_disc rods orig_rod dest_rod;
      
      print_mov orig_rod dest_rod;
      step ();

      (*moves the rest of the discs to dest_rod*)
      move rods (num_discs - 1) temp_rod dest_rod orig_rod;
    )
;;

(*main function*)
let hanoi total_discs total_rods=
  init ();
  
  let rods =
    Array.make total_rods [] in rods.(0) <- init_rod total_discs [];
  
  (*-> disc_count equals to the number of discs to be stored in
    the intermediate rods.
    -> disc_rods is the remainder of the division, those discs can
    be distributed freely between the intermediate rods *)
  let disc_count = (total_discs - 1) / (total_rods - 2) and
      disc_rest = (total_discs - 1) mod (total_rods - 2) in

  (*FIRST STEP:
    distributes all of the discs except for the largest between
    the intermediate rod using the last one as a buffer*)
  for i=1 to total_rods-2 do
    if i = 1 then
      move rods (disc_count + disc_rest) 0 i (total_rods - 1)
    else
      move rods disc_count 0 i (total_rods - 1)
  done;

  (*SECOND STEP: moves the largest disc to the last rod*)
  move_disc rods 0 (total_rods - 1);
  
  print_mov 0 (total_rods - 1);
  step ();

  (*THIRD STEP:
    stacks the discs stocked in the intermediate
    rods on top of the largest one in the last rod*)
  for i = total_rods-2 downto 1 do
    if i = 1 then
      move rods (disc_count + disc_rest) i (total_rods - 1) 0
    else
      move rods disc_count i (total_rods - 1) 0
  done;

  print_int (get ());
  print_newline ()
;;

(*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*)
