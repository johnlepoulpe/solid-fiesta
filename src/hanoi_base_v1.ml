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

(*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*)

(*AUXILIARY FUNCTIONS*)

(*returns a list of the type [1; ...;n]*) 
let rec init_rod n rod =
  if n == 0 then rod
  else init_rod (n-1) (n :: rod)
;;

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
  else failwith "a or b are not between 0 and 2"
;;

(* Another version of choose:
let choose2 a b = 3 - a - b;; *)

(*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*)

(*CORE FUNCTIONS*)

(*moves a single disc from orig_rod to dest_rod*)
let move_disc rods orig_rod dest_rod =
  let disc = top rods.(orig_rod) in
  rods.(orig_rod) <- pop rods.(orig_rod);
  rods.(dest_rod) <- push disc rods.(dest_rod)
;;
      
(*moves num_discs discs from orig_rod to dest_rod*)
let rec move rods num_discs orig_rod dest_rod =
  if num_discs = 1 then
    (
      move_disc rods orig_rod dest_rod;
      
      print_mov orig_rod dest_rod;
      step ();
    )
  else
    (
      let temp_rod = choose orig_rod dest_rod in
      
      move rods (num_discs - 1) orig_rod temp_rod;
      move_disc rods orig_rod dest_rod;
      
      print_mov orig_rod dest_rod;
      step ();
      
      move rods (num_discs - 1) temp_rod dest_rod;
    )
;;

(*main function*)
let hanoi total_discs =
  init ();
  let rods = [|[]; []; []|] in rods.(0) <- init_rod total_discs [];

  (*FIRST STEP: move all discs but one to the middle rod using the
    last one as a buffer*)
  move rods (total_discs-1) 0 1;

  (*SECOND STEP: move biggest disc to last rod*)
  move_disc rods 0 2;
  
  print_mov 0 2;
  step ();

  (*THIRD STEP: move the discs on the middle rod to the last one
    using the first one as a buffer*)
  move rods (total_discs-1) 1 2;
  
  print_int (get ());
  print_newline ()
;;

(*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*)
