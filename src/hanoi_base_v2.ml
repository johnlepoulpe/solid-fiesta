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
      
(*moves num_discs discs from orig_rod to dest_rod using temp_rod*)
let rec move rods num_discs orig_rod dest_rod temp_rod=
  if num_discs = 1 then
    (
      move_disc rods orig_rod dest_rod;
      
      print_mov orig_rod dest_rod;
      step ();
    )
  else
    (
      move rods (num_discs - 1) orig_rod temp_rod dest_rod;
      move_disc rods orig_rod dest_rod;
      
      print_mov orig_rod dest_rod;
      step ();
      
      move rods (num_discs - 1) temp_rod dest_rod orig_rod;
    )
;;

(*main function*)
let hanoi total_discs =
  init ();
  let rods = [|[]; []; []|] in rods.(0) <- init_rod total_discs [];

  (*FIRST STEP: move all discs but one to the middle rod using the
    last one as a buffer*)
  move rods (total_discs-1) 0 1 2;

  (*SECOND STEP: move biggest disc to last rod*)
  move_disc rods 0 2;
  
  print_mov 0 2;
  step ();

  (*THIRD STEP: move the discs on the middle rod to the last one
    using the first one as a buffer*)
  move rods (total_discs-1) 1 2 0;
  
  print_int (get ());
  print_newline ()
;;

(*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*)
