#use "hanoi_mod.ml";;
open Hanoi_base;;

(*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*)

(*main function*)
let hanoi total_discs =
  init ();
  let rods = [|[]; []; []|] in rods.(0) <- init_rod total_discs [];

  (*FIRST STEP: move all discs but one to the middle rod using the
    last one as a buffer*)
  move rods (total_discs-1) 0 1 2;

  (*SECOND STEP: move biggest disc to last rod*)
  move rods 1 0 2 1;

  print_mov 0 2;

  (*THIRD STEP: move the discs on the middle rod to the last one
    using the first one as a buffer*)
  move rods (total_discs-1) 1 2 0;

  print_int (get ());
  print_newline ()
;;

(*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*)
