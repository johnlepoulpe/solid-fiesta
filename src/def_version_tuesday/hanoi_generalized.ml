(*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*)

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
  move rods 1 0 (total_rods - 1) 0;

  print_mov 0 (total_rods - 1);

  (*THIRD STEP:
    stacks the discs stored in the intermediate
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
