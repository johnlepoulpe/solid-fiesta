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

(*returns a list of the type [1; ...;n]*) 
let rec init_rod n rod =
  if n == 0 then rod
  else init_rod (n-1) (n :: rod);;

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
let move_disc rods orig_rod dest_rod =
  let disc = top rods.(orig_rod) in
  rods.(orig_rod) <- pop rods.(orig_rod);
  rods.(dest_rod) <- push disc rods.(dest_rod);
  print rods
;;
      
(*moves num_discs discs from orig_rod to dest_rod*)
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
    );;

(*main function*)
let hanoi total_discs total_rods=
  init ();
  
  let rods =
    Array.make total_rods [] in rods.(0) <- init_rod total_discs [];
  let disc_count = (total_discs - 1) / (total_rods - 2) and
      disc_rest = (total_discs - 1) mod (total_rods - 2) in
  
  for i=1 to total_rods-2 do
    if i = 1 then
      move rods (disc_count + disc_rest) 0 i (total_rods - 1)
    else
      move rods disc_count 0 i (total_rods - 1)
  done;
  
  move_disc rods 0 (total_rods - 1);
  print_mov 0 (total_rods - 1);
  step ();
  
  for i = total_rods-2 downto 1 do
    if i = 1 then
      move rods (disc_count + disc_rest) i (total_rods - 1) 0
    else
      move rods disc_count i (total_rods - 1) 0
  done;

  print_int (get ());
  print_newline ();;

(*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*)

(*DEBUG FUNCTIONS*)

let rec print_int_list u =
  match u with
  | [] -> print_char '\t';
  | [x] -> print_int x; print_char '\t';
  | x :: xs -> print_int x; print_int_list xs;;

(*prints an array of int list*)
let print t =
  let len = Array.length t in
  for i=0 to (len-1) do
    print_int_list t.(i);
  done;
  print_newline ();;

(*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*)
