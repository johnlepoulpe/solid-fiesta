(*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*)

(*SECONDARY FUNCTIONS AND TYPE DEFINITIONS*)

#load "unix.cma";;
open Unix;;
open Printf;;

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
  rods.(dest_rod) <- push disc rods.(dest_rod);;
      
(*moves num_discs discs from orig_rod to dest_rod*)
let rec move rods num_discs orig_rod dest_rod =
  if num_discs = 0 then ()
  else if num_discs = 1 then
    (
      move_disc rods orig_rod dest_rod;
      
      step ();
    )
  else
    (
      let temp_rod = choose orig_rod dest_rod in
      move rods (num_discs - 1) orig_rod temp_rod;
      move_disc rods orig_rod dest_rod;
      
      step ();
      move rods (num_discs - 1) temp_rod dest_rod;
    );;

(*main function*)
let hanoi total_discs =
  init ();
  let rods = [|[]; []; []|] in rods.(0) <- init_rod total_discs [];
  move rods (total_discs-1) 0 1;
  move_disc rods 0 2;
  
  step ();
  move rods (total_discs-1) 1 2;
  print_int (get ());
  print_newline ();
  get ();;

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

(*TEST FUNCTIONS*)

let time f x =
  let now = gettimeofday () in f x;
  gettimeofday () -. now;;

let write_steps name start stop =
  let oc = open_out ("RStats/" ^ name ^ ".data") in
  for i=start to stop do
    fprintf oc "%i , %i\n" i (hanoi i)
  done;
  close_out oc;;

let write_time name start stop times =
  let oc = open_out ("RStats/" ^ name ^ ".data") in
  for k=start to stop do
    for i=1 to times do
      if i = 1 then
	  fprintf oc "%i" k
      else
	fprintf oc " , %f" (time hanoi k)
    done;
    fprintf oc "\n"
  done;
  close_out oc;;

(*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*)
