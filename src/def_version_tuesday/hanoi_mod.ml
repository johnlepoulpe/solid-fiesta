module Hanoi_base:

sig
  val empty_stack : 'a list
  val push : 'a -> 'a list -> 'a list
  val pop : 'a list -> 'a list
  val top : 'a list -> 'a
  val c : int ref
  val init : unit -> unit
  val step : unit -> unit
  val get : unit -> int
  val print_mov : int -> int -> unit
  val init_rod : int -> int list -> int list
  val mirror : 'a list -> 'a list
  val move_disc : 'a list array -> int -> int -> unit
  val move : 'a list array -> int -> int -> int -> int -> unit
end

=

struct

  (*stack structure definition*)
  let empty_stack = []
  let push x stack = x :: stack
  let pop stack =
    match stack with
    | [] -> failwith "empty stack pop"
    | x :: xs -> xs
  let top stack =
    match stack with
    | [] -> failwith "empty stack top"
    | x :: xs -> x

  (*counter declaration*)
  let c = ref 0
  let init () = c := 0
  let step () = c := !c + 1
  let get () = !c

  (*prints movement message in interpreter*)
  let print_mov origin destination =
    print_string ("I move a disc from rod " ^
                  (string_of_int origin) ^
                  " to rod " ^
                  (string_of_int destination));
    print_newline()

  (*returns a list of the type [1; ...;n]*) 
  let rec init_rod n rod =
    if n == 0 then rod
    else init_rod (n-1) (n :: rod)

  (*reverses a list in a linear time*)
  let mirror u =
    let rec aux u v =
      match u with
      | [] -> v
      | x :: xs -> aux xs (x::v) in
    aux u []

  (*moves a single disc from orig_rod to dest_rod*)
  let move_disc rods orig_rod dest_rod =
    let disc = top rods.(orig_rod) in
    rods.(orig_rod) <- pop rods.(orig_rod);
    rods.(dest_rod) <- push disc rods.(dest_rod)

  (*moves num_discs discs from orig_rod to dest_rod using temp_rod*)
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
        move rods (num_discs - 1) orig_rod temp_rod dest_rod;
        move rods 1 orig_rod dest_rod temp_rod;

        print_mov orig_rod dest_rod;

        move rods (num_discs - 1) temp_rod dest_rod orig_rod;
      )
end
;;
