let movement origin destination =
  print_string ("I move a disc from rod " ^
		(string_of_int origin) ^
		" to rod " ^
		(string_of_int destination));
  print_newline();; (* counter := !counter + 1 *)

let rec move origin destination number=
  if number=1 then movement origin destination
  else  begin
      let last_rod = 6 - (origin + destination) in
      move origin last_rod (number-1);
      move origin destination 1;
      move last_rod destination (number-1)
    end;;

move 1 3 4;;
    

	     
