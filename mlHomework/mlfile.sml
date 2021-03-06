(*
  Brent Turner
  CSC 345 -- ML Homework
  mlfile.sml
*)
(*--------------------------------------------------------------------------------------*)

(* a function that flips every element with the next 
element taking an 'a list -> 'a list *)
fun flip (x::y::xs) = ([y] @ [x] @ flip (xs))
  | flip (x::xs) = [x] (* Base Case *)
  | flip _ = [];   (* nonexhuastive match *)

(*--------------------------------------------------------------------------------------*)

(* function to delete the ith element in a list 'a list * int -> 'a list *)
fun deleteIth (nil, _) = nil                         (* list is empty *)
  | deleteIth  (_::xs, 1) = xs                       (* Base case *)

  (* Recurisve case - this case will also check to see if i is larger than the list, if so
  it returns the list*)
  | deleteIth (x::xs, i) = if (length ([x] @ xs) < i)
			   then x::xs else [x] @ deleteIth(xs, i-1);

(*--------------------------------------------------------------------------------------*)

(* This will be my helper function that takes in a 
char list -> char list that has checked all char for a vowel*)
fun latinize (x::xs) = if x = #"a" orelse x = #"i" orelse x = #"e"
		       orelse x = #"o" orelse x = #"u"
		       then [x] @ xs else latinize (xs @ [x])							      
  | latinize _ = []; (* nonexhuastive match *)


(* Helper function that takes char list -> char list thats checks if the first char
is a char *)
fun vowelAtStart (x::xs) =  if x = #"a" orelse x = #"i" orelse x = #"e"
			    orelse x = #"o" orelse x = #"u"
			    then [x] @ xs @ [#"y", #"a", #"y"]
			    else latinize (xs @ [x])  @ [#"a", #"y"]
  | vowelAtStart _ = []; (* nonexhuastive match *)


(* Main Function for piglatinize problem string -> string that 
expodes to pass a char list to latinize then implodes back at the end *)
fun piglatinize word = implode (vowelAtStart (explode word));



