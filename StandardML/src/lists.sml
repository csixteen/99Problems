(* Find the last box of a list *)
fun myLast lst =
  case lst of
       [] => NONE
     | x::[] => SOME lst
     | _::xs => myLast xs;

(* Find the last but one box of a list *)
fun myButLast lst =
  case lst of
       [] => NONE
     | x::[] => NONE
     | x::y::[] => SOME lst
     | _::xs => myButLast xs;

(* Find the kth element of a list. First index is 1. *)
fun elementAt ([], n) = NONE
  | elementAt (x::_, 1) = SOME x
  | elementAt (x::xs, n) = elementAt (xs, n-1);
