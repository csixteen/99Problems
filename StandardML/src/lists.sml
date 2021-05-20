fun myLast lst =
  case lst of
       [] => NONE
     | x::[] => SOME lst
     | _::xs => myLast xs;

fun myButLast lst =
  case lst of
       [] => NONE
     | x::[] => NONE
     | x::y::[] => SOME lst
     | _::xs => myButLast xs;

