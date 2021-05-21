structure Lists =
struct
  datatype 'a nestedList = L of 'a
                         | N of 'a nestedList list;

  local
    fun append [] ys      = ys
      | append (x::xs) ys = x :: append xs ys;
  in

    (* Find the last box of a list *)
    fun myLast lst =
      case lst of
           []    => NONE
         | x::[] => SOME lst
         | _::xs => myLast xs;

    (* Find the last but one box of a list *)
    fun myButLast lst =
      case lst of
           []       => NONE
         | x::[]    => NONE
         | x::y::[] => SOME lst
         | _::xs    => myButLast xs;

    (* Find the kth element of a list. First index is 1. *)
    fun elementAt [] n      = NONE
      | elementAt (x::_) 1  = SOME x
      | elementAt (x::xs) n = elementAt xs (n-1);

    (* Find the number of elements of a list. *)
    fun myLength []      = 0
      | myLength (_::xs) = 1 + myLength xs;

    (* Reverse a list *)
    fun reverse lst =
      let fun aux [] acc      = acc
            | aux (x::xs) acc = aux xs (x::acc)
      in
        aux lst []
      end;

    (* Find out whether a list is a palidrome *)
    fun isPalindrome lst = lst = reverse lst;

    (*
    * Flatten a nested list structure. Since lists in StandardML
    * are homogeneous, we need to define a nestedList datatype.
    * *)
    fun flatten (L x)  = [x]
      | flatten (N xs) = List.concat (map flatten xs);

    (* Eliminate consecutive duplicates of list elements. *)
    fun compress []         = []
      | compress (x::[])    = [x]
      | compress (x::y::xs) = if x = y
                              then compress (y::xs)
                              else x :: compress (y::xs);
  end
end
