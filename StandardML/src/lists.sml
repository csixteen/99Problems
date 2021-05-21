structure Lists =
struct
  datatype 'a nestedList = L of 'a
                         | N of 'a nestedList list;

  local
    fun takeWhile [] p      = []
      | takeWhile (x::xs) p = if p x
                              then x :: takeWhile xs p
                              else [];

    fun dropWhile [] p      = []
      | dropWhile (x::xs) p = if p x
                              then dropWhile xs p
                              else x::xs;
  in

    (* 1 - Find the last box of a list *)
    fun myLast lst =
      case lst of
           []    => NONE
         | x::[] => SOME lst
         | _::xs => myLast xs;

    (* 2 - Find the last but one box of a list *)
    fun myButLast lst =
      case lst of
           []       => NONE
         | x::[]    => NONE
         | x::y::[] => SOME lst
         | _::xs    => myButLast xs;

    (* 3 - Find the kth element of a list. First index is 1. *)
    fun elementAt [] n      = NONE
      | elementAt (x::_) 1  = SOME x
      | elementAt (x::xs) n = elementAt xs (n-1);

    (* 4 - Find the number of elements of a list. *)
    fun length []      = 0
      | length (_::xs) = 1 + length xs;

    (* 5 - Reverse a list *)
    fun reverse lst =
      let fun aux [] acc      = acc
            | aux (x::xs) acc = aux xs (x::acc)
      in
        aux lst []
      end;

    (* 6 - Find out whether a list is a palidrome *)
    fun isPalindrome lst = lst = reverse lst;

    (*
    * 7 - Flatten a nested list structure. Since lists in StandardML
    * are homogeneous, we need to define a nestedList datatype.
    * *)
    fun flatten (L x)  = [x]
      | flatten (N xs) = List.concat (map flatten xs);

    (* 8 - Eliminate consecutive duplicates of list elements. *)
    fun compress []         = []
      | compress (x::[])    = [x]
      | compress (x::y::xs) = if x = y
                              then compress (y::xs)
                              else x :: compress (y::xs);

    (* 9 - Pack consecutive duplicates of list elements into sublists. *)
    fun pack [] = []
      | pack (x::xs) =
        (x::takeWhile xs (fn y => y = x)) :: pack (dropWhile xs (fn y => y = x));

    (* 10 - Run-length encoding of a list. *)
    fun encode xs = map (fn x => (length x, hd x)) (pack xs);
  end
end
