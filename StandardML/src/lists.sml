infixr !>
fun f !> x = f x

structure Lists =
struct
  datatype 'a nestedList = L of 'a
                         | N of 'a nestedList list;

  datatype 'a listItem = S of 'a
                       | M of int * 'a;

  local
      fun take [] n = []
        | take (x::xs) 0 = []
        | take (x::xs) n = x :: take xs (n-1);
      
      fun takeWhile [] p      = []
        | takeWhile (x::xs) p = if p x
                                then x :: takeWhile xs p
                                else [];

      fun dropWhile [] p      = []
        | dropWhile (x::xs) p = if p x
                                then dropWhile xs p
                                else x::xs;

      fun repeat x 0 = []
        | repeat x n = x :: repeat x (n-1); 
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

    (* 11 - Modified run-length encoding. *)
    fun encodeModified lst =
        let fun enc g =
                case g of
                    x::[] => S x
                  | x::xs => M (length g, x)
        in
            map enc (pack lst)
        end;

    (* 12 - Decode a run-length encoded list. *)
    fun decode lst =
        let
            fun dec e = case e of
                            S x => [x]
                          | M (n, x) => repeat x n
        in
            List.concat !> map dec lst
        end;
  end
end
