infixr !>
fun f !> x = f x

structure Lists =
struct
  datatype 'a nestedList = L of 'a
                         | N of 'a nestedList list;

  datatype 'a listItem = S of 'a
                       | M of int * 'a;

  local
      fun take n []      = []
        | take 0 (x::xs) = []
        | take n (x::xs) = x :: take (n-1) xs;
      
      fun takeWhile p []      = []
        | takeWhile p (x::xs) = if p x
                                then x :: takeWhile p xs
                                else [];

      fun drop n []      = []
        | drop 0 (x::xs) = x::xs
        | drop n (x::xs) = drop (n-1) xs;

      fun dropWhile p []      = []
        | dropWhile p (x::xs) = if p x
                                then dropWhile p xs
                                else x::xs;

      fun repeat 0 x = []
        | repeat n x = x :: repeat (n-1) x;
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
        (x::takeWhile (fn y => y = x) xs) :: pack (dropWhile (fn y => y = x) xs);

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
                          | M (n, x) => repeat n x
        in
            List.concat !> map dec lst
        end;

    (* 14 - Duplicate the elements of a list. *)
    fun duplicate [] = []
      | duplicate (x::xs) = x :: x :: duplicate xs;

    (* 15 - Replicate the elements of a list a given number of times. *)
    fun replicate n = List.concat o map (repeat n);

    (* 16 - Drop every N'th element from a list. *)
    fun dropEvery n [] = []
      | dropEvery n xs = take (n-1) xs @ dropEvery n (drop n xs);

    (* 17 - Split a list into two parts; the length of the first part is given. *)
    fun splitAt n xs = [take n xs, drop n xs];
  end
end
