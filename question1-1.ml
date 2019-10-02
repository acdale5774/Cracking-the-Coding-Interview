(* Question 1.1: Implement an algorithm to determine if a string has
 * all unique characters. What if you cannot use additional data
 * structures *)

open String;;

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) [];;

let rec contains x l =
    match l with
        [] -> false
        | h::t -> 
                if h==x then true
                else contains x t;;

let rec charsAreUniqueInner s =
    match s with
        [] -> true
        | h::t -> 
                if contains h t then false
                else charsAreUniqueInner t;;

let charsAreUnique a = charsAreUniqueInner (explode a);;
