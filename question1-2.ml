(* Question 1.1: Given two strings, write a method to decide if 
 * one is a permutation of the other.
 *)

open String;;

let rotate1 l =
    match l with
        [] -> []
        | h::t -> t @ [h];;

let rec rotate l i k = 
    match l with
        [] -> []
        | h::t -> if (i = k) then l
                    else
                        rotate (rotate1 l) (i+1) k;;

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) [];;

let rec temp c r orig =
    match r with
        [] -> [c]
        | h::t -> if r = orig
            then
                temp (c@[h]) (rotate1 t) t
            else
                temp (c@[h]) (rotate1 t) t @ temp c (rotate1 r) orig;;

let perm s = temp [] (rotate1 (explode s)) (explode s);;
