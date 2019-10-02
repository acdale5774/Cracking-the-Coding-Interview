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

let implode c_l =
    let buf = Buffer.create 16 in
        List.iter (Buffer.add_char buf) c_l;
        Buffer.contents buf;;

let rec getStringList c_ll =
    match c_ll with
        [] -> []
        | h::t -> [implode h] @ getStringList t;;

let rec permInner c r orig =
    match r with
        [] -> [c]
        | h::t -> if r = orig
            then
                permInner (c@[h]) (rotate1 t) t
            else
                permInner (c@[h]) (rotate1 t) t 
                    @ permInner c (rotate1 r) orig;;

let perm s = getStringList (permInner [] (rotate1 (explode s)) (explode s));;
