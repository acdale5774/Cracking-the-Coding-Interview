(* Question 1.4: Given a string, write a function to check if it is a
 * permutation of a palindrome.
 *)

open String;;

let rec size l =
    match l with
        [] -> 0
        | h::t -> 1 + size t;;

let rec minusLast l =
    match l with
        [] -> []
        | [x] -> []
        | h::t -> [h] @ minusLast t;;

let rotate1 l =
    match l with
        [] -> []
        | h::t -> t @ [h];;

let rec rotateInner l i k = 
    match l with
        [] -> []
        | h::t -> if (i = k) then l
                    else
                        rotateInner (rotate1 l) (i+1) k;;

let rotate l n = rotateInner l 0 n;;

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) [];;

let implode c_l =
    let buf = Buffer.create 16 in
        List.iter (Buffer.add_char buf) c_l;
        Buffer.contents buf;;

let rec getList ll =
    match ll with
        [] -> []
        | h::t -> [h] @ getList t;;

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

let perm l = getList (permInner [] (rotate1 l) l);;

let permStr s = getStringList (permInner [] (rotate1 (explode s)) (explode s));;

let rec isPalindrome l =
    match l with
        [] -> true
        | [x] -> true
        | h::t -> match (rotate l ((size l) - 1)) with
            [] -> false
            | h2::t2 -> if h = h2
                        then
                            isPalindrome (minusLast t)
                        else
                            false;;

let isPalindromeStr s = isPalindrome (explode s);;

let rec containsPal l =
    match l with
        [] -> false
        | h::t ->
                if isPalindrome h
                    then
                        true
                    else
                        containsPal t;;

let isPermOfPal l = containsPal (perm l);;

let isPermOfPalStr s = isPermOfPal (explode s);;
