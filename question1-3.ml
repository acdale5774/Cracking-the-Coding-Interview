(* Question 1.1: Write a method to replace all spaces in a string
 * with "%20".
 *)

open String;;

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) [];;

let implode c_l =
    let buf = Buffer.create 16 in
        List.iter (Buffer.add_char buf) c_l;
        Buffer.contents buf;;

let rec _URLifyInner l =
    match l with
        [] -> []
        | h::t -> if h = ' '
                    then
                        ['%'] @ ['2'] @ ['0'] @ _URLifyInner t
                    else
                        [h] @ _URLifyInner t;;

let _URLify s = implode (_URLifyInner (explode s));;
