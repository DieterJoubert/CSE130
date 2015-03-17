#use "reviewfa13.ml"

open Printf;;

print_int (count [1;2;3;4;5] 10);;
print_string "\n";;
print_int (count [1;2;3;4;5] 3);;
print_string "\n";;
print_int ( count [1;3;2;3;4;3;5] 3);;
print_string "\n";;
print_string "\n";;
let () = List.iter (printf "%d ") (make_palyndrome [1;2] );;
print_string "\n";;
let () = List.iter (printf "%d ") (make_palyndrome [2;1;1;2] );;
print_string "\n";;
let () = List.iter (printf "%d ") (make_palyndrome [] );;
print_string "\n";;
(*
let () = List.iter (printf "%d ") (mulByDigit 2 [9;9;9] );;
print_string "\n";;

let () = List.iter (printf "%d ") (mulByDigit 3 [3;3;3] );;
print_string "\n";;

let () = List.iter (printf "%d ") (mulByDigit 9 [9;9;9;9] );;
print_string "\n";;
*)