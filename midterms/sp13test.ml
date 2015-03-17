#use "reviewsp13.ml"

open Printf;;

print_int (length [1;2;3;4;5] );;
print_string "\n";;
print_int (length [1;2;3;4;5;3;4;320] );;
print_string "\n";;

let () = List.iter (printf "%d ") (remove [1;2;3;2;2;3;4] 2 );;
print_string "\n";;

print_string (ith ["a";"b";"c";"d"] 0 "notfound" );;
print_string "\n";;
print_string (ith ["a";"b";"c";"d"] 1 "notfound" );;
print_string "\n";;
print_string (ith ["a";"b";"c";"d"] 2 "notfound" );;
print_string "\n";;
print_string (ith ["a";"b";"c";"d"] 3 "notfound" );;
print_string "\n";;
print_string (ith ["a";"b";"c";"d"] 4 "notfound" );;
print_string "\n";;

let () = List.iter (printf "%s ") (update ["a";"b";"c";"d"] 0 "ZZZ");;
print_string "\n";;
let () = List.iter (printf "%s ") (update ["a";"b";"c";"d"] 1 "ZZZ");;
print_string "\n";;
let () = List.iter (printf "%s ") (update ["a";"b";"c";"d"] 2 "ZZZ");;
print_string "\n";;
let () = List.iter (printf "%s ") (update ["a";"b";"c";"d"] 3 "ZZZ");;
print_string "\n";;
let () = List.iter (printf "%s ") (update ["a";"b";"c";"d"] 4 "ZZZ");;
print_string "\n";;

let () = List.iter (printf "%s ") (update2 ["a";"b";"c";"d"] 0 "ZZZ" "y");;
print_string "\n";;
let () = List.iter (printf "%s ") (update2 ["a";"b";"c";"d"] 1 "ZZZ" "y");;
print_string "\n";;
let () = List.iter (printf "%s ") (update2 ["a";"b";"c";"d"] 2 "ZZZ" "y");;
print_string "\n";;
let () = List.iter (printf "%s ") (update2 ["a";"b";"c";"d"] 3 "ZZZ" "y");;
print_string "\n";;
let () = List.iter (printf "%s ") (update2 ["a";"b";"c";"d"] 4 "ZZZ" "y");;
print_string "\n";;
let () = List.iter (printf "%s ") (update2 ["a";"b";"c";"d"] 5 "ZZZ" "y");;
print_string "\n";;
let () = List.iter (printf "%s ") (update2 ["a";"b";"c";"d"] 6 "ZZZ" "y");;
print_string "\n";;
let () = List.iter (printf "%s ") (update2 ["a";"b";"c";"d"] 7 "ZZZ" "y");;
print_string "\n";;
