(* FINAL FALL 2013 *)

(*
# insert [] 10;;
- : int list = [10]
# insert [1;2;3;4] 3;;
- : int list = [1; 2; 3; 3; 4]
# insert [10;15;20;30] 40;;
- : int list = [10; 15; 20; 30; 40]
# insert [10;15;20;30] 5;;
- : int list = [5; 10; 15; 20; 30]
*)
let rec insert l i =
  match l with
  | [] -> [i]
  | h::t -> if i<h then i::(h::t) else h::(insert t i)

let insertion_sort l = 
  failwith "not yet"

type expr =
| Var of string
| Const of int
| Plus of expr * expr

