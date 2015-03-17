type expr = 
  | Const of int
  | Var of string
  | Op of string * expr * expr;;

(* rename_var : expr -> string -> string -> expr *)
(* rewrite all expression match cases *)
let rec rename_var e n1 n2 = 
  match e with
  | Const i -> Const i
(*| Var v -> if v = n1 then Var n2 else Var v    *)
  | Var v -> Var (if v = n1 then n2 else v)
  | Op (name, e1, e2) -> Op (name, rename_var e1 n1 n2, rename_var e2 n1 n2)

let to_str e =
  let rec str_helper e top_level =
  match e with
  | Const i -> string_of_int i
  | Var v -> v
  | Op (name, e1, e2) -> if top_level=true 
        then (str_helper e1 false)^name^(str_helper e2 false) 
        else "("^(str_helper e1 false)^name^(str_helper e2 false)^")" 
  in
  str_helper e true;;

let average_if f l =
  let folding_fn (sum,count) elmt = 
    if (f elmt) then (sum+elmt,count+1) else (sum,count)
  in
  let base = (0,0)
  in
  let (a,b) = List.fold_left folding_fn base l 
  in
  match (a,b) with
  | (0,0) -> 0
  | (x,y) -> x / y

(*
  let (sum,cnt) = List.fold ...
  if cnt = 0 then 0 else sum/cnt
*)

(*
let length_2 l = 
  let fn acc x = acc + List.length x
  in List.fold_left fn 0 l
*)

let length_2 l = List.fold_left (+) 0 (List.map List.length l);;

let length_3 l = List.fold_left (+) 0 (List.map length_2 l);;

let f1 = List.map (fun x-> 2*x);;
f1 [1;2;3;4] = [2;4;6;8]

let f2 = List.fold_left (fun x y -> (y+2)::x) [];;
(* let f2 = List.fold_left (fun acc elmt -> (elmt+2)::acc) [];; *)
(* f2 [3;5;7;9] = 
[]
[5]
[7;5]
[9;7;5] *)
f2 [3;5;7;9] = [11;9;7;5];;

(* 
with items reversed reversed
let f2 = List.fold_left (fun x y -> x @ [(y+2)] [])
*)

let f3 = List.fold_left (fun acc elmt -> acc @ [3*elmt]) [];;

(*
let f3 = List.fold_left (fun acc elmt -> acc @ [3*elmt]) [];;
f3 [1;3;6] = [3;9;18];;

let f = List.fold_left (fun acc elmt -> elmt acc);;
*)

let f = List.fold_left (fun x y -> y x);;

f 1 [(+) 1; (-) 2];;

(*
f "abc" [(^) "zzz"; (^) "yyy"];;
"abc"
^ "zzz" "abc" = "zzzabc"
^ "yyy" "zzzabc" = "yyyzzzabc"
*)

f [1;2;3] [f1;f2;f3];;

(*
[1;2;3]
[2;4;6] after f1
[8;6;4] after f2
[24;18;12]
*)




