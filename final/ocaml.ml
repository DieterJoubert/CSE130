(* FALL 2013 *)

let rec insert l i =
  match l with
  | [] -> [i]
  | h::t -> if (i>h) then i::h::t else h::(insert t i)

let insertion_sort = 
  List.fold_left insert []

type expr = 
  | Var of string
  | Const of int
  | Plus of expr * expr

let rec simpl e =
  match e with
  | Plus (e1, e2) ->
      (let e1' = simpl e1 in
      let e2' = simpl e2 in
      match (e1',e2') with
      | (Const a, Const b)  -> Const(a+b)
      | _ -> Plus (e1',e2'))
  | _ -> e

(* SPRING 2013 *)

let count f l = 
  let fold_fn acc elmt = 
    if (f elmt) then acc+1 else acc
  in List.fold_left fold_fn 0 l

let stretch l = 
  let fold_fn acc elmt = 
    acc @ [elmt] @ [elmt]
  in List.fold_left fold_fn [] l

type 'a tree = 
  | Empty
  | Node of 'a * 'a tree list;;

let rec zip l1 l2 =
  match (l1,l2) with
  | ([],[]) -> []
  | (h1::t1, h2::t2) -> (h1,h2)::(zip t1 t2)
  | _ -> raise (Failure "foo");;

let rec tree_zip t1 t2 = 
  match (t1,t2) with
  | (Empty,Empty) -> Empty
  | (Node (x1, l1), Node(x2, l2)) -> 
              let lst = (zip l1 l2) in
              let f (h1,h2) = tree_zip h1 h2 in
              let newList = List.map f lst in
              Node((x1,x2),newList)
  | _ -> raise (Failure "foo");;

(* WINTER 2013 *)

let sum_matrix m = 
  let fold_fn acc elmt = 
    acc + (List.fold_left (+) 0 elmt)
  in List.fold_left fold_fn 0 m

(*              *)



























