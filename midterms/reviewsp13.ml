let length l =
  let f acc item = acc+1
  in List.fold_left f 0 l

let remove l x =
  let f acc item = if item != x then (acc @ [item]) else acc
  in List.fold_left f [] l

let rec ith l i d =
  match l with
  | [] -> d
  | h::t -> if i = 0 then h else (ith t (i-1) d)

let rec update l i n =
  match l with
  | [] -> l
  | h::t -> if i = 0 then n::t else (h :: (update t (i-1) n))

(*
let rec update2 l i n d =
  match l with
  | [] -> if i = (-1) then l else (d :: (update2 l (i-1) n d))
  | h::t -> if i = 0 then n::t else (h :: (update2 t (i-1) n d))
*)

let rec update2 l i n d =
  match l with
  | [] -> if i=0 then [n] else (d :: (update2 [] (i-1) n d))
  | h::t -> if i=0 then n::t else (h :: (update2 t (i-1) n d))

let categorize f l = 
  let base = []
  in
  let fold_fn acc elmt = 
    let exist = ith acc (f elmt) []
    in 
    if exist = [] 
    then update2 acc (f elmt) [elmt] []
    else update2 acc (f elmt) (exist @ [elmt]) []
    (*
    let i = (f elmt) in
    if ((ith acc i []) = []) 
    then (update2 acc i [elmt] [])
    else (update acc i ( (ith acc i [(-1)]) @ [elmt] ) ) *)
  in
  List.fold_left fold_fn base l

let f i = if i < 0 then 0
  else (if i < 10 then 1
  else (if i < 20 then 2 else 3));;