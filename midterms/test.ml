let rec ith l i d =
  match l with
  | [] -> d
  | h::t -> if i = 0 then h else (ith t (i-1) d)

let f i = if i<0 then 0
  else (if i < 10 then 1
                     else (if i < 20 then 2 else 3));;


let rec update2 l i n d =
  match l with
  | [] -> if i=0 then [n] else (d :: (update2 [] (i-1) n d))
  | h::t -> if i=0 then n::t else (h :: (update2 t (i-1) n d))


let categorize f l =
  let base = [] in
  let fold_fn acc elmt = 
  let exist = ith acc (f elmt) [] in                                           
  update2 acc (f elmt) (exist @ [elmt]) []
  in
  List.fold_left fold_fn base l

    (* If you want to test it: *)
  (* Convert a string to a list of characters *)
let rec explode = function
      | "" -> []
      | s  -> (String.get s 0) :: explode (String.sub s 1 ((String.length s) - 1));;

(* Convert a list of characters to a string *)  
let rec implode = function
      | [] -> ""
      | charlist -> (Char.escaped (List.hd charlist)) ^ (implode (List.tl charlist));;

let replace s =
    let fn x =
      if x='-' then ' ' else x
    in
    implode (List.map fn (explode s))


let rec zip l1 l2 = 
  match (l1,l2) with
  | _ -> [] 
  | (h1::t1, h2::t2) -> (h1,h2)::(zip t1 t2)
