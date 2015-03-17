let rec split l = 
  let base = (0,[],[])
  in
  let fold_fn (i,l1,l2) elmt = 
    if i < (List.length l)/2 then (i+1, l1 @ [elmt], l2)
    else (i+1, l1, l2 @ [elmt])
  in
  let (_,l1,l2) = List.fold_left fold_fn base l in
  (l1,l2)

let rec merge l1 l2 =
  match (l1, l2) with
  | ([],l) -> l
  | (l,[]) -> l
  | (h1::t1,h2::t2) ->
      if h1 < h2 then h1 :: ( merge t1 (h2::t2))
      else h2 :: ( merge (h1::t1) t2)

let rec merge_sort l = 
  match l with 
  | [] -> []
  | [x] -> [x]
  | [x;y] -> merge [x] [y]
  | _ -> let (a,b) = split l in
    merge (merge_sort a) (merge_sort b)

(* Convert a string to a list of characters *)
let rec explode = function
    "" -> []
  | s  -> (String.get s 0) ::
          explode (String.sub s 1 ((String.length s) - 1));;

(* Convert a list of characters to a string *)  
let rec implode = function
    []       -> ""
  | charlist -> (Char.escaped (List.hd charlist)) ^
                (implode (List.tl charlist));;

let replace s = 
  let rec helper l = 
    match l with
    | [] -> []
    | h::t -> if (h = '-') then (' ' :: (helper t)) else (h :: (helper t))
  in
  implode (helper (explode s))

let rec app l item = 
  match l with
  | [] -> []
  | h::t -> (h item)::(app t item)































