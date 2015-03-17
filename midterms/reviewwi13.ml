open Printf;;

type 'a maybe =
  | None
  | Some of 'a

let first f l =
  let base = (None)
  in
  let fold_fn acc elmt = 
  match acc with 
  | None -> if (f elmt) then Some elmt else None
  | Some x -> Some x
  in List.fold_left fold_fn base l 

let rec zip l1 l2 =
  match (l1,l2) with
  | ([],[]) -> ([],[])
  | ((h::t),[]) -> ([],[])
  | ([],(h::t)) -> ([],[])
  | (h1::t1, h2::t2) -> (h1,h2) :: (zip t1 t2)

(*
let map2 f l1 l2 = 
  let fn acc (x,y) = acc @ (f x y)
  in List.fold_left fn [] (zip l1 l2)
*)

let map2 f l1 l2 = 
  let g (x,y) = f x y in
  let lz = zip l1 l2
  in List.map g lz

let map3 f l1 l2 l3 = 
  let g ((x,y),z) = f x y z in
  let lz = zip (zip l1 l2) l3
  in List.map g lz

let rec unzip l =
  match l with
  | [] -> ([], [])
  | (x,y)::t -> 














