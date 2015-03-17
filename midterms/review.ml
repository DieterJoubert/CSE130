type 'a maybe = 
  | None
  | Some of 'a

(* 
first (a -> bool) -> a list -> a maybe 
let even x = (x mod 2 = 0)
first even [1;3;5] -> None
first even [1,2,4] -> Some 2 
*)

let first f l =
  let base = None
  in
  let fold_fn acc elmt = 
    match acc with
    | None -> if (f elmt) then Some elmt else None
    | Some y -> Some y
  in
  List.fold_left fold_fn base l

let l = [1;2;3;4;5];;

(* remember, "< x y" evaluates to " if x < y then true else false" 
    since < is a function *)

(* 
first ((=) 4) l = Some 4
first ((=) 10) l = None
first ((<) 3) l = Some 4
first ((<) 10) l = None
first ((<) 0) l = Some 1
*)

let rec zip l1 l2 =
  match (l1,l2) with
  | ([],[]) -> []
  | (h::t,[]) -> []
  | ([],h::t) -> []
  | (h1::t1,h2::t2) -> (h1,h2) :: (zip t1 t2)

(* 
alternatively, using wildcards (_):
let rec zip l1 l2 =
  match (l1,l2) with
  | (_,[]) -> []   
  | ([],_) -> []
  | (h1::t1,h2::t2) -> (h1,h2) :: zip t1 t2   
*)

let map2 f l1 l2 =
  let l = zip l1 l2 in 
  let g (x,y) = (f x y) 
  in List.map g l

let map3 f l1 l2 l3 =
  let l = zip l1 l2 in
  let a = zip l l3 in
  let g((x,y),z) = f x y z 
  in List.map g a

(* unzip : (a*b) list -> (a list * b list)   *)
let rec unzip l =
  match l with
  | [] -> ([],[])
  | (x,y)::t -> let (a,b) = unzip t in (x::a, y::b)
















