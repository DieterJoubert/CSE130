let count l x =
  let f acc item =
    if item=x then acc+1 else acc
  in List.fold_left f 0 l 

let make_palyndrome l =
  let f acc item = 
    match item with
    | x -> x::acc
  in List.fold_left f l l

let count2 l x = 
  let fold a b = if (b = x) then (a + 1) else a in
  let base = 0     in 
  List.fold_left fold base l

let fold_2 f b l =
   let fn (comp,index) elmt =
       let f_ret = (f comp elmt index) in
         (f_ret, index+1) in
   let (result,_) = List.fold_left fn (b,0) l
 in result

let rec ith l i d =
  let fn a elmt index =
    if index=i then elmt else a
  in fold_2 fn d l 

type 'a fun_tree = 
  | Leaf of ('a -> 'a)
  | Node of ('a fun_tree) * ('a fun_tree)

let rec apply_all t x =
  match t with
  | Leaf fx -> fx x
  | Node (a,b) -> apply_all b (apply_all a x)

let f1 x = x + 1;;
let f2 x = x * 2;;
let f3 x = x + 3;;
let t1 = Node(Leaf f1, Node(Leaf f2, Leaf f3));;
apply_all t1 0;;

let f4 = (+) 1;;
let f5 = (-) 2;;
let f6 = (+) 3;;
let t2 = Node(Node(Leaf f4, Leaf f5), Leaf f6);;
apply_all t2 0;;

let f7 = (^) "a";;
let f8 x = x ^ "b";;
let f9 x = x ^ "ab";;
let t3 = Node(Leaf f7, Node(Leaf f8, Leaf f9));;
apply_all t3 "123";;

let f10 = List.fold_left (fun x y -> (y*2)::x) [];;
let f11 = List.fold_left (fun x y -> x@[y]) [];;
let t4 = Node(Node(Leaf f10, Leaf f11), Node(Leaf f10, Leaf f11));;
apply_all t4 [1;2;3];;

let rec compose t1 t2 = 
  match (t1,t2) with
  | (Leaf a, Leaf b) -> Leaf (fun x -> b (a x))
  | (Node (x1,y1), Node (x2,y2)) -> Node (compose x1 x2, compose y1 y2)

let t21 = Node(Leaf f1, Leaf f2);;
let t22 = Node(Leaf f3, Leaf f4);;
let tCompose = compose t21 t22;;













