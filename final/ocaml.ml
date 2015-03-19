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

(* WINTER 2012 *)

let prices = [ ("Baseball Bat", 20); ("Soccer Ball", 10); ("Tennis Racket", 40) ]

let rec find d k =
  match d with
  | [] -> raise Not_found
  | (k',v') :: t -> if k>k' then (raise Not_found) else
                    if k=k' then v' else find t k

let rec add d k v =
  match d with
  | [] -> [(k,v)]
  | (k',v') :: t -> if k>k' then (k,v)::(k',v')::t else
                    if k=k' then (k,v)::t else (k',v')::(add t k v)

let keys d = let fn (key,value) = key
  in List.map fn d

let values d = let fn (key,value) = value
  in List.map fn d

let key_of_max_val d = 
  let fold_fn (kmax,vmax) (k,v) = 
    if v>vmax then (k,v) else (kmax,vmax)
  in
  match d with
  | [] -> raise Not_found
  | base::t -> let (max_k_found,max_v_found) = List.fold_left fold_fn base t
                in max_k_found

(* WINTER 2011 *)

type ’a dict = Empty | Node of string * 'a * 'a dict * 'a dict

let fruitd =
  Node ("grape", 2.65,
    Node ("banana", 1.50,
      Node ("apple", 2.25, Empty, Empty),
      Node ("cherry", 2.75, Empty, Empty)),
    Node ("orange", 0.75,
      Node ("kiwi", 3.99, Empty, Empty),
      Node ("peach", 1.99, Empty, Empty)))

let rec find d k =
  match d with
  | Empty -> raise Not_found
  | Node (k',v',l,r) ->
        if k=k' then v' else
        if k<k' then find l k else
          find r k

let rec add d k v =
  match d with
  | Empty -> Empty
  | Node (k', v', l, r) ->
        if k=k' then Node(k,v,l,r) else
        if k<k' then Node(k',v',add l k v, r) else
            Node(k',v',l,add r k v)

(* tree fold function by in-order traversal *)
let rec fold f b d =
  match d with
  | Empty -> b
  | Node(k,v,l,r) -> fold f (f k v (fold f b l)) r

let rec lookup ns name =
   match ns with
   | EmptyNameSpace -> raise NotFound
   | Info ([],parent_ns) -> lookup parent_ns name
   | Info ((s,v)::t, parent_ns) ->
          if s = name then v
          else lookup (Info (t, parent_ns)) name
