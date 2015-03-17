(* WINTER 2012 *)

let rec split l =
  let base = (0,[],[])
  in
  let fold_fn (i,l1,l2) elmt =
    if i < List.length l / 2 
    then (i+1,l1 @ [elmt],l2) 
    else (i+1,l1,l2 @ [elmt]) 
  in let (_,l1,l2) = List.fold_left fold_fn base l in
  (l1,l2)

let rec merge l1 l2 =
  match (l1,l2) with
  | ([],l) -> l
  | (l,[]) -> l
  | (h1::t1,h2::t2) -> if h1<h2 
    then h1 :: (merge t1 (h2::t2)) 
    else h2 :: (merge (h1::t2) t2)

let rec merge_sort l =
  match l with
  | [] -> []
  | [x] -> [x]
  | [x;y] -> merge [x] [y]
  | _ -> let (a,b) = split l in merge (merge_sort a) (merge_sort b)

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

let replace str =
  let fn s = 
  if s='-' then ' ' else s
  in implode (List.map fn (explode str))

(*
let rec app l item = 
  match l with
  | [] -> []
  | h::t -> (h item)::(app t item)
*)

let app l x =
  let fn y = y x
  in List.map fn l

(* --------------------------------------------------------------- *)
(* WINTER 2013 *)

type 'a maybe =
  | None
  | Some of 'a

let first f l =
  let base = None
  in
  let fold_fn acc elmt =
    match acc with
    | Some x -> Some x
    | None -> if f elmt then Some elmt else None
  in List.fold_left fold_fn base l

let rec zip l1 l2 =
  match (l1,l2) with
  | (_,[]) -> []
  | ([],_) -> []
  | (h1::t1,h2::t2) -> (h1,h2) :: (zip t1 t2)

let map2 f l1 l2 =
  let fn (a,b) = f a b
  in List.map fn (zip l1 l2)

let map3 f l1 l2 l3 =
  let fn ((a,b),c) = f a b c
  in List.map fn (zip (zip l1 l2) l3)

let rec unzip l =
  match l with
  | [] -> ([], [])
  | (x,y)::t -> let (a,b) = unzip t in (x::a, y::b)

(* --------------------------------------------------------------- *)
(* SPRING 2013 *)

let length l =
  let fn acc elmt = acc+1
  in List.fold_left fn 0 l

let remove l x =
  let fn acc elmt = 
    if elmt=x then acc else acc @ [elmt]
  in List.fold_left fn [] l

let rec ith l i d =
  match l with
  | [] -> d
  | h::t -> if i=0 then h else (ith t (i-1) d)

let rec update l i n =
  match l with
  | [] -> []
  | h::t -> if i=0 then n::(update t (i-1) n) else h::(update t (i-1) n)

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
  in
  List.fold_left fold_fn base l

(*
let categorize f l = 
  let base = [] in
  let fold_fn acc elmt = 
    let index = f elmt in
    let arg = ith acc index [] in
      update2 acc index (arg @ [elmt]) [] 
  in List.fold_left fold_fn base l;;  
*)

(* --------------------------------------------------------------- *)
(* FALL 2013 *)

let count l x =
  let fn acc elmt =
  if elmt=x then (acc+1) else acc
  in List.fold_left fn 0 l

let make_palyndrome l = 
  let fn acc elmt = elmt::acc
  in List.fold_left fn l l

let fold_2 f b l =
  let fn (comp,index) elmt =
    (f comp elmt index, index+1)
  in
  let (res,_) = List.fold_left fn (b,0) l
  in res



let rec ith l i d =
  let fn a elmt index =
    if index=i then elmt else a
  in fold_2 fn d l 

type 'a fun_tree =
| Leaf of ('a -> 'a)
| Node of ('a fun_tree) * ('a fun_tree);;

let rec apply_all t x =
  match t with
  | Leaf fn -> (fn x)
  | Node (fn1,fn2) -> apply_all fn2 (apply_all fn1 x)

let rec compose t1 t2 =
  match (t1,t2) with
  | (Leaf a, Leaf b) -> Leaf (fun x -> b (a x)) 
  | (Node (a1,a2), Node (b1,b2)) -> Node (compose a1 b1, compose a2 b2)

(* --------------------------------------------------------------- *)
(* FALL 2014 *)

type expr =
| Const of int
| Var of string
| Op of string * expr * expr;;

let rec rename_var e nOld nNew =
  match e with
  | Const x -> Const x
  | Var s -> Var (if s=nOld then nNew else s)
  | Op (s,e1,e2) -> Op (s, rename_var e1 nOld nNew, rename_var e2 nOld nNew)

let to_str e =
  let rec str_helper e top_level =
  match e with
  | Const i -> string_of_int i
  | Var s -> s
  | Op (s,e1,e2) -> if top_level = true 
       then (str_helper e1 false) ^ s ^ (str_helper e2 false)
       else "(" ^ (str_helper e1 false) ^ s ^ (str_helper e2 false) ^ ")"
  in
  str_helper e true;;

let average_if f l =
  let folding_fn (s,c) elmt =
  match (f elmt) with
  | true -> (s+elmt,c+1)
  | false -> (s,c)
  in
  let base = (0,0) in
  let (sum,count) = List.fold_left folding_fn base l in
  if count = 0 then 0 else sum/count

let length_2 l = 
  List.fold_left (+) 0 (List.map List.length l)

let length_3 l =
  List.fold_left (+) 0 (List.map length_2 l)
