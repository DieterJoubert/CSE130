
let rec fold_right f a lst = match lst with
  | [] -> a
  | x :: xs -> f x (fold_right f a xs);;

let rec fold_left f a lst = match lst with
  | [] -> a
  | x :: xs -> fold_left f (f a x) xs


(* Define the foldr function, equivalent to the List.fold_right function
   except for the order of the arguments *)
let rec foldr f a lst = match lst with 
  | [] -> a
  | x::xs -> f x (foldr f a xs);;

(* Define the foldl function, equivalent to List.fold_left function *)
let rec foldl f a lst = match lst with
  | [] -> a
  | x::xs -> foldl f (f a x) xs;;

(* Define an exception for attempting to operate on an empty list  *)
exception Empty_list;;

(* Fold right using the first element of the list as the accumulator *)
let foldr1 f lst = match lst with 
  | [] -> raise Empty_list
  | x::xs -> foldr f x xs;;

(* Fold left using the first element of the list as the accumulator *)
let foldl1 f lst = match lst with 
  | [] -> raise Empty_list
  | x::xs -> foldl f x xs;;

(* Define the identity function *)
let id x = x;;

(* Define map in terms of foldr *)
let map f= foldr (fun x lst-> (f x)::lst) [];;

(* Define the compose function acting on two functions *)
let compose f g x = f(g x);;

(* Define the compose function acting on a list of functions*)
let composelst = foldr compose id;; 

(* Define a length function for a list, in terms of foldl since
   it is more efficient (tail-recursive) *)
let len = foldl (fun x lst -> x+1) 0;;

(* Define a list reversal function *)
let rev = foldl (fun lst x -> x::lst) [];;

(* Sum the elements in an integer list *)
let sum = foldl (fun acc x -> acc + x) 0;;

(* Multiply all elements of an integer list together *)
let prod = foldl (fun acc x -> acc * x) 1;;

(* Return the maximum element of a list *)
let max = foldl1 (fun x y -> if x>y then x else y);;

(* Return the minimum element of a list *)
let min = foldl1 (fun x y -> if x<y then x else y);;

(* Filter out values satisfying a predicate p *)
let filter p = foldr (fun x acc -> if p x then x::acc else acc) [];;

(* Position of elements in a list *)
let position p alst = rev(snd(
                       foldl (fun (ind,lst) x-> if p x then (ind+1,ind::lst)
                                 else (ind+1,lst)) (1,[]) alst));;

(* Position of elements matching the argument *)
let pos e lst = position (fun x->x=e) lst;;

(* Partition the list into two lists depending on a predicate *)
let partition p = foldr (fun x (tlst,flst)->
                   if p x then (x::tlst,flst) else (tlst,x::flst))
           ([],[]);;

(* Take the initial segment of a list while predicate is satisfied *)
let rec takeWhile p lst = match lst with 
  | [] -> []
  | x::xs -> if p x then x :: (takeWhile p xs) else [];;

(* Concatenate a list of lists together *)
let concat = foldr ( @ ) [];;

(* Zip two lists (possibly unequal lengths) into a tuple *)
let rec zip lst1 lst2 = match lst1,lst2 with
  | [],_ -> []
  | _, []-> []
  | (x::xs),(y::ys) -> (x,y) :: (zip xs ys);;

(* Unzip a list of tuples to two lists *)
let rec unzip tuplst = foldr (fun (x,y) (flst,slst)->(x::flst,y::slst))
                             ([],[]) tuplst;;

(* Wrap a value into a singleton list *)
let wrap x = [x];;

(* Get the nth element of a list, raise Not_found if list is 
   too short (indexing starts from 1) *)
exception Not_found;;

let rec nth n lst = match n,lst with 
  | _,[] -> raise Not_found 
  | 1, (x :: xs)-> x
  | k, (x :: xs)-> nth (n-1) xs;;

(* Return the last element of the list *)
let last lst = let lastind = len lst in nth lastind lst;;


(* ************ Polynomials represented as lists [a0;a1;a2;...] ************ *)
(* Evaluate poly at x: an*x**n + ... + a1*x + a0
    = a0 + x(a1 + x*(a2 + x*(a3+....))) *)
let evalpoly lst x = foldr (fun acc coeff-> acc+. x*. coeff) 0. lst;; 

(* Sum two polynomials together *)
let rec sumpoly p1 p2 = match p1,p2 with
  | [],[] -> []
  | [],(y::ys as lst) -> lst
  | (x::xs as lst),[] -> lst
  | (x::xs, y::ys)-> (x+.y)::(sumpoly xs ys);;
(* ******** End Polynomials represented as lists [a0;a1;a2;...] ************ *)

(* ******************* Test values ***************************** *)
Random.init 25632;;
let tstilst=[1;2;3];;       (* Test integer list *)
let tstflst = [1.;2.;3.];;  (* Test float list *)
let tstrnd=map (fun x->Random.int 100) [0;0;0;0;0;0;0;0;0];; (* Random int list *)
partition (fun x->x mod 2=0) tstrnd;;
max tstrnd;;
min tstrnd;;

(* Some test functions on integers *)
let sq x = x*x;;
let cube x= x*x*x;;
let sextuple = composelst [sq;cube];; (* Compose the square and cube functions *)

(* Test some of the functions defined above on an integer list *)
map sextuple tstilst;;
rev tstilst;;
len tstilst;;
sum tstilst;;
prod tstilst;;

(* Test the polynomial functions *)
map (evalpoly [0.;0.;1.]) tstflst;; (* i.e. x^2 *)
sumpoly [1.;2.;3.;4.] [3.;6.;2.];;