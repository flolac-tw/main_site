(*

2007 Formosan Summer School on Logic, Language, and Computation

Supplementary OCaml Code for 
  Introduction to Functional Program Derivation

Shin-Cheng Mu

*)

open List

let rec fact = function
    | 0 -> 1
    | n -> n * fact(n - 1)
    
let rec foldr f e lst = match lst with
  | [] -> e
  | x :: xs -> f x (foldr f e xs)

let sum = foldr (+) 0

let id x = x

(* Function composition *)

let (<<) f g = fun x -> f (g x)

(* iTree and eTree as defined in the lecture. *)

type 'a iTree = Null | Node of 'a * 'a iTree * 'a iTree
type 'a eTree = Tip of 'a | Bin of 'a eTree * 'a eTree

let square n = n * n

let rec sumsq lst = match lst with
   | [] -> 0
   | x :: xs -> square x + sumsq xs
   
let reverse = rev
let rcat xs ys = reverse xs @ ys

let rec ssp lst n = match lst with
   | [] -> n
   | x :: xs -> ssp xs (square x + n)
   
let rec steep lst = match lst with
   | [] -> true
   | x :: xs -> steep xs && x > sum xs
   
let rec steepsum lst = match lst with
   | [] -> (true, 0)
   | x :: xs -> let (b,y) = steepsum xs
                in (b && x > y, x + y)
                
let bmax a b = if a >= b then a else b

let max = foldr bmax (-32767)

let prod = foldr (fun x y -> x * y) 1

let annd = foldr (fun x y -> x && y) true

let rec takeWhile p lst = match lst with
   | [] -> []
   | x :: xs -> if p x then x :: takeWhile p xs
                 else []
                 
let rec dropWhile p lst = match lst with
   | [] -> []
   | x :: xs -> if p x then dropWhile p xs
                  else x::xs

let rec inits lst = match lst with
   | [] -> [[]]
   | x :: xs -> [] :: (map (fun ys -> x :: ys) (inits xs))

let rec tails lst = match lst with
   | [] -> [[]]
   | x :: xs -> let (ys::xss) = tails xs
                in (x::ys) :: ys :: xss
                
let segs = concat << map inits << tails

let mss = 
     let step x (m,y) = (bmax (bmax 0 (x+y)) m,
                         bmax 0 (x+y))
     in fst << foldr step (0,0)
     
let rec foldiT f e tr = match tr with
   | Null -> e
   | Node (a,t,u) -> f a (foldiT f e t) (foldiT f e u)
   
let rec foldeT f g tr = match tr with
   | Tip x -> g x
   | Bin (t,u) -> f (foldeT f g t) (foldeT f g u)
   
let sizeiTree = foldiT (fun x m n -> m + n + 1) 0

let sumeTree = foldeT (+) id

let flatteniT = foldiT (fun x xs ys -> xs @[x]@ ys) []
let flatteneT = foldeT (@) (fun x -> [x])

let rec unfoldr p f s = 
   if p s then [] else
     let (x,s') = f s 
     in x :: unfoldr p f s'
     
let split (f,g) a = (f a, g a)

let fromto m = unfoldr (fun n -> n >= m) (split (id, fun n -> n+1))

let null lst = match lst with
   | [] -> true
   | xs -> false

let const a b = a

let tailsp = unfoldr null (split (id, tl))

let from = unfoldr (const false) (split (id, fun n -> n + 1))

let rec iterate f x = x :: iterate f (f x)

let merge = 
   let null2 (xs,ys) = null xs && null ys and
       mrg (lst1, lst2) = match (lst1, lst2) with
          | ([], y::ys) -> (y, ([], ys))
          | (x::xs, []) -> (x, (xs, []))
          | (x::xs, y::ys) -> if x <= y then (x, (xs, y::ys))
                                        else (y, (x::xs, ys))
   in unfoldr null2 mrg
  
let rec unfoldiT p f s =
   if p s then Null
    else let (x,s1,s2) = f s
         in Node (x, unfoldiT p f s1,
                     unfoldiT p f s2)
                
let rec unfoldeT p f g s =
   if p s then Tip (g s)
     else let (s1,s2) = f s
          in Bin (unfoldeT p f g s1,
                  unfoldeT p f g s2)
                 
let single lst = match lst with
    | [x] -> true
    | xs -> false
    
let half =
   let step x (xs,ys) = (ys, x::xs) 
   in foldr step ([],[])
   
let unflatteneT = unfoldeT single half id

let msort = foldeT (fun xs ys -> merge (xs,ys)) id << unflatteneT

let partition (x::xs) = (x, filter (fun y -> y <= x) xs,
                            filter (fun y -> y > x) xs)
                            
let qsort = flatteniT << unfoldiT null partition

let isort = 
   let insert x xs = takeWhile (fun y -> y < x) xs @ [x] @
                     dropWhile (fun y -> y < x) xs
   in foldr insert []

let rec hyloiT f e p g s =
   if p s then e
     else let (x,s1,s2) = g s
          in f x (hyloiT f e p g s1)
                 (hyloiT f e p g s2)
                 
                 
(* exercises *)

let rec descend n = match n with
   | 0 -> []
   | n -> n :: descend (n-1)
   
let rec mapiTree f tr = match tr with
   | Null -> Null
   | Node (a,t,u) -> Node (f a, mapiTree f t, mapiTree f u)
   
let rec swapiTree tr = match tr with
   | Null -> Null
   | Node (a,t,u) -> Node (a, swapiTree u, swapiTree t)