
let (<<) f g x = f (g x)

let rec sumFloat = function
  | [] -> 0.0
  | x :: xs -> x +. sumFloat xs

let rec combineWith f xs ys = match xs, ys with
  | [], [] -> []
  | x :: xs, y:: ys -> f x y :: combineWith f xs ys

let rec take n xs = match n with
  | 0 -> []
  | n -> match xs with
    | [] -> []
    | x :: xs -> x :: take (n-1) xs

let rec drop n xs = match n with
  | 0 -> xs
  | n -> match xs with
    | [] -> []
    | x :: xs -> drop (n-1) xs

let rec fromTo m n =
  if m < n 
  then m :: fromTo (m+1) n
  else []

let rec minimum = function 
  | [x] -> x
  | x :: xs -> min x (minimum xs)

let rec is_even = function
  | 0 -> true
  | n -> is_odd (n-1)
and is_odd = function 
  | 0 -> false
  | n -> is_even (n-1)

(* string <-> list *)

let explode s =
  let rec expl i l =
    if i < 0 then l else
    expl (i - 1) (s.[i] :: l) in
  expl (String.length s - 1) [];;

let implode l =
  let result = String.create (List.length l) in
  let rec imp i = function
  | [] -> result
  | c :: l -> result.[i] <- c; imp (i + 1) l in
  imp 0 l;;

(* undefined *)

let rec undefined () = undefined ()
