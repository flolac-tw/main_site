(*  *)

open List

#use "Utils.ml"

let let_to_int c = int_of_char c - int_of_char 'a'

let int_to_let i = char_of_int (int_of_char 'a' + i)

let is_lower c = let i = int_of_char c
		 in int_of_char 'a' <= i && i <= int_of_char 'z'

let shift n c = 
     if is_lower c then int_to_let ((let_to_int c + n + 26) mod 26)
     else c

(* ** Task 1 ** Define
    encode : int -> char list -> char list
    encode_str : int -> string -> string
    that perform Ceaser ciphering. *)

let encode n = map (shift n) 

let encode_str n = implode << encode n << explode

(* A frequency table of English alphabets. *)

let table = [8.2; 1.5; 2.8; 4.3; 12.7; 2.2; 2.0; 6.1; 7.0; 0.2; 0.8; 4.0; 2.4;
             6.7; 7.5; 1.9; 0.1; 6.0; 6.3; 9.1; 2.8; 1.0; 2.4; 0.2; 2.0; 0.1]

let alphas = ['a';'b';'c';'d';'e';'f';'g';'h';'i';'j';'k';'l';'m';'n';'o';
              'p';'q';'r';'s';'t';'u';'v';'w';'x';'y';'z']

(* Define functions

    count_eq : 'a -> 'a list -> int
    count_lower : char list -> int

   where count_eq x xs yields the number of occurrences of x in xs, 
   while count_lower xs yields the number of lowercase letters in xs.
   You may find their definitions very similar --- both are instances
   of a function 

     count : ('a -> bool) -> 'a list -> int

   such that count p xs counts the number of elements in xs that
   satisfies predicate p.
 *)

let count p = length << filter p

let count_eq x = count ((==) x)

let count_lower = count is_lower

(* Define function

     histo : char list -> float list

   that computes the percentage of each lowercase character in the input
   list. Note that the denominator should be the number of lowercase
   letters, rather than the length of the entire string.

   For example, 
     histo (explode "aabc");;
   evaluates to
     [50.; 25.; 25.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.;
      0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.]
   because 50% of the characters are 'a', 25% are 'b', etc.

   Hint: you may find the list alphas defined above useful.
*)

let histo xs = let n = float (count_lower xs)
	       in map (fun x -> (float (count_eq x xs) /. n) *. 100.0) alphas

(*
   The function call
     chisqr es os
   computes the similarity between es and os.
   The smaller the outcome, the more similar os is to es. 

   ** Note: the order matters: es is the "model", while os is a particular
   table to compare against es. 
*)

let chisqr es os = 
      sumFloat (combineWith (fun e o -> (o -. e) ** 2.0 /. e) es os)

(* The functions rotate, index, positions, etc., should have been
   defined in Exercise 1 *)

let rotate n xs = drop n xs @ take n xs

let index xs = combine (fromTo 0 (length xs)) xs

let positions x xs = map fst (filter (fun (_,y) -> y == x) (index xs))

(*
  Define 

    crack : char list -> int

  that takes an supposedly encoded string and compute the most possible
  offset. You will need plenty of helper functions and values
  including table (the average histogram of English alphabets),
  rotate, index, map, filter, etc. You may also find it useful to
  have functions including:

    . minimum : 'a list -> 'a, that returns the minimum element of a given list;
    . hd : 'a list -> 'a, that returns the first (left-most) element of a list.
    . chisqr, defined above.

  One possible way to compute crack xs is to
    . compute the histogram of the input list. Call the result freqs.
    . Compute all the 26 rotations of freqs.
    . Find, among all the rotations of freqs, the position of the element
      that is the most similar to table.
  
  Hint: how do we find the position of the minimum element in a list, say,
  ys? Of course, it is the position of the left-most element that equals 
  minumum ys.

*)

let crack xs = let freqs = histo xs in
	       let chitab = map (fun n -> chisqr table (rotate n freqs)) (fromTo 0 26) in
	       hd (positions (minimum chitab) chitab)
 
let decode xs = encode (0 - crack xs) xs

let decode_str = implode << decode << explode
