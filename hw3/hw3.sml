(* CSE341, HW3 Provided Code *)

exception NoAnswer

datatype pattern = WildcardP
                 | VariableP of string
                 | UnitP
                 | ConstantP of int
                 | ConstructorP of string * pattern
                 | TupleP of pattern list

datatype valu = Constant of int
              | Unit
              | Constructor of string * valu
              | Tuple of valu list

fun g f1 f2 p =
    let 
        val r = g f1 f2 
    in
        case p of
            WildcardP         => f1 ()
          | VariableP x       => f2 x
          | ConstructorP(_,p) => r p
          | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
          | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = AnythingT
             | UnitT
             | IntT
             | TupleT of typ list
             | DatatypeT of string

(**** you can put all your code here ****)

(* Question 1
   filters strings starting with lower case letters from a list *)
fun only_lowercase (ls : string list) =
    List.filter(fn s => Char.isLower(String.sub(s, 0))) ls;

(* Question 2
   find the longest string from a string list
   if there is a tie, returns the string appeared first *)
fun longest_string1 (ls : string list) =
    List.foldl(fn (s1, s2) => if String.size(s1) > String.size(s2)
			      then s1
			      else s2) "" ls;

(* Question 3
   find the longest string from a string list
   if there is a tie, returns the string appeared last *)
fun longest_string2 (ls : string list) =
    List.foldl(fn (s1, s2) => if String.size(s1) >= String.size(s2)
			      then s1
			      else s2) "" ls;

(* Question 4 *)
(* Helper function; takes in a function and a string list and
   calls the function on the length of each string to determine
   the result *)
val longest_string_helper = fn f => fn ls => List.foldl(fn (s1, s2) => if f (String.size(s1), String.size(s2))
								       then s1
								       else s2) "" ls;

(* find the longest string from a string list
   if there is a tie, returns the string appeared first *)
val longest_string3 = longest_string_helper (fn (i1, i2) => i1 > i2);
(* find the longest string from a string list
   if there is a tie, returns the string appeared last *)
val longest_string4 = longest_string_helper (fn (i1, i2) => i1 >= i2);

(* question 5
   find the longest string that begins with a lower case character from the string list *)
val longest_lowercase = longest_string1 o only_lowercase;

(* question 6
   transfer all letters from a string to capital and remove all "x" and "x" *)
val caps_no_X_string = String.implode o (List.filter (fn c => c <> #"X")) o (List.map Char.toUpper) o String.explode;

(* Question 7
   Takes in a function and a list, calls the function on the elements
   from the list. If the result is SOME v, returns v. If there is no
   such result, raise NoAnswer exception *)
fun first_answer f a =
    case a of
	[] => raise NoAnswer
      | head :: a' => case f(head) of
			  NONE => first_answer f a'
			| SOME v => v;

(* Question 8
   Takes in a function and a list, calls the function
   on the elements. If any element generates NONE, then output
   NONE, else outputs SOME([listn, listn - 1, ... , list0]) *)
fun all_answers f a =
    let fun all_answers_help f a acc =
	    case a of
		[] => SOME acc
	      | head :: a' => case f head of
				  NONE => NONE
				| SOME v => all_answers_help f a' (v @ acc)
    in all_answers_help f a []
    end;

(* Question 9 
   function g takes in two functions and a pattern
   if the pattern is a wild card, calls f1 on void
   if the pattern is VariableP of a string, calls f2 on string
   if the pattern is ConstrcutorP of a tupe of string and another pattern,
   calls g again on the two functions and the pattern within ConstructorP
   if the pattern is TupleP of a pattern list, uses foldl to call g with
   the two functions and each patter element from the list and sums the
   results of these function calls
   otherwise returns 0*)

(* Count the number of wildcards within the pattern *)
fun count_wildcards p = g (fn p => 1) (fn s => 0) p;

(* Takes a pattern and returns the sum of the number
   of wildcards and the total length of strings in the variables *)
fun count_wild_and_variable_lengths p =
    g (fn p => 1) (String.size) p;

(* Takes a string and a pattern
   returns the number of times the string appeas as a variable in the pattern*)
fun count_a_var (s, p) =
    g (fn p => 0) (fn str => if s = str then 1 else 0) p;

(* Question 10 
   takes a pattern and checks if there is any repeated variable inside the pattern *)
fun check_pat p =
    case p of
	TupleP ps => let fun all_names ps =
			     (* returns a list of strings appear in all variable within the pattern list*)
			     List.foldl (fn (p, l) => case p of
							  VariableP s => s :: l
							| TupleP pl => all_names pl @ l
							| _ => l) [] ps
		     in let fun check_repeats ls =
				(* checks if the string list has repeated elements or not *)
				case ls of
				    [] => true
				  | head :: ls' => (not (List.exists (fn s => s = head) ls')) andalso check_repeats ls'
			in (check_repeats o all_names) ps
			end
		     end
      | ConstructorP (_, p') => check_pat p'
      | _ => true; 

(* Question 11
   takes in a valu and a pattern and matches them
   if they match retuns the option of the matching result
   else returns NONE *)
fun match (v, p) =
    case (v, p) of
	(_, WildcardP) => SOME []
      | (_, VariableP x) => SOME [(x, v)]
      | (Unit, UnitP) => SOME []
      | (Constant i1, ConstantP i2) => if i1 = i2
				       then SOME []
				       else NONE
      | (Constructor (s1, v'), ConstructorP (s2, p')) => if s1 = s2
							 then match (v', p')
							 else NONE
      | (Tuple vs, TupleP ps) => if List.length(ps) = List.length(vs)
				 then all_answers match (ListPair.zip(vs, ps))
				 else NONE
      | _ => NONE;

(* Question 12
   takes a valu and a list of patterns
   returns the option for the first matching
   if there is no matching returns NONE *)
fun first_match v ps =
    SOME (first_answer (fn p => match(v, p)) ps)
    handle NoAnswer => NONE;
