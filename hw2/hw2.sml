(* CSE 341, HW2 Provided Code *)

(* main datatype definition we will use throughout the assignment *)
datatype json =
         Num of real (* real is what SML calls floating point numbers *)
       | String of string
       | False
       | True
       | Null
       | Array of json list
       | Object of (string * json) list

(* some examples of values of type json *)
val json_pi    = Num 3.14159
val json_hello = String "hello"
val json_false = False
val json_array = Array [Num 1.0, String "world", Null]
val json_obj   = Object [("foo", json_pi), ("bar", json_array), ("ok", True)]

(* some provided one-liners that use the standard library and/or some features
   we have not learned yet. (Only) the challenge problem will need more
   standard-library functions. *)

(* dedup : string list -> string list -- it removes duplicates *)
fun dedup xs = ListMergeSort.uniqueSort String.compare xs

(* strcmp : string * string -> order compares strings alphabetically
   where datatype order = LESS | EQUAL | GREATER *)
val strcmp = String.compare                                        
                        
(* convert an int to a real *)
val int_to_real = Real.fromInt

(* absolute value of a real *)
val real_abs = Real.abs

(* convert a real to a string *)
val real_to_string = Real.toString

(* return true if a real is negative : real -> bool *)
val real_is_negative = Real.signBit

(* We now load 3 files with police data represented as values of type json.
   Each file binds one variable: small_incident_reports (10 reports), 
   medium_incident_reports (100 reports), and large_incident_reports 
   (1000 reports) respectively.

   However, the large file is commented out for now because it will take 
   about 15 seconds to load, which is too long while you are debugging
   earlier problems.  In string format, we have ~10000 records -- if you
   do the challenge problem, you will be able to read in all 10000 quickly --
   it's the "trick" of giving you large SML values that is slow.
*)

(* Make SML print a little less while we load a bunch of data. *)
       ; (* this semicolon is important -- it ends the previous binding *)
Control.Print.printDepth := 3;
Control.Print.printLength := 3;

use "parsed_small_police.sml";
use "parsed_medium_police.sml";

(* uncomment when you are ready to do the problems needing the large report*)
use "parsed_large_police.sml"; 

val large_incident_reports_list =
    case large_incident_reports of
        Array js => js
      | _ => raise (Fail "expected large_incident_reports to be an array")

(* Now make SML print more again so that we can see what we're working with. *)
; Control.Print.printDepth := 20;
Control.Print.printLength := 20;

(**** PUT PROBLEMS 1-8 HERE ****)
(* Question 1
   takes in an integer and makes a json array consisted by json objects
   with first field "n" holding i.0, (i - 1).0 ... 1.0 and second field "b"
   holding True *)
fun make_silly_json (i : int) =
    let fun make_json_list (n : int)  =
	    if n = 0
	    then []
	    else Object [("n", Num (int_to_real(n))), ("b", True)] :: make_json_list(n - 1)
    in Array (make_json_list(i))
    end;

(* Question 2
   takes in a list consists of pairs and another value
   if the first value in a pair is equal to the passed in value,
   returns the option of the second field
   if no value from the list meets the requirement, returns NONE*)
fun assoc (k, xs) =
    case xs of
	[] => NONE
      | x :: xs' => case x of
			(k1, v1) => if k1 = k
				    then SOME(v1)
				    else assoc(k, xs');

(* Question 3
   takes in a json and a string
   if the json is a json object and has a field
   with the same name as the passed in string
   returns the option of the value
   if the passed in json is not a json object or
   does not contain the string as field name,
   returns NONE *)
fun dot (j : json, f : string) =
    case j of
	Object (l) => assoc(f, l)
     |  _=> NONE;

(* Question 4
   takes in a json
   if the json is a json object
   returns a list holding all of its field names
   else returns an empty list *)
fun one_fields (j : json) =
    case j of
	Object (l) => let fun one_fields_help (l, strl) =
			      case l of
				  [] => strl
				| head :: l' => case head of
						    (str, v) => one_fields_help(l', str :: strl)
		      in one_fields_help(l, [])
		      end
      | _ => []; 

(* Question 5
   takes in a string list and checks if there is repetition *)
fun no_repeats (strl : string list) =
    length(strl) = length(dedup(strl));

(* help function; assume input is not a json array *)
fun json_no_repeat_field (j : json) =
    let val strl = one_fields(j)
    in if no_repeats(strl)
       then case j of
		(* checks the jsons within nested json object *)
		Object l => let fun object_no_repeat_field (ls : (string * json) list) =
				    case ls of
					[] => true
				      | head :: ls' => case head of
							   (s, j) => json_no_repeat_field(j) andalso
								     object_no_repeat_field (ls')
			    in object_no_repeat_field(l)
			    end
	      | _ => true
       else false
    end;

(* Question 6
   takes in a json and checks if there is any object
   with repeated field names inside the json *)
fun recursive_no_field_repeats (j : json) =
    case j of
	Array (jl) => let fun recursive_no_repeats_help (l : json list) = case l of
										[] => true
									      | head :: l' => (json_no_repeat_field(head)) andalso
											      recursive_no_repeats_help (l')
		      in recursive_no_repeats_help(jl)
		      end
      | _ => json_no_repeat_field(j);

(* Question 7
   takes in a string list and an error expression
   if the string list is not sorted, raise the error expression
   else, counts the occurences of each string inside the string and
   stores that in the returned string * int list *)
fun count_occurrences (l : string list, err : exn) =
    (*  check if the string list is sorted *)
    let	fun check_sort (ls : string list) =
	    case ls of
		[] => true
	      | prev :: ls' =>  case ls' of
				    [] => true
				  | cur :: ls'' => not (strcmp(prev, cur) = GREATER) andalso check_sort(ls')
    in if not (check_sort(l))
       then raise err
       else let
	   fun count (str : string, strl : string list, value : int) =
	       case strl of
		   [] => (str, value) :: []
		 | head :: strl' => if (head = str)
				    then count(str, strl', value + 1)
				    else (str, value) :: count(head, strl', 1)
       in case l of
	      [] => []
	    | cur :: l' => count(cur, l', 1)
       end
    end;

(* Question 8
   takes in a string and a json list and returns a string list
   for any json object in the list, if it has a field name equal to
   the string and has a string value, put the value in the returned list *)
fun string_values_for_field (str : string, jl : json list) =
    case jl of
	[] => []
      | head :: jl' => case head of
			   (* object, checks field names *)
			   Object l => let val temp = assoc(str, l)
				       in case temp of
					      SOME(String s) => s :: string_values_for_field(str, jl')
					    (* no match found *)
					    | _ => string_values_for_field(str, jl')
				       end
			 (* No field names to check *)
			 | _ => string_values_for_field(str, jl');

(* histogram and historgram_for_field are provided, but they use your 
   count_occurrences and string_values_for_field, so uncomment them 
   after doing earlier problems *)

(* histogram_for_field takes a field name f and a list of objects js and 
   returns counts for how often a string is the contents of f in js. *)

exception SortIsBroken

fun histogram (xs : string list) : (string * int) list =
  let
    fun compare_strings (s1 : string, s2 : string) : bool = s1 > s2

    val sorted_xs = ListMergeSort.sort compare_strings xs
    val counts = count_occurrences (sorted_xs,SortIsBroken)

    fun compare_counts ((s1 : string, n1 : int), (s2 : string, n2 : int)) : bool =
      n1 < n2 orelse (n1 = n2 andalso s1 < s2)
  in
    ListMergeSort.sort compare_counts counts
  end

fun histogram_for_field (f,js) =
    histogram (string_values_for_field (f, js))
	      

(**** PUT PROBLEMS 9-11 HERE ****)
(* Question 9
   takes in 2 strings and a json list and returns another json list
   if a element in the list has the field name same as the first string
   and value the same as the second string, put the element into the returned list *)
fun filter_field_value (s1 : string, s2 : string, jl : json list) =
    case jl of
	[] => []
      | head :: jl' => case head of
			   (* object, checks field names*)
			   Object l => let val temp = assoc(s1, l)
				       in case temp of
					      (* field name matches, checks value *)
					      SOME(String s) => if (s = s2)
								then head :: filter_field_value(s1, s2, jl')
								(* value doesn't match*)
								else filter_field_value(s1, s2, jl')
					    (* no match field name found *)
					    | _ => filter_field_value(s1, s2, jl')
				       end
			 (* no field name to check *)
			 | _ => filter_field_value(s1, s2, jl');

(* Question 10
   bind to variable large_event_clearance_description_histogram a histogram of
   the objects in large_incident_reports_list based on the
   "event_clearance_description" field *)
val large_event_clearance_description_histogram =
    histogram_for_field ("event_clearance_description",
			 large_incident_reports_list);

(* Question 11
   bind to the variable large_hundred_block_location_histogram a histogram
   of the objects in large_incident_reports_list based on the
   "hundred_block_location" field *)
val large_hundred_block_location_histogram =
    histogram_for_field ("hundred_block_location",
			 large_incident_reports_list);

;Control.Print.printDepth := 3;
Control.Print.printLength := 3;

(**** PUT PROBLEMS 12-15 HERE ****)
(* Question 12
   bind to the variable forty_third_and_the_ave_reports a json list containing the elements of
   large_incident_reports_list whose "hundred_block_location" field contains the JSON string
   "43XX BLOCK OF UNIVERSITY WAY NE" *)
val forty_third_and_the_ave_reports =
    filter_field_value ("hundred_block_location",
			"43XX BLOCK OF UNIVERSITY WAY NE",
			large_incident_reports_list);

(* Question 13
   Bind to the variable forty_third_and_the_ave_event_clearance_description_histogram a histogram
   based on the "event_clearance_description" field, but considering only
   the objects whose "hundred_block_location" field contains "43XX BLOCK OF UNIVERSITY WAY NE" *)
val forty_third_and_the_ave_event_clearance_description_histogram =
    histogram_for_field ("event_clearance_description",
			 forty_third_and_the_ave_reports);

(* Question 14
   Bind to the variable nineteenth_and_forty_fifth_reports a json list containing the elements
   of large_incident_reports_list whose "hundred_block_location" field contains the JSON string
   "45XX BLOCK OF 19TH AVE NE"*)
val nineteenth_and_forty_fifth_reports =
    filter_field_value ("hundred_block_location",
			"45XX BLOCK OF 19TH AVE NE",
			large_incident_reports_list);

(* Question 15
   Bind to the variable nineteenth_and_forty_fifth_event_clearance_description_histogram a
   histogram based on the "event_clearance_description" field, but considering
   only the objects whose "hundred_block_location" field contains "45XX BLOCK OF 19TH AVE NE" *)
val nineteenth_and_forty_fifth_event_clearance_description_histogram =
    histogram_for_field ("event_clearance_description",
			 nineteenth_and_forty_fifth_reports);

;Control.Print.printDepth := 20;
Control.Print.printLength := 20;

(**** PUT PROBLEMS 16-19 HERE ****)
(* Question 16
   takes in a string and a string list,
   concatenates all the strings from the list with the passed in string
   between every two of them and returns the resultant string *)
fun concat_with (separator : string, l : string list) =
    let fun concat_help (separator : string, l : string list, res : string) =
	    case l of
		[] => res
	      | head :: [] => res ^ head
	      | head :: l' => concat_help(separator, l', res ^ head ^ separator)
    in concat_help (separator, l, "")
    end;

(* Question 17
   puts double quotes at the two ends of the passed in string
   and returns the new string *)
fun quote_string (s : string) =
    "\"" ^ s ^ "\"";

(* Question 18
   converts a real number into a string
   if the real number is negative
   replace the "~" at the start of the string with "-"*)
fun real_to_string_for_json (n : real) =
    if (n > 0.0)
    then real_to_string (n)
    else "-" ^ real_to_string(real_abs(n));

(* Question 19
   takes in a json and prints out its string representation *)
fun json_to_string (j : json) =
    case j of
	Num n => real_to_string_for_json n
      | String s => quote_string s
      | False => "false"
      | True => "true"
      | Null => "null"
      | Array jl => let fun json_list_to_string (jl : json list) =
			    case jl of [] => []
				     | head :: jl' => json_to_string(head) :: json_list_to_string(jl')
		    in "[" ^ concat_with(", ", json_list_to_string(jl)) ^ "]"
		    end
      | Object l => let fun object_to_string (l : (string * json) list) =
			    case l of
				[] => []
			      | head :: l' => case head of
						  (str, j) => quote_string(str) ^ " : " ^ json_to_string(j) :: object_to_string(l')
		    in "{" ^ concat_with(", ", object_to_string(l)) ^ "}"
		    end;
(* For CHALLENGE PROBLEMS, see hw2challenge.sml *)

