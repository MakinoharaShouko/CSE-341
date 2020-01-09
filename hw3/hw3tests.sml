val only_lowercase_test1 = only_lowercase [] = [];
val only_lowercase_test2 = only_lowercase ["Ajay", "gran", "zoo", "Breach"]
			   = ["gran", "zoo"];
val only_lowercase_test3 =
    only_lowercase ["james", "114", "---", "Land", "close"]
    = ["james", "close"];

val longest_string1_test1 = longest_string1 ["tadokoro", "kmr", "mur"]
			    = "tadokoro";
val longest_string1_test2 = longest_string1 [] = "";
val longest_string1_test3 = longest_string1 ["123456", "114514", "1919", "810"]
			    = "123456";

val longest_string2_test1 = longest_string2 ["tadokoro", "kmr", "mur"]
			    = "tadokoro";
val longest_string2_test2 = longest_string2 [] = "";
val longest_string2_test3 = longest_string2 ["123456", "114514", "1919", "810"]
			    = "114514";

val longest_string3_test1 = longest_string3 ["tadokoro", "kmr", "mur"]
			    = "tadokoro";
val longest_string3_test2 = longest_string3 [] = "";
val longest_string3_test3 = longest_string3 ["123456", "114514", "1919", "810"]
			    = "123456";

val longest_string4_test1 = longest_string4 ["tadokoro", "kmr", "mur"]
			    = "tadokoro";
val longest_string4_test2 = longest_string4 [] = "";
val longest_string4_test3 = longest_string4 ["123456", "114514", "1919", "810"]
			    = "114514";

val longest_lowercase_test1 = longest_lowercase [] = "";
val longest_lowercase_test2 = longest_lowercase ["114514", "Tadokoro", "mur", "1919810114514"]
			      = "mur";
val longest_lowercase_test3 = longest_lowercase ["kmr", "Tadokoro", "mur"]
			      = "kmr";

val caps_no_X_string_test1 = caps_no_X_string "AbxXCC" = "ABCC";
val caps_no_X_string_test2 = caps_no_X_string "" = "";
val caps_no_X_string_test3 = caps_no_X_string "114514" = "114514";

val first_answer_test1 = first_answer (fn i => (if i > 0
					        then SOME i
					        else NONE)) [0, ~5, ~6] = 7
			 handle NoAnswer => true;
val first_answer_test2 = first_answer (fn i => (if i > 0
					        then SOME i
					        else NONE)) [0, ~5, 6, ~8, 9] = 6;
val first_answer_test3 = first_answer (fn i => (if i > 0
					        then SOME i
					        else NONE)) [] = 7
			 handle NoAnswer => true;

val all_answers_test1 = all_answers (fn i => (if i > 0
					      then SOME [i]
					      else NONE)) [6, 9, 7] = SOME([7, 9, 6]);
val all_answers_test2 = all_answers (fn i => (if i > 0
					      then SOME [i]
					      else NONE)) [] = SOME([]);
val all_answers_test3 = all_answers (fn i => (if i > 0
					      then SOME [i]
					      else NONE)) [0, ~5, ~6] = NONE;

val count_wildcards_test1 = count_wildcards (WildcardP) = 1;
val count_wildcards_test2 = count_wildcards (UnitP) = 0;
val count_wildcards_test3 =
    count_wildcards (TupleP [WildcardP, ConstantP 6, ConstructorP ("wild", WildcardP), TupleP [UnitP, WildcardP]]) = 3;

val count_wild_and_variable_lengths_test1 =
    count_wild_and_variable_lengths WildcardP = 1;
val count_wild_and_variable_lengths_test2 =
    count_wild_and_variable_lengths (VariableP "114514") = 6;
val count_wild_and_variable_lengths_test3 =
    count_wild_and_variable_lengths (TupleP [UnitP, VariableP "MUR", WildcardP, ConstructorP ("810", ConstantP 810), TupleP [WildcardP, VariableP "kmr"]]) = 8;

val count_a_var_test1 = count_a_var ("114514", UnitP) = 0;
val count_a_var_test2 = count_a_var ("810", VariableP "810") = 1;
val count_a_var_test3 =
    count_a_var ("kmr", TupleP [WildcardP, VariableP "kmr", TupleP [VariableP "tadokoro", ConstructorP ("kmr", VariableP "kmr")]]) = 2;

val check_pat_test1 = check_pat WildcardP = true;
val check_pat_test2 = check_pat (TupleP [TupleP [VariableP "MUR", VariableP "kmr", UnitP], WildcardP, ConstantP 114, VariableP "mur"]) = true;
val check_pat_test3 = check_pat (TupleP [TupleP [VariableP "mur", VariableP "kmr", UnitP], WildcardP, ConstantP 114, VariableP "mur"]) = false;
val check_pat_test4 = check_pat (ConstructorP ("t4", TupleP [VariableP "mur", UnitP, ConstantP 514, VariableP "mur"])) = false;

val match_test1 = match (Constant 10, WildcardP) = SOME [];
val match_test2 = match (Tuple [Unit, Constant 810, Unit], TupleP [VariableP "test", WildcardP, UnitP]) = SOME [("test", Unit)];
val match_test3 = match (Tuple [Constructor ("114", Unit), Constructor ("514", Tuple [Unit, Constant 17, Constant 810])],
			 TupleP [WildcardP, ConstructorP ("514", TupleP [UnitP, VariableP "kmr", ConstantP 810])]) = SOME [("kmr", Constant 17)];
val match_test4 = match (Tuple [Unit, Constant 114, Constructor ("mur", Tuple [Constant 810, Constructor ("tadokoro", Constant 810)])],
			 TupleP [UnitP, VariableP "kmr", ConstructorP ("mur", TupleP [WildcardP])]) = NONE;

val first_match_test1 = first_match Unit [TupleP[WildcardP, VariableP "114514"], ConstantP 810, UnitP, VariableP "mur"] = SOME [];
val first_match_test2 = first_match (Tuple [Unit, Constant 114514])
				     [TupleP [WildcardP], TupleP [VariableP "mur", ConstantP 114514], WildcardP, UnitP] = SOME[("mur", Unit)];
val first_match_test3 = first_match (Constructor ("kmr", Tuple [Unit, Tuple[Constant 114]]))
				    [ConstructorP ("KMR", TupleP [WildcardP]), UnitP, ConstructorP ("kmr", TupleP[WildcardP, TupleP[VariableP "mur", WildcardP]])]
			= NONE;
