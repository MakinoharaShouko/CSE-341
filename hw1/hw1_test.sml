val older_test1 = is_older ((12,6,1988),(17,12,2000)) = true;
val older_test2 = is_older ((4,5,1999),(4,4,1999)) = false;
val older_test3 = is_older ((1,1,2005),(1,1,2005)) = false;
val older_test4 = is_older ((1,1,2005), (2,1,2005)) = true;
							 
val number_in_month_test1 = number_in_month ([(19,6,2015),(25,6,1997),(30,3,2004),(20,5,2007)],6) = 2;
val number_in_month_test2 = number_in_month ([(1,5,1998),(4,6,1989),(7,7,1999)],1) = 0;
val number_in_month_test3 = number_in_month ([], 7) = 0;

val number_in_months_test1 = number_in_months ([],[]) = 0;
val number_in_months_test2 = number_in_months ([(1,2,2001),(15,3,2007),(4,2,2009),(31,8,2019)],[2,3,6]) = 3;

val dates_in_month_test1 = dates_in_month ([],12) = [];
val dates_in_month_test2 = dates_in_month ([(19,6,2015),(25,6,1997),(30,3,2004),(20,5,2007)],6) = [(19,6,2015),(25,6,1997)];

val dates_in_months_test1 = dates_in_months ([],[]) = [];
val dates_in_months_test2 = dates_in_months ([(1,2,2001),(15,3,2007),(4,2,2009),(31,8,2019)],[2,3,6]) = [(1,2,2001),(4,2,2009),(15,3,2007)];

val get_nth_test1 = get_nth (["Hello","World"],1) = "Hello";
val get_nth_test2 = get_nth (["I","am","fine","and","you"],3) = "fine";
						     
val date_to_string_test1 = date_to_string((1,2,2002)) = "Feburary-1-2002";
val date_to_string_test2 = date_to_string((30,6,1987)) = "June-30-1987";

val number_before_reaching_sum_test1 = number_before_reaching_sum (10,[1,1,4,5,1,4]) = 3;
val number_before_reaching_sum_test2 = number_before_reaching_sum (23,[1,9,1,9,8,1,0]) = 4;
val number_before_reaching_sum_test3 = number_before_reaching_sum (15,[1,4,6,4,5,7,8]) = 3;

val what_month_test1 = what_month (31) = 1;
val what_month_test2 = what_month (33) = 2;
val what_month_test3 = what_month (365) = 12;

val month_range_test1 = month_range(1,4) = [1,1,1,1];
val month_range_test2 = month_range(30,34) = [1,1,2,2,2];
val month_range_test3 = month_range(167,34) = [];

val oldest_test1 = oldest([]) = NONE;
val oldest_test2 = oldest[(1,12,2010),(11,11,2010),(1,11,2010),(11,2,1787),(1,3,2009)] = SOME((11,2,1787));

val cumulative_sum_test1 = cumulative_sum([1,2,3]) = [1,3,6];
val cumulative_sum_test2 = cumulative_sum([]) = [];
val cumulative_sum_test3 = cumulative_sum([11,23,25,56,34,15]) = [11,34,59,115,149,164];

val remove_duplicate_test1 = remove_duplicate([1,1,4,5,1,4],[]) = [5,4,1];
val remove_duplicate_test2 = remove_duplicate([],[]) = [];
val remove_duplicate_test3 = remove_duplicate([1,2,3,4,5],[]) = [5,4,3,2,1];

val number_in_months_challenge_test1 = number_in_months_challenge ([],[]) = 0;
val number_in_months_challenge_test2 = number_in_months_challenge ([(1,2,2001),(15,3,2007),(4,2,2009),(31,8,2019)],[2,3,6]) = 3;

val dates_in_months_challenge_test1 = dates_in_months_challenge ([],[]) = [];
val dates_in_months_challenge_test2 = dates_in_months_challenge ([(1,2,2001),(15,3,2007),(4,2,2009),(31,8,2019)],[2,3,6]) = [(15,3,2007),(1,2,2001),(4,2,2009)];

val reasonable_date_test1 = reasonable_date((29,2,2004)) = true;
val reasonable_date_test2 = reasonable_date((29,2,2007)) = false;
val reasonable_date_test3 = reasonable_date((0,5,1199)) = false;
val reasonable_date_test4 = reasonable_date((19,~6,2014)) = false;
val reasonable_date_test5 = reasonable_date((1,1,0)) = false;
val reasonable_date_test6 = reasonable_date((114,515,1919)) = false;
