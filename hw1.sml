fun is_older (date1 : int*int*int, date2 : int*int*int) =
    if (#3 date1 < #3 date2 orelse
	(#3 date1 = #3 date2 andalso #2 date1 < #2 date2) orelse
	(#3 date1 = #3 date2 andalso #2 date1 = #2 date2 andalso
	 #1 date1 < #1 date2))
    then true
    else false;

fun number_in_month (dates : (int*int*int) list, month : int) =
    if null dates
    then 0
    else if (#2 (hd dates) = month)
    then 1 + number_in_month(tl dates, month)
    else 0 + number_in_month(tl dates, month);

fun number_in_months (dates : (int*int*int) list, months : int list) =
    if null months
    then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months);

fun dates_in_month (dates : (int*int*int) list, month : int) =
    if null dates
    then []
    else if (#2 (hd dates) = month)
    then hd dates :: dates_in_month(tl dates, month)
    else dates_in_month(tl dates, month);

fun dates_in_months (dates : (int*int*int) list, months : int list) =
    if null months
    then []
    else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months);

fun get_nth (l : string list, index : int) =
    if index = 1
    then hd l
    else get_nth (tl l, index - 1);

fun date_to_string (date : int*int*int) =
    let val month = ["January","Feburary","March","April","May","June","July","August","September","October","November","December"]
    in get_nth (month, #2 date) ^ "-" ^ Int.toString(#1 date) ^ "-" ^ Int.toString(#3 date)
    end;

fun number_before_reaching_sum (sum : int, l : int list) =
    if null l orelse hd l >= sum
    then 0
    else 1 + number_before_reaching_sum (sum - hd l, tl l);

fun what_month (day : int) =
    let val month_date = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in 1 + number_before_reaching_sum (day, month_date)
    end;

fun month_range (day1 : int, day2 : int) =
    if day1 > day2
    then []
    else what_month (day1) :: month_range(day1 + 1, day2);

fun find_oldest (date : int*int*int , dates : (int*int*int) list) =
    if null dates
    then date
    else if (is_older(date, hd dates))
    then find_oldest(date, tl dates)
    else find_oldest(hd dates, tl dates);

fun oldest (dates : (int*int*int) list) =
    if null dates
    then NONE
    else SOME(find_oldest(hd dates, tl dates));

fun partial_sum (l : int list, sum : int) =
    if null l
    then []
    else sum + hd l :: partial_sum(tl l, sum +hd l);

fun cumulative_sum (l : int list) =
    partial_sum(l, 0);

fun remove_duplicate (l1 : int list, l2 : int list) =
    if null l1
    then l2
    else let fun check_duplicate(n : int, l : int list) =
                 if null l
                 then false
                 else if (n = hd l)
                 then true
                 else check_duplicate(n, tl l)
	 in if (check_duplicate(hd l1, l2))
            then remove_duplicate(tl l1, l2)
	    else remove_duplicate(tl l1, hd l1 :: l2)
	 end;

fun number_in_months_challenge (dates : (int*int*int) list, months : int list) =
    let val months_nondup = remove_duplicate(months, [])
    in number_in_months (dates, months_nondup)
    end;

fun dates_in_months_challenge (dates : (int*int*int) list, months : int list) =
    let val months_nondup = remove_duplicate (months, [])
    in dates_in_months (dates, months_nondup)
    end;

fun reasonable_date (date : int*int*int) =
    if (#3 date <= 0)
    then false
    else if (#2 date < 1 orelse #2 date > 12)
    then false
    else if ((#3 date mod 4) = 0)
    then let val dates = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	 in if (#1 date < 1 orelse #1 date > List.nth(dates, #2 date - 1))
	    then false
	    else true
	 end
    else let val dates = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	 in if (#1 date < 1 orelse #1 date > List.nth(dates, #2 date - 1))
	    then false
	    else true
	 end;
