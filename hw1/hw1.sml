(* CSE341 HW1*)
(* author : Siyadong Xiong (sx225@cornell.edu) *)

fun is_older (date1 : int*int*int, date2 : int*int*int) = 
  #1 date1 < #1 date2 orelse
  (#1 date1 = #1 date2 andalso 
    (#2 date1 < #2 date2 orelse 
      (#2 date1 = #2 date2 andalso #3 date1 < #3 date2)))

fun number_in_month (dates : (int*int*int) list, month : int) = 
  if null dates
  then 0
  else
    let val tl_ans = number_in_month (tl dates, month)
    in
      if #2 (hd dates) = month
      then 1 + tl_ans
      else tl_ans
    end

fun number_in_months (dates : (int*int*int) list, months : int list) = 
  if null months
  then 0
  else number_in_month(dates, hd months) + number_in_months(dates, tl months)

fun dates_in_month (dates : (int*int*int) list, month : int) = 
(* return a list of dates*)
  if null dates
  then []
  else
    let val tl_ans = dates_in_month(tl dates, month)
    in
      if #2 (hd dates) = month
      then (hd dates) :: tl_ans
      else tl_ans
    end

fun dates_in_months (dates : (int*int*int) list, months : int list) = 
  if null months
  then []
  else dates_in_month(dates, hd months) @ (dates_in_months(dates, tl months))

fun get_nth (strs : string list, n : int) = 
  if n = 1
  then hd strs
  else get_nth(tl strs, n - 1)

fun date_to_string (date : int*int*int) = 
  let
    val month_string_representation = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
  in
    get_nth(month_string_representation, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
  end

fun number_before_reaching_sum (sum : int, xs : int list) = 
(* assume all positive*)
  if sum <= (hd xs)
  then 0
  else 1 + number_before_reaching_sum(sum - (hd xs), tl xs)
  

fun what_month (day : int) = 
  let 
    val days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
    1 + number_before_reaching_sum(day, days)
  end
  
fun month_range (day1 : int, day2 : int) = 
  (* return an int list*)
  if day1 > day2
  then []
  else what_month(day1) :: month_range(day1 + 1, day2)

fun oldest (dates : (int*int*int) list) = 
  if null dates
  then NONE
  else
    let
      fun oldest_date (dates: (int*int*int) list) =
        if null (tl dates)
        then hd dates
        else
          let val oldest_tl = oldest_date(tl dates)
          in
            if is_older(hd dates, oldest_tl)
            then hd dates
            else oldest_tl
          end
    in
      SOME(oldest_date dates)
    end
