(* Coursera Programming Languages-partA hw2*)
(* author : Siyadong Xiong (sx225@cornell.edu) *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option (str, str_lst) = 
  case str_lst of
       [] => NONE
     | str'::tl_lst => if same_string(str, str') 
                       then SOME tl_lst
                       else case all_except_option(str, tl_lst) of
                                 NONE => NONE
                               | SOME tmp => SOME (str'::tmp)

fun get_substitutions1 (substitutions, str) = 
  case substitutions of 
       [] => [] 
     | hd_lst :: tl_lsts => case all_except_option (str, hd_lst) of 
                                 NONE => get_substitutions1 (tl_lsts, str)
                               | SOME str_lst => str_lst @ get_substitutions1 (tl_lsts, str)

fun get_substitutions2 (substitutions, str) = 
  let
    fun do_get (subs, result_lst) =
      case subs of
           [] => result_lst
         | hd_lst :: tl_lsts => case all_except_option (str, hd_lst) of
                                     NONE => do_get(tl_lsts, result_lst)
                                   | SOME str_lst => do_get(tl_lsts, result_lst@str_lst)
  in 
    do_get (substitutions, [])
  end

fun similar_names (subs, name) = 
  let 
    val {first=f, middle=m, last=l} = name
    val alternatives = get_substitutions2(subs, f)
    fun helper(result, alternatives) =
      case alternatives of
           [] => result
         | x::xs => helper(result@[{first=x, middle=m, last=l}], xs)
  in
    name :: helper([], alternatives)
  end


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
fun card_color (s, r) =
  case s of
     (Spades | Clubs) => Black
     | _ => Red

fun card_value (s, r) =
  case r of
       Num x => x
     | Ace => 11
     | _ => 10

fun remove_card (cs, c, e) =
  let
    fun helper(result, cs) = 
      case cs of
           [] => raise e
         | hd_c :: tl_cs => if hd_c = c then result @ tl_cs
                            else helper(result @ [hd_c], tl_cs)
  in 
    helper([], cs)
  end

fun all_same_color cs =
  case cs of
       ([] | _::[]) => true
     | c1::(c2::tl_lst) => (card_color c1) = (card_color c2) andalso all_same_color (c2::tl_lst)

fun sum_cards cs =
  let 
    fun helper(sum, cs) =
      case cs of
           [] => sum
         | c :: tl_cs => helper(sum + card_value c, tl_cs)
  in
    helper(0, cs)
  end

fun score (cs, goal) = 
  let
    val held_sum = sum_cards cs
    val p_score = if held_sum > goal then 3 * (held_sum - goal)
                  else goal - held_sum
  in
    if all_same_color cs then p_score div 2
    else p_score
  end


fun officiate (card_list, mvs, goal) = 
  let
    fun helper (held_cards, card_list, mvs) =
      let val current_score = score (held_cards, goal)
      in case mvs of
             [] => current_score
           | mv :: tl_mvs => case mv of
                                  Discard c => helper (remove_card (held_cards, c, IllegalMove ), card_list, tl_mvs)
                                | Draw => case card_list of
                                               [] => current_score
                                             | c :: tl_cards => let val new_held_cards = c :: held_cards
                                                                in if sum_cards (new_held_cards) > goal then score (new_held_cards, goal)
                                                                   else helper (new_held_cards, tl_cards, tl_mvs)
                                                                end
      end
  in
    helper ([], card_list, mvs)
  end
