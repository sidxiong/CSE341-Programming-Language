(* Coursera Programming Languages-partA hw3*)
(* author : Siyadong Xiong (sx225@cornell.edu) *)

exception NoAnswer


(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

(* part 1 *)
fun only_capitals strs = 
  List.filter (fn str => Char.isUpper (String.sub (str, 0))) strs

fun longest_string1 strs = 
  foldl (fn (str', str) => if String.size str' > String.size str then str' else str) "" strs

fun longest_string2 strs = 
  foldl (fn (str', str) => if String.size str' >= String.size str then str' else str) "" strs

fun longest_string_helper cmp strs = 
  foldl (fn (str', str) => if cmp(String.size str', String.size str) then str' else str) "" strs

val longest_string3 = longest_string_helper (fn (x,y) => x > y)
val longest_string4 = longest_string_helper (fn (x,y) => x >= y)

val longest_capitalized = longest_string3 o only_capitals

val rev_string = String.implode o rev o String.explode

fun first_answer checker lst = 
  case lst of
       [] => raise NoAnswer
     | x :: xs => case checker x of
                       NONE => first_answer checker xs
                     | SOME v => v

fun all_answers transformer xs =
let fun helper (result_lst, xs) =
      case xs of
           [] => SOME result_lst
         | x :: xs' => case transformer x of
                            NONE => NONE
                          | SOME ys => helper (result_lst@ys, xs')
in helper ([], xs) end

(* part 2 *)
datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end


val count_wildcards = g (fn _ => 1) (fn _ => 0) 
val count_wild_and_variable_lengths = g (fn _ => 1) String.size 
fun count_some_var (str, p) = g (fn _ => 0) (fn x => if str=x then 1 else 0) p

val check_pat =
let
  fun get_var_names (p, result) =
      case p of
           Variable x => result @ [x]
         | TupleP ps => foldl get_var_names result ps
         | ConstructorP (s, p') => get_var_names (p', result@[s])
         | _ => result
  fun is_unique strs =
    case strs of
         [] => true
       | str :: strs' => (not (List.exists (fn str' => str' = str) strs')) andalso is_unique strs'
in
  is_unique o (fn p => get_var_names (p, []))
end


fun match (valu, pat) =
  case (valu, pat) of
       (Tuple vs, TupleP ps) => if length ps = length vs then all_answers match (ListPair.zip (vs, ps)) 
                                else NONE
     | (Constructor (s2,v), ConstructorP (s1,p)) => if s1 = s2 then match (v, p)
                                                    else NONE
     | (Const y, ConstP x) => if x = y then SOME [] else NONE
     | (Unit, UnitP) => SOME []
     | (_, Variable name) => SOME [(name, valu)]
     | (_, Wildcard) => SOME []
     | _ => NONE
     
 
fun first_match valu ps = (* return (string*valu) list option *)
  SOME (first_answer (fn p => match (valu, p)) ps)
  handle NoAnswer => NONE

(*
fun typecheck_patterns
* *)
