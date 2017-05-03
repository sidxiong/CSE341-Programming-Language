(* Code for notes*)


(* Map, Filter, and Reduce *)
fun map (f, xs) = 
  let fun map_acc (xs, ys) =
        case xs of
             [] => ys
           | x::xs' => map_acc (xs', ys@[f x])
  in map_acc (xs, [])
  end

fun double xs = 
  map (fn x => x * 2, xs)

fun filter (f, xs) = 
  case xs of 
       [] => []
     | x::xs' => if f x then x::filter (f, xs')
                 else filter (f, xs')

fun allGreaterThanN (xs, n) = 
  filter (fn x => x > n, xs)

fun reduce (f, acc, xs) = 
  case xs of
       [] => acc
     | x::xs' => reduce (f, f (acc, x), xs')

fun reduce_sum xs =
  reduce (fn (x, y) => x + y, 0, xs)

fun numInRange (xs, lo, hi) = 
  reduce (fn (acc, x) => acc + (if x >= lo andalso x <= hi then 1 else 0), 0, xs)

val t1 = double ([1,2,3,4,2,5])
val t2 = allGreaterThanN ([1,2,3,4,2,5], 3)
val t3 = reduce_sum ([1,2,3,4,2,5])
val t4 = numInRange ([1,2,3,4,2,5], 2, 4)

fun fold f = fn acc => fn xs =>
  case xs of 
       [] => acc
     | x::xs' => fold f (f (acc, x)) xs'


fun fold2 f acc xs =
  case xs of
       [] => acc
     | x::xs' => fold2 f (f (acc, x)) xs'

val sum2 = fold (fn (x, y) => x + y) 0

val t5 = sum2 ([1,2,3,4,2,5])
val t6 = fold2 (fn (x, y) => x + y) 0 [1,2,3,4,2,5]


(* Combining Functions *)
fun compose (f, g) = fn x => f (g x)
val increThenDouble = compose (fn x => x * 2, fn x => x + 1)

val increThenDouble2 = (fn x => x * 2) o (fn x => x + 1)

infix |>
fun x |> f = f x
val increThenDouble3 = fn x => x |> (fn x => x + 1) |> (fn x => x * 2)

val itd = increThenDouble 5
val itd2 = increThenDouble2 5
val itd3 = increThenDouble3 5

(* Curring *)
fun exists checker xs =
  case xs of
       [] => false
     | x::xs' => checker x orelse exists checker xs'

val t7 = exists (fn x => x > 5) [1,2,3,4,5,6]

fun zip xs ys =
  case (xs, ys) of
       ([], []) => []
     | (x::xs', y::ys') => (x,y)::zip xs' ys'

fun range i j =
    if i > j then []
    else i :: range (i+1) j  

val rangeTo = range 0

fun enumerate xs =
  zip (rangeTo (length xs - 1)) xs
val t8 = enumerate [5,4,3]

fun uncurryZip (xs, ys) = zip xs ys
fun curryZip xs ys = uncurryZip (xs, ys)
val t9 = uncurryZip ([1,2,3], [4,5,6])
val t9_ = curryZip [1,2,3] [4,5,6]

(* Callbacks using Mutation*)
val callbacks : (int -> unit) list ref = ref []
fun onKeyEvent f = callbacks := f::(!callbacks)
fun onEvent x =
let fun loop fs =
      case fs of
           [] => ()
         | f::fs' => (f x; loop fs')
in loop (!callbacks)
end

val numPressed = ref 0;
val _ = onKeyEvent (fn x => (numPressed := 1 + (!numPressed);
                             print ("Pressed " ^ Int.toString x ^ "; Pressed " ^ Int.toString (!numPressed) ^ " times\n")))
fun printIfPressed y = onKeyEvent (fn x => if x=y then print ("Got " ^ Int.toString y ^ "\n")
                                           else ())

val _ = printIfPressed 10 (* use its side effect *)

(* ADT *)
datatype set = S of {insert : int -> set, is_member : int -> bool, size : unit -> int}

val empty_set =
  let
    fun make_set xs =
      let 
        fun contains i = List.exists (fn x => i = x) xs
      in
        S {insert = fn x => if contains x then make_set xs
                          else make_set (x::xs),
           is_member = contains,
           size = fn () => length xs
          }
      end
  in
    make_set []
  end

val S es = empty_set
val S s2 = (#insert es) 10

val iii = (#is_member s2) 10
val sz = (#size s2) ()
