(*Numbers can be Int, Float or Fractions*)
type number =
  | Int of int
  | Float of float
  | Fraction of int * int
;;

(*Modulo works for Int only, else raise Not Found Error*)
let modulo x y =
  match (x, y) with
  | (Int a, Int b) -> a mod b
  | (_, _) -> raise Not_found (*All other cases raise Not Found Error*)
;;

(*Recursive GCD function*)
let rec gcd (x,y) =
  let (x,y) = if x >= y then (x,y) else (y,x) in
  if y = 0 then x else gcd(y, modulo (Int x) (Int y))
;;

(*Normaization handeling for fraction operations, works on Int else raise Not Found Error*)
let normalize x y =
  if x = y then Int 1 else
  match (x, y) with
  | (Int a, Int b) -> if (modulo (Int a) (Int b)) = 0 then Int (a / b) else let d = gcd(a, b) in Fraction(a/d, b/d)
  | (_, _) -> raise Not_found   (*All other cases raise Not Found Error*)
;;

(*Safe division for all number types*)
let divide x y =
  match (x, y) with
  | (_, Int 0) | (_, Float 0.0) | (_, Fraction(0, _)) -> raise Division_by_zero
  (*Int*)
  | (Int a, Int b) -> if (modulo (Int a) (Int b)) = 0 then Int (a / b) else Float (float a /. float b)
  | (Int a, Float b) ->   Float (float a /. b)
  | (Int x, Fraction (a, b)) -> normalize (Int (x * b)) (Int a)
  (*Float*)
  | (Float a, Int b) ->   Float (a /. float b)
  | (Float a, Float b) -> Float (a /. b)
  | (Float x, Fraction (a, b)) -> Float ( x /. (float a /. float b) )
  (*Fraction*)
  | (Fraction (a, b), Int y) -> normalize (Int a) (Int (y * b))
  | (Fraction (a, b), Float y) -> Float (((float a) /. (float b)) /. y)
  | (Fraction (a, b), Fraction (c, d)) -> normalize (Int (a * d)) (Int (b * c))
;;

(*Safe multiplication for all number types*)
let multiply x y =
  match (x, y) with
  (*Int*)
  | (Int a, Int b) -> Int (a * b)
  | (Int a, Float b) ->   Float (float a *. b)
  | (Int x, Fraction (a, b)) -> normalize (Int (x * a)) (Int b)
  (*Float*)
  | (Float a, Int b) ->   Float (a *. float b)
  | (Float a, Float b) -> Float (a *. b)
  | (Float x, Fraction (a, b)) -> Float ( x *. (float a /. float b) )
  (*Fraction*)
  | (Fraction (a, b), Int y) -> normalize (Int (a * y)) (Int b)
  | (Fraction (a, b), Float y) -> Float (y *. ((float a) /. (float b)))
  | (Fraction (a, b), Fraction (c, d)) -> normalize (Int (a * c)) (Int (b * d))
;;

(*Safe addition for all number types*)
let add x y =
  match (x, y) with
  (*Int*)
  | (Int a, Int b) -> Int (a + b)
  | (Int a, Float b) ->   Float (float a +. b)
  | (Int x, Fraction (a, b)) -> normalize (Int (a + (b * x))) (Int b)
  (*Float*)
  | (Float a, Int b) ->   Float (a +. float b)
  | (Float a, Float b) -> Float (a +. b)
  | (Float x, Fraction (a, b)) -> Float ((float a +. (float b *. x)) /. (float b))
  (*Fraction*)
  | (Fraction (a, b), Int y) -> normalize (Int (a + (b * y))) (Int b)
  | (Fraction (a, b), Float y) -> Float ((float a +. (float b *. y)) /. (float b))
  | (Fraction (a, b), Fraction (c, d)) -> normalize (Int((a * d) + (c * b))) (Int (b * d))
;;

(*Safe subtraction for all number types*)
let subtract x y =
  match (x, y) with
  (*Int*)
  | (Int a, Int b) -> Int (a - b)
  | (Int a, Float b) ->   Float (float a -. b)
  | (Int x, Fraction (a, b)) -> normalize (Int((b * x) - a)) (Int b)
  (*Float*)
  | (Float a, Int b) ->   Float (a -. float b)
  | (Float a, Float b) -> Float (a -. b)
  | (Float x, Fraction (a, b)) -> Float (((float b *. x) -. float a) /. (float b))
  (*Fraction*)
  | (Fraction (a, b), Int y) -> normalize (Int(a - (b * y))) (Int b)
  | (Fraction (a, b), Float y) -> Float ((float a -. (float b *. y)) /. (float b))
  | (Fraction (a, b), Fraction (c, d)) -> normalize (Int((a * d) - (c * b))) (Int (b * d))
;;

(*Prints a number type*)
let print_number n = 
  match n with
  | Int n -> Printf.printf "%i\n" n 
  | Float n  -> Printf.printf "%f\n" n
  | Fraction (m,n) -> Printf.printf "%i/%i\n" m n
;;

(*Test Data*)
(*Addition*)
print_number ( add (Int 6) (Int 5) );;                               (*11*)
print_number ( add (Int 6) (Float 4.5) );;                           (*10.5*)
print_number ( add (Int 5) (Fraction (1, 4)) );;                     (*21/4*)
Printf.printf "\n";;
print_number ( add (Float 4.5) (Int 6) );;                           (*10.5*)
print_number ( add (Float 4.5) (Float 4.5) );;                       (*9.0*)
print_number ( add (Float 5.75) (Fraction (1, 4)) );;                (*6.0*)
Printf.printf "\n";;
print_number ( add (Fraction (1, 3)) (Int 3) );;                     (*10/3*)
print_number ( add (Fraction (1, 4)) (Float 5.75) );;                (*6.0*)
print_number ( add (Fraction (50, 100)) (Fraction (2, 8)) );;        (*3/4*)
Printf.printf "\n";;
(*Subtraction*)
print_number ( subtract (Int 6) (Int 5) );;                          (*1*)
print_number ( subtract (Int 6) (Float 4.5) );;                      (*1.5*)
print_number ( subtract (Int 5) (Fraction (1, 4)) );                 (*19/4*)
Printf.printf "\n";;
print_number ( subtract (Float 6.5) (Int 6) );;                      (*0.5*)
print_number ( subtract (Float 6.5) (Float 4.5) );;                  (*2.0*)
print_number ( subtract (Float 5.75) (Fraction (1, 4)) );;           (*5.5*)
Printf.printf "\n";;
print_number ( subtract (Fraction (7, 3)) (Int 2) );;                (*1/3*)
print_number ( subtract (Fraction (9, 2)) (Float 1.5) );;            (*3.0*)
print_number ( subtract (Fraction (75, 100)) (Fraction (2, 8)) );;   (*1/2*)
Printf.printf "\n";;
(*Multiplication*)
print_number ( multiply (Int 6) (Int 5) );;                          (*30*)
print_number ( multiply (Int 6) (Float 4.5) );;                      (*27.0*)
print_number ( multiply (Int 20) (Fraction (1, 4)) );                (*5*)
print_number ( multiply (Int 21) (Fraction (1, 4)) );                (*21/4*)
Printf.printf "\n";;
print_number ( multiply (Float 6.5) (Int 6) );;                      (*39.0*)
print_number ( multiply (Float 6.5) (Float 4.5) );;                  (*29.25*)
print_number ( multiply (Float 20.5) (Fraction (1, 4)) );;           (*5.125*)
Printf.printf "\n";;
print_number ( multiply (Fraction (2, 7)) (Int 3) );;                (*6/7*)
print_number ( multiply (Fraction (9, 2)) (Float 1.5) );;            (*6.75*)
print_number ( multiply (Fraction (75, 100)) (Fraction (2, 8)) );;   (*3/16*)
Printf.printf "\n";;
(*Division*)
print_number ( divide (Int 6) (Int 5) );;                          (*1.2*)
print_number ( divide (Int 30) (Int 6) );;                         (*5*)
print_number ( divide (Int 6) (Float 1.5) );;                      (*4.0*)
print_number ( divide (Int 3) (Fraction (2, 5)) );                 (*15/2*)
print_number ( divide (Int 20) (Fraction (1, 4)) );                (*80*)
Printf.printf "\n";;
print_number ( divide (Float 3.5) (Int 2) );;                      (*1.75*)
print_number ( divide (Float 1.5) (Float 0.5) );;                  (*3.0*)
print_number ( divide (Float 20.5) (Fraction (1, 4)) );;           (*82.0*)
Printf.printf "\n";;
print_number ( divide (Fraction (9, 3)) (Int 3) );;                (*1*)
print_number ( divide (Fraction (10, 3)) (Int 3) );;               (*10/9*)
print_number ( divide (Fraction (9, 2)) (Float 1.5) );;            (*3.0*)
print_number ( divide (Fraction (75, 100)) (Fraction (2, 8)) );;   (*3*)
Printf.printf "\n";;
print_number ( divide (Fraction (75, 100)) (Fraction (0, 8)) );;   (*Error, divide by zero*)