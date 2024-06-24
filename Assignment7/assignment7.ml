open List
open Printf

type myBool = T | F;;

let myBool2bool b = match b with
  | T -> true
  | F -> false
;;

let bool2myBool b = match b with 
  | true -> T
  | false -> F
;;

type exp = Num of int | Bl of myBool | Plus of exp * exp | Times of exp * exp | And of exp * exp | Or of exp * exp | Not of exp | Eq of exp * exp | Gt of exp * exp | IfTE of exp * exp * exp | Pair of exp * exp | Fst of exp | Snd of exp | V of string | Abs of string * exp | App of exp * exp;;
type clos = Clos of exp * (string * clos) list;;
type stack = clos list;;

let rec krivine c s = match c with
  | Clos(V x, env) -> (match assoc_opt x env with
                       | Some c' -> krivine c' s
                       | None -> failwith ("Unbound variable: " ^ x))
  | Clos(Abs(x, e), env) -> (match s with
                             | [] -> Clos(Abs(x, e), env)
                             | arg::s' -> krivine (Clos(e, (x, arg)::env)) s')
  | Clos(App(e1, e2), env) -> krivine (Clos(e1, env)) (Clos(e2, env)::s)
  | Clos(Num n, _) -> Clos(Num n, [])
  | Clos(Bl b, _) -> Clos(Bl b, [])
  | Clos(Plus(e1, e2), env) ->
      let Clos(Num n1, _) = krivine (Clos(e1, env)) [] in
      let Clos(Num n2, _) = krivine (Clos(e2, env)) [] in
      Clos(Num (n1 + n2), [])
  | Clos(Times(e1, e2), env) ->
      let Clos(Num n1, _) = krivine (Clos(e1, env)) [] in
      let Clos(Num n2, _) = krivine (Clos(e2, env)) [] in
      Clos(Num (n1 * n2), [])
  | Clos(And(e1, e2), env) ->
      let Clos(Bl b1, _) = krivine (Clos(e1, env)) [] in
      let Clos(Bl b2, _) = krivine (Clos(e2, env)) [] in
      Clos(Bl (bool2myBool (myBool2bool b1 && myBool2bool b2)), [])
  | Clos(Or(e1, e2), env) ->
      let Clos(Bl b1, _) = krivine (Clos(e1, env)) [] in
      let Clos(Bl b2, _) = krivine (Clos(e2, env)) [] in
      Clos(Bl (bool2myBool (myBool2bool b1 || myBool2bool b2)), [])
  | Clos(Not(e), env) ->
      let Clos(Bl b, _) = krivine (Clos(e, env)) [] in
      Clos(Bl (bool2myBool (not (myBool2bool b))), [])
  | Clos(Eq(e1, e2), env) ->
      let Clos(Num n1, _) = krivine (Clos(e1, env)) [] in
      let Clos(Num n2, _) = krivine (Clos(e2, env)) [] in
      Clos(Bl (bool2myBool (n1 = n2)), [])
  | Clos(Gt(e1, e2), env) ->
      let Clos(Num n1, _) = krivine (Clos(e1, env)) [] in
      let Clos(Num n2, _) = krivine (Clos(e2, env)) [] in
      Clos(Bl (bool2myBool (n1 > n2)), [])
  | Clos(IfTE(e0, e1, e2), env) ->
      (match krivine (Clos(e0, env)) [] with
       | Clos(Bl T, _) -> krivine (Clos(e1, env)) s
       | Clos(Bl F, _) -> krivine (Clos(e2, env)) s
       | _ -> failwith "Condition in IfTE must evaluate to a boolean")
  | Clos(Pair(e1, e2), env) ->
  let e1' = krivine (Clos(e1, env)) [] in
  let e2' = krivine (Clos(e2, env)) [] in
  (match (e1', e2') with
    | (Clos(v1, _), Clos(v2, _)) -> Clos(Pair(v1, v2), [])
    | _ -> failwith "Pair elements must evaluate to values")
    
  | Clos(Fst(e), env) ->
    (match krivine (Clos(e, env)) s with
      | Clos(Pair(e1, _), pairEnv) -> krivine (Clos(e1, pairEnv)) s
      | _ -> failwith "Fst applied to non-pair")
  | Clos(Snd(e), env) ->
      (match krivine (Clos(e, env)) s with
        | Clos(Pair(_, e2), pairEnv) -> krivine (Clos(e2, pairEnv)) s
        | _ -> failwith "Snd applied to non-pair")
    
  | _ -> failwith "Expression not supported or requires implementation";;

(* Test Cases *)
(* let test1 = Clos(App(Abs("x", Plus(V "x", Num 6)), Plus(Num 7, Num 8)), []);;
let result1 = krivine test1 [];;

let test2 = Clos(App(App(Abs("x", Abs("y", V "x")), Num 7), Plus(Num 1, Num 2)), []);;
let result2 = krivine test2 [];;

let test3 = Clos(App(Abs("x", App(Abs("y", Plus(V "x", V "y")), Num 10)), Num 20), []);;
let result3 = krivine test3 [];;


let env4 = [("a", Clos(Num 10, []))];;
let test4 = Clos(App(Abs("x", Plus(V "x", V "a")), Num 5), env4);;
let result4 = krivine test4 [];;

let test5 = Clos(IfTE(Bl T, Num 5, App(Abs("y", V "x"), Num 10)), []);;
let result5 = krivine test5 [];;

let test6 = Clos(Pair(Plus(Num 3, Num 4), Times(Num 5, Num 6)), []);;
let result6 = krivine test6 [];;

let test7 = Clos(Fst(Pair(Num 3, Num 4)), []);;
let result7 = krivine test7 [];;

let test8 = Clos(Snd(Pair(Num 3, Num 4)), []);;
let result8 = krivine test8 [];;

let pairexp = Pair(Abs("x", Plus(V "x", Num 1)), Num 1);;
let test9 = Clos(App(Fst(pairexp), Num 10), []);;
let result9 = krivine test9 [];;

let test10 = Clos(Snd(pairexp), []);;
let result10 = krivine test10 [];;

let test11 = Clos(IfTE(Bl F, Num 42, IfTE(Gt(Num 5, Num 10), App(Abs("x", V "x"), V "x"), Num 100)),[]);;
let result11 = krivine test11 [];;


let rec eval_and_print_exp c =
  match krivine c [] with
  | Clos(Num n, _) -> sprintf "Num: %d" n
  | Clos(Bl b, _) -> sprintf "Bool: %b" (b = T)
  | _ -> "Expression not evaluated to a basic value"

and print_closure_result = function
  | Clos(Num n, _) -> printf "Num: %d\n" n
  | Clos(Bl b, _) -> printf "Bool: %b\n" (b = T)
  | Clos(Pair(e1, e2), env) ->
      let str1 = eval_and_print_exp (Clos(e1, env)) in
      let str2 = eval_and_print_exp (Clos(e2, env)) in
      printf "Pair(%s, %s)\n" str1 str2
  | _ -> print_endline "Result is a non-numeric/non-boolean value or requires further evaluation"
;;


print_closure_result result1;;
print_closure_result result2;;
print_closure_result result4;;
print_closure_result result5;;
print_closure_result result6;;
print_closure_result result7;;
print_closure_result result8;;
print_closure_result result9;;
print_closure_result result10;;
print_closure_result result11;; *)