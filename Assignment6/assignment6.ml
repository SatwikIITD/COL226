open List;;
type myBool = T | F;;
let myBool2bool b = match b with 
| T -> true
| F -> false
;;

(* Defining the custom expression data type *)
type exp = Num of int | Bl of myBool | Plus of exp * exp | Times of exp * exp | And of exp * exp | Or of exp * exp | Not of exp | Eq of exp * exp | Gt of exp * exp | V of string | IfTE of exp * exp * exp | Pair of exp * exp | Fst of exp | Snd of exp | Abs of string * exp | App of exp * exp;;
(* Define the custom values data type which takes the value of an expression *)
type values = N of int | B of bool | P of values * values | VClos of string * opcode list * environment
(* Define type environments *)
and environment = (string * values) list
(* Define the custom opcode datatype which is used for compiling an expression *)
and opcode = APP | MKCLOS of string * opcode list | RET | LDN of int | LDB of bool | LOOKUP of string | COND of opcode list * opcode list | PAIR of opcode list * opcode list | FST | SND | PLUS | TIMES | EQ | GT | AND | OR | NOT;;

(* Define the compile function to convert an expression into a list of opcodes *)
let rec compile e = match e with 
| Num n -> [LDN n]
| Bl b -> [LDB (myBool2bool b)]
| V x -> [LOOKUP x]
| Pair(e1,e2) -> [PAIR((compile e1), (compile e2))]
| Plus(e1,e2) -> (compile e1) @ (compile e2) @ [PLUS]
| Times(e1,e2) -> (compile e1) @ (compile e2) @ [TIMES]
| And(e1,e2) -> (compile e1) @ (compile e2) @ [AND]
| Or(e1,e2) -> (compile e1) @ (compile e2) @ [OR]
| Not(e1) -> (compile e1) @ [NOT]
| Gt(e1,e2) -> (compile e2) @ (compile e1) @ [GT]
| Eq(e1,e2) -> (compile e1) @ (compile e2) @ [EQ]
| IfTE(e0,e1,e2) -> (compile e0) @ [COND (compile e1,compile e2)]
| Fst(e0) -> (compile e0) @ [FST]
| Snd(e0) -> (compile e0) @ [SND]
| Abs(x,e) -> [MKCLOS(x, (compile e) @ [RET])]
| App(e1,e2) -> (compile e1) @ (compile e2) @ [APP]
;;

(* Define helper function to update environment *)
let update_env var value env = 
  let filtered_env = filter (fun(v,_) -> v <> var) env in (var,value)::filtered_env
;;

(* Define the SECD Machine *)
let rec secd stk env c dump = match c with 
| [] -> (match stk with 
          | v::_ -> v
          | _ -> failwith("Empty Control and Stack"))
| (LDN n)::c -> secd ((N n)::stk) env c dump
| (LDB b)::c -> secd ((B b)::stk) env c dump
| (LOOKUP x)::c -> secd ((helper_var x env)::stk) env c dump
| (MKCLOS(x,c0))::c -> let clos = VClos(x,c0,env) in secd (clos::stk) env c dump
| APP::c -> (match stk with 
              | arg::VClos(x,c0,clos_env)::stk ->  let new_env = update_env x arg clos_env in secd [] new_env c0 ((stk, env, c)::dump)
              | _-> failwith "APP instruction requries a function and an argument on a stack")
| RET::c -> (match dump with 
              | (s_old,env_old,c_old)::dump ->
                (match stk with 
                  | v::_ -> secd (v::s_old) env_old c_old dump
                  | _ -> failwith "RET instruction executed with an empty stack")
              | [] -> failwith "Dump is empty during RET calculation"    
              )
| PAIR(c1,c2)::c -> let v1 = secd stk env c1 dump and v2 = secd stk env c2 dump in secd (P(v1,v2)::stk) env c dump
| PLUS::c -> (match stk with 
              | (N n1)::(N n2)::stk -> secd (N(n1+n2)::stk) env c dump) 
| TIMES::c -> (match stk with 
              | (N n1)::(N n2)::stk -> secd (N(n1*n2)::stk) env c dump)
| AND::c -> (match stk with 
              | (B b1)::(B b2)::stk -> secd (B(b1 && b2)::stk) env c dump)
| OR::c -> (match stk with 
              | (B b1)::(B b2)::stk -> secd (B(b1||b2)::stk) env c dump)
| GT::c -> (match stk with 
              | (N n1)::(N n2)::stk -> secd (B(n1>n2)::stk) env c dump)
| EQ::c -> (match stk with 
              | (N n1)::(N n2)::stk -> secd (B(n1=n2)::stk) env c dump)
| NOT::c -> (match stk with
              | (B b)::stk -> secd (B(not b)::stk) env c dump)
| COND(c1,c2)::c -> (match stk with 
                      | (B true)::stk -> secd stk env (c1 @ c) dump
                      | (B false) ::stk -> secd stk env (c2 @ c) dump
                      | _ -> failwith "Error in COND")
| FST::c -> (match stk with 
                      | P(v1,v2)::stk -> secd (v1::stk) env c dump)
| SND::c -> (match stk with 
                      | P(v1,v2)::stk -> secd (v2::stk) env c dump)
| _ -> failwith "Unhandled instruction"
;;

let rec string_of_values = function 
  | N n -> "N " ^ string_of_int n
  | B b -> "B " ^ string_of_bool b
  | P(v1,v2) -> "P (" ^ string_of_values(v1) ^ ", " ^ string_of_values(v2) ^ ")"
  | VClos(x,_,_) -> "Closure with parameter" ^ x
;;

(* let expr1 = Plus(Times(Num 3, Num 4), Num 2);;
let opcode_list1 = compile expr1;;
let result1 = secd [] [] opcode_list1 [];;
print_endline(string_of_values result1);; *)


(* let expr2 = IfTE(Gt(Num 4, Num 3), Num 5, Num 6);;
let opcode_list2 = compile expr2;;
let result2 = secd [] [] opcode_list2 [];;
print_endline(string_of_values result2);; *)


(* let expr3 = App(Abs("x", Plus(V "x", Num 2)), Num 3);;
let opcode_list3 = compile expr3;;
let result3 = secd [] [] opcode_list3 [];;
print_endline(string_of_values result3);; *)


(* let expr4 = Plus(V "x", V "y");;
let test_env4 = [("x", N 2); ("y", N 3)];; 
let opcode_list4 = compile expr4;;
let result4 = secd [] test_env4 opcode_list4 [];;
print_endline(string_of_values result4);; *)

(* let expr5 = App(Abs("z", Plus(V "z", V "x")), Num 4);;
let test_env5 = [("x", N 10)];;
let opcode_list5 = compile expr5;;
let result5 = secd [] test_env5 opcode_list5 [];;
print_endline(string_of_values result5);; *)

(* let complex_expr = 
  App(
    App(
      Abs(
        "x", 
        Abs("y",
          Times(V "x", Plus(V "y", V "a"))
        )
      ),
      V "b"
    ),
    Plus(V "a", V "b")
  )
;;
let complex_env = [("a",N 10); ("b",N 20)];;
let complex_opcode = compile complex_expr;;
let complex_result = secd [] complex_env complex_opcode [];;
print_endline(string_of_values complex_result);; *)


(* let expr5 = IfTE((Gt(Fst(Pair(Num 1,Num 4)),Snd(Pair(Num 3,Num 5)))),And(Bl T,Bl F),Or(Bl T,Bl F)) ;;
let test_env5 = [];; 
let opcode_list5 = compile expr5;;
let result5 = secd [] test_env5 opcode_list5 [];;
print_endline(string_of_values result5);; *)