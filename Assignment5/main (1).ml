open List;;
open Printf;;

type symbol = string * int;;
type signature = symbol list;;

(* Part 1: check_sig function definition *)

let check_sig sig1 =
  let rec helper_check_sig seen_symbols remaining = 
    match remaining with 
    [] -> true
    | (sym,arity)::rest -> 
        if arity<0 || mem sym seen_symbols then
            false 
        else helper_check_sig (sym::seen_symbols) rest 
  in
  helper_check_sig [] sig1
;;

(* Test Case for Part 1 *)

let sig1 = [("0", 0); ("1", 0); ("*", 3); ("+", 2)];;
let is_valid = check_sig sig1;;
(* printf "%b\n" is_valid;; *)

(* Part 2 - Definition of Well Formed Tree *)

type tree = V of string    
    | C of {node:symbol; children: tree list};;

let rec find_symb sym signature = 
    match signature with 
    | [] -> None
    | (s,arity)::tail -> 
        if s = sym then Some(s,arity)
        else find_symb sym tail 

let rec wftree sig1 tree = match tree with
    V _ -> true
    | C {node;children} -> 
    match find_symb (fst node) sig1 with 
        | Some(_,arity) -> 
            if arity = length children then 
                for_all (wftree sig1) children
            else 
                false 
        | None -> false

(* Test Case for Part 2 *)

let sig1 = [("0", 0); ("1", 0); ("+", 2); ("*", 2)];;
let tree1 = C {node = ("+", 2); children = [V "x"; V "y"]};;
let tree2 = C {node = ("*", 2); children = [V "x"]};;

let is_well_formed1 = wftree sig1 tree1;;
let is_well_formed2 = wftree sig1 tree2;;
(* printf "%b\n" is_well_formed2;; *)

let x = V "x";;
let y = V "y";;
let z = V "z";;

let zero = C {node = ("0", 0); children = []};;
let one = C {node = ("1", 0); children = []};;
let plus_zero_one = C {node = ("+", 2); children = [zero; one] };;
let times_one_x = C {node = ("*", 2); children = [one; x] };;
let plus_zero_y = C {node = ("+", 2); children = [zero; y] };;
let plus_timesonex_pluszeroy = C {node = ("+", 2); children = [times_one_x; plus_zero_y ] };;
let plus_timesonex_z = C {node = ("+", 2); children = [times_one_x; z ] };;

(* Part 3: Height of tree *)

let rec ht t = match t with 
    V _ -> 0
    | C r -> 
        let (s,n) = r.node
        in (if n = 0 
            then 0
            else 1 + fold_left max 0 (map ht r.children))
;;

(* Test Case for height of tree *)
let height = ht zero;;
(* print_endline(string_of_int height);; *) 

(* Part 3: Size of tree *)
let rec size t = match t with 
    V _ -> 1
    | C r -> 1 + (fold_left (+) 0 (map size r.children))
;;

(* Test Case for size of tree *)
let sz = size zero;;
(* print_endline(string_of_int sz);; *)

(* Part 3: Vars Function *)

let rec helper_concat acc child = 
    acc @ variable_list child

and variable_list t = match t with
    V var -> [var]
    | C {children} -> fold_left helper_concat [] children
;;

let remove_duplicates lst = 
    let rec helper_dup acc x = 
        if mem x acc then acc
        else x::acc
    in fold_left helper_dup [] lst
;;

let vars tree = 
    let lst = variable_list tree in 
    remove_duplicates lst 
;;

(* Test Case for vars Function *)
let example_tree = C {node = ("+", 2); children = [V "x"; C {node = ("*", 2); children = [V "y"; V "x"]} ]};;
let variable_set = vars example_tree;;

let print_set  variable_set= 
    iter (fun var -> print_endline var) variable_set
;;
(* print_set variable_set;; *)

(* Part 4: Mirror Tree *)

let rec mirror t = match t with 
    V _ as leaf -> leaf
    | C {node;children} -> C {node;children = rev_map mirror children}
;;

(* Test Case for Part 4 *)

let example_tree = C {node = ("+", 2); children = [V "x"; C {node = ("*", 2); children = [V "y"; V "z"]} ]};;
let mirrored_tree = mirror example_tree;;

let rec print_tree depth tree =
  let print_indentation depth =
    for _ = 1 to depth do print_string "  " done
  in
  match tree with
  | V var -> 
      print_indentation depth; 
      Printf.printf "V \"%s\"\n" var
  | C {node; children} ->
      print_indentation depth; 
      printf "C {node = (\"%s\", %d); children = [\n" (fst node) (snd node);
      iter (print_tree (depth + 1)) children;
      print_indentation depth; 
      print_string "]}\n"
;;

(* print_tree 0 mirrored_tree;; *)

(* Part 5 *)

type sub = (string * tree) list;;
let id_sub: sub = []

(* Part 6: subst function definition *)

let rec subst_helper table x = match table with
    [] -> None
    | (a,b)::rest -> 
        if a = x then Some(a,b)
        else subst_helper rest x
;;

let rec subst table t = match t with 
  | V x -> 
    (match subst_helper table x with 
    | None -> V x  
    | Some(_, b) -> b  
    ) 
  | C r -> C {node = r.node; children = List.map (subst table) r.children}
;; 

let list_to_hashtbl table_list =
  let table = Hashtbl.create (List.length table_list) in
  iter (fun (var, subst) -> Hashtbl.add table var subst) table_list;
  table
;;

let rec hash_subst table t =
  match t with
  | V x ->
    (match Hashtbl.find_opt table x with
     | None -> V x
     | Some b -> b)
  | C r -> C {node = r.node; children = map (hash_subst table) r.children}
;;

let compose_subst (s1: sub) (s2: sub): sub =
  List.map (fun (var, tree) -> (var, subst s1 tree)) s2 @ s1
;;

(* Test Case for Part 6 *)

let table_list = [("x", V "a"); ("y", C {node = ("f", 2); children = [V "b"; V "c"]})];;
let table_hashtbl = list_to_hashtbl table_list;;

let t = C {node = ("g", 1); children = [V "x"; V "y"]};;
let result = hash_subst table_hashtbl t;;
(* print_tree 0 result;; *)

let table1 = [("x",one);("y",x)];;
let sub1 = subst table1 example_tree;;
(* print_tree 0 sub1;; *)

(* Part 7: mgu function definition *)

let rec occurs_check x tree = match tree with
  | V y -> x = y
  | C {children} -> exists (occurs_check x) children

let rec mgu t u = match (t,u) with 
    | V x, V y when x = y -> Some id_sub 
    | V x, V y -> Some [(x,V y)]
    | V x, _ -> if occurs_check x u then None else Some [(x,u)]
    | _ , V x -> if occurs_check x t then None else Some [(x,t)]
    | C {node = n1; children = c1}, C{node = n2; children = c2} -> 
        if n1 <> n2 || length c1 <> length c2 then None 
        else 
            let result = 
        try  fold_left2 (fun acc t_child u_child -> match acc with 
                | None -> raise Exit
                | Some substitution -> 
                    let t_child_subst = subst substitution t_child in 
                    let u_child_subst = subst substitution u_child in 
                    match mgu t_child_subst u_child_subst with
                    | None -> raise Exit 
                    | Some new_subst -> Some (compose_subst substitution new_subst))
                (Some id_sub) c1 c2 with
        | Exit -> None 
        in 
        result


let t = C {node = ("+", 2); children = [V "x"; C {node = ("*", 2); children = [V "y"; V "z"]} ]};;
let u = C {node = ("+", 2); children = [V "a"; C {node = ("*", 2); children = [V "b"; V "x"]} ]};;
let result = mgu t u;;

let print_subst subst =
  match subst with
  | None -> print_endline "FAIL"
  | Some subst_list ->
    List.iter (fun (var, tree) ->
      Printf.printf "%s -> " var;
      print_tree 0 tree  (* Start with depth 0 for each tree *)
    ) subst_list
;;

(* print_subst result;; *)

(* Test Cases for mgu *)
let t1 = V "x";;
let u1 = V "x";;
let result1 = mgu t1 u1;;
(* print_subst result1;; *)

let t2 = V "x";;
let u2 = V "y";;
let result2 = mgu t2 u2;;
(* print_subst result2;; *)

let t3 = V "x";;
let u3 = C {node = ("+", 2); children = [V "y"; V "z"]};;
let result3 = mgu t3 u3;;
(* print_subst result3;; *)

let t4 = C {node = ("+", 2); children = [V "x"; V "z"]};;
let u4 = V "x";;
let result4 = mgu t4 u4;;
(* print_subst result4;; *)

let t5 = C {node = ("+", 2); children = [V "x"; V "y"]};;
let u5 = C {node = ("*", 2); children = [V "a"; V "b"]};;
let result5 = mgu t5 u5;;
(* print_subst result5;; *)

let t6 = C {node = ("+", 2); children = [V "x"]};;
let u6 = C {node = ("+", 3); children = [V "x"; V "y"; V "z"]};;
let result6 = mgu t6 u6;;
(* print_subst result6;; *)

let t7 = C {node = ("+", 2); children = [V "x"; V "y"]};;
let u7 = C {node = ("+", 2); children = [V "a"; V "b"]};;
let result7 = mgu t7 u7;;
(* print_subst result7;; *)

let t8 = C {
  node = ("f", 3); children = [
    V "x";  
    C { node = ("g", 1); children = [V "a"]};  
    C { node = ("h", 2); children = [V "y"; V "b"]}  
  ]
};;

let u8 = C {
  node = ("f", 3); children = [
    C { node = ("g", 1); children = [V "z"]};  
    V "y";  
    C { node = ("h", 2); children = [V "z"; V "b"]}  
  ]
};;
let result8 = mgu t8 u8;;
(* print_subst result8;; *)


      
    