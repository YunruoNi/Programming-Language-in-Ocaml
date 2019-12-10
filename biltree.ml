(*  define a function write_list_int: string -> int list -> unit
 *  that takes the name of a file, a list of integers and writes
 *  all the elements of the list in the file, from left to right, 
 *  one per line, and then returns unit.
 *  Remember to close any possible channel before ending.
 *)
 let rec write_line(txtoutput:out_channel)(my_test:int list): unit=
  match my_test with
    | l::ls->let _=Printf.fprintf txtoutput "%d\n" l in write_line txtoutput ls
    | []->()

let write_list_int (s: string) (int_list: int list): unit =
  (* please replace the expression () below with your code *)
  let oc=open_out s in
  let _=write_line oc int_list in
  let _=close_out oc in 
  ();;


  


(*  define a function read_list_int_rev: string -> int list
 *  that takes the name of a text file (which contains one integer per line) 
 *  and returns a list of all the integers in reversed order. That is 
 *  if the file looks like this: 
 *  1
 *  2
 *  3
 *  the returned list has to be [3;2;1] .
 *  Finally, remember to close any input channel before ending.
 *) 


let rec read_line (txtinput:in_channel):int list=
  match input_line txtinput with
  |l ->int_of_string l :: read_line txtinput
  |exception End_of_file -> [];;


let read_list_int_rev (f: string): (int list) = 
  (* please replace the expression [] below with your code *)
  
  let ic=open_in f in
  let int_list=read_line ic in
  let _=close_in ic in
  match int_list with
  |x-> List.rev x ;;
  




(* Definition of biltrees *)
type biltree = B of bool   | I of int   | L of int list | T of biltree * char  * biltree

(* Some examples of biltrees *)
let ex1 = T (T (B true, 'a' , T (I (-34), 'b', L [-21; 53; 12])), 'c', T (I (-18), 'd' , B true))
let ex2 = T (T (T (T (I 31, 'h', L [9; 34; -45]), 'e', L [70; 58; -36; 28]), 'l', I 3), 'l', T (I 2, 'o', I 49))
let ex3 = T (T (T (L [9; 4; -1; 0; -5], 'c', B true), 's', B true), '3', T (B false, '2', I (-3)))


(* Functions you need to define *)
        
let rec count_nodes (p: biltree):int=
  (* please replace the 0 below with your code *)
  match p with
  | B _-> 1
  | I _-> 1
  | L _-> 1
  | T(left,char,right) -> (count_nodes left) +1+(count_nodes right)

let classfi (left_B: bool option)  (right_B: bool option): bool option=
  match left_B with
  | None -> (match right_B with
    | None-> None
    | Some x-> Some x)
  | Some x3 -> (match right_B with
    | None -> Some x3
    | Some x2-> Some (x3 && x2))  

let rec global_and (p: biltree): bool option =
  (* please replace the None below with your code *)
  match p with
  | B my_B -> Some my_B
  | I _-> None
  | L _-> None
  | T (left,char,right) ->  classfi (global_and left) (global_and right)

  
let rec sum_lists (p: biltree) : biltree =
  match p with
  | B a-> B a
  | I a-> I a
  | L x->I (List.fold_left (+) 0 x )
  | T (left,char,right) -> T (sum_lists left,char, sum_lists right)
  (* please replace the expression B true below with your code *)


  
let rec f_on_all_ints (f : int -> int) (b: biltree): biltree =
  (* please replace the expression B true below with your code *)
  match b with
  | B a-> B a
  | I x-> I (f x)
  | L y -> L (List.map(f) y)
  | T (left,char,right) -> T (f_on_all_ints f left,char,f_on_all_ints f right)

let string_of_int_list (z: int list):string =
  let z= List.map(string_of_int) z in 
  List.fold_left (^) "" z  
  
let rec tostring_mlr (b : biltree): string =
  match b with
  | B x-> string_of_bool x
  | I y-> string_of_int y
  | L z-> string_of_int_list z
  | T (left,char,right) -> (Char.escaped char) ^ (tostring_mlr left) ^(tostring_mlr right)



  (* please replace the empty string "" below with your code *)


