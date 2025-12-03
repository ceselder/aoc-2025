
let read_file filename = (*one day I will learn how core works, but not today!*)
let lines = ref [] in
let chan = open_in filename in
try
  while true; do
    lines := input_line chan :: !lines
  done; !lines
with End_of_file ->
  close_in chan;
  List.rev !lines ;;

let () = Printexc.record_backtrace true;; (*lord and savior line, I cansee whats going on*)

let explode s = List.init (String.length s) (String.get s);;

let rec get_max_int_index top_cap (highest_num, then_list) lst = (*find max in string not at index banned*)
  if List.length lst < top_cap
  then
      (highest_num, then_list)
  else
  (    match lst with
      | [] -> (highest_num,then_list)
      | h :: t -> if ( h > highest_num ) then (get_max_int_index top_cap (h,t) t) else (get_max_int_index top_cap (highest_num, then_list) t)
  );;

let rec rec_get_max nums lst =
  match nums with
  | 0 -> []
  | x -> (let (result, t) = (get_max_int_index (x) ((-1),lst) lst) in result :: (rec_get_max (x - 1) t) )


let filtered = read_file "input.txt"
  |> List.map(explode)
  |> List.map(List.map(fun f -> int_of_char f - int_of_char '0'))
  |> List.map(fun x -> rec_get_max 12 x)
  |> List.map(List.map(string_of_int))
  |> List.map(String.concat "")
  |> List.map(int_of_string)
  |> List.fold_left (+) 0;;


(* List.iter(fun x -> Printf.printf "%s\n" (x)) (read_file "input.txt");; *)
(* List.iter(fun (x) -> Printf.printf "%s \n" (x)) filtered;; *)

(* Printf.printf "%d\n" (get_max_int_index (-1) 0 (-1) (-1) [3;4;5;6;5;4;3]);; *)

Printf.printf "%d\n" (filtered);;

(*Disgusting solution, memory inefficient as fuck. whatever*)