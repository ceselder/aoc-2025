
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

let rec get_max_int_index banned i highest_i highest_num lst = (*find max in string not at index banned*)
    match lst with
     [] -> i
    |[h] -> if banned = (-1) then highest_i else (if h >= highest_num then i else highest_i) (* edge case where its the last number case*)
    | h :: t -> if (i > banned && h > highest_num) then (get_max_int_index banned (i + 1) i h t)  else (get_max_int_index banned (i + 1) highest_i highest_num t);;

let filtered = read_file "input.txt"
  |> List.map(explode)
  |> List.map(List.map(fun f -> int_of_char f - int_of_char '0'))
  |> List.map(fun x -> ((get_max_int_index (-1) 0 (-1) (-1) x), x))
  |> List.map(fun (x, y) -> (x, (get_max_int_index x 0 (-1) (-1) y) ,y))
  |> List.map(fun (x, y, lst) -> ((List.nth lst x ), (List.nth lst y)))
  |> List.map(fun (x,y) -> (string_of_int x ^ string_of_int y))
  |> List.map(int_of_string)
  |> List.fold_left (+) 0;;


List.iter(fun x -> Printf.printf "%s\n" (x)) (read_file "input.txt");;
(* List.iter(fun (x) -> Printf.printf "%s \n" (x)) filtered;; *)

(* Printf.printf "%d\n" (get_max_int_index (-1) 0 (-1) (-1) [3;4;5;6;5;4;3]);; *)

Printf.printf "%d\n" (filtered);;