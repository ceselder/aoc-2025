open String
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


let filtered = split_on_char ',' (List.hd (read_file "input.txt")) (* guys what if we called "fst" "hd" to troll the trans woman doing this in 2025 (she cant use llms)*)
  |> List.filter(fun x -> x <> "")
  |> List.map (fun x ->  split_on_char '-' x)
  |> List.map (fun x ->  List.map (fun local -> int_of_string local) x);;

let counter = ref 0;;

let f (elem) =
    ()
in List.iter f filtered;;

Printf.printf "%d\n" !counter;