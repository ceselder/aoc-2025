let read_file filename = (*yes this is what it takes if you want to read a file without using jane street library
  tommorow I will use jane street library*)
let lines = ref [] in
let chan = open_in filename in
try
  while true; do
    lines := input_line chan :: !lines
  done; !lines
with End_of_file ->
  close_in chan;
  List.rev !lines ;;

let real_input = read_file "input.txt"

let filtered = List.filter(fun x -> x <> "") real_input;;

let knob = ref 50;;
let counter = ref 0;; (*muteabilititity*)

let f elem =
    let the_number = int_of_string (String.sub elem 1 ((String.length elem) - 1)) (*disgusting*)
      in 
        (* Printf.printf "I'm looking at element %s %d now\n" left_or_right the_number; *)
        if ((String.sub elem 0 1) = "L") then (knob := (!knob + the_number) mod 100) else (knob := (!knob - the_number) mod 100);
        if (!knob = 0) then counter := !counter + 1 else ()
  in
List.iter f filtered;;

Printf.printf "%d " !counter