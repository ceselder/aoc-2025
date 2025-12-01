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

let filtered = List.filter(fun x -> x <> "") (read_file "input.txt");;
let knob = ref 50;;
let counter = ref 0;; (*muteabilititity*)

(*ok chibli ok chibli I'll do it imperatively chibli*)
let f elem =
    let the_number = int_of_string (String.sub elem 1 ((String.length elem) - 1)) (*disgusting*)
      in (*whole code is disgusting tbh*)
        if ((String.sub elem 0 1) = "R") then 
          (if (!knob + (the_number mod 100) >= 100) then (counter := !counter + 1) else ();
            (knob := (!knob + the_number) mod 100);)
        else 
          (if (!knob - (the_number mod 100) <= 0 && !knob <> 0) then (counter := !counter + 1) else ();
            knob := ((!knob - the_number) mod 100););

        if !knob < 0 then knob := !knob + 100 else (); (*chud ocaml negative mod function hacky fix*) 

        counter := !counter + (the_number / 100);
  in
List.iter f filtered;;

Printf.printf "%d " !counter