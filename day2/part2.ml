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

let little_regex_that_could = Str.regexp "^\\(\\([0-9]+\\)\\2+\\)$";; (*I spent about 2 minutes writing this regex, and about 2 hours getting it to compile/figuring out what library to use in ocaml*)
 
let f (elem) =
    (for i = (List.hd elem) to (List.nth elem 1) do 
      if Str.string_match little_regex_that_could (string_of_int i) 0 then (counter := !counter + i) else ();
    done;)
in List.iter f filtered;;

Printf.printf "%d\n" !counter;