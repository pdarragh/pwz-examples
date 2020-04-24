#mod_use "types.ml";;
#mod_use "pwz.ml";;
#mod_use "grammars.ml";;

open Grammars;;

let introduction =
  [ "################################################################################"
  ; ""
  ; "Parsing with Zippers REPL"
  ; ""
  ; "################################################################################"
  ];;
List.iter (fun s -> Printf.printf "%s\n" s) introduction;;
