#mod_use "types.ml";;
#mod_use "grammars.ml";;
#mod_use "pwz.ml";;

open Types;;
open Grammars;;
open Pwz;;

let introduction =
  [ "################################################################################"
  ; ""
  ; "Parsing with Zippers REPL"
  ; ""
  ; "################################################################################"
  ];;
List.iter (fun s -> Printf.printf "%s\n" s) introduction;;
