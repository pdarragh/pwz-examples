#mod_use "types.ml";;
#use "pwz.ml";;

let introduction =
  [ "################################################################################"
  ; ""
  ; "Parsing with Zippers REPL"
  ; ""
  ; "################################################################################"
  ];;
List.iter (fun s -> Printf.printf "%s\n" s) introduction;;
