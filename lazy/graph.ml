open Types

let print (pos : int) (zs : zipper list): unit =
  let get_e' (e : exp): exp' = (Lazy.force e).e' in

  (* Track exp' *)
  let ids_exp' : (exp' * string) list ref = ref [] in
  let seen_exp' (e' : exp'): bool = List.mem_assq e' !ids_exp' in
  let id_exp' (e' : exp'): string =
    match List.assq_opt e' !ids_exp' with
    | Some id -> id
    | None ->
        let id = Printf.sprintf "exp%d" (List.length !ids_exp') in
        ids_exp' := (e', id) :: !ids_exp';
        id in

  (* Track cxt *)
  let ids_cxt : (cxt * string) list ref = ref [] in
  let seen_cxt (c : cxt): bool = List.mem_assq c !ids_cxt in
  let id_cxt (c : cxt): string =
    match List.assq_opt c !ids_cxt with
    | Some id -> id
    | None ->
        let id = Printf.sprintf "cxt%d" (List.length !ids_cxt) in
        ids_cxt := (c, id) :: !ids_cxt;
        id in

  let rec go_exp' (e' : exp'): unit =
    if not (seen_exp' e') then
      let id = id_exp' e' in
      match e' with
      | Tok t ->
        Printf.printf "  %s [ label=\"Tok %d %s\" shape=box ];\n" id (fst t) (snd t)
      | Seq (l, es) ->
        List.iter go_exp' (List.map get_e' es);
        Printf.printf "  %s [ label=\"Seq %s\" shape=invhouse ];\n" id l;
        let print_edge (i : int) (e1 : exp): unit =
          Printf.printf "  %s -> %s [ label=\"%d\"];\n" id (id_exp' (Lazy.force e1).e') i in
        List.iteri print_edge es
      | Alt es ->
        List.iter go_exp' (List.map get_e' !es);
        Printf.printf "  %s [ label=\"Alt\" shape=invtriangle];\n" id;
        let print_edge (i : int) (e1 : exp): unit =
          Printf.printf "  %s -> %s;\n" id (id_exp' (Lazy.force e1).e') in
        List.iteri print_edge !es
  in

  let rec go_cxt (c : cxt): unit =
    if not (seen_cxt c) then
      let id = id_cxt c in
      match c with
      | TopC ->
        Printf.printf "  %s [ label=\"TOP\" style=filled color=gray ];\n" id
      | SeqC (m, l, left, right) ->
        List.iter go_cxt m.parents;
        List.iter go_exp' (List.map get_e' left);
        List.iter go_exp' (List.map get_e' right);
        Printf.printf "  %s [ label=\"SeqC %s %d %d\" shape=house style=filled color=gray ];\n" id l (List.length left) (List.length right);
        let print_edge (i : int) (c1 : cxt): unit =
          Printf.printf "  %s -> %s [ dir=back style=dotted ];\n" (id_cxt c1) id in
        List.iteri print_edge m.parents;
        let print_edge (i : int) (e1 : exp): unit =
          Printf.printf "  %s -> %s [ label=\"%d\"];\n" id (id_exp' (Lazy.force e1).e') (List.length left - i - 1) in
        List.iteri print_edge left;
        let print_edge (i : int) (e1 : exp): unit =
          Printf.printf "  %s -> %s [ label=\"%d\"];\n" id (id_exp' (Lazy.force e1).e') (i + 1 + List.length left) in
        List.iteri print_edge right
      | AltC m ->
        List.iter go_cxt m.parents;
        Printf.printf "  %s [ label=\"AltC\" shape=triangle style=filled color=gray];\n" id;
        let print_edge (i : int) (c1 : cxt): unit =
          Printf.printf "  %s -> %s [ dir=back style=dotted ];\n" (id_cxt c1) id in
        List.iteri print_edge m.parents
  in

  let rec go_zipper (i : int) (z : zipper): unit =
    let id = Printf.sprintf "zipper%d" i in
    Printf.printf "  %s [ label=\"zipper %d\" color=red style=bold style=filled shape=diamond ];\n" id i;
    let (e', m) = z in
    go_exp' e';
    List.iter go_cxt m.parents;
    Printf.printf "  %s -> %s;\n" id (id_exp' e');
    let print_edge (i : int) (c1 : cxt): unit =
      Printf.printf "  %s -> %s [ dir=back style=dotted ];\n" (id_cxt c1) id in
    List.iteri print_edge m.parents
  in

  Printf.printf "digraph g {\n";
  Printf.printf "  label=\"Zippers at position %d\";\n" pos;
  List.iteri go_zipper zs;
  Printf.printf "}\n"