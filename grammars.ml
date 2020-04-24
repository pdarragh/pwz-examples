open Types

module type Grammar = sig
  val start : exp
end

(*
The empty grammar through self-reference.

   e ::= e
*)
module Grammar1 = struct
end

(*
The empty grammar, but seems productive.

   e ::= Ae
*)
module Grammar2 = struct
end

(*
Ambiguously empty grammar.

   e ::= Ae
       | eA
*)
module Grammar3 = struct
end

(*
Another tricky empty grammar.

   e ::= AeA
*)
module Grammar4 = struct
end

(*
Right-recursive with infinite parse forests.

   e ::= e
       | Ae
       | ε
*)
module Grammar5 = struct
end

(*
Left-recursive with infinite parse forests.

   e ::= e
       | eA
       | ε
*)
module Grammar6 = struct
end

(*
Palindromes. Not ambiguous, and not LL(k) or LR(k) for any k.

   e ::= AeA
       | BeB
       | ε
*)
module Grammar7 = struct
end

(*
Hidden production with left-recursion.

   e1 ::= e2A
        | ε
   e2 ::= e1
*)
module Grammar8 = struct
end

(*
Hidden production with right-recursion.

   e1 ::= Ae2
        | ε
   e2 ::= e1
*)
module Grammar9 = struct
end

(*
Empty grammar through mutual reference.

   e1 ::= e2
   e2 ::= e1
*)
module Grammar10 = struct
end

(*
Tricky single-token grammar.

   e1 ::= e2
        | A
   e2 ::= e1
*)
module Grammar11 = struct
end

(*
Highly ambiguous for parsing ABABABABABABABA.

   e ::= A
       | eBe
*)
module Grammar12 = struct
end
