open Types

(*
TEST GRAMMARS FOR PARSING WITH ZIPPERS

   These are test grammars issued by a reviewer to convince them of the general
   properties we claim about PwZ.

   Each grammar conforms to the Grammar module signature. This allows them to be
   kept separate and use similar naming conventions. We chose the first
   production of each grammar to be used as the `start` production.

   The grammars each have a comment above that explain the purpose of the
   grammar and show it written in a BNF format. These are more-or-less copied
   directly from the review, though some liberties were taken with formatting.
   Specifically, all alternates appear on separate lines, and spaces have been
   inserted between symbols for clarity.

   Note that the BNF specifications use the unusual convention that lowercase
   letters represent non-terminals while capital letters represent terminals.
   This is to keep a style more consistent with the reviewer's specification.
*)

module type Grammar = sig
  val start : exp
end

(*
The empty grammar through self-reference.

   e ::= e
*)
module Grammar1 = struct
  let rec e : exp = { m = m_bottom; e' = Seq ("e", [e]) }

  let start = e
end

(*
The empty grammar, but seems productive.

   e ::= A e
*)
module Grammar2 = struct
end

(*
Ambiguously empty grammar.

   e ::= A e
       | e A
*)
module Grammar3 = struct
end

(*
Another tricky empty grammar.

   e ::= A e A
*)
module Grammar4 = struct
end

(*
Right-recursive with infinite parse forests.

   e ::= e
       | A e
       | ε
*)
module Grammar5 = struct
end

(*
Left-recursive with infinite parse forests.

   e ::= e
       | e A
       | ε
*)
module Grammar6 = struct
end

(*
Palindromes. Not ambiguous, and not LL(k) or LR(k) for any k.

   e ::= A e A
       | B e B
       | ε
*)
module Grammar7 = struct
end

(*
Hidden production with left-recursion.

   e1 ::= e2 A
        | ε
   e2 ::= e1
*)
module Grammar8 = struct
end

(*
Hidden production with right-recursion.

   e1 ::= A e2
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
  let rec e1 = { m = m_bottom; e' = Seq ("e1", [e2]) }
      and e2 = { m = m_bottom; e' = Seq ("e2", [e1]) }

  let start = e1
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
       | e B e
*)
module Grammar12 = struct
end
