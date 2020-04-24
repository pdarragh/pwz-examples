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

A NOTE ON LIMITATIONS IMPOSED BY OCAML

   The OCaml language features a number of limitations that prevent us from
   expressing grammars as succinctly and clearly as we would like. Namely, these
   are:

   1. Recursive record definitions must be completely static, i.e., we cannot use
      "smart constructors" to reduce syntactic overhead.
   2. Names of functions, values, and variables cannot begin with a capital
      letter. (We address this by prepending these names with an underscore.)
   3. Identifiers cannot contain non-ASCII characters. (This means we cannot use
      the ε character as an identifier.)

   2 and 3 are easy enough to deal with. However, 1 is more of a nuisance. One
   potential solution would be to write a syntax macro (e.g., using ppx) to
   allow for easier specification of recursive forms for defining grammars.
   Unfortunately, none of the authors is sufficiently skilled in the OCaml
   syntax system to implement this completely within the time limit afforded us
   by the author response. However, it is something we would like to provide.
*)

(*
tok_list_of_string

   This function allows for easy conversion from a string to a list of tokens,
   useful for parsing the simple grammars encoded here.

   Each grammar contains an association list mapping single characters to token
   expressions. A string consisting solely of these characters can then be
   converted to a list of the token expressions.

   Note that the string cannot have any extraneous whitespace or other
   characters that are not defined in the association list.
*)
let tok_list_of_string (str : string) (assocs : (char * tok) list) : tok list =
  let limit = String.length str in

  let rec tok_list_of_string (idx : int) (acc : tok list) : tok list =
    if idx < limit
    then tok_list_of_string (idx + 1) ((List.assoc (String.get str idx) assocs) :: acc)
    else acc

  in List.rev (tok_list_of_string 0 [])

(*
Grammar module signature

   This module signature shows the interface through which all of the below
   grammars can be used.
*)
module type Grammar = sig
  val tokens : (char * tok) list
  val start : exp
end

(*
Token definitions

   These are simple definitions of type Types.tok. They are used by some of the
   grammars defined below.
*)
let t_A = (1, "A")
let t_B = (2, "B")

let tokens_A = [('A', t_A)]
let tokens_AB = [('A', t_A); ('B', t_B)]

(*
parse

   This is an alias for the Pwz module's `parse` function.

   It takes two arguments: a Grammar module G (examples of which are defined
   below in this file) and a string. The string will be converted to a list of
   tokens according to the association list G.tokens. Then, the parse will
   proceed by starting at the G.start grammar expression.
*)

(* TODO: fix the error generated when this is defined:

   File "grammars.ml", line *, characters 46-53:
   Error: This expression has type exp but an expression was expected of type
            Types.exp

   * The error refers to the second line below, which begins with "Pwz.parse".

let parse ((module G) : (module Grammar)) (str : string) : exp list =
  Pwz.parse (tok_list_of_string str G.tokens) G.start
*)

(*
Grammar1: The empty grammar through self-reference.

   e ::= e
*)
module Grammar1 : Grammar = struct
  let tokens = []

  let rec e = { m = m_bottom; e' = Seq ("e", [e]) }

  let start = e
end

(*
Grammar2: The empty grammar, but seems productive.

   e ::= A e
*)
module Grammar2 : Grammar = struct
  let tokens = tokens_A

  let rec _A = { m = m_bottom; e' = Tok t_A }
      and e  = { m = m_bottom; e' = Seq ("e", [_A; e]) }

  let start = e
end

(*
Grammar3: Ambiguously empty grammar.

   e ::= A e
       | e A
*)
module Grammar3 : Grammar = struct
  let tokens = tokens_A

  let rec _A  = { m = m_bottom; e' = Tok t_A }
      and _Ae = { m = m_bottom; e' = Seq ("Ae", [_A; e]) }
      and _eA = { m = m_bottom; e' = Seq ("eA", [e; _A]) }
      and e   = { m = m_bottom; e' = Alt (ref [ _Ae; _eA ]) }

  let start = e
end

(*
Grammar4: Another tricky empty grammar.

   e ::= A e A
*)
module Grammar4 : Grammar = struct
  let tokens = tokens_A

  let rec _A = { m = m_bottom; e' = Tok t_A }
      and e  = { m = m_bottom; e' = Seq ("AeA", [_A; e; _A]) }

  let start = e
end

(*
Grammar5: Right-recursive with infinite parse forests.

   e ::= e
       | A e
       | ε
*)
module Grammar5 : Grammar = struct
  let tokens = tokens_A

  let rec _A   = { m = m_bottom; e' = Tok t_A }
      and _Ae  = { m = m_bottom; e' = Seq ("Ae", [_A; e]) }
      and _eps = { m = m_bottom; e' = Alt (ref []) }
      and e    = { m = m_bottom; e' = Alt (ref [e; _Ae; _eps]) }

  let start = e_bottom
end

(*
Grammar6: Left-recursive with infinite parse forests.

   e ::= e
       | e A
       | ε
*)
module Grammar6 : Grammar = struct
  let tokens = tokens_A

  let rec _A   = { m = m_bottom; e' = Tok t_A }
      and _eA  = { m = m_bottom; e' = Seq ("eA", [e; _A]) }
      and _eps = { m = m_bottom; e' = Alt (ref []) }
      and e    = { m = m_bottom; e' = Alt (ref [e; _eA; _eps]) }

  let start = e_bottom
end

(*
Grammar7: Palindromes. Not ambiguous, and not LL(k) or LR(k) for any k.

   e ::= A e A
       | B e B
       | ε
*)
module Grammar7 : Grammar = struct
  let tokens = tokens_AB

  let rec _A   = { m = m_bottom; e' = Tok t_A }
      and _B   = { m = m_bottom; e' = Tok t_B }
      and _AeA = { m = m_bottom; e' = Seq ("AeA", [_A; e; _A]) }
      and _BeB = { m = m_bottom; e' = Seq ("BeB", [_B; e; _B]) }
      and _eps = { m = m_bottom; e' = Alt (ref []) }
      and e    = { m = m_bottom; e' = Alt (ref [_AeA; _BeB; _eps]) }

  let start = e
end

(*
Grammar8: Hidden production with left-recursion.

   e1 ::= e2 A
        | ε
   e2 ::= e1
*)
module Grammar8 : Grammar = struct
  let tokens = tokens_A

  let rec _A   = { m = m_bottom; e' = Tok t_A }
      and _eps = { m = m_bottom; e' = Alt (ref []) }
      and _e2A = { m = m_bottom; e' = Seq ("e2A", [e2; _A]) }
      and e1   = { m = m_bottom; e' = Alt (ref [_e2A; _eps]) }
      and e2   = { m = m_bottom; e' = Seq ("e2", [e1]) }

  let start = e1
end

(*
Grammar9: Hidden production with right-recursion.

   e1 ::= A e2
        | ε
   e2 ::= e1
*)
module Grammar9 : Grammar = struct
  let tokens = tokens_A

  let rec _A   = { m = m_bottom; e' = Tok t_A }
      and _eps = { m = m_bottom; e' = Alt (ref []) }
      and _Ae2 = { m = m_bottom; e' = Seq ("Ae2", [_A; e2]) }
      and e1   = { m = m_bottom; e' = Alt (ref [_Ae2; _eps]) }
      and e2   = { m = m_bottom; e' = Seq ("e2", [e1]) }

  let start = e1
end

(*
Grammar10: Empty grammar through mutual reference.

   e1 ::= e2
   e2 ::= e1
*)
module Grammar10 : Grammar = struct
  let tokens = []

  let rec e1 = { m = m_bottom; e' = Seq ("e1", [e2]) }
      and e2 = { m = m_bottom; e' = Seq ("e2", [e1]) }

  let start = e1
end

(*
Grammar11: Tricky single-token grammar.

   e1 ::= e2
        | A
   e2 ::= e1
*)
module Grammar11 : Grammar = struct
  let tokens = tokens_A

  let rec _A = { m = m_bottom; e' = Tok t_A }
      and e1 = { m = m_bottom; e' = Alt (ref [e2; _A]) }
      and e2 = { m = m_bottom; e' = Seq ("e2", [e1]) }

  let start = e1
end

(*
Grammar12: Highly ambiguous for parsing ABABABABABABABA.

   e ::= A
       | e B e
*)
module Grammar12 : Grammar = struct
  let tokens = tokens_AB

  let rec _A   = { m = m_bottom; e' = Tok t_A }
      and _B   = { m = m_bottom; e' = Tok t_B }
      and _eBe = { m = m_bottom; e' = Seq ("eBe", [e; _B; e]) }
      and e    = { m = m_bottom; e' = Alt (ref [_A; _eBe]) }

  let start = e
end
