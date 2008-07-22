DEFINITION MODULE adaLex; (* 03-Apr-89. (c) KRONOS *)

FROM SYSTEM     IMPORT  WORD;
FROM adaNum     IMPORT  long_int, long_float;

TYPE lex_elem=(
  ident,                int_number,
  real_number,          char,
  string,               ampersand,
  apostrophe,           left_parenthesis,
  right_parenthesis,    star,
  plus,                 comma,
  minus,                point,
  slash,                colon,
  semicolon,            less,
  equal,                greater,
  bar,
  arrow,        -- =>
  double_dot,   -- ..
  double_star,  -- **
  assign,       -- :=
  inequality,   -- /=
  greater_equal,-- >=
  less_equal,   -- <=
  label_left,   -- <<
  label_right,  -- >>
  box,          -- <>
  abort, abs, accept, access, all, and, array, at, begin, body,
  case, constant, declare, delay, delta, digits, do, else, elsif,
  end, entry, exception, exit, for, function, generic, goto, if, in, is,
  limited, loop, mod, new, not, null, of, or, others, out, package,
  pragma, private, procedure, raise, range, record, rem, renames,
  return, reverse, select, separate, subtype, task, terminate, then,
  type, use, when, while, with, xor,
  eot    -- end of text
  );

TYPE
  value=RECORD
    CASE : INTEGER OF
      |0: -- string
        str : ARRAY [0..255] OF CHAR;
        len : INTEGER; -- in bytes
      |1: -- integer
        int : long_int;
      |2: -- float
        flo : long_float;
    END;
  END;

VAR
  sy      : lex_elem;
  lex_pos : INTEGER;
  next_sy : lex_elem;
  next_pos: INTEGER;
  val     : value;

PROCEDURE next;
PROCEDURE set_pos(pos: INTEGER);

PROCEDURE error(pos: INTEGER; fmt: ARRAY OF CHAR; SEQ args: WORD);
-- %. in fmt - fault

PROCEDURE put_errors;
PROCEDURE error_cnt(): INTEGER;

END adaLex.
