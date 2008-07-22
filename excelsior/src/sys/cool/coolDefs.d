DEFINITION MODULE coolDefs; (* Ned 06-Jan-90. (c) KRONOS *)

IMPORT  SYSTEM;

CONST -- io kinds
  def    = 0;   -- definition module
  imp    = 1;   -- implementation module
  main   = 2;   -- module
  text   = 3;   --
  sym_in = 4;
  sym_ou = 5;
  ref    = 6;
  code   = 7;
  mcode  = 8;   -- code of program module

TYPE
  PRINT  = PROCEDURE (ARRAY OF CHAR, SEQ SYSTEM.WORD);
  io_ptr = POINTER TO io_rec;
  io_rec = RECORD
             kind : INTEGER;
             doio : PROCEDURE (io_ptr);
             done : BOOLEAN;
             print: PRINT;          -- for error message
             buf  : STRING;
             len  : INTEGER;        -- of buf
             exts : SYSTEM.ADDRESS;
           END;

TYPE
  INI    = PROCEDURE (VAR io_ptr, ARRAY OF CHAR, INTEGER, PRINT);
  EXI    = PROCEDURE (VAR io_ptr);
  ERROR  = PROCEDURE (
                       INTEGER,           -- line
                       INTEGER,           -- col
                       ARRAY OF CHAR,     -- source line
                       ARRAY OF CHAR,     -- format
                       SEQ SYSTEM.WORD    -- args
                       );
            -- line and col defines position of error in
            -- source line. format and args defines error message.

END coolDefs.
