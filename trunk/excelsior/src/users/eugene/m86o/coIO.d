DEFINITION MODULE coIO; (* Ned 04-Mar-90. (c) KRONOS *)

IMPORT  def: coDefs;

PROCEDURE ini(VAR io: def.io_ptr;
                name: ARRAY OF CHAR;
                unit: INTEGER;
               print: def.PRINT);

PROCEDURE exi(VAR io: def.io_ptr);

PROCEDURE set(text_path,sym_path,out: ARRAY OF CHAR; print: def.PRINT);

PROCEDURE dispose;

END coIO.
