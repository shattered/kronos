DEFINITION MODULE Lexicon; (* Leo 18-Jan-90. (c) KRONOS *)

IMPORT  SYSTEM;

TYPE LEX;

VAL null: LEX;
    done: BOOLEAN;
   error: INTEGER;
  sysmsg: LEX;

PROCEDURE open(VAR lex: LEX; lexicon_device_name: ARRAY OF CHAR);

PROCEDURE get(lex: LEX; code: INTEGER; VAR data: STRING);

PROCEDURE dispose(VAR data: STRING);

PROCEDURE sprint(VAR str: ARRAY OF CHAR;
                     lex: LEX;
                    code: INTEGER;
                  format: ARRAY OF CHAR;
                SEQ args: SYSTEM.WORD);

PROCEDURE close(VAR lex: LEX);

PROCEDURE perror(VAR str: ARRAY OF CHAR;
                    code: INTEGER;
                  format: ARRAY OF CHAR;
                SEQ args: SYSTEM.WORD);

PROCEDURE change_sysmsg(lex: LEX);

(*************************************************************

   Lexicon: Multiple lexicons interface
   -------

   ATTENTION! Library is ussing memory allocated in Heap.


PROCEDURE open(lex: LEX; special_device_name: ARRAY OF CHAR);
---------

PROCEDURE close(lex: LEX);
---------

    'lex' - lexicon file (driver for special device with
  appropriate name must exist and be running in
  the system).
     At instalation time text file read at 'lex' device
  and this library provides access to readen text.


PROCEDURE get(lex: LEX; code: INTEGER; VAR data: STRING);
--------------

     Take string of bytes from device 'lex',
  ALLOCATE 'data' in Heap, and copy taken string
  to 'data' memory.
     Array 'data' is always 1 byte longer then
  string from 'lex' and value of this addition
  byte is 000c.
     Text file data base with special format:
       <file> = <line> { 036c <line> } .
       <line> = <num> ["h"] " " <text> .
  must be read at instalation time at 'lex' device.

     If format of file was wrong 'inconsistency' error is
  returning.
     If line with such number absent in file 'no_entry'
  error is returning.

     If 'lex' device absent, string: "%s.%04hh",lex,code
  allocated and returned, error="open device error".


  NB:
     You must 'dispose' 'data' when you have no need to
  use it. If you have memory allocated for 'data' before
  'get' or 'read' call this memory will be lost.

PROCEDURE dispose(VAR data: STRING);
-----------------

     Dispose 'data' array allocated by 'get' or 'read' call.
  NB: You must always call 'dispose' after 'get',
  even if you have bad result at 'get' operation.

PROCEDURE print(VAR str: ARRAY OF CHAR;
---------------     lex: LEX;
                   code: INTEGER;
                 format: ARRAY OF CHAR;
               SEQ args: SYSTEM.WORD);

     String read by 'get' procedure will be inserted
  at position %%s of 'format'.

     If 'lex' device absent, string: "%s.%04hh",lex,code
  inserted at %%s position.

perror
------

     Prints error message in string using 'MSG' tskEnv
     parameter

*************************************************************)

END Lexicon.
