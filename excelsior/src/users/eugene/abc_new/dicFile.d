DEFINITION MODULE dicFile; (* Sem 21-Aug-89. (c) KRONOS *)

TYPE
  dictionary;
  text=ARRAY [0..255] OF CHAR;

VAR
  io_error: PROC;
  time    : INTEGER;

PROCEDURE open  (VAR d: dictionary; name: ARRAY OF CHAR): BOOLEAN;
PROCEDURE create(VAR d: dictionary; name: ARRAY OF CHAR);
PROCEDURE close (VAR d: dictionary);

TYPE iter_proc=PROCEDURE (text,text): BOOLEAN;

PROCEDURE find  (d: dictionary; pattern: text; ip: iter_proc);
PROCEDURE get   (d: dictionary; inp: text; VAR out: text): BOOLEAN;
PROCEDURE put   (d: dictionary; inp,out: text);
PROCEDURE remove(d: dictionary; inp: text);
PROCEDURE rebalance(d: dictionary; ip: iter_proc);

PROCEDURE flash_cash;

END dicFile.
