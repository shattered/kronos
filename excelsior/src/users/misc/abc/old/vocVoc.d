DEFINITION MODULE vocVoc; (* Sem 21-Aug-89. (c) KRONOS *)

TYPE
  dictionary;

VAR
  key  : ARRAY [0..79] OF CHAR;
  word : ARRAY [0..79] OF CHAR;
  trn  : ARRAY [0..255] OF CHAR;

PROCEDURE open  (VAR d: dictionary; name: ARRAY OF CHAR): BOOLEAN;
PROCEDURE create(VAR d: dictionary; name: ARRAY OF CHAR): BOOLEAN;
PROCEDURE close (VAR d: dictionary): BOOLEAN;

TYPE iter_proc=PROCEDURE (): BOOLEAN;

PROCEDURE find  (d: dictionary; ip: iter_proc): BOOLEAN;
PROCEDURE put   (d: dictionary): BOOLEAN;
PROCEDURE remove(d: dictionary): BOOLEAN;

END vocVoc.
