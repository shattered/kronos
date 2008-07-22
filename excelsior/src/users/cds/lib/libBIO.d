DEFINITION MODULE libBIO; (* Sem 09-Feb-91. (c) KRONOS *)

FROM SYSTEM      IMPORT ADDRESS;

IMPORT  io : BIO;

TYPE
  FILE = io.FILE;
  PATHs= io.PATHs;

PROCEDURE open  (VAR file: FILE; path,mode: ARRAY OF CHAR);
PROCEDURE create(VAR file: FILE; path,mode: ARRAY OF CHAR;
                                      size: INTEGER);
PROCEDURE close (VAR file: FILE);
PROCEDURE purge (VAR file: FILE);

PROCEDURE cut   (file: FILE; size: INTEGER);
PROCEDURE end   (file: FILE; size: INTEGER);
PROCEDURE extend(file: FILE; size: INTEGER);
PROCEDURE eof   (file: FILE): INTEGER;

PROCEDURE seek (file: FILE; pos,mode: INTEGER);

PROCEDURE read (file: FILE; buf: ADDRESS; len: INTEGER);
PROCEDURE write(file: FILE; buf: ADDRESS; len: INTEGER);

PROCEDURE fread (file: FILE; buf: ADDRESS; pos,len: INTEGER);
PROCEDURE fwrite(file: FILE; buf: ADDRESS; pos,len: INTEGER);

PROCEDURE fopen  (cd: FILE; VAR file: FILE; path,mode: ARRAY OF CHAR);
PROCEDURE fcreate(cd: FILE; VAR file: FILE; path,mode: ARRAY OF CHAR;
                                                       size: INTEGER);

END libBIO.
