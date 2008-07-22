DEFINITION MODULE lkPass; (* 21-Feb-89. *)

TYPE filename = ARRAY [0..32] OF CHAR;
     filePnt  = POINTER TO fileDesc;
     fileDesc = RECORD
                  fname: filename;
                  ftype: ARRAY [0..3] OF CHAR;
                  fsou : INTEGER;
                  fno  : INTEGER;
                  fnext: filePnt;
                END;

VAR filelist: filePnt;
    objlist : filePnt;
    liblist : filePnt;
    curfile : filePnt;


PROCEDURE InitLink;
PROCEDURE Pass1;
PROCEDURE AfterPass1;
PROCEDURE BeforePass2;
PROCEDURE Pass2;
PROCEDURE AfterPass2;


END lkPass.
