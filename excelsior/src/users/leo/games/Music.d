DEFINITION MODULE Music; (* Shu 21-Jan-87. (c) KRONOS *)

(* Модуль позволяет работать со звукогенератором LABTAM 3000.
*)

FROM StdIO     IMPORT   Stream;

TYPE

  Notes = (
    do, do_, re, re_, mi, fa, fa_, sol, sol_, la, la_, si, pause, none
  );

  Note  = RECORD
    note: Notes;
    oct : INTEGER;
    dou : INTEGER;
  END;


PROCEDURE PlayFile  (f: Stream);
PROCEDURE PlayString(s: ARRAY OF CHAR);
PROCEDURE PlayNote  (n: Note);

PROCEDURE PackNote(VAR n: Note; note: Notes; oct,dou: INTEGER);

END Music.
