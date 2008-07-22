DEFINITION MODULE abcVoc; (* Hady. 18-Apr-88. (c) KRONOS *)

TYPE Word = RECORD
       org: ARRAY  [0..79] OF CHAR;
       trn: ARRAY [0..255] OF CHAR
     END;

(*************** W H O L E   V O C A B U L A R Y ******************)
PROCEDURE   OpenVoc(name: ARRAY OF CHAR): INTEGER;
PROCEDURE CreateVoc(name: ARRAY OF CHAR): INTEGER;
PROCEDURE CloseVoc() : INTEGER;

PROCEDURE FindWord(pat:ARRAY OF CHAR; VAR ret:Word):INTEGER;
PROCEDURE PutWord(word:Word): INTEGER;
PROCEDURE GetWord(pat:ARRAY OF CHAR; VAR word:Word):INTEGER;


TYPE IterProc = PROCEDURE (VAR Word): BOOLEAN; (* TRUE == stop *)
PROCEDURE IterTree(pat:ARRAY OF CHAR; show: IterProc): INTEGER;

END abcVoc.
