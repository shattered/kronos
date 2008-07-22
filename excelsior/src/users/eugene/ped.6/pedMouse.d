DEFINITION MODULE pedMouse; (* Sem 09-Oct-88. (c) KRONOS *)

VAR rel: BOOLEAN;

PROCEDURE empty?(): BOOLEAN;

PROCEDURE first(VAR x,y: INTEGER; VAR ch: CHAR);

PROCEDURE drop;

PROCEDURE wait;

PROCEDURE Read(): CHAR;

END pedMouse.
