DEFINITION MODULE cdsTTY; (* Sem 18-Jan-91. (c) KRONOS *)

FROM libWindows  IMPORT window;

PROCEDURE init(bmd: window);

PROCEDURE write(str: ARRAY OF CHAR; len: INTEGER);

END cdsTTY.
