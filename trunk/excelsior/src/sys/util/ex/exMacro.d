DEFINITION MODULE exMacro; (* 29-Jun-87. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADDRESS;

PROCEDURE StartMacro;

PROCEDURE intomacro(k: CHAR);

PROCEDURE in?(): BOOLEAN;
PROCEDURE out?(): BOOLEAN;

PROCEDURE FinishGold(char: CHAR; del: INTEGER);

PROCEDURE FinishBronze(char: CHAR; del: INTEGER);

PROCEDURE getmacro(): CHAR;
PROCEDURE peekmacro(): CHAR;

PROCEDURE GetBronzeMacro(char: CHAR);
PROCEDURE GetGoldMacro(char: CHAR);

PROCEDURE newGmacro(ch: CHAR; VAR adr: ADDRESS; size: INTEGER);
PROCEDURE newBmacro(ch: CHAR; VAR adr: ADDRESS; size: INTEGER);
PROCEDURE newSmacro(ch: CHAR; VAR adr: ADDRESS; size: INTEGER);

PROCEDURE getGmacro(ch: CHAR; VAR adr: ADDRESS; VAR size: INTEGER);
PROCEDURE getBmacro(ch: CHAR; VAR adr: ADDRESS; VAR size: INTEGER);
PROCEDURE getSmacro(ch: CHAR; VAR adr: ADDRESS; VAR size: INTEGER);

END exMacro.
