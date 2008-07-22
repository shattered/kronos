DEFINITION MODULE exIO; (* Leo 30-Jun-87. (c) KRONOS *)

PROCEDURE  ReadFile(name: ARRAY OF CHAR; ins: BOOLEAN; resh: PROC;
                VAR  sep: CHAR ): BOOLEAN;
PROCEDURE WriteFile(name: ARRAY OF CHAR; from,to: INTEGER;
                    add: BOOLEAN; nl: CHAR): BOOLEAN;

END exIO.
