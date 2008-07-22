DEFINITION MODULE ModelPbl; (* Sem 11-Feb-87. (c) KRONOS *)

FROM SYSTEM    IMPORT   ADDRESS, WORD;
FROM osKernel  IMPORT   process;

TYPE Exception=BOOLEAN;
     pReaction=POINTER TO Reaction;
      Reaction=RECORD
                   Next: pReaction;
                  G_reg: ADDRESS;
                  L_reg: ADDRESS;
                 PC_reg: INTEGER;
                  M_reg: BITSET;
                  PL: ARRAY [0..5] OF WORD;
               END;

CONST Sorry=Exception(1); Failure=Exception(2);
      MemoryOverflow = Exception(3);
      IOerror        = Exception(4);
      SyntaxError    = Exception(5);
      CrashInModel   = Exception(6);
      BadModel       = Exception(6);

VAR Message: ARRAY [0..80] OF CHAR;

PROCEDURE Exception?(VAR r: Reaction): Exception;

PROCEDURE KillReaction(VAR r: Reaction);

PROCEDURE Raise(p: process; n: Exception);

PROCEDURE RaiseInMe(n: Exception);

END ModelPbl.
