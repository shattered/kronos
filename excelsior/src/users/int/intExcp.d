DEFINITION MODULE intExcp; (* Sem 11-Feb-87. (c) KRONOS *)

FROM SYSTEM    IMPORT   ADDRESS, WORD;
FROM Scheduler IMPORT   ProcessId;

TYPE Exception=BOOLEAN;
     pReaction=POINTER TO Reaction;
      Reaction=RECORD
                   Next: pReaction;
                  G_reg: ADDRESS;
                  L_reg: ADDRESS;
                 PC_reg: CARDINAL;
                  M_reg: BITSET;
                  PL: ARRAY [0..5] OF WORD;
               END;

CONST Sorry=Exception(1); Failure=Exception(2);
      NoPage         = Exception(3);
      WriteProtect   = Exception(4);
      IllegalAdr     = Exception(5);

PROCEDURE Exception?(VAR r: Reaction): Exception;

PROCEDURE KillReaction(VAR r: Reaction);

PROCEDURE Raise(p: ProcessId; n: Exception);

PROCEDURE RaiseInMe(n: Exception);

END intExcp.
