IMPLEMENTATION MODULE Code;

FROM SYSTEM    IMPORT  ADR, ADDRESS, WORD;
IMPORT  mcd: mCodeMnem;

PROCEDURE DeRef(w: WORD): ADDRESS; CODE     mcd.lsw0 END DeRef;
PROCEDURE MOVE(from,to: ADDRESS; n: INTEGER); CODE mcd.move END MOVE;

PROCEDURE GetCatr(C: ADDRESS; Atr: CodeAtr): WORD;
BEGIN
  IF Atr<=Kind THEN RETURN DeRef(C+INTEGER(Atr))
  ELSE
    CASE Atr OF
      StrAdr : RETURN C+NoCatrs
     |CodeAdr: RETURN C+NoCatrs+INTEGER(GetCatr(C,StrSz))
     |ExtAdr : RETURN INTEGER(GetCatr(C,CodeAdr))+INTEGER(GetCatr(C,CodeSz))
    ELSE ASSERT(FALSE)
    END;
  END;
END GetCatr;
 
PROCEDURE SetCatr(C: ADDRESS; Atr: CodeAtr; Val: WORD);
  VAR A: ADDRESS;
BEGIN
  IF Atr<=Kind THEN A:=C+INTEGER(Atr); A^:=Val
  ELSE ASSERT(FALSE)
  END;
END SetCatr;

PROCEDURE Default(C: ADDRESS);
BEGIN
  SetCatr(C,Vers,CurrentVersion);
  SetCatr(C,DefT,0);     SetCatr(C,ImpT,0);
  SetCatr(C,Stk0,0);     SetCatr(C,StkSz,0);
  SetCatr(C,SymOfs,0);   SetCatr(C,XrefOfs,0);
END Default;

PROCEDURE GetCode(Entry: ADDRESS): ADDRESS;
BEGIN RETURN DeRef(ADDRESS(Entry^)+1)-NoCatrs
END GetCode;

PROCEDURE MustBeInit(Entry: ADDRESS): BOOLEAN;
  VAR s: ARRAY [0..7] OF CHAR; w: WORD;
BEGIN
  w:=GetCatr(GetCode(Entry),Kind);
  MOVE(ADR(s),ADR(w),1); s[4]:=0c;
  RETURN s="Mod0";
END MustBeInit;

PROCEDURE Kind?(C: ADDRESS; VAR S: ARRAY OF CHAR);
  VAR W: WORD;
BEGIN W:=GetCatr(C,Kind);
  MOVE(ADR(S),ADR(W),1); S[4]:=0c
END Kind?;

PROCEDURE SetKind(C: ADDRESS; VAR S: ARRAY OF CHAR);
  VAR W: WORD;
BEGIN MOVE(ADR(W),ADR(S),1); SetCatr(C,Kind,W)
END SetKind;

PROCEDURE ModuleName(Entry: ADDRESS): ADDRESS;
BEGIN RETURN GetCode(Entry)+NoCatrs
END ModuleName;

END Code.
