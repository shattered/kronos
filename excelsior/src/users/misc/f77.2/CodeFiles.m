IMPLEMENTATION MODULE CodeFiles; (* Ned 22-Dec-86. (c) KRONOS *)

FROM SYSTEM       IMPORT  ADR, ADDRESS, WORD;

FROM Code         IMPORT  CodeAtr, SetCatr, NoCatrs, SetKind, Default;

FROM Strings      IMPORT  Str1, AppStr, Len;
FROM Streams      IMPORT  Stream, Create, Write, Close;
FROM StdIO        IMPORT  Why?, WriteString, Show;
FROM FileNames    IMPORT  codext, AppExt;
IMPORT FileNames;

CONST BytesPerWord=4;

(*
TYPE
  Attrs = RECORD (* All size in WORDs *)
                     CPU: INTEGER;
                Language: ARRAY [0..31] OF CHAR;
                 DefTime: INTEGER; (* = ImpTime for prog. module *)
                 ImpTime: INTEGER;
                  ProcCo: INTEGER;
                  ExtsCo: INTEGER; (* = LDFT size *)
                CodeSize: INTEGER;
              ConstsSize: INTEGER; (* size of strings pool *)
             GlobalsSize: INTEGER; (* without LDFT size *)
            MinProcStack: INTEGER;
            OptimalStack: INTEGER;
          END;
*)

VAR   fn: ARRAY [0..255] OF CHAR;
    code: Stream;
   Fault: PROC;


PROCEDURE Check(r: INTEGER);
  VAR msg: ARRAY [0..79] OF CHAR;
BEGIN
  IF r<0 THEN
    Why?(r,msg); WriteString(msg); Show(fn); Fault;
  END;
END Check;

PROCEDURE write(VAR a: ARRAY OF WORD; l: INTEGER);
  VAR r: INTEGER;
BEGIN l:=l*BytesPerWord;
  r:=Write(code,ADR(a),l);
  IF r<0 THEN Check(r)
  ELSIF r#l THEN
    WriteString("ошибка записи "); Show(fn); Fault;
  END
END write;

PROCEDURE errfn(s: ARRAY OF CHAR); BEGIN Show(s); Fault END errfn;

PROCEDURE CreateCode(modname: ARRAY OF CHAR; error: PROC);
  VAR l: INTEGER;
BEGIN Fault:=error; FileNames.error:=errfn;
  Str1(fn,modname); AppExt(fn,codext);
  code:=Create(fn);
END CreateCode;

PROCEDURE Catrs(co: ADDRESS; VAR a: Attrs);
  VAR sz: INTEGER;
BEGIN
  Default(co);
  IF a.CPU=1 THEN SetCatr(co,Vers,100h)
  ELSE            SetCatr(co,Vers,101h)
  END;
  SetKind(co,a.Language);
  SetCatr(co,DefT,a.DefTime);
  SetCatr(co,ImpT,a.ImpTime);
  SetCatr(co,NoPrc,a.ProcCo);
  SetCatr(co,NoExt,a.ExtsCo);
  SetCatr(co,Stk0 ,a.MinProcStack);
  SetCatr(co,StkSz,a.OptimalStack);
  SetCatr(co,StrSz,a.ConstsSize);
  sz:=a.CodeSize;
  IF a.CPU=1 THEN INC(sz,(a.ProcCo+1) DIV 2)
  ELSE            INC(sz,a.ProcCo)
  END;
  SetCatr(co,CodeSz,sz);
  SetCatr(co,GloSz,a.GlobalsSize+a.ExtsCo);
  SetCatr(co,XrefOfs,0FFFFh);
  SetCatr(co,SymOfs,0FFFFh);
END Catrs;

PROCEDURE WriteInfo(VAR a: Attrs);
  VAR inf: ARRAY [0..NoCatrs-1] OF WORD; r: INTEGER;
BEGIN
  FOR r:=0 TO HIGH(inf) DO inf[r]:=0 END;
  Catrs(ADR(inf),a);
  write(inf,NoCatrs);
END WriteInfo;

PROCEDURE WriteExt(no: INTEGER; nm: ARRAY OF CHAR);
  VAR e: ARRAY [0..35] OF CHAR; n: INTEGER;
BEGIN
  Str1(e,nm); n:=Len(e);
  REPEAT e[n]:=0c; INC(n) UNTIL (n MOD BytesPerWord)=0;
  write(e,n DIV BytesPerWord);
END WriteExt;

PROCEDURE WriteProcTable(VAR a: Attrs; VAR pTab: ARRAY OF INTEGER);
  VAR i,n,m,no: INTEGER;
BEGIN
  IF a.CPU#1 THEN
    FOR i:=0 TO a.ProcCo-1 DO
      IF pTab[i]#0FFFFh THEN INC(pTab[i],a.ProcCo*4) END;
    END;
    write(pTab,a.ProcCo);
  ELSE
    IF ODD(a.ProcCo) THEN no:=a.ProcCo+1 ELSE no:=a.ProcCo END;
    i:=0;
    WHILE i<no DO
      n:=pTab[i]; m:=pTab[i+1];
      ASSERT((n<10000h) & (m<10000h));
      IF n#0FFFFh THEN INC(n,no*2) END;
      IF m#0FFFFh THEN INC(m,no*2) END;
      pTab[i DIV 2]:=INTEGER(BITSET(n)+BITSET(m<<16));
      INC(i,2);
    END;
    write(pTab,no DIV 2);
  END
END WriteProcTable;

PROCEDURE WriteConsts(a: Attrs; VAR consts: ARRAY OF WORD);
BEGIN write(consts,a.ConstsSize);
END WriteConsts;

PROCEDURE WriteCode(VAR a: Attrs; VAR Code: ARRAY OF WORD);
BEGIN write(Code,a.CodeSize);
END WriteCode;

PROCEDURE CloseCode;
BEGIN Check(Close(code));
END CloseCode;

END CodeFiles.
