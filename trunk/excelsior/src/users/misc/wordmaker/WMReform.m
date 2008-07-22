IMPLEMENTATION MODULE WMReform;
(* Reformating overlay for Wordmaker by Andrus Moor *)

FROM WMBuffer   IMPORT ModeSoftHyph,ModeSpacing,LineType,PosNo,CurLine,
                       LineBuf,LowCur,COfset,CColNr,
                       Overlay,Next_Proc,RMrgn,LMrgn,CLineNr,
                       Off_Line_Mode,VTravel,Ins_Line,Rem_Line,Delete,
                       LInsCorrect,LRemCorrect,MoveBytes,Travelling;
FROM WMScreen   IMPORT White_Space,Text_Beg,Retrieve;
FROM WMUtil     IMPORT GotoXY,DROP,Int_Type,Soft_CR;
FROM Terminal   IMPORT Clear;
FROM Misc       IMPORT Capital;

VAR
  AddedLines: INTEGER;

PROCEDURE SoftLine( VAR ln: LineType ): BOOLEAN;
(* True if soft carret RETURN in end of line *)

BEGIN
  RETURN (ln[0]<>0c) & (ln[ORD(ln[0])]=CHAR(Soft_CR));
END SoftLine;

PROCEDURE Vowel(ch:CHAR): BOOLEAN;

BEGIN
  ch:= Capital(ch);
  RETURN (ch='A') OR (ch='E') OR (ch='I') OR (ch='O') OR
          (ch='U') OR (ch='У') OR (ch='Ы') OR (ch='А') OR (ch='О')
         OR (ch='И');
END Vowel;

PROCEDURE Cons(ch: CHAR): BOOLEAN;
BEGIN
  ch:=Capital(ch);
  RETURN (NOT Vowel(ch)) & ((ch>='B') & (ch<='Z'));
END Cons;

PROCEDURE Hyphen( VAR w: LineType; VAR rem: INTEGER );
(*Try to hyphenate a word so that <=rem letters (including hyph sign)
remains in current line after hyphenation*)
(*Return position to start hyphenation or 1*)
(*Assumed that length of word > rem >= 3 on entry*)

CONST
  hypstart = 8; (*Start hyphenation if more letters remains*)
  Passive_Hyphen = 1fh;

VAR
  HypFound: BOOLEAN;
  ind: PosNo;

PROCEDURE Soft_Hyphen( VAR w: LineType; VAR rem: INTEGER );

VAR
  prevhyp, hyp: [1..255];
  found: BOOLEAN;

BEGIN (*Soft_Hyphen*)
  hyp:=1;
  REPEAT
    prevhyp:=hyp;
    INC(hyp,2);
    found:=FALSE;
    REPEAT
      (*Find consonant*)
      WHILE (hyp<=rem) & (NOT Cons(w[hyp])) DO
        INC(hyp); END;
      IF (hyp<ORD(w[0])) & (Vowel(w[hyp+1])) THEN
        found:= TRUE
        ELSE INC(hyp)
        END;
      UNTIL (hyp>rem) OR found;
    UNTIL (hyp>rem);
  rem:=prevhyp;
END Soft_Hyphen;

PROCEDURE User_Hyphen( VAR w: LineType; VAR rem: INTEGER );
(*Hyphenate a word in user-defined manner*)

VAR
  prevhyp, hyp, lpos: INTEGER;

BEGIN
  IF w[1]=CHAR(Passive_Hyphen) THEN rem:=1  (*Hyphen disabled by user*)
   ELSE
    hyp:=1;
    lpos:=1;
    REPEAT
      prevhyp:= hyp;
      INC(hyp);
      WHILE (lpos<rem) &(w[hyp]<>CHAR(Passive_Hyphen)) &(hyp<ORD(w[0])) DO
        INC(lpos);
        INC(hyp);
        END;
      UNTIL (lpos>=rem) OR (hyp>=ORD(w[0]));
    IF prevhyp=1 THEN rem:=1 ELSE rem:=prevhyp+1; END;
    END;
END User_Hyphen;

BEGIN (*Hyphen*)
  IF rem<hypstart THEN rem:=1 ELSE 
    HypFound := FALSE;
    ind := 1;
    WHILE (NOT HypFound) & (ind<=ORD(w[0])) DO
      HypFound:= w[ind]= CHAR(Passive_Hyphen);
      INC(ind); END;
    IF HypFound THEN User_Hyphen( w,rem )
      ELSIF ModeSoftHyph THEN Soft_Hyphen( w,rem ); 
      ELSE rem:= 1; END;
    END;
END Hyphen;

PROCEDURE DistributeSpaces();
(* Expand line buffer to current low memory *)

VAR
  lpos: CARDINAL;      (*Current logical position*)
  ppos: PosNo;         (*        physical position*)
  delimc,              (*Word deliminators in line*)
  wordt,               (*Times to insert spaces between words*)
  spaces: [0..255];      (* # of spaces  to insert between words*)
  delim: ARRAY [1..128] OF PosNo;
  remains,
  spins, insc: [0..254];
  sourcep,
  destp: PosNo;
  ind: PosNo;

PROCEDURE Scan( ch: CHAR; VAR lpos: CARDINAL );
(*Scan a char*)

BEGIN
  IF ch<' ' THEN
    CASE ORD(ch) OF
      8: IF lpos>1 THEN
           DEC(lpos); END;
      |13: lpos := 1;
     ELSE ;
     END;
   ELSE
    INC( lpos );
    END;
END Scan;

BEGIN   (*Distribution*)
  IF NOT ModeSpacing THEN (*spacing disabled*)
    LowCur^.txt:= LineBuf;
    RETURN; END;
  delimc := 0;
  lpos := Text_Beg(LineBuf);

  (*Scan source line by counting all spaces*)
  FOR ppos:=lpos TO ORD(LineBuf[0]) DO
      Scan( LineBuf[ppos], lpos );
      IF LineBuf[ppos]=' ' THEN
        (*Deliminator scanned*)
        INC(delimc);
        delim[delimc] := ppos;
        (*if specdel THEN
              INC(sdelimc);
              delim[delimc].spec := TRUE;
          ELSE
            delim[delimc]. spec := FALSE; *)
        END;
      END;
  IF (delimc=0) OR (lpos>RMrgn) THEN
    (*Single word in source line or line too long*)
    LowCur^.txt := LineBuf;
    RETURN;
    END;
    
  (*Compute # of spaces to insert*)
  spins := RMrgn - lpos + 1;
  remains := spins;
  wordt := remains MOD delimc;
  spaces := remains DIV delimc + 1;

  (*Copy source line by inserting spaces*)
  sourcep := 1;
  destp := 1;
  FOR delimc := 1 TO delimc DO
    MoveBytes( delim[delimc]-sourcep+1,LineBuf,LowCur^.txt,sourcep, destp);
    INC(destp, delim[delimc] - sourcep + 1);
    sourcep := delim[delimc] + 1;
    IF wordt>0 THEN
      insc := spaces;
      DEC( wordt );
     ELSE
      insc := spaces - 1;
      END;
   FOR ind:=destp TO destp+insc DO LowCur^.txt[ind]:=' '; END;
   INC(destp, insc);
   END;
  MoveBytes( ORD(LineBuf[0])-sourcep+1,LineBuf,LowCur^.txt,sourcep,destp);
  LowCur^.txt[0] := CHAR( ORD(LineBuf[0])+spins );
END DistributeSpaces;

PROCEDURE Reformat();
(* Reformat until end of paragraph *)

VAR 
  endp: BOOLEAN;        (*True if last line in paragraph*)
  tcount: CARDINAL;     (* # of lines moved *)
  bufp: PosNo;          (*Current free pos in line buffer*)
                        (*For first line indention*)
  startp: PosNo;        (*Start of scanning in current line*)
  lastword: LineType;

PROCEDURE Word_Distribute(): BOOLEAN;
(*Copy words to line buffer to fill line into right margin*)
(*Try to hyphenate last word in line*)
(*False on end of paragraph*)

CONST Actine_Hyphen=2dh;     (*-*)

VAR
  diff: INTEGER;
  cnt: CARDINAL;

PROCEDURE Get_Full_Word(): BOOLEAN;
(*Get next word to last word buffer. Replace active hyphens*)
(*True if word exists*)
(*Global startp marks start of word in current line*)

VAR freep, wbufp: PosNo;

PROCEDURE Get_Word( VAR startp, freep: PosNo ): BOOLEAN;
(*Scan next word and ret info about it*)
(*False on end of paragraph (no next word exists) *)

PROCEDURE Line_Present(): BOOLEAN;
(*Get next line if needed by deleting current line *)
(*False if current line is last in paragraph and no next word exists *)

BEGIN
  IF startp>ORD(CurLine^ .txt[0]) THEN
    IF NOT Rem_Line() THEN RETURN FALSE; END;
    DEC(AddedLines);
    IF endp THEN RETURN FALSE; END;
    WITH CurLine^ DO
      IF NOT SoftLine(txt) THEN endp := TRUE; END;
      startp:= 1;
      RETURN txt[0]<>0c;
      END;
  ELSE
    RETURN TRUE; END;
END Line_Present;

BEGIN  (*Get_Word*)
  IF NOT Line_Present() THEN RETURN FALSE; END;
  (*Scan deliminators*)
  WHILE White_Space(CurLine^.txt[startp])
    OR (CurLine^.txt[startp]=CHAR(Soft_CR)) DO
    INC(startp);
    IF (startp-1)>=ORD(CurLine^.txt[0]) THEN
      IF NOT Line_Present() THEN RETURN FALSE END;
      END;
    END;
  (*Scan word*)
  freep := startp;
  WITH CurLine^ DO
    REPEAT
      INC(freep);
      UNTIL White_Space(txt[freep]) OR (freep>ORD(txt[0]))
        OR (txt[freep]=CHAR(Soft_CR));
    END;
  RETURN TRUE;
END Get_Word;

BEGIN (*Get_Full_Word*)
  IF lastword[0]<>0c THEN
    RETURN TRUE
  ELSE
    wbufp:=2;
    REPEAT
      DEC(wbufp);       (*Remove hyphenation sign*)
      IF NOT Get_Word(startp,freep) THEN
        lastword[0] := CHAR(wbufp-1);
        RETURN lastword[0]<>0c;
        END;
      MoveBytes( freep-startp,CurLine^.txt,lastword,startp, wbufp);
      INC(wbufp, freep-startp);
      startp:=freep;
      UNTIL (lastword[wbufp-1]<>CHAR(Actine_Hyphen)) OR (wbufp<3);
    lastword[0] := CHAR(wbufp-1);
    RETURN TRUE;
    END;
END Get_Full_Word;

PROCEDURE Copy_Word( chcount: INTEGER );
(*Copy letters from last word to line buffer, include a space after word*)
VAR ind: CARDINAL;

BEGIN
  MoveBytes(chcount,lastword,LineBuf,1,bufp);
  Delete(lastword,1,chcount);
  (*MoveLeft( lastword, lastword, chcount+1, 1, ORD(lastword[0])-chcount);
  lastword[0] := CHAR(ORD(lastword[0]) -chcount ); *)
  INC(bufp,chcount);
  LineBuf[bufp]:=' ';
  INC(bufp);
END Copy_Word;

BEGIN  (*Word_Distribute*)
  (*Copy leading spaces*)
  (* fillchar( LineBuf[1], bufp-1, ' ' ); *)
  FOR cnt:= 1 TO bufp-1 DO LineBuf[cnt] := ' '; END;
  IF NOT Get_Full_Word() THEN (*No line to copy*)
    LineBuf[0] := 0c;
    RETURN FALSE;
    END;

  REPEAT
    Copy_Word( ORD(lastword[0]));
    IF Get_Full_Word() THEN
      diff:=bufp+ORD(lastword[0])-RMrgn;
      IF diff>1 THEN
        (*Line buffer is full, try to hyphen*)
        diff := RMrgn-bufp;
        Hyphen( lastword, diff );
        IF diff=1 THEN (*Can't hyphenate*)
          LineBuf[ bufp-1 ] := CHAR(Soft_CR);
          LineBuf[0] := CHAR(bufp-1);
        ELSE (*Hyphenation is possible*)
          Copy_Word(diff-1);
          LineBuf[bufp-1]:=CHAR(Actine_Hyphen);
          LineBuf[bufp]:= CHAR(Soft_CR);
          LineBuf[0]:= CHAR(bufp);
          END;
        RETURN TRUE;
        END; (*If line buffer full*)
    ELSE
      (*End of paragraph*)
        LineBuf[0] := CHAR(bufp-2);
        RETURN FALSE;
      END;
  UNTIL FALSE;
END Word_Distribute;

BEGIN   (*Reformat*)
  Off_Line_Mode();
  endp := NOT SoftLine(CurLine^.txt);
  tcount := 0;
  bufp := LMrgn;
  AddedLines:= 0;
  IF VTravel(FALSE) THEN
    IF NOT SoftLine( CurLine^.txt ) THEN
      (*Previous line is not part of current paragraph*)
      ASSERT( VTravel(TRUE) );
      WITH CurLine^ DO
        IF (txt[0]<>0c) & (txt[1]=' ') THEN INC(bufp,4) END;
      END;
    ELSE ASSERT( VTravel(TRUE) );
    END;
  ELSE INC(bufp,4);
  END;
  lastword[0] := 0c;
  startp:=1;
  LOOP IF NOT Word_Distribute() THEN EXIT END;
    DistributeSpaces();
    IF Ins_Line() THEN EXIT; END;
    INC( AddedLines);
    IF VTravel(TRUE) THEN INC(tcount); END;
    bufp := LMrgn;
    END;
  LowCur^ .txt := LineBuf;
  IF NOT Ins_Line() THEN INC(AddedLines); END;
  IF AddedLines>0 THEN LInsCorrect(AddedLines)
    ELSE LRemCorrect(-AddedLines); END;
  ASSERT(Travelling(-tcount)=tcount);
  COfset := 0;
  CColNr := 1;
  GotoXY( CLineNr, 1 );
  Clear();
  Retrieve( eos );
  Overlay := pEdit;
END Reformat;

END WMReform.
