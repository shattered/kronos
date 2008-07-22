IMPLEMENTATION MODULE abcTerm; (* Hady. 28-Apr-88. (c) KRONOS *)

FROM Strings    IMPORT  Len, Str0, Str1, Truncate, App, SubStr
                      , InsCh, DelCh, Str2, AppStr;
FROM Terminal   IMPORT  SetCrs, Home, Clear, ClearLine, Write
                      , WriteString, print, RollDw, DC
                      , IC, Left, Right, Reverse;
FROM abcDefs    IMPORT  INFO, WRK, SCR, SCRH, SCRW, LFTM;
FROM Misc       IMPORT  Latin?, Kiril?, Bold, Special?;
FROM Keyboard   IMPORT  ReadKey, uppg, dwpg, up, dw, left
                      , break, cr, lf, newln, delc, insc
                      , del, ltab, rtab, right, gold, lpull, rpull
                      , silver, bronze, delln, insln;
FROM ASCII      IMPORT  CR, BEL, DEL;
FROM Image      IMPORT  image0, GetNum;
FROM SYSTEM     IMPORT  ADR;
FROM Heap       IMPORT  ALLOCATE, DEALLOCATE;

CONST MOVE = 16;

TYPE Lines = [0..SCRH];
     Stack = POINTER TO State;
     State = RECORD
               pmt : ARRAY [0..31] OF CHAR;
               str : ARRAY [0..255] OF CHAR;
               crs, shad, fin: INTEGER;
               next: Stack;
             END;

VAR (* temporary *)
    ON_OFF: ARRAY BOOLEAN OF ARRAY [0..4] OF CHAR;
   Lat_Kir: ARRAY BOOLEAN OF ARRAY [0..7] OF CHAR;
       tmp: ARRAY [0..255] OF CHAR;
   INVERSE: BOOLEAN;

VAR
(**************** S C R E E N   S T A T E *******************)
    Screen: ARRAY Lines OF ARRAY [0..255] OF CHAR;
                                       (* состояние экрана  *)
    Margin: ARRAY Lines OF INTEGER;(* видимые границы строк *)
   Written: ARRAY Lines OF CHAR;   (*отметка о записи в файл*)
      info: ARRAY [0..80] OF CHAR; (* состояние инфо-строки *)
      Line: Lines;      (* номер выбранной строки на экране *)
     First: INTEGER;    (* номер первой строки на экране    *)
  infoBell: BOOLEAN;    (* звонок on-off                    *)
 infoLatin: BOOLEAN;    (* клавиатура Latin-Кирил           *)
  infoRoll: BOOLEAN;    (* роллинг on-off                   *)
infoOn_Off: BOOLEAN;    (* инфо-строка on-off               *)
(************** E D I T   L I N E   S T A T E ***************)
    CURSOR: INTEGER;    (* положение курсора в строке       *)
 pmtLength: INTEGER;    (* длинна текущего приглашения      *)
    SHADOW: INTEGER;    (* номер первого видимого символа   *)
    PROMPT: ARRAY  [0..31] OF CHAR; (* текущее приглашение  *)
    CharsL,                         (* наборы символов      *)
    CharsK: ARRAY CHAR OF CHAR;     (* кириллицы и латинницы*)
   ChCoder: POINTER TO ARRAY CHAR OF CHAR; (* выбор набора  *)


VAR stk: Stack;

(******************* I N F O   L I N E  S U P P O R T ************************
0         1         2         3         4         5         6
01234567890123456789012345678901234567890123456789012345678901234567890123456789
<<<                  Write off  Latin  Bell off Roll off File aaaaaaaaaaaaa >>>
******************************************************************************)

PROCEDURE onWrk; FORWARD;

PROCEDURE Info!(on_off: BOOLEAN);
BEGIN
  infoOn_Off:=on_off;
  RefreshInfo; onWrk;
END Info!;

PROCEDURE Info?(): BOOLEAN;
BEGIN RETURN infoOn_Off END Info?;

PROCEDURE insStr(s: ARRAY OF CHAR; from,len: INTEGER);
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO len DO info[i+from]:=s[i] END;
  IF infoOn_Off THEN
    i:=Reverse(1);
    SetCrs(INFO,from);
    WriteString(s);
    i:=Reverse(i);
  END;
  onWrk;
END insStr;

PROCEDURE Write!(on_off: BOOLEAN);
BEGIN
  insStr(ON_OFF[on_off],27,2);
END Write!;

PROCEDURE Roll!(on_off: BOOLEAN);
BEGIN
  infoRoll:=on_off;
  insStr(ON_OFF[infoRoll],53,2);
END Roll!;

PROCEDURE Roll?():  BOOLEAN;
BEGIN RETURN infoRoll END Roll?;

PROCEDURE File!(s: ARRAY OF CHAR);
  VAR i: INTEGER;
      t,t1: ARRAY [0..15] OF CHAR;
BEGIN
  IF s[0]=0c THEN i:=0; Str0(t);
    WHILE i<14 DO App(t,' '); INC(i) END;
  ELSE
    Str1(t1,s); Str0(t); i:=(14-Len(t1)) DIV 2;
    WHILE i>0 DO App(t,' '); DEC(i) END;
    AppStr(t,t1); i:=Len(t);
    WHILE i<14 DO App(t,' '); INC(i) END;
  END;
  insStr(t,62,14);
END File!;

PROCEDURE Bell!(on_off: BOOLEAN);
BEGIN
  infoBell:=on_off;
  insStr(ON_OFF[infoBell],44,2);
END Bell!;

PROCEDURE Bell?():  BOOLEAN;
BEGIN RETURN infoBell END Bell?;

PROCEDURE latin!(on_off: BOOLEAN);
BEGIN
  infoLatin:=on_off;
  IF infoLatin THEN ChCoder:=ADR(CharsL)
  ELSE ChCoder:=ADR(CharsK)
  END;
  insStr(Lat_Kir[infoLatin],32,4);
END latin!;

PROCEDURE latin?():BOOLEAN;
BEGIN RETURN infoLatin END latin?;

PROCEDURE RefreshInfo();
  VAR i: INTEGER;
BEGIN
  SetCrs(INFO,0);
  IF infoOn_Off THEN
    i:=Reverse(1);
    SetCrs(INFO,0); WriteString(info);
    i:=Reverse(i);
    Clear;
  ELSE Clear;
  END;
END RefreshInfo;

(********** W O R K   L I N E   S U P P O R T **************)

PROCEDURE SaveWrk;
  VAR t: Stack;
BEGIN
  ALLOCATE(t,SIZE(State)); ASSERT(t#NIL);
  t^.pmt := PROMPT; t^.str := str_work;
  t^.crs:=CURSOR;   t^.shad:=SHADOW;
  t^.fin:=FINAL;
  t^.next:=stk;     stk:=t;
END SaveWrk;

PROCEDURE RestoreWrk;
  VAR t: Stack;
BEGIN
  ASSERT(stk#NIL);
  PROMPT:=stk^.pmt;
  CURSOR:=stk^.crs;
  SHADOW:=stk^.shad;
  FINAL :=stk^.fin;
  pos:=CURSOR+SHADOW;
  str_work:=stk^.str;
  pmtLength:=Len(PROMPT);
  t:=stk;
  stk:=stk^.next;
  DEALLOCATE(t,SIZE(State));
  RefreshWrk();
END RestoreWrk;

PROCEDURE RefreshWrk();
BEGIN
  SetCrs(WRK,0);
  IF REFRESH THEN
    SubStr(str_work,SHADOW,Len(str_work),tmp);
    WriteString(PROMPT); WriteString(tmp);
    ClearLine; SetCrs(WRK,CURSOR+pmtLength);
  ELSE ClearLine;
  END;
END RefreshWrk;

PROCEDURE Prompt(VAL s: ARRAY OF CHAR);
BEGIN
  Str1(PROMPT,s); pmtLength:=Len(PROMPT);
  SetCrs(WRK,0);  WriteString(s);
  ClearLine;
END Prompt;

PROCEDURE onWrk();
BEGIN SetCrs(WRK,CURSOR+pmtLength) END onWrk;

(************* S C R E E N    S U P P O R T ****************)

PROCEDURE ChoosLine(l: Lines);
  VAR i,j: INTEGER;
BEGIN
  j:=(l+First) MOD (SCRH+1);
  SetCrs(Line+SCR,0); Write(' ');
  SetCrs(l+SCR,0);
  IF INVERSE THEN
    i:=Reverse(1);
    Write(Written[j]);
    i:=Reverse(i)
  ELSE
    IF Written[j]=' ' THEN Write(DEL);
    ELSE Write(Written[j])
    END;
  END;
  Line:=l;
  onWrk;
END ChoosLine;

PROCEDURE Home!; BEGIN ChoosLine(0) END Home!;

PROCEDURE UP(); BEGIN ChoosLine((Line+SCRH) MOD (SCRH+1)) END UP;

PROCEDURE DW(); BEGIN ChoosLine((Line+1) MOD (SCRH+1)) END DW;

PROCEDURE WriteStr(s: ARRAY OF CHAR);
  VAR i: INTEGER;
BEGIN
  i:=(Line+First) MOD (SCRH+1);
  Str1(Screen[i],s); Margin[i]:=0; Written[i]:=' ';
  IF Len(Screen[i])>SCRW THEN Truncate(s,SCRW) END;
  SetCrs(Line+SCR,LFTM);
  WriteString(s);
  ClearLine;
END WriteStr;

PROCEDURE RefreshScreen();
  VAR i,l: INTEGER;
      s  : ARRAY [0..SCRW+1] OF CHAR;
BEGIN
  RefreshInfo;
  SetCrs(WRK+1,0); ClearLine;
  FOR i:=0 TO SCRH DO
    l:=(i+First) MOD (SCRH+1);
    SubStr(Screen[l],Margin[l],SCRW+Margin[l]-1,s);
    SetCrs(i+SCR,LFTM);
    WriteString(s); ClearLine;
  END;
  ChoosLine(Line);
END RefreshScreen;

PROCEDURE GetLine(VAR s: ARRAY OF CHAR);
BEGIN
  Str1(s,Screen[(Line+First) MOD (SCRH+1)])
END GetLine;

PROCEDURE Bell;
BEGIN
  IF infoBell THEN Write(BEL) END
END Bell;

PROCEDURE WriteMess(VAL s: ARRAY OF CHAR);
  VAR i: INTEGER;
BEGIN
  i:=Reverse(1);
  SetCrs(INFO, 0);
  print('>>> %60.60s >>>         ',s);
  i:=Reverse(i);
END WriteMess;

PROCEDURE MarkString(c: CHAR);
  VAR i: INTEGER;
BEGIN
  i:=Reverse(1);
  SetCrs(Line+SCR,0);
  Write(c);
  i:=Reverse(i);
  onWrk;
  Written[(Line+First) MOD (SCRH+1)]:=c;
END MarkString;

(**************** E D I T   S T R I N G *******************)

VAR Operate_Key: ARRAY CHAR OF keyProc;

PROCEDURE InitChars();
  VAR ch,t: CHAR;
BEGIN
  FOR ch:=0c TO 377c DO CharsL[ch]:=ch; CharsK[ch]:=ch; END;
  FOR ch:=0c TO 177c DO t:=CHAR(INTEGER(ch)+200b);
    IF Latin?(ch) OR Special?(ch) THEN
      CharsL[ch]:=Bold(ch);
      CharsL[t]:=CharsL[ch];
    ELSE CharsL[ch]:=0c; CharsL[t]:=0c;
    END;
  END;
  FOR ch:=200c TO 377c DO t:=CHAR(INTEGER(ch)-200b);
    IF Kiril?(ch) THEN
      CharsK[ch]:=Bold(ch);
      CharsK[t]:=CharsK[ch];
    ELSIF NOT Special?(ch) THEN
      CharsK[ch]:=0c; CharsK[t]:=0c;
    ELSE CharsK[ch]:=ch;
    END;
  END;
END InitChars;

PROCEDURE MoveStart();
BEGIN CURSOR:=0; pos:=0;
  IF SHADOW=0 THEN SetCrs(WRK, pmtLength);
  ELSE
    Str1(tmp, str_work); Truncate(tmp, SCRW-pmtLength);
    SetCrs(WRK,pmtLength); WriteString(tmp); ClearLine;
    SetCrs(WRK,pmtLength); SHADOW:=0;
  END;
END MoveStart;

PROCEDURE MoveFin();
  VAR i: INTEGER;
      r: BOOLEAN;
BEGIN
  i:=pos; pos:=Len(str_work); r:=FALSE;
  WHILE
    pos+pmtLength-SHADOW>SCRW
  DO INC(SHADOW); r:=TRUE END;
  IF r THEN
    SubStr(str_work, SHADOW, pos, tmp);
    SetCrs(WRK, pmtLength);
    WriteString(tmp); ClearLine;
  ELSE
    SubStr(str_work, i, pos-1, tmp);
    WriteString(tmp);
  END;
  CURSOR:=pos-SHADOW;
END MoveFin;

PROCEDURE MoveRight();
BEGIN
  IF pos>=HIGH(str_work)-2 THEN RETURN END;
  IF pos<Len(str_work) THEN
  ELSE
    str_work[pos]:=' ';
    str_work[HIGH(str_work)]:=CHAR(pos+1);
  END;
  IF CURSOR+pmtLength>=SCRW THEN SetCrs(WRK,pmtLength);
    IF DC() THEN END; INC(SHADOW);
    SetCrs(WRK,pmtLength+CURSOR-1);
    Write(str_work[pos]);
  ELSE Write(str_work[pos]); INC(CURSOR);
  END;
  INC(pos);
END MoveRight;

PROCEDURE MoveLeft();
BEGIN
  IF pos>0 THEN DEC(pos);
    IF CURSOR=0 THEN SetCrs(WRK, pmtLength);
      IF IC() THEN END; DEC(SHADOW);
      Write(str_work[pos]); Left;
    ELSE Left; DEC(CURSOR);
    END;
  END;
END MoveLeft;

PROCEDURE TabLeft();
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO 7 DO MoveLeft END;
END TabLeft;

PROCEDURE TabRight();
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO 7 DO MoveRight END;
END TabRight;

PROCEDURE CharIns();
BEGIN
  str_work[Len(str_work)]:=0c;
  InsCh(str_work,pos);
  IF IC() THEN END;
END CharIns;

PROCEDURE CharDel();
  VAR i: INTEGER;
BEGIN
  str_work[Len(str_work)]:=0c;
  DelCh(str_work,pos);
  IF DC() THEN END;
  IF Len(str_work)>=SCRW+SHADOW-pmtLength THEN
    SetCrs(WRK,SCRW-1);
    Write(str_work[SCRW+SHADOW-pmtLength-1]);
    SetCrs(WRK,CURSOR+pmtLength);
  END;
END CharDel;

PROCEDURE Deleet();
BEGIN
  IF pos>0 THEN
    DEC(pos); str_work[pos]:=' ';
    IF CURSOR=0 THEN
      IF IC() THEN END; DEC(SHADOW);
    ELSE Left; Write(' '); Left; DEC(CURSOR);
    END;
  END;
END Deleet;

PROCEDURE dummyKEY(c: CHAR): BOOLEAN;
BEGIN    RETURN FALSE   END dummyKEY;

PROCEDURE Char(ch: CHAR);
BEGIN
  IF ch=0c THEN RETURN END;
  IF pos<HIGH(str_work)-2 THEN
    str_work[pos]:=ch;
    IF CURSOR+pmtLength>=SCRW THEN
      SetCrs(WRK,pmtLength);
      IF DC() THEN END;
      INC(SHADOW);
      SetCrs(WRK,pmtLength+CURSOR-1);
      Write(ch);
    ELSE INC(CURSOR); Write(ch);
    END;
    INC(pos);
    IF pos>=Len(str_work) THEN
      str_work[HIGH(str_work)]:=CHAR(pos)
    END;
  END;
END Char;

PROCEDURE SwitchLat();
BEGIN latin!(NOT infoLatin) END SwitchLat;

PROCEDURE final();
BEGIN Truncate(str_work,pos);
  ClearLine; FINAL:=normal;
END final;

PROCEDURE dummy(); BEGIN END dummy;

PROCEDURE StandReaction(ch: CHAR);
BEGIN Operate_Key[ch] END StandReaction;

PROCEDURE StandRead(): CHAR;
  VAR ch: CHAR;
BEGIN
  char:=ReadKey(); ch:=ChCoder^[char];
--IF ch#0c THEN RETURN ch
  IF ch#0c THEN RETURN char  -- Sem 20-Aug-89
  ELSE Operate_Key[char]; RETURN 0c;
  END;
END StandRead;

PROCEDURE ReadWork(pmt: ARRAY OF CHAR;
                VAR st: ARRAY OF CHAR; sp: Way);
  VAR ch: CHAR;
BEGIN
  Prompt(pmt); FINAL:=-1;
  CASE sp OF
    |old: Str1(str_work,st);
    |new: Str0(str_work);
    |conf: Str1(str_work,st); pos:=Len(str_work); SHADOW:=0;
           IF pos+pmtLength>SCRW THEN
             WHILE
               pos-SHADOW>SCRW-pmtLength
             DO INC(SHADOW) END;
             SubStr(str_work, SHADOW, pos, tmp);
             WriteString(tmp); CURSOR:=Len(tmp);
           ELSE CURSOR:=pos; WriteString(str_work);
           END;
  ELSE ASSERT(FALSE,4Ah);
  END;
  REPEAT Char(StandRead()) UNTIL FINAL>0;
  IF FINAL=update THEN Str0(st)
  ELSE Str1(st,str_work)
  END;
  Str0(str_work); pos:=0; SHADOW:=0; CURSOR:=0;
END ReadWork;

PROCEDURE RollDown();
BEGIN Home;
  IF REFRESH THEN ClearLine END;
  RollDw;
  IF REFRESH THEN
    RefreshWrk; RefreshInfo;
  END;
  First:=(First+SCRH) MOD (SCRH+1);
END RollDown;

PROCEDURE ShowString(VAL s: ARRAY OF CHAR);
  VAR t: ARRAY [0..SCRW+1] OF CHAR;
      i: INTEGER;
BEGIN
  IF infoRoll THEN
    SetCrs(Line+SCR,0); Write(' ');
    RollDown;
    ChoosLine(0);
  ELSE DW;
  END;
  i:=(Line+First) MOD (SCRH+1);
  Str1(Screen[i],s);
  Margin[i]:=0;
  Written[i]:=' ';
  SetCrs(Line+SCR,LFTM);
  IF Len(Screen[i])>SCRW THEN
    Str1(t,s); WriteString(t);
  ELSE WriteString(s);
  END;
  ClearLine;
END ShowString;

PROCEDURE PoolString();
  VAR i: INTEGER;
BEGIN
  i:=(Line+First) MOD (SCRH+1);
  IF Margin[i]+MOVE<Len(Screen[i]) THEN
    INC(Margin[i],MOVE);
    SubStr(Screen[i],Margin[i],Margin[i]+SCRW-1,tmp);
    SetCrs(Line+SCR,LFTM); WriteString(tmp); ClearLine;
  END;
  onWrk;
END PoolString;

PROCEDURE PushString();
  VAR i: INTEGER;
BEGIN
  i:=(Line+First) MOD (SCRH+1);
  IF Margin[i]>=MOVE THEN
    DEC(Margin[i],MOVE);
    SubStr(Screen[i],Margin[i],Margin[i]+SCRW-1,tmp);
    SetCrs(Line+SCR,LFTM); WriteString(tmp); ClearLine;
  END;
  onWrk;
END PushString;

(************* I N I T I A L I S A T I O N **************)

PROCEDURE InitInfo();
BEGIN
  Str1(info,'<<<                  WRITE            '
            ' BELL     ROLL     FILE               >>>');
  infoOn_Off:=FALSE;
  Bell!(TRUE); latin!(TRUE);
  Roll!(TRUE); Write!(FALSE);
  infoOn_Off:=TRUE;
  RefreshInfo;
END InitInfo;

PROCEDURE SetReaction(key: CHAR; p: keyProc);
BEGIN Operate_Key[key]:=p    END SetReaction;

PROCEDURE InitOperate_Key();
  VAR c: CHAR;
BEGIN
  FOR c:=0c TO 377c DO Operate_Key[c]:=dummy END;
  Operate_Key [uppg] := MoveStart;
  Operate_Key [dwpg] := MoveFin  ;
  Operate_Key [left] := MoveLeft ;
  Operate_Key[right] := MoveRight;
  Operate_Key [rtab] := TabRight;
  Operate_Key [ltab] := TabLeft;
  Operate_Key  [cr]  := final;
  Operate_Key  [lf]  := final;
  Operate_Key[newln] := final;
  Operate_Key  [up]  := UP;
  Operate_Key  [dw]  := DW;
  Operate_Key [insc] := CharIns;
  Operate_Key [delc] := CharDel;
  Operate_Key [del]  := Deleet ;
  Operate_Key[bronze]:= SwitchLat;
  Operate_Key[rpull] := PushString;
  Operate_Key[lpull] := PoolString;
END InitOperate_Key;

PROCEDURE InitScreen;
  VAR i: INTEGER;
BEGIN Home; Clear;
  FOR i:=0 TO SCRH DO
    Str0(Screen[i]);
    Margin[i]:=0;
    Written[i]:=' ';
  END;
  CURSOR:=0;     SHADOW:=0;     pos:=0;
  First:=0;      Prompt('');
  InitInfo ;     ChoosLine(0);
  REFRESH:=TRUE; InitOperate_Key();
  InitChars();   ChCoder:=ADR(CharsL);
  i:=Reverse(1);
  IF i>=0 THEN INVERSE:=TRUE; i:=Reverse(i)
  ELSE INVERSE:=FALSE
  END
END InitScreen;

PROCEDURE SaveScreen(ws: strProc);
  VAR i: INTEGER; s: ARRAY [0..79] OF CHAR;
BEGIN
  image0(s,'%d',First); ws(s);
  FOR i:=0 TO SCRH DO ws(Screen[(i+First) MOD (SCRH+1)]) END;
  Str1(s,Written);
  i:=0;
  WHILE (i<HIGH(s)) & (s[i]#0c) DO
    IF s[i]=' ' THEN s[i]:='_' END; INC(i);
  END; ws(s);
END SaveScreen;

PROCEDURE resScreen(gs: getString): INTEGER;
  VAR i,r: INTEGER; s: ARRAY [0..255] OF CHAR;
BEGIN
  r:=gs(s); IF r<0 THEN RETURN r END;
  r:=GetNum(s,i);
  IF r>=0 THEN First:=i ELSE RETURN -1 END;
  i:=0;
  WHILE i<=SCRH DO
    r:=gs(s); IF r<0 THEN RETURN r END;
    Str1(Screen[(First+i) MOD (SCRH+1)],s); INC(i);
  END;
  r:=gs(s); IF r<0 THEN RETURN r END;
  i:=0;
  WHILE (i<HIGH(s)) & (i<=HIGH(Written)) & (s[i]#0c) DO
    IF s[i]='_' THEN Written[i]:=' ' ELSE Written[i]:=s[i] END;
    INC(i);
  END;
  RefreshScreen;
  RETURN 0;
END resScreen;

VAR i: INTEGER;

BEGIN
  Str1(ON_OFF[FALSE],'off'); Str1(ON_OFF[TRUE],' on');
  Str1(Lat_Kir[FALSE],'Kiril'); Str1(Lat_Kir[TRUE],'Latin');
END abcTerm.
