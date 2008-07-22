IMPLEMENTATION MODULE Music; (* Shu 29-Oct-86. (c) KRONOS *)
                             (* Shu 21-Dec-86. (c) KRONOS *)

IMPORT Scheduler, StdIO, KRONOS, Image, Strings, SYSTEM;

FROM StdIO     IMPORT   GetS, Write;

CONST

  sharp   = '#';  bemol = 'b';
  fixsharp= '@';
  paus    = 'P';  time  = '|';
  between = "'";  becar = '%';

 (*  control language symbolws *)
  sChng   = "{";  eChng = "}";
  tgth    = "_";  stgth = '<';  etgth = '>';


  min     = -20;  max   = 40;


TYPE
  Sound = RECORD
    dou: INTEGER;
    hei: INTEGER;
  END;

  SoundList = POINTER TO ListRec;
  ListRec = RECORD
    note: Sound;
    next: SoundList;
  END;

  Accord = RECORD
    dou:  INTEGER;
    list: SoundList;
  END;


  VAR
    Sounds : ARRAY [-1..12] OF INTEGER;
    Transl: ARRAY [0..6] OF INTEGER;

    Fixeds: ARRAY [min..max] OF CHAR;
    CurrOct : INTEGER;
    CurrPart: INTEGER;
    Together: BOOLEAN;
    Unit: INTEGER;


MODULE miscellanios;

IMPORT  SoundList, ListRec;

FROM SYSTEM    IMPORT   ADR;
FROM StdIO     IMPORT   print, Why?, Show, WriteLn, Write, Stream;
FROM Image     IMPORT   image;
FROM Strings   IMPORT   Str0;


EXPORT  CurrDou, CurrHei, FreeSound, SetHei, SetDou, ClearMem;

  CONST MemSize = 100;

  VAR
    CurrDou,CurrHei: INTEGER;

    SoundMem: ARRAY [0..MemSize-1] OF ListRec;
    MemTop  : INTEGER;


  PROCEDURE Chk(r: INTEGER): INTEGER;
    VAR msg: ARRAY [0..80] OF CHAR;
  BEGIN
    IF r<0 THEN Why?(r,msg); Show(msg); HALT ELSE RETURN r END;
  END Chk;

  PROCEDURE priv(s: ARRAY OF CHAR);
  BEGIN
    print("" 33c "?%sT",s);
  END priv;

  PROCEDURE SetDou(msk: INTEGER);
    VAR s: ARRAY [0..10] OF CHAR;
  BEGIN
  (*IF CurrDou=msk THEN RETURN END;*)
    Str0(s); image(s,"42;%d",msk DIV 20); priv(s);
    CurrDou:=msk;
  END SetDou;

  PROCEDURE SetHei(frk: INTEGER);
    VAR s: ARRAY [0..10] OF CHAR;
  BEGIN
  (*IF CurrHei=frk THEN RETURN END;*)
    Str0(s); image(s,"41;%d",frk); priv(s);
    CurrHei:=frk;
  END SetHei;

  PROCEDURE FreeSound(): SoundList; BEGIN
    IF MemTop=MemSize THEN Show("Переполнение пула нот."); HALT END;
    INC(MemTop); RETURN ADR(SoundMem[MemTop-1])
  END FreeSound;

  PROCEDURE ClearMem; BEGIN
    MemTop:=0
  END ClearMem;

END miscellanios;


MODULE SimplePlays;

IMPORT Together, Notes, Accord, Sound, SoundList;

FROM miscellanios IMPORT  SetHei, SetDou;
FROM Scheduler IMPORT   Ticker;
FROM StdIO     IMPORT   Bell;

EXPORT PlaySound, PlayAccord;

  PROCEDURE PlaySound(n: Sound);
    VAR t2,t: INTEGER;
  BEGIN
    SetHei(n.hei);
    IF NOT Together THEN
      SetDou(n.dou);
      IF n.hei # INTEGER(MAX(Notes))+1 THEN Bell END;
    END;
    t:=Ticker();
    t2 := t + n.dou DIV 20;
    REPEAT UNTIL (Ticker() >= t2);
  END PlaySound;

  PROCEDURE PlayAccord(a: Accord);
    VAR p: SoundList;
      t2: INTEGER;
  BEGIN
    SetDou(0); Bell;
    t2 := Ticker() + a.dou DIV 20;
    REPEAT
      p:=a.list;
      ASSERT(p#NIL);
      WHILE p^.next#NIL DO
        SetHei(p^.note.hei); Bell;
        p:=p^.next;
      END;
    UNTIL Ticker() > t2;
    SetDou(20); Bell;
  END PlayAccord;

END SimplePlays;


MODULE Convertions;

IMPORT Sounds, sharp, bemol, CurrOct, Transl, CurrPart, between
     , Fixeds, Sound, Notes, Note, paus, Unit;

FROM Image     IMPORT   PeekNum;
FROM KRONOS    IMPORT   SHL, SHR;
FROM StdIO     IMPORT   Show;


  EXPORT MakeSound, NoteSound, PackNote;


  PROCEDURE MakeSound(VAR n: Sound; img: ARRAY OF CHAR);
   VAR
     h,m,m2,OctOffs: INTEGER; p: INTEGER;
     c: CHAR;

    PROCEDURE Infl(): CHAR; BEGIN
      IF img[p]#0c THEN INC(p); RETURN img[p-1] ELSE RETURN Fixeds[m] END;
    END Infl;

    PROCEDURE TakeHei(h,OctOffs: INTEGER; c: CHAR): INTEGER;
    BEGIN
      IF c=sharp THEN INC(h) ELSIF c=bemol THEN DEC(h) END;
      IF OctOffs+CurrOct<0 THEN RETURN SHL(Sounds[h],-(OctOffs+CurrOct))
      ELSE RETURN SHR(Sounds[h],OctOffs+CurrOct)
      END;
    END TakeHei;

    PROCEDURE Div(m,n: INTEGER): INTEGER;
    BEGIN
      IF m<0 THEN RETURN m DIV n -1 ELSE RETURN m DIV n END
    END Div;

    PROCEDURE Mod(m,n: INTEGER): INTEGER;
    BEGIN
      IF m<0 THEN RETURN m MOD n + n ELSE RETURN m MOD n END
    END Mod;


    VAR r: INTEGER;
  BEGIN
    p:=0;
    IF img[p]=paus THEN
      n.hei:=INTEGER(MAX(Notes))+1;
      n.dou := Unit DIV CurrPart;
      RETURN
    END;
    r:=PeekNum(img,p,m);
    IF r<0 THEN
      Show(img); Show("Ожидался # линейки."); HALT(1)
    ELSE p:=r
    END;
    IF m<0 THEN INC(m) END; m:=m*2;
    IF img[p]=between THEN INC(m); INC(p) END;
    m2:=Transl[Mod(m,7)]; m2 := m2 + 12*Div(m,7);
    OctOffs:=Div(m2,12); h:=Mod(m2,12);
    c:=Infl();
    n.hei:=TakeHei(h,OctOffs,c);
    n.dou := Unit DIV CurrPart;
    ASSERT(img[p]=0c);
  END MakeSound;

  PROCEDURE NoteSound(n: Note; VAR s: Sound);
    VAR h: INTEGER;
  BEGIN
    s.dou:=n.dou;
    h:=Sounds[INTEGER(n.note)];
    IF n.oct<0 THEN s.hei:=SHL(h,-n.oct) ELSE s.hei:=SHR(h,n.oct) END;
  END NoteSound;

  PROCEDURE PackNote(VAR n: Note; note: Notes; oct,dou: INTEGER);
  BEGIN
    n.dou:=dou; n.note:=note; n.oct:=oct;
  END PackNote;

END Convertions;


MODULE Scan;


  IMPORT  Sound, Accord, Note, Together, min, max, CurrOct, Unit, becar
        , sChng, eChng, tgth, stgth, etgth, StdIO, CurrPart, Fixeds
        , SetDou, fixsharp, sharp, bemol, MakeSound, FreeSound
        , PlayAccord, PlaySound, SoundList, ClearMem;

  FROM  Image    IMPORT PeekNum;
  FROM StdIO     IMPORT   Bell;

  EXPORT QUALIFIED PlayString, NewString;

  VAR
    s: ARRAY [0..79] OF CHAR;
    p: INTEGER;

  MODULE Errors;

    IMPORT s,p;
    FROM StdIO     IMPORT Show, Write, WriteLn, print;

    EXPORT Expc, Err;

    PROCEDURE ShowErr;
      VAR i: INTEGER;
    BEGIN
       WriteLn; Show(s); i:=p;
       WHILE i>0 DO DEC(i); Write('_') END; Show("^");
    END ShowErr;

    PROCEDURE Err(n: INTEGER);
    BEGIN
      ShowErr;
      CASE n OF
        |0: Show("ожидался номер октавы")
        |1: Show("должна быть длительность целой ноты")
        |2: Show("ожидался номер октавы")
        |3: Show("ожидалась длительность")
        |5: Show("слищком длинная строка")
      ELSE
        print("ошибка номер %d\n",n); HALT(1)
      END; HALT
    END Err;

    PROCEDURE Expc(c: CHAR);
    BEGIN
      IF s[p]#c THEN
        ShowErr; print("ожидался символ %c\n",c); HALT
      ELSE INC(p) END
    END Expc;

  END Errors;

    VAR tInterval: BOOLEAN;

  PROCEDURE NewString(VAR str: ARRAY OF CHAR);
  BEGIN
    IF HIGH(str)>HIGH(s) THEN Err(5) ELSE s:=str; p:=0 END
  END NewString;

  PROCEDURE Skip;
    VAR c: CHAR;
  BEGIN c:=s[p];
    WHILE (c=' ') OR (c=tgth) OR (c=stgth) OR (c=etgth) DO
      IF (c=tgth) THEN
        IF NOT Together THEN
          SetDou(0); Bell; Together:=TRUE;
        END;
      ELSIF c=stgth THEN
        IF NOT Together THEN
          SetDou(0); Bell; Together:=TRUE; tInterval:=TRUE;
        END;
      ELSIF c=etgth THEN
        IF Together THEN
          SetDou(20); Bell; Together:=FALSE; tInterval:=FALSE
        END;
      END;
      INC(p); c:=s[p]
    END;
    IF NOT tInterval THEN
      IF Together THEN SetDou(20); Bell END
    END
  END Skip;

  PROCEDURE ChangeOct;
    VAR r: INTEGER;
  BEGIN Expc(sChng);
    r:=PeekNum(s,p,CurrOct);
    IF r<0 THEN Err(0) ELSE p:=r END;
    Expc(eChng);
  END ChangeOct;

  PROCEDURE SetUnit(): BOOLEAN;
    VAR r: INTEGER;
  BEGIN
    IF s[p]#'/' THEN RETURN FALSE ELSE INC(p) END;
    r:=PeekNum(s,p,Unit);
    IF r<0 THEN Err(1) ELSE p:=r END;
    Expc('/'); RETURN TRUE
  END SetUnit;

  PROCEDURE SetFixed(): BOOLEAN;
    VAR sgn:CHAR; cou,start,step,i,j:INTEGER;
  BEGIN
    IF s[p] # fixsharp THEN RETURN FALSE ELSE INC(p) END;
    sgn:=s[p]; INC(p);
    cou:=INTEGER(s[p])-INTEGER('0'); INC(p);
    IF (sgn#sharp)&(sgn#bemol) OR NOT(cou IN {0..7}) OR (s[p]#fixsharp) THEN
      ASSERT(FALSE)
    END;
    FOR i:=min TO max DO Fixeds[i]:=becar END;
    IF sgn=sharp THEN
      step:=4; start:=3-(3-min)DIV 7 * 7;
    ELSE
      step:=3; start:=6-(6-min)DIV 7 * 7;
    END;
    FOR i:=0 TO cou-1 DO
      FOR j:=start+(step * i)MOD 7  TO max BY 7 DO
        Fixeds[j]:=sgn
    END END;
    RETURN TRUE
  END SetFixed;

  PROCEDURE GetSound(VAR n: Sound);
    VAR img: ARRAY [0..80] OF CHAR;
      i: INTEGER; c: CHAR;
  BEGIN i:=0; c:=s[p];
    WHILE (c#' ') & (c#')') & (c#']') & (c#'_') DO
      img[i]:=c; INC(i); INC(p); c:=s[p];
    END; img[i]:=0c;
    MakeSound(n,img);
  END GetSound;

  PROCEDURE MakeAccord(VAR a: Accord);
    VAR n: SoundList;
  BEGIN Expc('[');
    a.dou:=Unit DIV CurrPart; a.list:=NIL;
    Skip;
    WHILE s[p]#']' DO
      n:=FreeSound(); GetSound(n^.note);
      n^.next:=a.list; a.list:=n;
      Skip;
    END; INC(p);
  END MakeAccord;

  PROCEDURE PlayString;
    VAR r: INTEGER;
      acc: Accord; n: Sound;
  BEGIN
    Skip;
    IF SetUnit() OR SetFixed() THEN RETURN END;
    WHILE s[p]#0c DO
      Skip; IF s[p]=sChng THEN ChangeOct; Skip END;
      IF s[p]=0c THEN RETURN END;
      r:=PeekNum(s,p,CurrPart);
      IF r<0 THEN Err(3) ELSE p:=r END;
      Expc('('); Skip;
      WHILE s[p]#')' DO
        IF s[p]=sChng THEN ChangeOct; Skip END;
        IF s[p]='[' THEN
          MakeAccord(acc); PlayAccord(acc); ClearMem;
        ELSE
          GetSound(n); PlaySound(n);
        END;
        Skip;
      END;
      INC(p);
    END;
  END PlayString;

END Scan;


PROCEDURE PlayNote(n: Note);
  VAR s: Sound;
BEGIN
  IF n.note = none THEN RETURN END;
  NoteSound(n,s);
  PlaySound(s);
END PlayNote;

PROCEDURE PlayString(VAR s: ARRAY OF CHAR);
BEGIN
  Scan.NewString(s); Scan.PlayString;
END PlayString;


PROCEDURE PlayFile(f: INTEGER);
  VAR s: ARRAY [0..79] OF CHAR;
BEGIN
  WHILE GetS(f,s)>0 DO
    PlayString(s);
  END;
  IF Together THEN SetDou(20); Write(7c) END;
END PlayFile;

PROCEDURE InitPiano;
  VAR i: INTEGER;
BEGIN
  CurrDou:=NIL; CurrHei:=NIL;
  FOR i:=min TO max DO Fixeds[i]:=becar END;
  CurrOct:=0;  Unit:=1000;   (* msk *)
  ClearMem;
  Together:=FALSE;
  Sounds[-1] := 8098;      (*  Си   *)
  Sounds[0]  := 7643;      (*  До   *)
  Sounds[1]  := 7214;      (*  До # *)
  Sounds[2]  := 6809;      (*  Ре   *)
  Sounds[3]  := 6427;      (*  Ре # *)
  Sounds[4]  := 6066;      (*  Ми   *)
  Sounds[5]  := 5726;      (*  Фа   *)
  Sounds[6]  := 5404;      (*  Фа # *)
  Sounds[7]  := 5101;      (*  Соль   *)
  Sounds[8]  := 4815;      (*  Соль # *)
  Sounds[9]  := 4545;      (*  Ля   *)
  Sounds[10] := 4289;      (*  Ля # *)
  Sounds[11] := 4049;      (*  Си   *)
  Sounds[12] := 3822;      (*  До   *)
  Transl[0] := 0;
  Transl[1] := 2;
  Transl[2] := 4;
  Transl[3] := 5;
  Transl[4] := 7;
  Transl[5] := 9;
  Transl[6] := 11;
END InitPiano;

BEGIN
  InitPiano;
END Music.
