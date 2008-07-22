IMPLEMENTATION MODULE WMQuick;

FROM WMUtil IMPORT     UnImplemented,KeyPressed,Message,InterAct,
                       Int_Type,Interrupt,Soft_CR,
                       WriteText,WriteMenu,Mes_Write,GotoXY,
                       DirIn,ComLet,Aborted,IsAborted,Control;
FROM WMBuffer   IMPORT PosNo,CurLine,LowCur,Line,CPos,LineType,Cur_Menu,
                       VTravel,LOfset,LMrgn,RMrgn,
                       Replace_Line,CColNr,CLineNr,COfset,LLimit,
                       LineBuf,Overlay,Next_Proc,Off_Line_Mode,On_Line_Mode,
                       Set_Marker,MPrevious,MLastFB,MBBegin,MBEnd,
                       Delete,ModeHelp,LineMode,ModeSpacing,ModeAutoNewLine,
                       ModeIndention,ModeSoftHyph,MoveBytes,Travelling;
FROM WMScreen   IMPORT Word_Delim,VCorrect,Int_Handler,EdWriteToEOL,EdChOut,
                       MoveToMarker,Text_Beg,Place_Compute,Retrieve;
FROM Terminal   IMPORT ReadStr,ClearLine,Clear,Write,WriteLn,WriteString,
                       SetMode;
FROM Misc       IMPORT Capital,Ord16?,Dig?,Min;

CONST MaxInt=7fffffffh;
      MaxCard=MaxInt;

VAR FROpt: RECORD
    ReplaceToo,WholeWords,WithoutAsking,Backward,
    Global,IgnoreCase,NoDisplay: BOOLEAN;
    RepeatCount: CARDINAL;
    Search,Replacement: ARRAY [0..80] OF CHAR;
    END;

(* c u r s o r   m o v e m e n t *)
PROCEDURE MoveEndFile();
BEGIN
  Off_Line_Mode();
  Set_Marker(MPrevious);
  IF VCorrect(Travelling(MaxInt),FALSE) THEN
    Retrieve(screen); END;
END MoveEndFile;

PROCEDURE MoveTopFile();
BEGIN
  Off_Line_Mode();
  Set_Marker(MPrevious);
  IF VCorrect(-Travelling(-MaxInt),TRUE) THEN
    Retrieve(screen); END;
END MoveTopFile;

PROCEDURE MoveRightSide();
(*Move after last char in line*)

BEGIN
  IF On_Line_Mode() THEN RETURN END;
  CPos:= Min(ORD(LineBuf[0])+1, 255);
END MoveRightSide;

PROCEDURE MoveBottomScreen();
(*Move cursor to last line in screen*)
BEGIN
  Off_Line_Mode();
  Set_Marker(MPrevious);
  ASSERT(NOT VCorrect(Travelling(LLimit[TRUE]-CLineNr),FALSE));
END MoveBottomScreen;

PROCEDURE MoveTopScreen();
(*Move cursor to first line in screen*)
BEGIN
  Off_Line_Mode();
  Set_Marker(MPrevious);
  ASSERT(NOT VCorrect(-Travelling(LLimit[FALSE]-CLineNr),TRUE));
END MoveTopScreen;

(* D e l e t e   *)
PROCEDURE DelLineRight();
(* Delete right part of line *)

BEGIN
  IF On_Line_Mode() THEN RETURN END;
  IF CPos <= ORD(LineBuf[0]) THEN
    LineBuf[0] := CHAR( CPos - 1 );
    ClearLine();
    EdWriteToEOL(LineBuf,CColNr,LOfset+CLineNr);
    END;
END DelLineRight;

PROCEDURE DelLineLeft();
BEGIN
  IF On_Line_Mode() THEN RETURN; END;
  IF CPos>ORD(LineBuf[0]) THEN CPos:=ORD(LineBuf[0])+1; END;
  Delete(LineBuf,1,CPos-1);
  CPos:=1;
  GotoXY(CLineNr,1);
  ClearLine();
  EdWriteToEOL(LineBuf,1,LOfset+CLineNr);
END DelLineLeft;

(* F i n d   a n d   r e p l a c e *)
PROCEDURE Match( srcch, sourcech: CHAR ): BOOLEAN;
BEGIN
  IF FROpt.IgnoreCase THEN 
    RETURN srcch=Capital(sourcech);
    END;
  RETURN srcch=sourcech;
END Match;

PROCEDURE FindOccur(VAR searchstr, sourcestr: ARRAY OF CHAR;
     VAR startsrc: PosNo; OCaseIgn,OWholeWords: BOOLEAN): BOOLEAN;
(*Find  first occurance of search in source starting at start CHAR.
  Return TRUE if found & found position, FALSE if not.*)
(*Bug: Words with soft & hard hyphens are NOT recognized *)
VAR sourceind, searchind: PosNo;

BEGIN
  LOOP
    IF OWholeWords & (startsrc>1) THEN
      (*Previous CHAR must be word deliminator*)
      WHILE NOT Word_Delim(sourcestr[startsrc-1]) DO
        INC(startsrc);
        IF startsrc>ORD(sourcestr[0]) THEN RETURN FALSE END;
        END; END;
    IF (ORD(sourcestr[0])- startsrc+ 1) < ORD(searchstr[0]) THEN
      (*Remains fewer chars in source than in search string*)
      RETURN FALSE END;
    sourceind:= startsrc;
    searchind := 1;
    LOOP
      IF searchind>ORD(searchstr[0]) THEN
        (*Occurance found: search & source indexes points to next positns*)
        IF OWholeWords & (sourceind<ORD(sourcestr[0])) &
           (NOT Word_Delim(sourcestr[sourceind])) THEN
           (*NOT a word occurance*)
           EXIT; END;
        RETURN TRUE END;
      IF Match(searchstr[searchind],sourcestr[sourceind]) THEN
        INC(searchind); INC(sourceind);
      ELSE (*Mismatch found*) EXIT; END; END;
  INC(startsrc); END;
END FindOccur;

PROCEDURE MakeFind(VAR searchstr: ARRAY OF CHAR): BOOLEAN;
(*Find first occurance of search string starting at current line
  at current position. Use backward search, whole words only & CASE
  (THEN the search string must be in upper CASE) options *)
(*Return false if not found, true if found *)
(*Bug: backward search searches also at current pos to eol*)
VAR
  LineC: INTEGER;
  FindPos: [0..255];
  FindRes: BOOLEAN;

BEGIN
  LineC:= 0;
  Message(30,-1,continue);
  LOOP
    IF Aborted() THEN FindRes:= FALSE; EXIT; END;
    IF FindOccur(searchstr,CurLine^.txt,CPos,FROpt.IgnoreCase,
                                             FROpt.WholeWords ) THEN
      (*Occurance found*)
      FindRes:= TRUE; EXIT;
     ELSE (*Get next line*)
       CPos:= 1;
       IF NOT VTravel( NOT FROpt.Backward ) THEN
         (*Limit of file*)
         FindRes:= FALSE;
         EXIT; END;
       IF FROpt.Backward THEN DEC(LineC) ELSE INC(LineC); END;
       END; END;
  IF VCorrect(LineC, NOT FROpt.Backward) THEN Retrieve(screen); END;
  GotoXY(CLineNr,CColNr);
  IF Interrupt=eos THEN Retrieve(screen); END;
  IF NOT FROpt.NoDisplay THEN Int_Handler();
    ELSE Place_Compute(CurLine^.txt); END;
  RETURN FindRes;
END MakeFind;

PROCEDURE Replace(VAR source: LineType; VAR Replacing: ARRAY OF CHAR;
                  deletelen,atline: CARDINAL): BOOLEAN;
(*Replace from source at current position specified # of bytes with
  replacement. Return TRUE if need aborting because no free space*)
VAR
  AStackOvrFlFix,newlen: CARDINAL;

BEGIN
  newlen:= ORD(Replacing[0])+ ORD(source[0])- deletelen;
  IF newlen>255 THEN (*Can`t create so long lines*)
    Message(20,-1,escget);
    INC(CPos,deletelen);
    RETURN FALSE; END;
  MoveBytes(CPos-1,source,LowCur^.txt,1,1);
  MoveBytes(ORD(Replacing[0]),Replacing,LowCur^.txt,1,CPos);
  AStackOvrFlFix:= ORD(Replacing[0])+CPos;
  MoveBytes(ORD(source[0])- CPos- deletelen+ 1,
      source,LowCur^.txt, CPos+deletelen, AStackOvrFlFix );
  LowCur^.txt[0] := CHAR(newlen);
  INC(CPos,ORD(Replacing[0]));
  IF Replace_Line() THEN
    IsAborted:= TRUE;
    RETURN TRUE; END;
  GotoXY(atline,CColNr);
  ClearLine();
  EdWriteToEOL(CurLine^.txt,CColNr,LOfset+CLineNr);
  RETURN FALSE;
END Replace;

PROCEDURE Answer( mno,origline: CARDINAL): CHAR;
(*Return Y/N/Q answer to message*)
VAR ch: CHAR;

BEGIN
  LOOP
    IF NOT FROpt.NoDisplay THEN Int_Handler() END;
    GotoXY(LLimit[TRUE]+1, 1);
    Mes_Write(mno);
    GotoXY(origline, CColNr);
    ch:= ComLet(DirIn());
    IF (ch='Y') OR (ch='N') OR (ch='Q') THEN
      Retrieve(status);
      RETURN ch;
     ELSE Write(7c); END;
   END;
END Answer;

PROCEDURE SSet( VAR src, Dest: ARRAY OF CHAR);
(*convert os array to WordMaker string*)
VAR l: CARDINAL;

BEGIN
  l:= 0;
  WHILE src[l]<>0c DO
    Dest[l+1]:= src[l];
    INC(l); END;
  Dest[0]:= CHAR(l);
END SSet;

PROCEDURE FRMake();
(*Make operation according to find/replace descriptor*)
(*Problems: 1. Contol chars cannot be readed into search/repl strings
            2. Column markers are not corrected in replace
            3. Backward search searches whole current line *)

VAR
  srcstr,replstr: LineType;
  c: CARDINAL;
  atline,oldlineno,oldcofset: CARDINAL;

BEGIN
  Overlay:= pEdit;
  Off_Line_Mode();
  Set_Marker(MPrevious);
  IF FROpt.Backward THEN CPos:=1; END;
  SSet(FROpt.Search,srcstr);
  SSet(FROpt.Replacement,replstr);
  IF FROpt.Global THEN
    IF FROpt.Backward THEN MoveEndFile();
      ELSE MoveTopFile(); END;
    IF FROpt.ReplaceToo THEN FROpt.RepeatCount:= MaxCard; END;
    END;
  IF FROpt.IgnoreCase THEN
    FOR c:=1 TO ORD(srcstr[0]) DO
      srcstr[c] := Capital(srcstr[c]);
    END; END;
  atline:= LLimit[TRUE]; (* for setting to LLimit[FALSE] *)
  oldlineno := 0;
  oldcofset:= MaxCard;
  (* Find specified # of occurances, replace if need *)
  FOR c:=1 TO FROpt.RepeatCount DO
    Set_Marker(MLastFB);
    IF NOT MakeFind(srcstr) THEN (*not found*)
      IF (NOT IsAborted)& ((c=1) OR (NOT FROpt.Global)) THEN
        Message(4,-1, escget); END;
      RETURN; END;
    IF FROpt.NoDisplay THEN (*type line to screen*)
      IF ((LOfset+CLineNr)#oldlineno) OR (COfset#oldcofset) THEN
        IF atline=LLimit[TRUE] THEN atline:= LLimit[FALSE]
          ELSE INC(atline); END;
        oldlineno:= LOfset+CLineNr;
        oldcofset:= COfset;
        GotoXY(atline+1,1);
        ClearLine();
        GotoXY(atline,1);
        ClearLine();
        EdWriteToEOL(CurLine^.txt,1,LOfset+CLineNr);
        END;
      Retrieve(screen);
      ELSE atline:= CLineNr;
      END;
    IF FROpt.ReplaceToo THEN
      IF FROpt.WithoutAsking THEN
        IF Replace(CurLine^.txt,replstr,ORD(srcstr[0]),atline) THEN
          RETURN
        END;
       ELSE CASE Answer(5,atline) OF
         'N': INC(CPos, ORD(srcstr[0]));
        |'Y': IF Replace(CurLine^.txt,replstr,ORD(srcstr[0]),atline) THEN
                     RETURN; END;
        |'Q': RETURN; END;
        END;
      ELSE (*Set to find next occurance*)
        INC(CPos, ORD(srcstr[0])); END;
    END;(*For*)
END FRMake;

PROCEDURE InitFRDesc();
(* Set find/replace default values *)
BEGIN
  FROpt. WholeWords := FALSE;
  FROpt. WithoutAsking := FALSE;
  FROpt. Backward      := FALSE;
  FROpt. Global        := FALSE;
  FROpt. IgnoreCase    := FALSE;
  FROpt. RepeatCount   := 1;
  FROpt. NoDisplay     := FALSE;
END InitFRDesc;

PROCEDURE GetOptions();

VAR
  OptLine: LineType;
  ind: CARDINAL;
  ok: BOOLEAN;

BEGIN
  OptLine[0]:=0c;
  REPEAT
    LOOP
      Message(2,-1,continue);
      ReadStr( OptLine );
      IF OptLine[0]<>'?' THEN EXIT; END;
        WriteText(0); END;
    ind:=0;
    ok:= TRUE;
    InitFRDesc();
    LOOP
      IF OptLine[ind]=0c THEN EXIT; END;
      CASE ComLet(OptLine[ind]) OF
        'W': FROpt.WholeWords:= TRUE;
       |'N': FROpt.WithoutAsking:= TRUE;
       |'B': FROpt.Backward:= TRUE;
       |'U': FROpt.IgnoreCase:= TRUE;
       |'G': FROpt.Global:= TRUE;
       |'D': FROpt.NoDisplay:= TRUE;
       |'0'..'9': FROpt.RepeatCount:=0;
       REPEAT
         FROpt.RepeatCount:=FROpt.RepeatCount*10+Ord16?(OptLine[ind]);
         INC(ind);
       UNTIL NOT Dig?(OptLine[ind]);
       DEC(ind);
       ELSE
         Message(3,-1,escget);
         ok:= FALSE;
         EXIT; END;
      INC(ind);
      END;
    UNTIL ok;
END GetOptions;

PROCEDURE GetSearchString();

BEGIN
  (*Int_Handler();*)
  Message(0,-1,continue);
  ReadStr(FROpt.Search);
END GetSearchString;

PROCEDURE Find();
BEGIN
  GetSearchString();
  FROpt.ReplaceToo:= FALSE;
  GetOptions();
  FRMake();
END Find;

PROCEDURE FindReplace();

BEGIN
  GetSearchString();
  FROpt.ReplaceToo:= TRUE;
  Message(1,-1,continue);
  ReadStr(FROpt.Replacement);
  GetOptions();
  FRMake();
END FindReplace;

PROCEDURE Quick();
VAR comch: CHAR;

BEGIN
  WriteMenu();
  comch:= Control(DirIn());
  CASE comch OF
    (*A*) 01c: FindReplace();
   |(*B*) 02c: MoveToMarker(MBBegin);
   |(*C*) 03c: MoveEndFile();
   |(*D*) 04c: MoveRightSide();
   |(*E*) 05c: MoveTopScreen();
   |(*F*) 06c: Find();
   |(*K*) 13c: MoveToMarker(MBEnd);
   |(*P*) 20c: MoveToMarker(MPrevious);
   |(*R*) 22c: MoveTopFile();
   |(*S*) 23c: CPos:=1;
   |(*V*) 26c: MoveToMarker(MLastFB);
   |(*X*) 30c: MoveBottomScreen();
   |(*Y*) 31c: DelLineRight();
   |(*-*) 37c: DelLineLeft();
   |      ' ': ;
   | '0'..'9': MoveToMarker(ORD(comch)-ORD('0'));
   ELSE UnImplemented(); END;
  SetMode(FALSE); (*Facit terminal turns it ON?!*)
  Overlay := pEdit;
END Quick;

(* O n s c r e e n   M e n u *)
PROCEDURE ModeSwitch( VAR m: BOOLEAN );
BEGIN
  m:= NOT m;
END ModeSwitch;

PROCEDURE TypeMode(l: CARDINAL; m: BOOLEAN);
CONST mcol=54;

BEGIN
  GotoXY(l,mcol);
  IF m THEN WriteString(" ON");
  ELSE      WriteString("OFF"); END;
END TypeMode;

PROCEDURE ModesType();
BEGIN
  IF NOT ModeHelp OR (Cur_Menu#ORD(Overlay)) THEN RETURN; END;
  TypeMode(3,ModeSpacing);
  TypeMode(4,ModeAutoNewLine);
  TypeMode(5,ModeIndention);
  TypeMode(6,ModeSoftHyph);
END ModesType;

PROCEDURE SetMargin(VAR m: CARDINAL);
BEGIN
  m:= Min(COfset+ CColNr,255);
END SetMargin;

PROCEDURE CenterText();
(* Center current line *)

VAR
  oldstart,
  newstart: PosNo;
  pos: INTEGER;
  len: [0..255];

BEGIN
  Off_Line_Mode();
  IF CurLine^.txt[0]=0c THEN RETURN; END;
  oldstart := Text_Beg( CurLine^.txt );
  len := ORD(CurLine^.txt[0]) - oldstart + 1;
  pos := (RMrgn - LMrgn - len) DIV 2;
  IF pos>0 THEN newstart := pos
    ELSE newstart := LMrgn; END;
  FOR pos:=1 TO newstart-1 DO LowCur^.txt[pos]:= ' '; END;
  len:= ORD(CurLine^.txt[0])- oldstart+ 1;
  IF newstart+len>255 THEN Message(20,-1,escget); RETURN; END;
  MoveBytes( len,CurLine^ .txt,LowCur^.txt, oldstart, newstart);
  LowCur^ .txt[0] := CHAR( newstart + ORD(CurLine^.txt[0]) - oldstart );
  IF Replace_Line() THEN RETURN; END;
  GotoXY( CLineNr, 1);
  ClearLine();
  EdWriteToEOL(CurLine^ .txt,1,LOfset+CLineNr );
END CenterText;

PROCEDURE OnScreen();
VAR ch: CHAR;

BEGIN
  WriteMenu();
  ModesType();
  GotoXY(CLineNr,CColNr);
  ch:= Control(DirIn());
  IF      ch=27c THEN ModeSwitch(ModeAutoNewLine)
    ELSIF ch=11c THEN ModeSwitch(ModeIndention);
    ELSIF ch=12c THEN ModeSwitch(ModeSpacing);
    ELSIF ch= 5c THEN ModeSwitch(ModeSoftHyph);
    ELSIF ch=14c THEN SetMargin( LMrgn );
    ELSIF ch=22c THEN SetMargin( RMrgn);
    ELSIF ch=3c THEN CenterText()
    ELSIF ch=' ' THEN
    ELSE UnImplemented(); END;
  Overlay:= pEdit;
END OnScreen;

BEGIN
  InitFRDesc();
  FROpt. Search [0] := 0c;
  FROpt. ReplaceToo := FALSE;
END WMQuick.
