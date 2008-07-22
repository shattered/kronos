IMPLEMENTATION MODULE WMScreen;
(* Type screen of text *)
(* Module for Wordmaker by Andrus Moor *)

FROM WMBuffer  IMPORT  Line,Cur_Menu,CurLine,LOfset,COfset,CLineNr,CColNr,
                       CPos,LLimit,LineMode,LineBuf,
                       ModeInserting, ModeHelp, RMrgn,LMrgn,
                       Marker,Markers,MBBegin,MBEnd,VTravel,Travelling;
FROM WMUtil     IMPORT Mes_Write,InterAct,ComLet,NormalVideo,
                       CtrlOut,GotoXY,KeyPressed,BlVidOn,BlVidOff,
                       Remove_Line,Message,Interrupt,
                       Lines,Cols,Soft_CR,ht,MenuLn;
FROM Terminal   IMPORT WriteLn,ClearLine,Clear,print;
FROM TTYs       IMPORT Write;
FROM Misc       IMPORT Min;

VAR inblock: BOOLEAN;
    lpos: CARDINAL;

PROCEDURE ChOut( ch: CHAR );

BEGIN
  (*** for non-KOI-8 terminal use:
  IF ch>CHAR(127) THEN
    IF set_koi8 THEN clrbit( ch, 7 )
   ELSE set_english; END;
  ***)
  IF ch>=' ' THEN Write( ch )
  ELSE CtrlOut( ch ) END;
END ChOut;

PROCEDURE SetBlockEffect( absline: CARDINAL );
(*Set the block video to screen for given line*)
BEGIN
  IF BlockMarked() & (absline>=Markers[MBBegin].MLine) &
    (absline<=Markers[MBEnd].MLine) THEN
    BlVidOn(); inblock:= TRUE;
   ELSE inblock:= FALSE; END;
END SetBlockEffect;

PROCEDURE EdChOut( ch: CHAR );
(*Write character to current line with effect *)
BEGIN
  SetBlockEffect( LOfset+CLineNr );
  ChOut(ch);
  IF inblock THEN BlVidOff(); END;
END EdChOut;

PROCEDURE WriteToEOL(VAR ln: LineType; offset,stcol: CARDINAL );
(* Write current line to screen starting at current cursor position
  at specified offset & col *)
(* Assumed thet cursor is in proper place *)

VAR
  ppos: PosNo;
  count: CARDINAL;
  ch: CHAR;

BEGIN
  lpos := 1;
  ppos := 1;
  count:=offset+stcol; (*do'nt compute in LOOP it*)
  LOOP
    IF ppos>ORD(ln[0]) THEN RETURN END;
    IF lpos>=count THEN EXIT ELSE
    Screen_Pos_Process( ppos, lpos, ln ); END;
    INC(ppos);
    END;
  FOR count := count+ 1 TO lpos DO
    Write( ' ' );
    END;
  WHILE lpos<(offset+Cols) DO
    (* ChOut( ln[ppos] ); replaced for speed:*)
    ch:= ln[ppos];
    IF ch<>CHAR(ht) THEN
      IF ch>=' ' THEN Write( ch )
     ELSE CtrlOut( ch ) END;
     INC(lpos);
     ELSE
       FOR count:=lpos TO NextTab(lpos)-1 DO
         Write( ' ' ); END;
       lpos := NextTab( lpos );
       END;
     INC(ppos);
     IF ppos>ORD(ln[0]) THEN RETURN END;
     END;
  (*Write last column of screen*)
  IF (ppos#ORD(ln[0])) OR (ln[ppos]=CHAR(ht)) THEN
    (*Line continues after last column of screen*)
    Write('+');
    ELSE ChOut(ln[ppos]); END;
  INC(lpos);
END WriteToEOL;

PROCEDURE BlockMarked(): BOOLEAN;
(*Return TRUE if block markers are correctly set*)
BEGIN
  IF Markers[MBBegin].setted & Markers[MBEnd].setted &
    (Markers[MBBegin].MLine<=Markers[MBEnd].MLine) THEN
    RETURN TRUE
    ELSE RETURN FALSE; END;
END BlockMarked;

PROCEDURE EdWriteToEOL(VAR ln: LineType; stcol,abslineno: CARDINAL);
(* Write given absolute line, include block & place markers *)

BEGIN
  SetBlockEffect( abslineno );
  WriteToEOL(ln, COfset,stcol);
  IF inblock THEN
    FOR lpos:=lpos TO Cols DO Write(' '); END;
    BlVidOff();
    END;
END EdWriteToEOL;

PROCEDURE Place_Write();
(* Write line and column numbers *)

BEGIN
  (*NormalVideo();*)
  IF KeyPressed() THEN RETURN; END;
  GotoXY( LLimit[TRUE]+1, 27 );
  IF KeyPressed()  THEN RETURN; END;
  print('%3d',COfset+CColNr);
  IF KeyPressed() THEN RETURN; END;
  GotoXY( LLimit[TRUE]+1,17 );
  IF KeyPressed() THEN RETURN; END;
  print('%5d',LOfset+CLineNr);
END Place_Write;

PROCEDURE Put_Margin( mrg: CARDINAL; ch: CHAR );

BEGIN
  IF (mrg>COfset) & (mrg<COfset+Cols) THEN
    GotoXY( LLimit[TRUE]+1, mrg-COfset );
    Write( ch );
    END;
END Put_Margin;

PROCEDURE StatusLine();
(* Write status line to screen *)

VAR
  i: CARDINAL;

BEGIN
  IF Interrupt> status THEN RETURN; END;
  NormalVideo();
  GotoXY(Lines,1);
  ClearLine();
  (*** Write tabulators
  FOR i:=COfset+1 to COfset+max_col-9 DO
    dirout( tab[i] );
  ***)
  IF ModeInserting THEN
    GotoXY(LLimit[TRUE]+1,32);
    Mes_Write(33); END;
  Put_Margin( LMrgn, '<' );
  Put_Margin( RMrgn, '>' );
  (* For non-KOI-8 terminals: lang_Write; *)
  Interrupt := none;
  Place_Write();
END StatusLine;

PROCEDURE EOS_Write();
(* Write from current line until end of screen *)
(* Assumed that screen is cleared *)

VAR
  linec: CARDINAL;      (*line counter*)
  nextlexists: BOOLEAN; (* True if next line exists *)

BEGIN
  IF Interrupt>eos THEN RETURN END;
  IF KeyPressed() THEN
    Interrupt := eos;
    RETURN;
    END;
  (* Write current line *)
  GotoXY( CLineNr, 1 );
  IF LineMode THEN EdWriteToEOL( LineBuf,1, LOfset+CLineNr)
    ELSE EdWriteToEOL( CurLine^. txt,1, LOfset+CLineNr );
    END;
  nextlexists := VTravel(TRUE);
  linec := CLineNr + 1;

  WHILE (NOT KeyPressed()) & (linec<=LLimit[TRUE]) &
         nextlexists DO
    (* Write a line into screen *)
    GotoXY( linec, 1 );
    EdWriteToEOL(CurLine^. txt,1, LOfset+linec );
    INC(linec );
    nextlexists := VTravel( TRUE );
    END;
  IF KeyPressed() THEN Interrupt := eos
    ELSE Interrupt := status;
    END;
  StatusLine();
  (* Travel back to current line *)
  IF NOT nextlexists THEN DEC( linec ); END;
  FOR linec := linec TO CLineNr+1 BY -1 DO
    ASSERT( VTravel(FALSE) );
    END;
END EOS_Write;

PROCEDURE ScreenWrite();
(* Write current screen of text, col, line numbers *)

VAR
  ReqCnt,ActCnt,l: CARDINAL;

BEGIN
  ReqCnt:= CLineNr- LLimit[FALSE];
  (* Go to start line of current screen *)
  ActCnt:= Travelling(-ReqCnt);
  IF ActCnt<ReqCnt THEN
    (* Current screen is first in file, correct cursor *)
    DEC(CLineNr, ReqCnt-ActCnt);
    LOfset := 1 - LLimit[FALSE];
    END;
  (* Write part of screen before cursor *)
  GotoXY( LLimit[FALSE], 1 );
  Clear();
  Place_Write(); (*Let user to see line & column numbers*)
  Interrupt := eos;
  FOR l := LLimit[FALSE] TO CLineNr-1 DO
    IF KeyPressed() THEN
      Interrupt := screen
     ELSE
      GotoXY( l, 1 );
      EdWriteToEOL( CurLine^. txt,1, LOfset+l );
      END;
      ASSERT( VTravel(TRUE) );
    END;
  EOS_Write();
END ScreenWrite;

PROCEDURE Roll_Up( VAR ln: LineType );
(* Roll text up one line. *)
(* Assumed that given line becomes last in screen *)

BEGIN
  GotoXY( LLimit[FALSE], 1 );
  IF NOT Remove_Line() THEN
    (* Terminal do'nt have delete line PROCEDURE *)
    GotoXY(Lines,1);
    ClearLine();
    WriteLn();
    Cur_Menu := 255; (*becomes undefined which causes overwriting*)
    END;
  GotoXY( LLimit[TRUE], 1 );
  ClearLine();
  EdWriteToEOL(ln,1, LOfset+(LLimit[TRUE]-LLimit[FALSE]+1) );
  Retrieve( status );
END Roll_Up;

PROCEDURE VCorrect( voffset: INTEGER; first_line: BOOLEAN ): BOOLEAN;
(* True if screen needs to overwrite because vertical moving *)
(* first line: Locate cursor to first/last line of screen if
   new position is NOT in same vertical screen *)

BEGIN
  (* Set line & column according to voffset *)
  IF (CLineNr+voffset<LLimit[FALSE]) OR
     (CLineNr+voffset>LLimit[TRUE]) THEN
      (* Voffset points to new vertical screen *)
      (* Locate cursor to this screen *)
      IF first_line THEN
        (* Put to first line *)
        INC( LOfset, voffset + CLineNr - LLimit[FALSE] );
        CLineNr := LLimit[FALSE];
       ELSE
        (* Cursor to last line *)
        LOfset := LOfset+voffset + CLineNr - LLimit[TRUE];
        CLineNr := LLimit[TRUE];
        END;
      RETURN TRUE;
     ELSE
      (* Cursor stays in same vertical screen *)
      CLineNr := CLineNr + voffset;
      RETURN FALSE;
      END;
END VCorrect;

PROCEDURE NextTab( ccol: CARDINAL ): PosNo;
(* Find next tabulator after current column *)

BEGIN
  (* Variable tabulators:
  IF ccol<255 THEN ccol := ccol + 1;
  WHILE (tab[ccol]<>'!') & (ccol<255) DO
    ccol := ccol + 1; END;
  NextTab := ccol;
   ******)
  RETURN Min( ((ccol-1) DIV 8+1)*8+1, 255 );
END NextTab;

PROCEDURE MoveToMarker( mno: CARDINAL );
(*Travel to marker line, pos *)
(*Set correct line # and screen updating *)
VAR
  travc: INTEGER;

BEGIN
  IF NOT Markers[mno].setted THEN
    Message( 31,-1,escget);
    RETURN; END;
  travc:= Markers[mno].MLine - (LOfset+CLineNr);
  ASSERT( Travelling(travc)=ABS(travc) );
  IF VCorrect(travc,TRUE) THEN Retrieve(screen); END;
END MoveToMarker;

PROCEDURE White_Space( ch: CHAR ): BOOLEAN;
(*Return TRUE if current char is deliminator between words *)

BEGIN
  RETURN (ch=' ') OR (ch=CHAR(ht));
END White_Space;

PROCEDURE Word_Delim( ch: CHAR ): BOOLEAN;
(* Returns TRUE if character is deliminator between words *)

BEGIN
  CASE ch OF
    11c,' ','('..'/',':'..'>','[',']': RETURN TRUE;
  ELSE RETURN FALSE; END;
END Word_Delim;

PROCEDURE Screen_Pos_Process( ppos: PosNo; VAR logpos: CARDINAL;
                              VAR SrcLine: LineType );           (*!*)
(* Process current position in line and go to next position *)

BEGIN
  IF (SrcLine[ppos]#CHAR(ht)) THEN
    INC(logpos);
  ELSIF ppos<=ORD(SrcLine[0]) THEN
    logpos := NextTab(logpos)
  ELSE
    INC(logpos);
    END;
END Screen_Pos_Process;

PROCEDURE Screen_Position(VAR ln: LineType; CharNo: PosNo): PosNo;
(*Compute position in screen for given character in line*)
VAR
  ppos: PosNo;
  ScrnPos: CARDINAL;

BEGIN
  ScrnPos:= 1;
  FOR ppos:=1 TO CharNo-1 DO
    Screen_Pos_Process( ppos, ScrnPos, ln ); END;
  IF ScrnPos<255 THEN RETURN ScrnPos
    ELSE RETURN 255; END;
END Screen_Position;

PROCEDURE SamePos( VAR ln: LineType; logp: CARDINAL ): PosNo;
(*Compute nearest physical position for given
 logical position in current Line *)

VAR
  pp, lp: CARDINAL;

BEGIN
  pp := 1;
  lp := 1;
  WHILE lp<logp DO
    Screen_Pos_Process( pp, lp, ln );
    INC(pp);
    END;
  RETURN Min( pp,255 );
END SamePos;

PROCEDURE Place_Compute( VAR ln: LineType );
(* Compute place according to current CHARacter in line *)

CONST scrnstep = 25;

VAR ScColumn: CARDINAL;

BEGIN
  IF CPos>255 THEN
    CPos:=255;
    Message(20,-1,escget); END;
  ScColumn := Screen_Position( ln, CPos );
  IF ScColumn<=COfset THEN (*Roll backward required*)
    REPEAT DEC(COfset,scrnstep) UNTIL COfset<ScColumn;
    Retrieve(screen);
  ELSIF ScColumn>(COfset+Cols) THEN
    (*Hor roll forward*)
    REPEAT INC(COfset,scrnstep) UNTIL (COfset+Cols)>ScColumn;
    Retrieve(screen);
    END;
  CColNr := ScColumn - COfset;
END Place_Compute;

PROCEDURE Int_Handler();

BEGIN
  IF LineMode THEN Place_Compute(LineBuf)
  ELSE
    Place_Compute( CurLine^ .txt ); END;
  IF NOT KeyPressed() THEN
      CASE Interrupt OF
        none: Place_Write();
        |status: StatusLine();
        |eos:    EOS_Write();
        |screen: ScreenWrite();
      END;
    END;
END Int_Handler;

PROCEDURE Text_Beg( VAR srcline: LineType ): PosNo;
(* Find statrt of text in line *)

VAR
  beg: CARDINAL;
  
BEGIN
  beg := 1;
  WHILE (beg<ORD(srcline[0])) & White_Space(srcline[beg]) DO
    INC(beg);
    END;
  RETURN beg;
END Text_Beg;

PROCEDURE Retrieve( i: Int_Type );

BEGIN
  IF i>Interrupt THEN
    Interrupt := i; END;
END Retrieve;

END WMScreen.
