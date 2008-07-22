MODULE wm;
(* Wordmaker main module by Andrus Moor *)

(*FROM WMEdit    IMPORT Edit;*)
FROM WMEdBuf   IMPORT Print_Menu;
FROM WMBlMenu  IMPORT BlockMenu;
FROM WMQuick   IMPORT Quick,FRMake,OnScreen;
FROM WMReform  IMPORT Reformat;
FROM Terminal  IMPORT Show,Write,SetMode;
FROM WMFiles   IMPORT tFNSet,tFNGet,SrcFName,tOpen,tCloseUnchanged,Read_Line,
                      Write_Line;
FROM WMBuffer  IMPORT Ins_Line,Read_File,Clear_Marker,
                      pLine,Line,TBeg,CurLine,LowCur,
                      LOfset,COfset,CLineNr,CColNr,CPos,LLimit,
                      LineBuf,Next_Proc,Overlay,
                      Markers,Marker,MaxMarker,ModeHelp,BufIni;
FROM WMUtil    IMPORT Message,YNGet,ReadFN,Interrupt,InterAct,
                      Int_Type,Lines,Cols,MenuLn;

(* additional modules for edit: *)
FROM WMScreen   IMPORT EdWriteToEOL,Retrieve,VCorrect,Roll_Up,Int_Handler,
                       White_Space,Screen_Pos_Process,SamePos;
FROM WMEdBuf    IMPORT CharWrite,New_Line,Tabulation,Del_Char,Del_Word_Right,
                       Line_Del,Empty_Line,UnDo,Del_Prev_Char,Right_Word,
                       Left_Word;
FROM WMBuffer   IMPORT Off_Line_Mode,VTravel,LineType,PosNo,Make_Space,Cur_Menu
,
                       ModeInserting, RMrgn,LMrgn,Travelling,LineMode;
FROM WMUtil     IMPORT UnImplemented,InsertLine,IsAborted,ht,
                       KeyPressed,DirIn,GotoXY,WriteMenu,ComLet;

MODULE WMEdit;
(* Main editor loop *)
(* Module for Wordmaker by Andrus Moor *)

IMPORT EdWriteToEOL,Retrieve,VCorrect,Roll_Up,Int_Handler,
       White_Space,Screen_Pos_Process,SamePos;
IMPORT CharWrite,New_Line,Tabulation,Del_Char,Del_Word_Right,
       Line_Del,Empty_Line,UnDo,Del_Prev_Char,Right_Word,Left_Word;
IMPORT Off_Line_Mode,VTravel,LineType,PosNo,Cur_Menu,Line,
       LOfset,COfset,CLineNr,CColNr,CPos,LLimit,CurLine,LowCur,
       LineMode,LineBuf,Overlay,Next_Proc,Make_Space,
       ModeInserting, ModeHelp, RMrgn,LMrgn,Travelling;
IMPORT UnImplemented,InsertLine,IsAborted,Lines,Cols,ht,MenuLn,
       KeyPressed,DirIn,GotoXY,Int_Type,WriteMenu,
       InterAct,Interrupt,ComLet;
IMPORT Write;

EXPORT Edit;

PROCEDURE CharStep( dr: BOOLEAN );
(* Cursor left/right one char *)

BEGIN
  CASE dr OF
    FALSE: IF CPos>1 THEN DEC( CPos ); END;
    |TRUE: IF CPos<255 THEN INC( CPos ); END;
  END;
END CharStep;

PROCEDURE WordStep( dr: BOOLEAN );
(* Cursor left/right word  *)

BEGIN
  IF LineMode THEN
    IF dr THEN CPos:=Right_Word( LineBuf, CPos );
      ELSE CPos:=Left_Word( LineBuf, CPos ); END;
   ELSE
    IF dr THEN CPos:=Right_Word( CurLine^.txt, CPos );
      ELSE CPos:=Left_Word( CurLine^.txt, CPos ); END;
   END;   
END WordStep;

PROCEDURE AnotherLine();
(*Setup travelling to another line to same position*)
BEGIN
  Off_Line_Mode();
  CPos := SamePos( CurLine^.txt, COfset + CColNr );
END AnotherLine;

PROCEDURE LineStep( dr: BOOLEAN );
(* Cursor up/ down one Line *)

BEGIN
  AnotherLine();
  CASE dr OF
    FALSE: IF VTravel(FALSE) THEN
         IF VCorrect( -1, TRUE ) THEN
           GotoXY(CLineNr,1);
           IF InsertLine() THEN
             EdWriteToEOL( CurLine^.txt,1,LOfset+CLineNr);
             Retrieve(status);
            ELSE Retrieve(screen); END;
         END; END;
    |TRUE: IF VTravel(TRUE) THEN
             IF VCorrect( 1, FALSE ) THEN Roll_Up( CurLine^ .txt );
           END; END;
  END;
END LineStep;

PROCEDURE ScreenStep( dr: BOOLEAN );
(* Cursor to previous/next screen of text *)
CONST ScreenOverLap = 1;

VAR TrvlLen: CARDINAL;

BEGIN
  AnotherLine();
  TrvlLen := LLimit[TRUE] - LLimit[FALSE] + (1-ScreenOverLap);
  IF dr THEN
    INC(LOfset, Travelling(TrvlLen));
  ELSE DEC(LOfset,Travelling(-TrvlLen)); END;
  Retrieve( screen );
END ScreenStep;

PROCEDURE ScRoll_Up();
(* Move text in screen a line up. Cursor moves with text if not
  in first line of screen *)
VAR TrvlCnt, ActCnt: CARDINAL;

BEGIN
  TrvlCnt:= LLimit[TRUE]-CLineNr+1;
  ActCnt := Travelling(TrvlCnt);
  (*Find new position in screen*)
  IF CLineNr> LLimit[FALSE] THEN (*Cursor moves with line*)
    DEC(CLineNr);
   ELSE (*Next line moves to cursor*)
    IF ActCnt=0 THEN RETURN END;
    AnotherLine();
    DEC(ActCnt); END;
  INC(LOfset);
  IF ActCnt=TrvlCnt THEN
    Roll_Up( CurLine^.txt);
   ELSE LowCur^.txt[0] := 0c;
     Roll_Up(LowCur^.txt);
     END;
   ASSERT( Travelling(-ActCnt)=ActCnt );
END ScRoll_Up;

PROCEDURE Scroll_Down();
(*Move text in screen a line down. Cursor moves with text
 if not in last line of screen*)
VAR TrvlCnt,ActCnt: CARDINAL;

BEGIN
  TrvlCnt := CLineNr-LLimit[FALSE] +1;
  ActCnt:= Travelling(-TrvlCnt);
  IF ActCnt<TrvlCnt THEN (*first line of screen is first in file*)
    ASSERT(Travelling(ActCnt)=ActCnt);
    LineStep(FALSE);
    RETURN
   ELSE (*line before beginnnig of screen exists*)
    IF CLineNr<LLimit[TRUE] THEN (*Cursor moves with line*)
      INC(CLineNr);
     ELSE (*Previous line moves to current line*)
       ASSERT(ActCnt>0);
       DEC( ActCnt ); END;
    DEC(LOfset);
    GotoXY(LLimit[FALSE], 1 );
    IF InsertLine() THEN
      (*Line inserting is available in terminal*)
      IF Interrupt<screen THEN
        EdWriteToEOL(CurLine^.txt,1, LOfset+LLimit[FALSE] );
        Retrieve(status); END;
      ELSE Retrieve(screen); END;
    ASSERT(Travelling(ActCnt)=ActCnt);
    END;
END Scroll_Down;

PROCEDURE Ins_Change();

BEGIN
  ModeInserting := NOT ModeInserting;
  Retrieve( status );
END Ins_Change;

PROCEDURE Restore_Screen();

BEGIN
  Cur_Menu := 255;
  Interrupt := screen;
END Restore_Screen;

PROCEDURE Help_Rotate();

BEGIN
  IF ModeHelp THEN
      (* Set Menu off *)
      LLimit[FALSE] := LLimit[FALSE] - MenuLn;
      ModeHelp := FALSE
  ELSE (* Menu on *)
      LLimit[FALSE] := LLimit[FALSE] + MenuLn;
      ModeHelp := TRUE;
      END;
  (* Locate cursor to screen *)
  LOfset := LOfset + CLineNr - LLimit[FALSE];
  CLineNr := LLimit[FALSE];
  Restore_Screen();
END Help_Rotate;

PROCEDURE Edit();
VAR comch: CHAR;

BEGIN   (* Edit *)
  IsAborted:= FALSE;
  REPEAT
    Int_Handler();
    WriteMenu();
    comch := DirIn();
    CASE ORD( comch ) OF
      (*@*) 0: ;
      (*A*) |1: WordStep( FALSE );       (* ws *)
      (*B*) |2: Overlay:= pReformat;     (* ws *)
      (*C*) |3: ScreenStep( TRUE );      (* ws *)
      (*D*) |4: CharStep(TRUE);          (* ws *)
      (*E*) |5: LineStep( FALSE );       (* ws *)
      (*F*) |6: WordStep(  TRUE );       (* ws *)
      (*G*) |7: Del_Char();              (* ws *)
      (*H*) |8: LineStep(FALSE);         (* ws *)
      (*I*) |9: Tabulation();            (* ws *)
      (*J*) |10: Help_Rotate();          (* ws *)
      (*K*) |11: Overlay := pBlockH;     (* ws *)
      (*L*) |12: Overlay:=pFRMake;       (* ws *)
      (*M*) |13: New_Line();             (* ws *)
      (*N*) |14: ;
      (*O*) |15: ;
      (*P*) |16: Overlay := pPrint;      (* ws *)
      (*Q*) |17: Overlay := pQuick;      (* ws *)
      (*R*) |18: ScreenStep( FALSE );    (* ws *)
      (*S*) |19: CharStep(FALSE);        (* ws *)
      (*T*) |20: Del_Word_Right();       (* ws *)
      (*U*) |21: UnDo();
      (*V*) |22: Ins_Change();           (* ws *)
      (*W*) |23: Scroll_Down();          (* ws *)
      (*X*) |24: LineStep( TRUE );       (* ws *)
      (*Y*) |25: Line_Del();             (* ws *)
      (*Z*) |26: ScRoll_Up();            (* ws *)
      (*[*) |27: Restore_Screen();
      (*\*) |28: Overlay := pOnScreen;
      (*]*) |29: Make_Space(777); (*UnImplemented();*)
      (*^*) |30: Empty_Line();
      (*-*) |31: UnImplemented();
            |127: Del_Prev_Char();       (* ws *)
    ELSE
      CharWrite( comch );
    END
    UNTIL Overlay<>pEdit;
  IF Overlay#pFRMake THEN Int_Handler(); END;
  GotoXY(LLimit[TRUE]+1, 1 );
  Write('^');
  Write( ComLet(comch) );
  Retrieve(status);
END Edit;

END WMEdit;

PROCEDURE NoFile();

BEGIN
  IF ReadFN( 25, 0 ) THEN
    Overlay := pBufReset
  ELSE Message(-1,-1, abort); END;
END NoFile;

PROCEDURE BufReset();
(* Set buffer to edit a file *)
(* Assumed that source file name is already set *)

CONST SrcFNo=0;
VAR m: CARDINAL;

BEGIN
  BufIni();
  FOR m:=0 TO 255 DO 
    LineBuf[m]:= 0c; END; (*to dedect asking in ^KQ*)
  tFNGet(SrcFNo,SrcFName);
  (* Attempt to open source file *)
  IF tOpen(SrcFNo) THEN
    (* File can'nt be opened: probably new *)
    Message( 24, SrcFNo, continue );
    IF NOT YNGet() THEN
      Overlay := pNoFile;
      RETURN;
    END;
  ELSE
    (* File is opened *)
    IF Read_File(0,m) THEN
      Overlay:= pNoFile;
      IF tCloseUnchanged(SrcFNo) THEN
        Message(13,SrcFNo,escget);
        END;
      RETURN
     ELSE CurLine:= TBeg.First END;
    IF tCloseUnchanged(SrcFNo) THEN
      Message(13,SrcFNo,escget); END;
   END;
  (* .. Del old & create new tempotary files if auto disk buffering used*)
  IF TBeg.First=TBeg.Last THEN (*No Lines*)
    LowCur^.txt[0]:= 0c;
    ASSERT( (NOT Ins_Line()) );
    END;
  IF ModeHelp THEN
    LLimit[FALSE] := MenuLn + 1
   ELSE LLimit[FALSE] := 1; END;
  LLimit[TRUE] := Lines - 1;
  LOfset := 1-LLimit[FALSE];
  CLineNr := LLimit[FALSE];
  CPos := 1;
  COfset := 0;
  CColNr := 1;
  Interrupt := screen;
  Overlay := pEdit;
  FOR m:=0 TO MaxMarker DO
    Clear_Marker(m);
    Markers[m].MLine:=1;
    Markers[m].MPos:=1;
    END;
  SetMode(FALSE); (* Fix Facit terminal strange thing*)
END BufReset;

BEGIN
  Show( "WordMaker 2" );
  Show("written by Andrus Moor");
  Show( "AT EKB 17-Jan-86" );
  REPEAT
    CASE Overlay OF
       pBufReset: BufReset();
      |pEdit:     Edit();
      |pNoFile:   NoFile();
      |pPrint:    Print_Menu();
      |pBlockH:   BlockMenu();
      |pOnScreen: OnScreen();
      |pQuick:    Quick();
      |pFRMake:   FRMake();
      |pReformat: Reformat();
      END;
  UNTIL FALSE;
END wm.
