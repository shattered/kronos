IMPLEMENTATION MODULE WMUtil;
(* Module for Wordmaker by Andrus Moor *)

FROM WMFiles   IMPORT tFNGet,tFNSet,PrintDir,IsDirectory,SrcFName;
FROM WMBuffer  IMPORT Cur_Menu,ModeHelp,CLineNr,CColNr,
                      Overlay,Next_Proc,LowIni,HighIni;
FROM Terminal  IMPORT Write, Read, SetCrs, Clear, ClearLine, ShowAndWait,
                      Esc,WriteString,
                      WriteLn,Show,DL,IL,print,SetMode,Pressed;
FROM Edit      IMPORT ReadString;
FROM WMScreen  IMPORT Retrieve;
FROM Args      IMPORT ScanFlags,Flag?,NumFlag?,TakeWord;
FROM TTYs      IMPORT MayBeAbo,GetTermType,SetTermType;
FROM Scheduler IMPORT MyTask, Sleep;
FROM KRONOS    IMPORT MySelf,ALLOC;
FROM Universe  IMPORT ProcessDesc,DescRec;

TYPE
  Menu=  ARRAY [0..7] OF
            ARRAY[0..88] OF CHAR;

VAR
  BigMTxt: ARRAY [0..5] OF RECORD
    tx: Menu;
    END;

PROCEDURE DROP( w: WORD );
BEGIN
END DROP;

VAR inch: CHAR;
    LowVideo: BOOLEAN;
    MyTerm: CARDINAL;

PROCEDURE KeyPressed(): BOOLEAN;
(* True if key pressed *)
BEGIN
  IF Pressed()>0 THEN inch:=Read(); RETURN TRUE  (* Leo *)
  ELSE inch:=0c; Sleep; RETURN FALSE             (* Leo *)
  END;                                           (* Leo *)
(*IF inch=0c THEN inch := BusyRead() END;
  RETURN  inch<>0C; *)
END KeyPressed;

PROCEDURE DirIn(): CHAR;

VAR
  svch: CHAR;

BEGIN
  REPEAT UNTIL KeyPressed();
  svch := inch;
  inch := 0c;
  RETURN svch;
END DirIn;

PROCEDURE GotoXY( x,y: CARDINAL );
(* Put cursor to line, col *)

BEGIN
  SetCrs( x-1, y-1 );
END GotoXY;

PROCEDURE Go_StatusLine();
(* Go and clear status line *)

BEGIN
  GotoXY( Lines, 1 );
  ClearLine();
  Retrieve(status);
END Go_StatusLine;

PROCEDURE Remove_Line(): BOOLEAN;
(* Deletes line & ret TRUE if del line code is present in terminal *)

BEGIN
  RETURN NOT DL();
END Remove_Line;

PROCEDURE InsertLine(): BOOLEAN;
(*Insert a empty line beFORe current line. True if this is done*)
BEGIN
  RETURN NOT IL();
END InsertLine;

PROCEDURE BlVidOn();
BEGIN
  CASE MyTerm OF
      2: Esc('<');Esc('['); WriteString("7m");
                  Esc('['); WriteString("?1h");
   |  3: Esc( CHAR(27h) );
   ELSE RETURN; END;
END BlVidOn;

PROCEDURE BlVidOff();
BEGIN
  CASE MyTerm OF
    2: Esc('<'); Esc('['); WriteString("27m");
                 Esc('['); WriteString("?1h");
   |3: Esc('(');
 ELSE END;
END BlVidOff;

PROCEDURE LowVidOn();
BEGIN
  IF LowVideo THEN RETURN END;
  LowVideo:= TRUE;
  CASE MyTerm OF
    2: Esc('<'); Esc('['); WriteString("2m");
                 Esc('['); WriteString("?1h");
   |3: Esc(',');
  ELSE END;
END LowVidOn;

PROCEDURE LowVidOff();
BEGIN
  IF LowVideo THEN
    LowVideo:= FALSE;
    CASE MyTerm OF
      2: Esc('<'); Esc('['); WriteString("32m");
                   Esc('['); WriteString("?1h");
     |3: Esc('+');
    ELSE END;
    END;
END LowVidOff;

PROCEDURE NormalVideo();
BEGIN
  (*BlVidOff();*)
  LowVidOff();
END NormalVideo;

PROCEDURE CtrlOut(ch: CHAR);
(* Write control char *)

BEGIN
  LowVidOn();
  Write( CHAR( ORD(ch)+ ORD('@') ) );
  LowVidOff();
END CtrlOut;

PROCEDURE Control( ch: CHAR ): CHAR;
(* Convert pressed key to control char *)

BEGIN
  ch:=CHAR(BITSET(ch)*{0..6});
  IF ch>='@' THEN ch := CHAR( BITSET(ch)*{0..4} ); END;
  RETURN ch;
END Control;

PROCEDURE ComLet( ch: CHAR ): CHAR;
(*Return 7-bit upper case char*)

BEGIN
  ch := Control(ch);
  IF ch<' ' THEN ch:= CHAR(ORD(ch) + ORD('@') ); END;
  RETURN ch;
END ComLet;

PROCEDURE MesLineWrite( VAR l: ARRAY OF CHAR);
VAR i: CARDINAL;

BEGIN
  i:= 0;
  LOOP
    CASE l[i] OF
     '[': LowVidOff();
    |']': LowVidOn();
    |0c : RETURN;
    ELSE Write(l[i]); END;
    INC(i);
    END;
END MesLineWrite;

PROCEDURE Mes_Write( mesno: CARDINAL);
(* Write a message *)

BEGIN
  CASE mesno OF
   0: WriteString("Find? ");
  |1: WriteString("Replace with? ");
  |2: WriteString("Options? (? for info) ");
  |3: WriteString("Illegal option, try again.");
  |4: WriteString("Not found.");
  |5: WriteString("Replace? Enter Y/N/Q ");
  |6: WriteString("Create or backup file error.");
  |7: WriteString("Writing");
  |8: WriteString(" Press ESC");
  |9: WriteString("Disk full or write error.");
  |10: WriteString("Unknown command.");
  |11: WriteString("Reading");
  |12: WriteString("Too few memory for running.");
  |13: WriteString("Disk full or close error.");
  |14: WriteString("Abandon edited version");
  |15: WriteString("Edited.");
  |16: WriteString("Not exists.");
  (*|17: WriteString(" Press ESC to continue"); *)
  |18: WriteString("Not exists or delete error.");
  |19: WriteString("Enter file to DELETE, directory or RET: ");
  |20: WriteString("Can't create lines longer than 255!");
  |21: WriteString(" Y/N");
  |22: WriteString("Block marker incorrect or not set!");
  |23: WriteString("Open or read error.");
  |24: WriteString("Not exists or open error. Create new file");
  |25: WriteString("Enter file, directory or RET: ");
  |26: WriteString("Memory buffer full.");
  |27: WriteString("... To interrupt press Y");
  |28: WriteString("Continue find?");
  |29: WriteString("Read error.");
  |30: WriteString("Wait");
  |31: WriteString("Marker not set!");
  |32: WriteString("Collecting garbage");
  |33: WriteString(" INSERT");
  ELSE
    WriteString("Unknown message");
    END;
END Mes_Write;

PROCEDURE Message( mesno, fileno: INTEGER; returncode: InterAct );
(* Type a message in statusline *)

VAR FN: ARRAY [0..80] OF CHAR;

BEGIN
  Go_StatusLine();
  Retrieve( status );
  IF fileno >= 0 THEN
    tFNGet( fileno, FN );
    WriteString( FN );
    END;
  IF mesno >= 0 THEN
    Write( ' ' );
    LowVidOn();
    Mes_Write( mesno );
    LowVidOff();
    END;
  CASE returncode OF
    abort: SetMode(TRUE);
           MayBeAbo( TRUE );
           (*del_tempfiles;*)
           HALT;
    |continue: RETURN;
    |escget,ask:
              Mes_Write(8);
              WHILE Control(DirIn())#CHAR(27) DO Write(7c); END;
              Go_StatusLine();
  END;
END Message;

PROCEDURE YNGet(): BOOLEAN;

VAR
  ch: CHAR;
  leganswer: BOOLEAN;

BEGIN
  Mes_Write( 21 );
  leganswer := FALSE;
  REPEAT
    ch := ComLet( DirIn() );
    IF (ch='Y') OR (ch='N') THEN
      leganswer := TRUE
    ELSE
      Write(7c);
    END;
  UNTIL leganswer;
  (*Go_StatusLine(); Clear line *)
  Retrieve( status );
  RETURN ch='Y';
END YNGet;

PROCEDURE UnImplemented();

BEGIN
  Message(10,-1,escget);
END UnImplemented;

PROCEDURE Aborted(): BOOLEAN;
(* True if user wants to abort *)

BEGIN
  IF NOT KeyPressed() THEN RETURN IsAborted; END;
  DROP( DirIn() );
  Message(27,-1,continue);
  IF ComLet(DirIn())='Y' THEN
    IsAborted:= TRUE;
    RETURN TRUE;
  ELSE
    Message(30,-1,continue);
    RETURN FALSE END;
END Aborted;

PROCEDURE Menu_Clear(intenable: BOOLEAN);
(*Clear the menu area & put cursor to start of it*)

VAR
 i: CARDINAL;

BEGIN
  Cur_Menu := 255; (*becomes undefined*)
  (*set_english;*)
  FOR i:=MenuLn TO 1 BY -1 DO
    IF intenable & KeyPressed() THEN RETURN; END;
    GotoXY( i, 1 );
    ClearLine();
    END;
END Menu_Clear;

PROCEDURE BigMessage( VAR m: Menu; intenable: BOOLEAN ): BOOLEAN;
(* Return true if printing interrupted *)

VAR i: CARDINAL;

BEGIN
  Menu_Clear(intenable);
  FOR i:=0 TO MenuLn-1 DO
    IF intenable & KeyPressed() THEN
      NormalVideo();
      RETURN TRUE; END;
    GotoXY(i+1,1);
    MesLineWrite( m[i] ); END;
  NormalVideo();
  RETURN FALSE;
END BigMessage;

PROCEDURE WriteMenu();
(*Write a menu to screen according current overlay # if help mode *)
CONST delay=450; (*for 2.x MHz frequenzy*)
VAR i: CARDINAL;

BEGIN
  GotoXY(CLineNr,CColNr);
  IF (Cur_Menu=ORD(Overlay)) OR (NOT ModeHelp) THEN RETURN END;
  IF Overlay#pEdit THEN (*So Main Menu is written immediately*)
    FOR i:=1 TO delay DO
      IF KeyPressed() THEN RETURN END;
      END; END;
  IF BigMessage( BigMTxt[ORD(Overlay)].tx,TRUE ) THEN Cur_Menu:= 255;
    ELSE Cur_Menu:= ORD(Overlay); END;
  GotoXY(CLineNr,CColNr);
END WriteMenu;

PROCEDURE WriteText( tno: CARDINAL );
(*Write long text (independent if help mode is on/off)
 to screen, set screen updating. Text numbering starts at zero*)

CONST toffset=5;

BEGIN
  DROP( BigMessage( BigMTxt[toffset+tno].tx, FALSE ));
  IF NOT ModeHelp THEN Retrieve(screen)
    ELSE Cur_Menu:= 255; END;
END WriteText;

PROCEDURE ReadFN( m,fno: CARDINAL): BOOLEAN;
(* Read & parse filename *)
(* False if user do'nt wish to enter *)

VAR
  FN: ARRAY [0..80] OF CHAR;
  err: BOOLEAN;

BEGIN
  LOOP
    Message( m,-1, continue );
    tFNGet( fno, FN );
    ReadString("",FN);
    IF FN[0]=0c THEN RETURN FALSE; END;
    tFNSet(fno, FN);
    IF IsDirectory(fno) THEN
      GotoXY(1,1);
      Clear();
      Retrieve(screen);
      Cur_Menu:= 255;
      IF PrintDir(fno,Lines-1) THEN Message(23,fno,escget); END;
      ELSE
        Go_StatusLine();
        RETURN TRUE; END;
     END;
END ReadFN;

PROCEDURE Ini1();
BEGIN
WITH BigMTxt[2] DO
tx[0]:=
"                    < < <  O N S C R E E N   M E N U  > > >";
tx[1]:=
"  [---Margins---]   :[-Line  Functions-]:"
"  [----Toggles----]  :  [-Other  Menus-]";
tx[2]:=
"L Set left margin : C  Center text  :J Justify   now    :(from Main only)";
tx[3]:=
"R Set right margin:                 :W Wrd Wrap  now    : K Block";
tx[4]:=
"                  :                 :I Indention now    : Q Quick P Print ";
tx[5]:=
"                  :                 :E Soft hyph now    : \ Onscreen";
tx[6]:=
"                  :                 :                   :Space Bar returns";
tx[7]:=
"                  :                 :                   :you to Main Menu.";
END;

WITH BigMTxt[3] DO
    tx[0]:=
"                   < < <     Q U I C K   M E N U     > > >";
tx[1]:=
"   [-- Cursor Movement --] : [-Delete-]"
" :  [--Miscellaneous--] : [--Other  Menus--]";
tx[2]:=
"S left side D right side :Y line  rt:F Find text in file : (from Main only)";
tx[3]:=
"E top scrn  X bottom scrn:DEL lin lf:A Find & Replace    : K Block";
tx[4]:=
"R top file  C end file              :                    : Q Quick P Print";
tx[5]:=
"B top block K end block             :                    : \ Onscreen";
tx[6]:=
"0-9 marker                          :                    : Space Bar returns";
tx[7]:=
"P previous  V last Find or Block    :                    : you to Main Menu.";
END;
END Ini1;

PROCEDURE Ini2(); BEGIN
WITH BigMTxt[4] DO
  tx[0]:=
"                    < < <   P R I N T   M E N U   > > >";
  tx[1]:=
"  [-------Special  Effects--------]  : [-Printing  Changes-] "
" :  [-Other  Menus-]";
  tx[2]:=
"(begin and end) : (one time each)  : A Alternate pitch═══: (from Main only)";
  tx[3]:=
"B Bold D Double : H Overprint char : N Standard pitch════: K Block";
  tx[4]:=
"S Underscore    :RET Overprint line: C Printing pause════: Q Quick P Print";
  tx[5]:=
"X Strikeout     : [--Reformatting--] : Y Other ribbon color: \ Onscreen";
  tx[6]:=
"V Subscript     : F Soft return    :  [--User  Patches--]  :Space Bar returns"
;
  tx[7]:=
"T Superscript   : _ User hyphen    : Q(1) W(2) E(3) R(4) :you to Main Menu.";
END;

WITH BigMTxt[5] DO
tx[0]:="";
tx[1]:=
"     Normally press return only, or enter one or more of:";
tx[2]:="";
tx[3]:=
"number=repeat count, B=search Backwards, W=whole Words only,";
tx[4]:=
"U=ignore case, N=replace w/o asking, G=replace in entire file,";
tx[5]:=
"D=display only lines matched";
tx[6]:="";
tx[7]:="";
END;
END Ini2;

VAR Pd: ProcessDesc;

BEGIN
  ScanFlags();
  IF Flag?('h') THEN
    Show("WordMaker parameters:");
    Show("-h Help");
    Show("-b Create backup Files");
    Show("t=<term type 0..3>");
    Show("Terminal types are: 1=15-ИЕ 2=Labtam in VT 52 mode");
    Show("                    3=Facit in enhanced VT 52 mode");
    HALT;
    END;
  IF NOT NumFlag?('t',MyTerm) THEN
    print(" Term# %d",MyTerm);
    SetTermType(MyTask(),MyTerm);
    END;
  CASE GetTermType(MyTask()) OF
    1: Show("ДВК (not tested)" );
   |2: Show("Labtam 3000");
   |3: Show("Facit 4420");
   ELSE Show("Unknown terminal"); END;
  MyTerm := GetTermType(MyTask());
  IF MyTerm#2 THEN Lines:=24 ELSE Lines:= 25; END;
  Cols := 80;
  MenuLn:= 8;
  SetMode(FALSE);
  MayBeAbo( FALSE );
  LowVideo:= FALSE;
  inch:= 0c;

  LowIni := ALLOC(0)+ 500h; (* 500h is very empirical*)
  Pd := MySelf();
  HighIni := Pd^ .H_reg;
  WriteString("Text buffer: ");
  print('%5hh..',WORD(LowIni));
  print('%5hh = ',WORD(HighIni));
  print('%4d',(INTEGER(HighIni)-INTEGER(LowIni)) DIV 256 );
  Show("K bytes");
  Cur_Menu := 255;
  TakeWord(SrcFName);
  IF SrcFName[0] = 0c THEN
    Overlay := pNoFile
   ELSE
    tFNSet( 0, SrcFName );
    Overlay:= pBufReset;
    END;

WITH BigMTxt[0] DO
tx[0]:=
"                   < < <      M A I N    M E N U     > > >";
tx[1]:=
"   [- Cursor Movement -]   :   [-Delete-]   "
":  [-Miscellaneous-]    :  [-Other Menus-]"
;tx[2]:=
"S char left D char right : G char       : I Tab    B Reform   :";
tx[3]:=
"A word left F word right : DEL char left: V INSERT ON/OFF     : K Block";
tx[4]:=
"E Line  up  X line down  : T word right : L Find/Replace again: Q Quick";
tx[5]:=
"     [--Scrolling--]       : Y line       : RETURN End paragraph: \ Onscreen";
tx[6]:=
"Z line up   W line down  : [--- Help ---] : ^ Insert empty line : P Print";
tx[7]:=
"C screen up R screen down: J Menu ON/OFF: U Undo last line    :"
END;

WITH BigMTxt[1] DO
tx[0]:=
"                   [< < <     B L O C K  M E N U      > > >]";
tx[1]:=
"[-Saving  Files-] : [-Block Operations-] "
":  [-File Operations-]  : [- Other Menus -]";
tx[2]:=
"S Save & resume : B  Begin  K  End   : R  Read             : (from Main only)"
;
tx[3]:=
"D Save--done    : H  Hide            :                     : K Block";
tx[4]:=
"X Save & exit   : C  Copy   Y  Delete: J  Delete  L  Look  : Q Quick P Print";
tx[5]:=
"Q Abandon file  : V  Move   W  Write :                     : \ Onscreen";
tx[6]:=
"[-Place Markers-] :                    "
":                     : Space Bar returns";
tx[7]:=
"0-9 set/hide 0-9:                    "
":                     : you to Main Menu.";
END;

  Ini1();
  Ini2();
END WMUtil.
