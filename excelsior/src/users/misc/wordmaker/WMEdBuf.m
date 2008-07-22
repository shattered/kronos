IMPLEMENTATION MODULE WMEdBuf;
(* Buffer changing *)
(* For Wordmaker by Andrus Moor *)

FROM WMScreen   IMPORT Text_Beg,EdWriteToEOL,VCorrect,Roll_Up,EdChOut,NextTab,
                       Screen_Position,Retrieve,Word_Delim,Place_Compute,
                       SamePos;
FROM WMUtil     IMPORT GotoXY,KeyPressed,DirIn,InsertLine,Int_Type,Remove_Line,
                       Message,Control,InterAct,DROP,Interrupt,WriteMenu,
                       Lines,Cols,Soft_CR,ht;
FROM WMBuffer   IMPORT Ins_Line,On_Line_Mode,Off_Line_Mode,Rem_Line,
                       VTravel,Replace_Line,LInsCorrect,LRemCorrect,Delete,
                       pLine,Line, CurLine ,Overlay,Next_Proc,
                       LOfset,COfset,CLineNr,CColNr,CPos,LLimit,
                       LineMode,LineBuf,LowCur,RMrgn,LMrgn,
                       ModeInserting, ModeIndention,ModeHelp,ModeAutoNewLine,
                       Gobble_White_Spaces,MoveBytes,Travelling;
FROM Terminal   IMPORT Clear,ClearLine;
FROM WMReform   IMPORT DistributeSpaces;

PROCEDURE Right_Word(VAR SrcLine: LineType; start: PosNo ): PosNo;
(* Find end of current or start of next word starting search at "start" *)
BEGIN
  IF Word_Delim(SrcLine[start]) THEN
    (* Find start of next word *)
    WHILE (start<=ORD(SrcLine[0])) & Word_Delim(SrcLine[start])
          & (start<255) DO
      INC( start ); END;
  ELSE (* Find end of current word *)
    WHILE (NOT Word_Delim(SrcLine[start])) & (start<=ORD(SrcLine[0]))
        & (start<255) DO
    INC(start); END;
    END;
  RETURN start
END Right_Word;

PROCEDURE Left_Word(VAR SrcLine: LineType; start: PosNo ): PosNo;
(* Find start of previous word starting search at "start" *)

BEGIN
  IF start>ORD((SrcLine[0]))+1 THEN
    RETURN ORD(SrcLine[0]) + 1;
    END;
  IF start=1 THEN RETURN 1 END;
  (* Find end of previous word *)
  REPEAT
    DEC(start );
    UNTIL (start=1) OR (NOT Word_Delim(SrcLine[start]) );
  (* Find begin of previous word *)
  WHILE ( NOT Word_Delim(SrcLine[start]) ) & ( start>1 ) DO
    DEC(start );
    END;
  IF start>1 THEN
    RETURN start+1
   ELSE RETURN 1;
    END;
END Left_Word;

PROCEDURE Ins_Line_Buffer(): BOOLEAN;
(* Insert Line buffer before current Line, return TRUE if no space *)

BEGIN
  LowCur^.txt:=LineBuf;
  RETURN Ins_Line();
END Ins_Line_Buffer;

PROCEDURE BufInsert();
(* Insert Line buffer before current Line, set updating OF screen *)

BEGIN
  Off_Line_Mode();
  IF Ins_Line_Buffer() THEN RETURN END;
  LInsCorrect(1);
  GotoXY( CLineNr, 1 );
  IF InsertLine() THEN
    EdWriteToEOL(CurLine^.txt,1,LOfset+CLineNr);
    Retrieve(status);
   ELSE
    Clear();
    Retrieve( eos ); END;
END BufInsert;

PROCEDURE Insert( ch: CHAR; VAR dest: ARRAY OF CHAR; index: CARDINAL );
VAR cnt: CARDINAL;

BEGIN
  FOR cnt:=ORD(dest[0]) TO index BY -1 DO
    dest[cnt+1] := dest[cnt]; END;
  dest[index] := ch;
  dest[0] := CHAR(ORD(dest[0])+1);
END Insert;

PROCEDURE InsertNextLine();
(*Insert a Line to screen after current Line, set cursor to this Line*)
BEGIN
  ASSERT(VTravel(TRUE));
  IF VCorrect(1,FALSE) THEN
    Roll_Up(CurLine^.txt);
   ELSE (*Current Line is not last in screen, insert a Line to screen*)
    GotoXY(CLineNr,1);
    IF InsertLine() THEN
      EdWriteToEOL(CurLine^.txt,1,LOfset+CLineNr);
      Retrieve(status);
     ELSE
      Clear();
      Retrieve(eos); END;
    END;
END InsertNextLine;

PROCEDURE Auto_Return(): BOOLEAN;
(* Return TRUE if auto newLine insterted, FALSE if not *)
(* Assumed that current line is in line buffer *)

VAR
  beg_prv_wrd,
  Right_Len: PosNo;
  cnt, text_start: PosNo;

BEGIN
  (* Insert soft cr ? *)
  beg_prv_wrd := Left_Word( LineBuf,CPos );
  IF (beg_prv_wrd<=LMrgn) THEN
    (* No: word too long to fit into margins *)
    RETURN FALSE;
   ELSE
    (* Yes: insert soft cr *)
    IF ModeIndention THEN
      text_start := Screen_Position( LineBuf, Text_Beg( LineBuf ) );
     ELSE text_start := LMrgn;
      END;
    (*Set next Line*)
    Right_Len := ORD(LineBuf[0]) - beg_prv_wrd + 1;
    FOR cnt:=1 TO text_start-1 DO
      LowCur^.txt[cnt] := ' ';
      END;
    MoveBytes( Right_Len,LineBuf, LowCur^.txt, beg_prv_wrd, text_start);
    LowCur^. txt[0] := CHAR( text_start + Right_Len - 1 );
    INC( CPos, text_start - beg_prv_wrd + 1 );
    (*On_Line_Mode quarantees space for insertion at least one Line: *)
    ASSERT( NOT Replace_Line() );

    (*Set current Line*)
    LineBuf[0]:= CHAR(beg_prv_wrd-1);
    Gobble_White_Spaces(LineBuf);
    LineBuf[ORD(LineBuf[0])+1]:= CHAR(Soft_CR);
    LineBuf[0]:= CHAR(ORD(LineBuf[0])+1);
    DistributeSpaces();

    IF NOT Ins_Line() THEN LInsCorrect(1); END;
    LineMode := FALSE;
    GotoXY( CLineNr, 1 );
    ClearLine();
    EdWriteToEOL( CurLine^ .txt,1,LOfset+CLineNr );
    InsertNextLine();
    RETURN TRUE;
  END; (*soft cr inserting*)
END Auto_Return;

PROCEDURE CharWrite( ch:CHAR );
(*  Write a letter *)

VAR
  eolw: BOOLEAN;
  cnt: CARDINAL;

BEGIN (* CharWrite *)
  IF On_Line_Mode() THEN RETURN END;
  eolw := FALSE;
  (* Set Line buffer according to readed CHAR *)
  IF CPos<=ORD(LineBuf[0]) THEN
    (* Cursor is in text part of Line *)
    IF ModeInserting THEN
      IF ORD(LineBuf[0])=255 THEN
        Gobble_White_Spaces(LineBuf);
        IF ORD(LineBuf[0])=255 THEN
          Message( 20, -1, escget ); (*Line too big*)
          RETURN; END;
        END;
      (* Insert a letter into Line *)
      Insert( ch, LineBuf, CPos )
     ELSE
      (* Overwrite current letter *)
      IF LineBuf[CPos]=CHAR(ht) THEN 
        eolw := TRUE;
        ClearLine();
        END;
      LineBuf[CPos] := ch;
     END;
    (*common code for text part of Line: *)
   ELSE
    (* Cursor is after last letter in Line *)
    (* Fill with spaces *)
    FOR cnt:=ORD(LineBuf[0])+1 TO CPos-1 DO
      LineBuf[cnt] := ' ';
      END;
    LineBuf[CPos] := ch;
    LineBuf[0] := CHAR( CPos );
   END;
  (*Common for Character Write *)
 IF ModeAutoNewLine & (COfset+CColNr = RMrgn) THEN
   IF Auto_Return() THEN RETURN; END; END;
 IF ModeInserting OR eolw THEN
   EdWriteToEOL(LineBuf,CColNr, LOfset+CLineNr )
  ELSE EdChOut( ch ); END;
  IF CPos<255 THEN INC(CPos); END;
END CharWrite;

PROCEDURE New_Line();
(* RETURN key pressing handler *)

VAR
  voffset: [0..1];
  New_CPos: PosNo;
  CharsRight: [0..255];

BEGIN
  IF ModeInserting THEN
      IF On_Line_Mode() THEN RETURN END;
      Gobble_White_Spaces(LineBuf);
      (* Insert new Line after current Line *)
      IF CPos>ORD(LineBuf[0]) THEN
          (*Cursor not in tex part of Line, next Line becomes empty *)
          LowCur^. txt[0] := 0c;
          IF ModeIndention THEN
            New_CPos := Text_Beg( LineBuf )
          ELSE New_CPos := LMrgn; END;
      ELSE
          (* Copy right part to next Line *)
          CharsRight := ORD(LineBuf[0]) - CPos + 1;
          MoveBytes( CharsRight, LineBuf, LowCur^.txt, CPos, 1);
          LineBuf[0] := CHAR( CPos-1 );
          LowCur^ .txt[0] := CHAR( CharsRight );
          New_CPos := 1;
          ClearLine();
          EdWriteToEOL( LineBuf,CColNr,LOfset+CLineNr);(*write block video*)
          END;
      (*Now Line buffer=new previous Line, lowcyr^=new next Line *)
      (* Replace CurLiner with new next Line *)
      ASSERT( NOT Replace_Line() );
      (* Insert previous Line before new next Line *)
      IF NOT Ins_Line_Buffer() THEN LInsCorrect(1); END;
      (* Travel to new current Line *)
      LineMode := FALSE;
      InsertNextLine();
  ELSE  (*not in insert MODe*)
      Off_Line_Mode();
      (* Put cursor to BEGIN of text in next Line *)
      IF VTravel(TRUE) THEN
        (* Not in last Line of text *)
        voffset := 1
      ELSE
        (* Current Line is last in text *)
        voffset := 0; END;
      IF ModeIndention THEN
        New_CPos := Text_Beg(CurLine^ .txt)
      ELSE New_CPos := LMrgn; END;
      IF VCorrect( voffset, FALSE ) THEN
        Roll_Up( CurLine^ .txt ); END;
      END; (*if ins/overwrite MODes*)
  CPos:= New_CPos;
END New_Line;

PROCEDURE Tabulation();
(* Cursor to next tabulator, in insert mode insert tabulator *)
  
BEGIN
  IF On_Line_Mode() THEN RETURN END;
  IF ModeInserting & (CPos<=ORD(LineBuf[0])+1) THEN
    CharWrite( CHAR(ht) )
  ELSE (*Cursor to next tabulator*)
    CPos := SamePos( LineBuf, NextTab(COfset+CColNr) );
    END;
END Tabulation;

PROCEDURE Del_Char();
(*Delete current char *)
VAR cnt: CARDINAL;
  
BEGIN
  IF On_Line_Mode() THEN RETURN END;
  IF CPos<=ORD(LineBuf[0]) THEN
      Delete( LineBuf, CPos, 1 );
      ClearLine();
      EdWriteToEOL( LineBuf,CColNr,LOfset+CLineNr );
   ELSE IF NOT VTravel(TRUE) THEN RETURN; END;
     (*Not last Line of text *)
    IF CPos+ORD(CurLine^ .txt[0])> 255 THEN
      Message( 20, -1, escget );
      ASSERT(VTravel(FALSE)); RETURN; END;
    ASSERT( VTravel(FALSE) );
    (*Concat current and next Lines*)
    ASSERT( Rem_Line() );
    FOR cnt:=ORD(LineBuf[0])+1 TO CPos-1 DO
      LineBuf[cnt] := ' '; END;
    MoveBytes( ORD(CurLine^.txt[0]), CurLine^ .txt, LineBuf, 1, CPos );
    LineBuf[0] := CHAR( CPos+ORD(CurLine^ .txt[0])-1 );
    DROP( Rem_Line() );
    DROP( Ins_Line_Buffer() );
    LRemCorrect(1);
    LineMode := FALSE;
    GotoXY(CLineNr,1);
    Clear();
    Retrieve(eos);
    END;
END Del_Char;

PROCEDURE Del_Word_Right();

VAR
  newpos: PosNo;

BEGIN
  IF On_Line_Mode() THEN RETURN END;
  IF CPos<=ORD(LineBuf[0]) THEN
      newpos := Right_Word( LineBuf, CPos );
      Delete( LineBuf, CPos, newpos-CPos );
      ClearLine();
      EdWriteToEOL(LineBuf,CColNr,LOfset+CLineNr );
   ELSE Del_Char(); END;
END Del_Word_Right;

PROCEDURE Line_Del();
(* Delete current Line *)
VAR trvlcnt,actcnt: CARDINAL;
BEGIN
  IF NOT LineMode THEN LineBuf := CurLine^. txt;
  ELSE LineMode := FALSE; END;
  IF NOT Rem_Line() THEN
    (*Previous line replaces current line*)
    IF VCorrect( -1, FALSE ) THEN Retrieve( screen )
      ELSE
      GotoXY(CLineNr+1,1);
      ClearLine(); END;
    LRemCorrect( 1 );
  ELSE (*next line replaces current line*)
    LRemCorrect( 1 );
    IF Remove_Line() THEN
      (*Terminal is capable to remove line*)
      GotoXY(LLimit[TRUE],1);
      ClearLine();
      trvlcnt:= LLimit[TRUE]-CLineNr;
      actcnt:= Travelling(trvlcnt);
      IF actcnt=trvlcnt THEN
        (*Last Line in screen exists*)
        EdWriteToEOL( CurLine^.txt,1,LOfset+LLimit[TRUE] );
        Retrieve(status); END;
      ASSERT(Travelling(-actcnt)=actcnt);
     ELSE GotoXY( CLineNr, 1 );
      Clear();
      Retrieve( eos ); END;
    END;
END Line_Del;

PROCEDURE Empty_Line();
(* Insert empty Line into buffer *)

BEGIN
  Off_Line_Mode();
  LineBuf[0] := 0c;
  BufInsert();
END Empty_Line;

PROCEDURE UnDo();

BEGIN
  IF LineMode THEN
    GotoXY( CLineNr, 1 );
    ClearLine();
    EdWriteToEOL(CurLine^. txt,1, LOfset+CLineNr );
    LineMode := FALSE;
  ELSE
    BufInsert();
    END;
END UnDo;

PROCEDURE Del_Prev_Char();

BEGIN
  IF On_Line_Mode() THEN RETURN END;
  IF CPos=1 THEN RETURN END;
  (*Is possible to go to previous column*)
  DEC(CPos);
  IF CPos<=ORD(LineBuf[0]) THEN
    (*Current position is in text part of line, must remove *)
    Delete( LineBuf, CPos, 1 );
    Place_Compute(LineBuf);
    GotoXY( CLineNr, CColNr );
    ClearLine();
    EdWriteToEOL( LineBuf,CColNr, LOfset+CLineNr);
    END;
END Del_Prev_Char;

PROCEDURE Print_Menu();

VAR ch: CHAR;

BEGIN
  WriteMenu();
  ch := Control(DirIn());
  Overlay := pEdit;
  IF ch>=' ' THEN RETURN
    ELSE CharWrite( ch );
    END;
END Print_Menu;

END WMEdBuf.
