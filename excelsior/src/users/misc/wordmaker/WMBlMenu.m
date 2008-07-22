IMPLEMENTATION MODULE WMBlMenu;
(* Block operations, jumping *)
(* For Wordmaker by Andrus Moor *)

FROM WMUtil     IMPORT Message,InterAct,YNGet,Control,UnImplemented,DirIn,DROP,
                       ReadFN,Int_Type,WriteMenu,
                       KeyPressed,ComLet,GotoXY,Lines;
FROM WMBuffer   IMPORT Overlay,Next_Proc,LOfset,CLineNr,CPos,CurLine,
                       CColNr,LineBuf,LineMode,Cur_Menu,
                       Marker,Markers,Set_Marker,Clear_Marker,
                       VTravel,Rem_Line,pLine,Line,TBeg,Off_Line_Mode,
                       MBBegin,MBEnd,MPrevious,MLastFB,Reset_Markers,LowCur,
                       LInsCorrect,LRemCorrect,Ins_Line,Travelling;
FROM WMFiles    IMPORT tFNSet,tOpen,tCreate,Write_Line,Read_Line,tClose,
                       SrcFName,tCloseUnchanged,File_Delete,
                       TempFile_Revert,TempFile_Set;
FROM WMScreen   IMPORT EdWriteToEOL,WriteToEOL,BlockMarked,VCorrect,
                       MoveToMarker,Retrieve;
FROM Terminal   IMPORT Clear,WriteLn,ClearLine,SetMode;
FROM SYSTEM     IMPORT ADR;

CONST
  DestFNo = 0;
  BlockFNo = 0;

VAR Changed: BOOLEAN;

PROCEDURE ComputeChanged();
VAR i: CARDINAL;
BEGIN
  FOR i:=0 TO 255 DO
    IF LineBuf[i]#0c THEN
      Changed:= TRUE;
      RETURN;
      END; END;
END ComputeChanged;

(* * * *   S a v i n g    f i l e s   * * * *)
PROCEDURE Save_Old_File(): BOOLEAN;
(* Create output file. Write whole buffer to disk. Close output file *)
(* Display message, delete output file & ret TRUE on write error *)
VAR
  OldCL: pLine; (*Used to restore current line on error*)

BEGIN
  Off_Line_Mode();
  tFNSet( DestFNo, SrcFName );
  Message( -1,DestFNo,continue );
  IF tCreate( DestFNo ) THEN
    Message( 6, DestFNo, escget );
    RETURN TRUE;
    END;
  Message( 7,DestFNo, continue ); (* Writing *)
  OldCL := CurLine;
  CurLine := ADR(TBeg);
  WHILE VTravel(TRUE) DO
    IF Write_Line( DestFNo, CurLine^.txt ) THEN
      Message(9,DestFNo, escget); (*write errror*)
      IF File_Delete(DestFNo) THEN Message( 18,DestFNo,escget) END;
      CurLine:= OldCL;
      RETURN TRUE;
      END;
    END;
  IF tClose( DestFNo) THEN
    Message( 13, DestFNo, escget );
    IF File_Delete(DestFNo) THEN Message(18,DestFNo,escget); END;
    CurLine := OldCL;
    RETURN TRUE;
    END;
  Changed:= FALSE;
  RETURN FALSE;
END Save_Old_File;

PROCEDURE Save_Done();
BEGIN
  IF Save_Old_File() THEN RETURN END;
  Reset_Markers();
  Overlay := pNoFile;
END Save_Done;

PROCEDURE Abandon_File();
BEGIN
  tFNSet( DestFNo, SrcFName );
  ComputeChanged();
  IF Changed THEN
    Message( 14,DestFNo, continue );
    IF NOT YNGet() THEN RETURN; END;
    END;
  Changed:= FALSE;
  Reset_Markers();
  Overlay := pNoFile;
END Abandon_File;

PROCEDURE Save_Resume();
BEGIN
  IF Save_Old_File() THEN RETURN END;
  Set_Marker(MPrevious);
  Overlay := pBufReset;
END Save_Resume;

PROCEDURE Save_Exit();

BEGIN
  IF Save_Old_File() THEN RETURN END;
  Message( 15, DestFNo, abort );
END Save_Exit;

(*  *  *   P l a c e    m a r k e r s    *  *  *)
(*PROCEDURE TypeMarker();
  Overwrite marker in current line
BEGIN
  ClearLine();
  IF LineMode THEN EdWriteToEOL(LineBuf,CColNr,LOfset+CLineNr)
   ELSE EdWriteToEOL(CurLine^.txt,CColNr,LOfset+CLineNr); END;
END TypeMarker; **)

PROCEDURE SetHide09(m: CARDINAL);
BEGIN
  IF Markers[m].setted & (Markers[m].MLine=LOfset+CLineNr) THEN
    (*Must hide marker*)
    Clear_Marker(m);
    ELSE (*must set marker*)
      Set_Marker(m); END;
  (*TypeMarker();*)
END SetHide09;

PROCEDURE MarkBlock( m: CARDINAL);
BEGIN
  Set_Marker(m);
  IF BlockMarked() THEN Retrieve(screen)
    ELSE (*TypeMarker(); *)END;
END MarkBlock;

PROCEDURE HideBlock();
BEGIN
  Clear_Marker(MBBegin);
  Clear_Marker(MBEnd);
  Retrieve(screen);
END HideBlock;

(*  *  *   F i l e   o p e r a t i o n s   *  *  *)
PROCEDURE FileDelete();
BEGIN
  IF File_Delete(BlockFNo) THEN
    Message(18,BlockFNo,escget);
    END;
END FileDelete;

PROCEDURE Delete_File();
BEGIN
  IF ReadFN(19,BlockFNo) THEN
    FileDelete(); END;
END Delete_File;

PROCEDURE FileOpen(): BOOLEAN;
(*Read file name & open block file *)
(*Ret TRUE if file not opened*)

BEGIN
  IF NOT ReadFN(25,BlockFNo) THEN RETURN TRUE; END;
  IF tOpen(BlockFNo) THEN
    Message(23, BlockFNo, escget);
    RETURN TRUE;
    END;
  RETURN FALSE;
END FileOpen;

PROCEDURE FileCloseUnchanged();
(*Close unchanged block file *)

BEGIN
  IF tCloseUnchanged(BlockFNo) THEN
    Message(13,BlockFNo,escget);
    END;
END FileCloseUnchanged;
  
PROCEDURE FileRead(): BOOLEAN;
(*Read previously opened block file before current line*)
(*Correct Markers according to readed lines *)
(*Set block to readed file, current line to first readed line*)
(*Ret TRUE if file not readed*)

VAR
  Readed_Lines: CARDINAL;
  err: BOOLEAN;

BEGIN
  Message( 11,BlockFNo,continue); (*Reading*)
  Readed_Lines := 0;
  err:= FALSE;
  LOOP
    IF Read_Line(BlockFNo,LowCur^.txt) THEN
      IF Ins_Line() THEN err:=TRUE; EXIT; END;
      INC(Readed_Lines);
      ASSERT(VTravel(TRUE));
     ELSE EXIT; END;
    END;
  ASSERT(Travelling(-Readed_Lines)=Readed_Lines);
  IF err THEN
    FOR Readed_Lines:=Readed_Lines TO 1 BY -1 DO
      ASSERT(Rem_Line()); END;
    RETURN TRUE; END;
  LInsCorrect( Readed_Lines );
  Markers[MBBegin] .setted := TRUE;
  Markers[MBBegin] .MLine := LOfset+CLineNr;
  Markers[MBBegin] .MPos := 1;
  Markers[MBEnd] .setted := TRUE;
  Markers[MBEnd] .MLine := LOfset+CLineNr+Readed_Lines-1;
  Markers[MBEnd] .MPos := 1;
  RETURN FALSE;
END FileRead;

PROCEDURE BlockWrite(): BOOLEAN;
(*Set last pos marker to current pos, travel to block BEGIN*)
(*Write block to block file, display Message & ret TRUE on error*)

VAR
  LineCount: CARDINAL;

BEGIN
  Set_Marker(MPrevious);
  MoveToMarker(MBBegin);
  FOR LineCount:=1 TO Markers[MBEnd].MLine-Markers[MBBegin].MLine+1 DO
    IF Write_Line(BlockFNo,CurLine^.txt) THEN
      (* NB! next line is generally not correct IF auto disk buffering used*)
      ASSERT(Travelling(-LineCount+1)=-LineCount+1);
      Message(9,BlockFNo,escget);
      FileDelete();
      RETURN TRUE;
      END;
    IF NOT VTravel(TRUE) THEN (*Last line of file*)
      DEC(LOfset); END;
    END;
  IF VCorrect(Markers[MBEnd].MLine-Markers[MBBegin].MLine+1,TRUE) THEN
    Retrieve(screen); END;
  RETURN FALSE;
END BlockWrite;

PROCEDURE Read_File();
(*Read a file into buffer before current line, set block Markers to this
  file *)

BEGIN
  IF FileOpen() THEN RETURN END;
  IF FileRead() THEN FileCloseUnchanged(); RETURN; END;
  GotoXY(CLineNr,1);
  Clear();
  FileCloseUnchanged();
  Retrieve(eos);
  Changed:= TRUE;
END Read_File;

PROCEDURE Look_File();
(* Type a file to screen *)

VAR
  NotTerminated: BOOLEAN;

BEGIN
  IF FileOpen() THEN RETURN END;
  NotTerminated := TRUE;
  WriteLn();
  WHILE Read_Line(BlockFNo,LowCur^.txt) & NotTerminated DO
    WriteToEOL(LowCur^.txt,0,1);
    WriteLn();
    IF KeyPressed() THEN
      DROP(DirIn());
      Message(27,BlockFNo,continue);
      NotTerminated:= ComLet(DirIn()) # 'Y';
      GotoXY(Lines,1);
      ClearLine();
      END;
    END;
  Cur_Menu := 255;
  IF NotTerminated THEN
    WriteLn();
    Message(-1, BlockFNo,escget);
    END;
  Retrieve(screen);
  FileCloseUnchanged();
END Look_File;

(*  *  *  B l o c k   o p e r a t i o n s  *  *  *)
PROCEDURE Check_Block(): BOOLEAN;
(*Display Message & ret TRUE if block Markers are not set or incorrect*)

BEGIN
  IF BlockMarked() THEN
    RETURN FALSE ELSE
   Message(22,-1,escget);
   RETURN TRUE;
  END;
END Check_Block;

PROCEDURE Write_Block();

BEGIN
  IF Check_Block() THEN RETURN END;
  IF NOT ReadFN(25,BlockFNo) THEN RETURN; END;
  IF tCreate(BlockFNo) THEN
    Message(6,BlockFNo,escget); 
    RETURN;
    END;
  IF BlockWrite() THEN RETURN END;
  IF tClose(BlockFNo) THEN
    Message(13,BlockFNo,escget);
    FileDelete();
    END;
  MoveToMarker( MBBegin );
END Write_Block;

PROCEDURE BlockDelete();
(*Delete block from buffer, correct other Markers*)
VAR
  RemCount: CARDINAL;

BEGIN
  FOR RemCount:=Markers[MBBegin].MLine TO Markers[MBEnd].MLine-1 DO
    ASSERT(Rem_Line()); END;
  DROP(Rem_Line());
  LRemCorrect( Markers[MBEnd].MLine-Markers[MBBegin].MLine+1 );
END BlockDelete;

PROCEDURE Delete_Block();

BEGIN
  IF Check_Block() THEN RETURN END;
  Set_Marker(MPrevious);
  MoveToMarker(MBBegin);
  BlockDelete();
  Clear_Marker(MBBegin);
  Clear_Marker(MBEnd);
  Retrieve(screen);
  Changed:= TRUE;
END Delete_Block;

PROCEDURE BlockRead(): BOOLEAN;
(* Revert temporary file, read block from it at last pos*)
(* Delete temporary file, set screen updating *)
(* Return TRUE if not readed *)

BEGIN
  IF TempFile_Revert(BlockFNo) THEN (*write error*)
    Message(9,BlockFNo,escget);
    FileDelete();
    RETURN TRUE;
    END;
  MoveToMarker(MPrevious);
  IF FileRead() THEN FileDelete(); RETURN TRUE; END;
  FileDelete();
  Retrieve(screen);
  RETURN FALSE;
END BlockRead;

PROCEDURE Copy_Block;
BEGIN
  IF Check_Block() THEN RETURN END;
  TempFile_Set(BlockFNo);
  IF BlockWrite() THEN RETURN END;
  Changed:= TRUE;
  IF BlockRead() THEN RETURN END;
END Copy_Block;

PROCEDURE Move_Block();
BEGIN
  IF Check_Block() THEN RETURN END;
  TempFile_Set(BlockFNo);
  Markers[MLastFB]:= Markers[MBBegin];
  IF BlockWrite() THEN RETURN END;
  IF BlockRead() THEN RETURN END;
  MoveToMarker( MLastFB );
  BlockDelete();
  MoveToMarker(MBBegin);
  Changed:= TRUE;
END Move_Block;

PROCEDURE BlockMenu();
VAR comch: CHAR;

BEGIN
  WriteMenu();
  Overlay := pEdit;
  comch := Control(DirIn());
  CASE comch OF
     (*B*) 2c: MarkBlock(MBBegin);
    |(*C*) 3c: Copy_Block();
    |(*D*) 4c: Save_Done();
    |(*H*)10c: HideBlock();
    |(*J*)12c: Delete_File();
    |(*K*)13c: MarkBlock(MBEnd);
    |(*L*)14c: Look_File();
     (*P  20c: Print_File(); *)
    |(*Q*)21c: Abandon_File();
    |(*R*)22c: Read_File();
    |(*S*)23c: Save_Resume();
    |(*V*)26c: Move_Block();
    |(*W*)27c: Write_Block();
    |(*X*)30c: Save_Exit();
    |(*Y*)31c: Delete_Block();
    |'0'..'9': SetHide09( ORD(comch)-ORD('0') );
    |' '     : ;
  ELSE
    UnImplemented();
  END;
  SetMode(FALSE); (*fix Facit terminal file oper*)
END BlockMenu;

BEGIN
  Changed:= FALSE;
END WMBlMenu.
