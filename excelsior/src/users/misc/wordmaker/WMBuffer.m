IMPLEMENTATION MODULE WMBuffer;
(* Buffer handling routines *)
(* Module for Wordmaker by Andrus Moor *)

FROM WMUtil    IMPORT Message,InterAct;
FROM WMFiles   IMPORT Read_Line;
FROM WMScreen  IMPORT White_Space;
FROM SYSTEM    IMPORT ADR,ADDRESS;
FROM KRONOS    IMPORT MOVE;
(*FROM StdIO     IMPORT ShowHex,ShowAndWait,Show,WriteLn;*)

VAR FreeLnChain: Line;   (*only Nxt field used*)

PROCEDURE NextAddress( VAR pl: pLine ): ADDRESS;

CONST adrsize=4;        (*Byte per address increment*)
      LineHDSize=2+1;   (*Words for line header +1 for length*)
(* ret first address after line pointed by pl *)

BEGIN
  RETURN ADDRESS( CARDINAL(pl) +
                   LineHDSize+  ORD(pl^. txt[0]) DIV adrsize);
END NextAddress;

PROCEDURE Freeze_Line( VAR fl: Line );
(* put line from chain to free chain *)
VAR nf,pf: pLine;
    p: pLine;

BEGIN
  (*Exclude line from old chain *)
  fl. Prv^. Nxt := fl. Nxt;
  fl. Nxt^. Prv := fl. Prv;
  (*p:= fl^.Prv; p^.Nxt:= fl^.Nxt;
  p:=fl^.Nxt; p^.Prv:=fl^.Prv; *)
  (* Include line to free line chain in increasing order *)
  (* assumed that all addresses < NIL *)
  pf:= ADR(FreeLnChain);
  nf:= FreeLnChain.Nxt;
  WHILE ADR(fl)>nf DO
    pf:= nf;
    nf:= nf^.Nxt; END;
  fl.Nxt:= nf;
  pf^.Nxt:= ADR(fl);
END Freeze_Line;

PROCEDURE MakeSinglePartition(VAR CurFree: pLine;
                              CurUsed,NextFree: pLine );
(* Input: ******     Output: ******
          ******             ******
CurFree^: ......             ******
          ......             ******
CurUsed^: ******   CurFree^: ......
          ******             ......
NextFree^: ......             ......
          ******                            *)

VAR NextLine: pLine;

BEGIN
  WHILE CurUsed#NextFree DO
    (***
    Show("Making single partition: ");
    ShowHex("ADR(CurFree)=",ADR(CurFree) ); WriteLn;
    ShowHex("CurFree=",CurFree); WriteLn;
    ShowHex("CurUsed=",CurUsed ); WriteLn;
    ShowHex("NextFree=",NextFree); WriteLn;
    ShowHex("NextLine=",NextLine); WriteLn;
    ShowAndWait("press key...");
    ***)
    ASSERT(CurUsed<NextFree);
    ASSERT(CurFree<CurUsed);
    (*correct pointers pointing to line becoming copied *)
    CurUsed^.Prv^.Nxt:= CurFree;
    CurUsed^.Nxt^.Prv:= CurFree;
    IF CurUsed=CurLine THEN CurLine:= CurFree; END;
    (*Compute next line to be moved*)
    NextLine:= NextAddress(CurUsed);
    MOVE(CurFree,CurUsed,CARDINAL(NextLine)-CARDINAL(CurUsed));
    CurUsed:= NextLine;
    CurFree:= NextAddress(CurFree);
    END;
END MakeSinglePartition;

PROCEDURE Make_Space( wrds: CARDINAL );
(* Try to make specified # of contiguous free memory at low memory*)
(* input: FreeLnChain: free chain in increasing order
          LowCur:      free memory
          LowIni,HighIni: buffer limits
          CurLine: pointer to be corrected *)

VAR
  nextfree,CurFree,CurUsed: pLine;

BEGIN
  Message(32,-1,continue);
  IF FreeLnChain.Nxt=NIL THEN RETURN; END;
  (* at least one free partition exists *)
  CurFree := FreeLnChain.Nxt;
  nextfree := CurFree;
  CurUsed := NextAddress(CurFree);
  LOOP
    nextfree := nextfree^.Nxt;
    IF nextfree#NIL THEN
      MakeSinglePartition(CurFree,CurUsed,nextfree);
      CurUsed:= NextAddress(nextfree);
    ELSE
      MakeSinglePartition(CurFree,CurUsed,LowCur);
      EXIT;
      END;
    END;
  LowCur:= CurFree;
  FreeLnChain.Nxt:= NIL;
END Make_Space;

PROCEDURE Free_Space(): BOOLEAN;
(* Make free space for insert specified # of lines *)
(* Display message & ret true if space not available *)

VAR ReqSp: CARDINAL;

BEGIN
  ReqSp:= 2*TSIZE(Line);
  IF (CARDINAL(HighCur)-CARDINAL(LowCur))>ReqSp THEN
    RETURN FALSE
   ELSE
     Make_Space( ReqSp );
     IF (CARDINAL(HighCur)-CARDINAL(LowCur))>ReqSp THEN
       RETURN FALSE
      ELSE
       Message( 26,-1,escget );
       RETURN TRUE;
       END;
    END;
END Free_Space;

PROCEDURE VTravel( dr: BOOLEAN ): BOOLEAN;
(* Vertical travel in buffer: line up/down *)
(* False on beginning/end of file *)

BEGIN (*VTravel*)
  IF dr THEN CurLine:= CurLine^. Nxt;
    IF CurLine<>ADR(TBeg) THEN RETURN TRUE
    ELSE CurLine:= CurLine^. Prv; END;
  ELSE
    (* Backward travelling*)
    CurLine := CurLine^. Prv;
    IF CurLine<>ADR(TBeg) THEN RETURN TRUE
      ELSE CurLine:= CurLine^. Nxt; END;
    END;
  RETURN FALSE;
END VTravel;

PROCEDURE Ins_Low();
(* Insert line into low memory before current line *)
(* Set current line to inserted line *)
(* Inc low memory pointer *)

VAR     hp: pLine;

BEGIN
  LowCur^. Nxt := CurLine;
  LowCur^. Prv := CurLine^. Prv;
  hp := CurLine;
  CurLine := CurLine^. Prv;
  CurLine^. Nxt := LowCur;
  hp^ . Prv := LowCur;
  CurLine := LowCur;
  LowCur:= NextAddress(LowCur);
END Ins_Low;

PROCEDURE Ins_Line(): BOOLEAN;
(* Insert line into low memory before current line *)
(* Set current line to inserted line *)
(* Does nothing & ret TRUE if buffer full *)

BEGIN
  IF Free_Space() THEN RETURN TRUE END;
  Ins_Low();
  RETURN FALSE;
END Ins_Line;

PROCEDURE Rem_Line(): BOOLEAN;
(* Remove current line from text *)
(* Ret FALSE & set current line to previous line if last line in file*)
(* i. e. TRUE if next line replaces current line, FALSE if previous
  line becomes current line *)

BEGIN
  IF VTravel(TRUE) THEN
    (*Not last line in file*)
    IF CurLine^ .Prv = ADR(TBeg) THEN
      (* Get previous line to memory *)
      ASSERT( VTravel(FALSE) );
      ASSERT( VTravel(TRUE) );
      END;
    (* Delete previous line *)
    Freeze_Line( CurLine^. Prv^ );
    RETURN TRUE;
  ELSE
    (*Current line is last in file*)
    IF VTravel( FALSE ) THEN
      (*Not first line in file*)
      Freeze_Line( CurLine^.Nxt^ );
      RETURN FALSE;
    ELSE
      (*Current line is only line in file *)
      LowCur^. txt[0] := 0c;
      IF Ins_Line() THEN RETURN TRUE END;
      Freeze_Line( CurLine^.Nxt^ );
      RETURN TRUE;
      END;
    END;
END Rem_Line;

PROCEDURE Replace_Line(): BOOLEAN;
(* Replace current line with low memory *)
(*Does nothing & ret TRUE if buffer full *)

BEGIN
  IF Ins_Line() THEN RETURN TRUE END;
  ASSERT( VTravel(TRUE) );
  IF Rem_Line() THEN
    (* not last line in file *)
    ASSERT( VTravel(FALSE) );
    END;
  RETURN FALSE;
END Replace_Line;

PROCEDURE Read_File( fno: CARDINAL; VAR Readed_Lines: CARDINAL): BOOLEAN;
(* Read previously opened file into text buffer before current line*)
(* Return TRUE if no space in buffer *)

VAR
  res: BOOLEAN;

BEGIN
  Readed_Lines := 0;
  res:= FALSE;
  Message( 11,fno, continue );
  LOOP
    IF Read_Line(fno,LowCur^. txt) THEN
      IF Ins_Line() THEN (* No free space *)
        res := TRUE;
        EXIT;
       ELSE
      IF NOT VTravel(TRUE) THEN CurLine := ADR(TBeg); END;
      INC( Readed_Lines );
      END;
    ELSE EXIT;
     END;
   END; (*LOOP*)
  RETURN res;
END Read_File;

PROCEDURE Gobble_White_Spaces( VAR Ln: LineType );
(*Remove trailing white spaces from line*)
BEGIN
  WHILE (ORD(Ln[0])>0) & White_Space(Ln[ ORD(Ln[0]) ]) DO
    Ln[0] := CHAR(ORD(Ln[0]) - 1); END;
END Gobble_White_Spaces;

PROCEDURE On_Line_Mode(): BOOLEAN;
(* copy current line to line buffer if not already copied *)
(* ret TRUE if buffer full: current line can'nt copied back*)

BEGIN
  IF LineMode THEN RETURN FALSE
    ELSIF Free_Space() THEN
      RETURN TRUE
     ELSE
      LineBuf := CurLine^. txt;
      LineMode := TRUE;
      RETURN FALSE;
    END;
END On_Line_Mode;

PROCEDURE Off_Line_Mode();

BEGIN
  IF LineMode THEN
    (* Current line needs replacing with line buffer *)
    Gobble_White_Spaces(LineBuf);
    LowCur^. txt:= LineBuf;
    ASSERT( NOT Replace_Line() );
    LineMode := FALSE;
    END;
END Off_Line_Mode;

(*  *  *     M  a  r  k  e  r  s     *  *  *)

PROCEDURE Set_Marker( mno: CARDINAL );
BEGIN
  Markers[mno].setted := TRUE;
  Markers[mno] .MLine := LOfset + CLineNr;
  Markers[mno] .MPos := CPos;
END Set_Marker;

PROCEDURE Clear_Marker( mno: CARDINAL );
BEGIN
  Markers[mno].setted := FALSE;
END Clear_Marker;

PROCEDURE Reset_Markers();
VAR mno: CARDINAL;

BEGIN
  FOR mno:=0 TO MaxMarker DO
    Markers[mno].setted := FALSE; END;
END Reset_Markers;

PROCEDURE CheckBlockHide();
BEGIN
  IF Markers[MBBegin].MLine>Markers[MBEnd].MLine THEN
    Clear_Marker(MBBegin);
    Clear_Marker(MBEnd);
    END;
END CheckBlockHide;

PROCEDURE LInsCorrect( nr: INTEGER );
(*Lines are inserted to buffer before current line, correct Markers*)
VAR mno: CARDINAL;

BEGIN
  FOR mno:=0 TO MaxMarker DO
    IF Markers[mno].setted THEN
      IF (LOfset+CLineNr) <=Markers[mno].MLine THEN
        (* Before marker *)
        INC( Markers[mno].MLine, nr ); END;
      END;
     END; (*for*)
END LInsCorrect;

PROCEDURE LRemCorrect( nr: CARDINAL);
(*Lines are removed from buffer at current line. Correct Markers*)
VAR mno,cline: CARDINAL;

BEGIN
  cline:= LOfset+CLineNr;
  FOR mno:=0 TO MaxMarker DO
    IF Markers[mno].setted & (Markers[mno].MLine>=cline) THEN
      (*Removing starts before marker*)
      IF (cline+nr-1)<Markers[mno].MLine THEN
        (*Removing all lines before BEGINning*)
        DEC(Markers[mno].MLine,nr);
      ELSE CASE mno OF
        MBBegin: INC(Markers[MBBegin].MLine, cline+nr-Markers[MBBegin].MLine);
                 CheckBlockHide();
       |MBEnd:   DEC(Markers[MBEnd].MLine,cline-Markers[MBEnd].MLine+1);
                 CheckBlockHide();
        ELSE Clear_Marker(mno); END;
      END;
      END; END;
END LRemCorrect;

PROCEDURE MoveBytes( cnt: CARDINAL; VAR src,dest: ARRAY OF CHAR;
                     srcind,dstind: CARDINAL );
(* Move bytes. Use special instruction if available *)

BEGIN
  FOR srcind:=srcind TO srcind+cnt-1 DO
    dest[dstind]:= src[srcind];
    INC(dstind);
    END;
END MoveBytes;

PROCEDURE Delete( VAR ln: ARRAY OF CHAR; index, sz: CARDINAL );
(* Delete size bytes from line starting at index *)
BEGIN
  MoveBytes( ORD(ln[0])-index-sz+1, ln,ln, index+sz, index );
  ln[0] := CHAR(ORD(ln[0])-sz);
END Delete;

PROCEDURE Travelling( Req_Lines: INTEGER ): CARDINAL;
(*Returns absolute # of lines actually travelled*)
VAR
  i: CARDINAL;
  dir: BOOLEAN;

BEGIN
  dir := Req_Lines>0;
  FOR i:=1 TO ABS(Req_Lines) DO
    IF NOT VTravel(dir) THEN RETURN i-1 END;
    END;
  RETURN ABS(Req_Lines);
END Travelling;

PROCEDURE BufIni();
BEGIN
  (***
  ShowHex( "LowIni=",LowIni );
  ShowHex( "ADR(FreeLnChain)=",ADR(FreeLnChain) );
  ShowHex( "ADR(LowCur)=",ADR(LowCur) );
  ShowHex( "ADR(CurLine)=",ADR(CurLine) );
  ShowAndWait( "Press key..");
  **)
  IF (CARDINAL(HighIni)-CARDINAL(LowIni))<=2*TSIZE(Line) THEN
    Message(12,-1,abort); END;
  LowCur:= LowIni;
  HighCur:= HighIni;
  FreeLnChain.Nxt:= NIL;
  TBeg. First:= ADR(TBeg);
  TBeg. Last:= ADR(TBeg);
  CurLine:= ADR(TBeg);
  LineMode:= FALSE;
END BufIni;

BEGIN
  ModeInserting:=TRUE;
  ModeIndention:=TRUE;
  ModeSoftHyph:=TRUE;
  ModeHelp:=TRUE;
  ModeAutoNewLine:=FALSE;
  ModeSpacing:=TRUE;
  RMrgn:=65;
  LMrgn:=1;
END WMBuffer.
