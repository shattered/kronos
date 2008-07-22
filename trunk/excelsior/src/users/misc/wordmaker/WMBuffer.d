DEFINITION MODULE WMBuffer;
(* Buffer handling routines *)
(* Module for Wordmaker by Andrus Moor *)

CONST
  MaxMarker= 13;
  MBBegin = 13;
  MBEnd = 12;
  MPrevious = 11;
  MLastFB = 10;

TYPE
  LineType = ARRAY [0..255] OF CHAR;
  pLine = POINTER TO Line;
  Line = RECORD
         Prv: pLine;
         Nxt: pLine;
         txt:     LineType;
        END;
  ChainLimit = RECORD
     Last:  pLine;
     First: pLine;
    END;
  PosNo = [0..255];

  Marker= RECORD
    setted: BOOLEAN;
    MLine:  CARDINAL;
    MPos:   PosNo;
    END;
  Next_Proc=(pEdit,pBlockH,pOnScreen,pQuick,pPrint,pNoFile,pReformat,pBufReset,
             pFRMake);

VAR
  Overlay: Next_Proc;
  (* Buffer place: *)
  TBeg:    ChainLimit;(*Last/first lines in buffer*)
  CurLine:  pLine;   (* Current line *)
  LOfset:  INTEGER;  (* Ofset to first line in screen *)
  COfset:  [0..255]; (* Number of positions before first col *)

  (* Screen place: *)
  CLineNr,CColNr:  CARDINAL;
  CPos:  PosNo;
  LLimit:  ARRAY[ FALSE..TRUE ] OF CARDINAL;

  (* Line mode controls: *)
  LineMode:  BOOLEAN; (* True if line not copied *)
  LineBuf:  LineType; (* Line buffer *)

  (* Markers: *)
  Markers: ARRAY [0..MaxMarker] OF Marker;

  (* Work memory status: *)
  LowIni, HighIni:  pLine;      (* Initial *)
  LowCur,HighCur:  pLine;   (* First free & used lines *)

  Cur_Menu: CARDINAL; (*current menu in screen*)
                      (*=255 if undefined menu*)
  ModeInserting,ModeIndention,ModeSoftHyph,ModeHelp,ModeAutoNewLine,
  ModeSpacing: BOOLEAN;
  RMrgn,LMrgn: CARDINAL;

PROCEDURE Set_Marker( m: CARDINAL );
PROCEDURE Clear_Marker( m: CARDINAL );
PROCEDURE Reset_Markers();
PROCEDURE LRemCorrect( nr: CARDINAL );
PROCEDURE LInsCorrect( nr: CARDINAL );
PROCEDURE VTravel( dr: BOOLEAN ): BOOLEAN;
PROCEDURE Ins_Line(): BOOLEAN;
PROCEDURE Rem_Line(): BOOLEAN;
PROCEDURE Replace_Line(): BOOLEAN;
PROCEDURE Read_File( fno: CARDINAL; VAR readedlines: CARDINAL): BOOLEAN;
PROCEDURE Gobble_White_Spaces( VAR l: LineType );
PROCEDURE On_Line_Mode(): BOOLEAN;
PROCEDURE Off_Line_Mode();
PROCEDURE Delete(VAR ln: ARRAY OF CHAR; index,sz: CARDINAL );

PROCEDURE Make_Space( wrds: CARDINAL);
PROCEDURE MoveBytes( cnt: CARDINAL; VAR Src,Dst: ARRAY OF CHAR;
                     srcind,dstind: CARDINAL );
PROCEDURE Travelling( tc: INTEGER ): CARDINAL;
PROCEDURE BufIni();
END WMBuffer.
