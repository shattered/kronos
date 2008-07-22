DEFINITION MODULE WMScreen;
(* Type screen of text *)

FROM WMBuffer   IMPORT LineType,PosNo;
FROM WMUtil     IMPORT Int_Type;

PROCEDURE WriteToEOL(VAR ln: LineType; offset,stcol: CARDINAL);
PROCEDURE EdWriteToEOL(VAR ln: LineType;stcol,abs: CARDINAL);
PROCEDURE EdChOut( ch: CHAR );
PROCEDURE BlockMarked(): BOOLEAN;
PROCEDURE Roll_Up( VAR ln: LineType );
PROCEDURE VCorrect( voffset: INTEGER; first_line: BOOLEAN ): BOOLEAN;
PROCEDURE NextTab( ccol: CARDINAL ): PosNo;
PROCEDURE Text_Beg( line: LineType ): PosNo;
PROCEDURE MoveToMarker( mno: CARDINAL );
PROCEDURE White_Space( ch: CHAR ): BOOLEAN;
PROCEDURE Word_Delim( ch: CHAR): BOOLEAN;
PROCEDURE Screen_Pos_Process(ppos: PosNo; VAR logpos: CARDINAL; (*!*)
                                                   VAR sl: LineType);
PROCEDURE Screen_Position(VAR ln: LineType; chno: PosNo): PosNo;
PROCEDURE Retrieve(i: Int_Type);
PROCEDURE Int_Handler();
(*!*) (*Can return position larger than 255, caller must check*)
PROCEDURE Place_Compute( VAR ln: LineType );
PROCEDURE SamePos( VAR l: LineType; logpos: CARDINAL ): PosNo;

END WMScreen.
