DEFINITION MODULE WMEdBuf;

FROM WMBuffer     IMPORT LineType,PosNo;

PROCEDURE CharWrite( ch: CHAR );
PROCEDURE New_Line();
PROCEDURE Tabulation();
PROCEDURE Del_Char();
PROCEDURE Del_Word_Right();
PROCEDURE Line_Del();
PROCEDURE Empty_Line();
PROCEDURE UnDo();
PROCEDURE Del_Prev_Char();
PROCEDURE Print_Menu();
PROCEDURE Right_Word( VAR l: LineType; p: PosNo ): PosNo;
PROCEDURE Left_Word ( VAR l: LineType; p: PosNo ): PosNo;

END WMEdBuf.
