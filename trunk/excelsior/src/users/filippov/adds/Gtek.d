DEFINITION MODULE Gtek; (* Sem 26-Oct-86. (c) KRONOS *)

VAR Type: ARRAY [0..15] OF CHAR;

TYPE ErrProc=PROCEDURE (ARRAY OF CHAR);
     GetProc=PROCEDURE (INTEGER): INTEGER;
     SetProc=PROCEDURE (INTEGER,INTEGER);

PROCEDURE SetType(n: INTEGER);

PROCEDURE Clear?(p: ErrProc);

PROCEDURE Read(First,Last: INTEGER; p: SetProc; e: ErrProc);

PROCEDURE Write(First,Last: INTEGER; p: GetProc; e: ErrProc);

PROCEDURE Init(type: CHAR): BOOLEAN;
(* returns TRUE when type ok
EPROM SELECTION MENU

    NMOS         NMOS         CMOS        EEPROM     W/ADAPT
A - 2758     G - 2508     L - 27C16    P - 5213     R - 874x-1K  
B - 2716     H - 2516     M - 27C32    Q - X2816A   S - 874x-2K  
C - 2732     I - 2532     N - MC6716   X - 48016    T - 874xH-1K 
D - 2732A    J - 2564     O - F27C64   Y - I2816A   U - 874xH-2K 
E - 2764     K - i68766   0 - I27C64   3 - I2817A   V - 8751     
1 - i2764A                8 - F27C256  9 - X2864A   W - 8755     
F - 27128                                                        
2 - i27128A                                                      
Z - i27256                                                       
7 - i27512   

*)


PROCEDURE Stop;

END Gtek.
