IMPLEMENTATION MODULE deBug; (* Leo 19-Dec-88. (c) KRONOS *)

IMPORT      SYSTEM;
IMPORT cod: defCodes;
IMPORT fmt: Formats;

(* Output of debug MUST be directed to serial line without
   driver (because deBugWSsio works without interrupts!
   You can't call os.print in system.m until you
   'reset_serials' there (see.).
*)

CONST     SIO = 1;    (*  Serial channel no *)

TYPE WORD = SYSTEM.WORD;
  ADDRESS = SYSTEM.ADDRESS;

CONST
  x1  = {0};            bits5 = {};             odd     = {4};
  x16 = {1};            bits6 = {2};            even    = {4,5};
  x64 = {0,1};          bits7 = {3};            stop1   = {6};
                        bits8 = {2,3};          stop1_5 = {7};
                                                stop2   = {6,7};
DEFAULT = x16 + stop2 + bits8 ;

(*CONTROL*)
  TxEN    = 0;          -- TRANSMIT ENABLE
  DTRY    = 1;          -- DATA TERMINAL READY (used to enable RECEIVE ipt)
  RxEN    = 2;          -- RECEIVE ENABLE
  SBRK    = 3;          -- SEND BREAK
  ER      = 4;          -- ERROR RESET (not used yet)
  RTS     = 5;          -- READY TO SEND (used to enable TRANSMIT ipt)
  IR      = 6;          -- INTERNAL RESET
  EH      = 7;          -- ENTER HUNT MODE (enable search for Sync characters)

  Ript = DTRY;
  Tipt = RTS;

(*STATUS*)
  DSR     =  7;         -- DATA SEND READY (used to indicate connection)
  SINDET  =  6;         --
  FE      =  5;         -- FRAMING  ERROR (stop bit lost)
  OE      =  4;         -- DATA     OVERRUN
  PE      =  3;         -- PARITY   ERROR
  TxE     =  2;         -- TRANSMIT EMPTY
  RxRDY   =  1;         -- RECEIVE  READY
  TxRDY   =  0;         -- TRANSMIT READY

VAR CSR: POINTER TO BITSET; DTR: POINTER TO CHAR;

PROCEDURE r;
  PROCEDURE d; BEGIN END d; (* delay about 10 mksec *)
BEGIN
  DTR:=ADDRESS(8200F0h+SIO*2);  d;
  CSR:=ADDRESS(DTR)+1;          d;
  CSR^:={};                     d;
  CSR^:={};                     d;
  CSR^:={};                     d;
  CSR^:={};                     d;
  CSR^:={IR};                   d; d; d; d;
  CSR^:=DEFAULT;                d; d; d; d;
  CSR^:={TxEN,RxEN};            d; d; d; d;
END r;

PROCEDURE write(ch: CHAR);
BEGIN
  REPEAT UNTIL TxRDY IN CSR^;
  DTR^:=ch
END write;

VAR first: BOOLEAN;

PROCEDURE write_str(xxx: INTEGER; VAL s: ARRAY OF CHAR; i,len: INTEGER);
  VAR ch: CHAR;
BEGIN
  IF first THEN r; first:=FALSE END;
  WHILE (i<=HIGH(s)) & (len>0) DO
    ch:=s[i];
    write(ch);
    INC(i); DEC(len)
  END
END write_str;

PROCEDURE print(f: ARRAY OF CHAR; SEQ a: WORD);
BEGIN
  fmt.format(0,write_str,f,a)
END print;

BEGIN
  first:=TRUE
END deBug.
