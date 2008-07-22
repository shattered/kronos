DEFINITION MODULE ForIO; (* 03-Apr-89. (c)  *)

FROM SYSTEM     IMPORT  ADDRESS, WORD, ADR;

TYPE
     EnStatus = (old,new,scratch,unknown,keep,delete,
                 formatted,unformatted,direct,seq,null,zero,
         internal,freeformat,read,write,const,text,int,real,logic,slsh,nul,
         sysIO, open, close, rewind, backspace, endfile);


VAR
  exchPar:    RECORD
                ioStat: ADDRESS;
                file:   ADDRESS; -- descriptor address
                status: ADDRESS;
                access: ADDRESS;
                form:   ADDRESS;
                blank:  ADDRESS;
                recl:   INTEGER;
              END;


PROCEDURE ForOpen(chan: INTEGER): INTEGER;

PROCEDURE ForClose(chan: INTEGER; iostat: ADDRESS): INTEGER;

PROCEDURE ForRewind(chan: INTEGER; instat: ADDRESS): INTEGER;

PROCEDURE ForBackspace(chan: INTEGER; iostat: ADDRESS): INTEGER;

PROCEDURE EndFile(chan: INTEGER; iostat: ADDRESS): INTEGER;

PROCEDURE InputIV(vv: ADDRESS);

PROCEDURE InputIA(ad: ADDRESS; ln: INTEGER);

PROCEDURE InputRA(ad: ADDRESS; ln: INTEGER);

PROCEDURE InputRV(ad: ADDRESS);

PROCEDURE InputLV(vv: ADDRESS);

PROCEDURE InputLA(ad: ADDRESS; ln: INTEGER);

PROCEDURE InputCV(vv: ADDRESS);

PROCEDURE InputCA(ad: ADDRESS; ln: INTEGER);

PROCEDURE OutputIV(vv: WORD);

PROCEDURE OutputIA(ad: ADDRESS; ln: INTEGER);

PROCEDURE OutputRA(ad: ADDRESS; ln: INTEGER);

PROCEDURE OutputRV(vv: WORD);

PROCEDURE OutputLA(ad: ADDRESS; ln: INTEGER);

PROCEDURE OutputLV(vv: WORD);

PROCEDURE OutputCV(vv: ADDRESS);

PROCEDURE OutputCA(ad: ADDRESS; ln: INTEGER);


PROCEDURE ReadSF(chan: INTEGER;  fmt: ADDRESS); -- fmt: format addr
PROCEDURE ReadSFR(chan: INTEGER;  fmt: ADDRESS);-- fmt:text format addr

PROCEDURE ReadSU(chan: INTEGER);

PROCEDURE ReadSL(chan: INTEGER);

PROCEDURE ReadDF(chan: INTEGER; fmt: ADDRESS; recNb: INTEGER);
PROCEDURE ReadDFR(chan: INTEGER; fmt: ADDRESS; recNb: INTEGER);

PROCEDURE ReadDU(chan: INTEGER;  recNb: INTEGER);
PROCEDURE ReadDL(chan: INTEGER;  recNb: INTEGER);

PROCEDURE ReadIF(dAdr: ADDRESS; fmt: ADDRESS);

PROCEDURE WriteSU(chan: INTEGER);

PROCEDURE WriteSF(chan: INTEGER; fmt: ADDRESS);
PROCEDURE WriteSFR(chan: INTEGER; fmt: ADDRESS);

PROCEDURE WriteSL(chan: INTEGER);

PROCEDURE WriteDFR(chan: INTEGER; fmt: ADDRESS; recNb: INTEGER);

PROCEDURE WriteDU(chan: INTEGER; recNb: INTEGER);
PROCEDURE WriteDL(chan: INTEGER; recNb: INTEGER);

PROCEDURE WriteIF(dAdr: ADDRESS; fmt: ADDRESS);

PROCEDURE IOend(ios: ADDRESS): INTEGER; -- <0 -eof, =0 -ok, >= -error
PROCEDURE IOerror(er: INTEGER);

PROCEDURE WriteDF(chan: INTEGER; fmt: ADDRESS; recNb: INTEGER);

END ForIO.
