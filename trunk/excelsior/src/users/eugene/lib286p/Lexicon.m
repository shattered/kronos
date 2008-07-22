IMPLEMENTATION MODULE Lexicon; (* Leo 18-Jan-90. (c) KRONOS *)

IMPORT       SYSTEM;
IMPORT  err: defErrors;
IMPORT  str: Strings;
IMPORT  mem: Heap;

(*$U+*)

WITH STORAGE (NEW    : mem.allocate;
              DISPOSE: mem.deallocate;
              RESIZE : mem.reallocate);

TYPE LEX=INTEGER;

VAR no_memory: ARRAY [0..11] OF CHAR;

PROCEDURE open(VAR lex: LEX; name: ARRAY OF CHAR);
BEGIN
  error:=err.inv_op; done:=FALSE;
END open;

PROCEDURE close(VAR lex: LEX);
BEGIN
  error:=err.inv_op; done:=FALSE;
END close;

PROCEDURE no_mem(VAR data: STRING);
BEGIN
  data^.ADR :=SYSTEM.ADR(no_memory); data^.HIGH:=HIGH(no_memory);
  done:=FALSE; error:=err.no_memory
END no_mem;

PROCEDURE bad(VAR data: STRING; erc,code: INTEGER);
BEGIN
  done:=FALSE; error:=erc;
  NEW(data,8);
  IF data^.ADR=NIL THEN no_mem(data); RETURN END;
  str.print(data,"%04hh",code)
END bad;

PROCEDURE io_error(code: INTEGER; VAR str: ARRAY OF CHAR);
BEGIN
(*
    |80000001h: str:='i/o error'
    |80000101h: str:='not ready'
    |80000201h: str:='time out'
    |80000401h: str:='write protect'
    |80000801h: str:='seek error'
    |80001001h: str:='invalid DMA'
    |80002001h: str:='data CRC error'
    |80004001h: str:='header CRC error'
    |80008001h: str:='sector ID not found'
    |80010001h: str:='data ID not found'
    |80020001h: str:='hardware failure'
    |80040001h: str:='seek track 0 failure'
    |80080001h: str:='bad block detected'
    |80100001h: str:='invalid disk address'
    |80200001h: str:='check (compare) error'
    |80400001h: str:='non correctable ECC error'
    |80800001h: str:='device programming error'
    |81000001h: str:='unsafe device'
    |82000001h: str:='parity error'
    |84000001h: str:='frame error'
    |88000001h: str:='data over run'
    |90000001h: str:='write fail'
*)
  str:='i/o error'
END io_error;


PROCEDURE get(lex: LEX; code: INTEGER; VAR data: STRING);
  VAR l: INTEGER; str: ARRAY [0..79] OF CHAR;
BEGIN
  IF lex#1 THEN error:=err.bad_desc; done:=FALSE; RETURN END;
  IF code<0 THEN io_error(code,str)
  ELSE
    CASE code OF
      |00000000h: str:='ok'
      |00000080h: str:='no memory'
      |00000081h: str:='not enough'
      |00000082h: str:='busy'
      |00000083h: str:='bad name'
      |00000084h: str:='bad parameter'
      |00000085h: str:='invalid operation'
      |00000086h: str:='interrupted operation'
      |00000087h: str:='bad descriptor'
      |00000088h: str:='illegal descriptor'
      |00000089h: str:='security violation'
      |0000008Ah: str:='superuser only'
      |0000008Bh: str:='data inconsistency'
      |0000008Ch: str:='illegal version'
      |0000008Dh: str:='duplicate object'
      |0000008Eh: str:='unsuitable object'
      |0000008Fh: str:='no such entry'
      |00000090h: str:='undefined now'
      |00000091h: str:='illegal access'
      |000000A0h: str:='is directory'
      |000000A1h: str:='cross device link'
      |000000A2h: str:='is not directory'
      |000000A3h: str:='no data'
      |000000A4h: str:='bad file system'
      |000000A5h: str:='too large'
      |000000A6h: str:='is not blocked device'
      |000000A7h: str:='no space on volume'
      |000000A8h: str:='file system overflow'
  
  
      |00000002h: str:='QUIT'
      |00000003h: str:='memory violation'
      |00000004h: str:='power crash'
      |00000007h: str:='unimplemented instruction'
      |00000008h: str:='call interrupt'
      |00000009h: str:='return interrupt'
      |0000000Bh: str:='trace  interrupt'
      |00000040h: str:='P-stack overflow'
      |00000041h: str:='integer overflow'
      |00000042h: str:='real overflow'
      |00000043h: str:='real underflow'
      |00000044h: str:='address underflow'
      |00000045h: str:='CASE without ELSE'
      |00000046h: str:='return from function without result'
      |00000047h: str:='HALT'
      |00000048h: str:='ASSERT'
      |00000049h: str:='invalid instruction'
      |0000004Ah: str:='bounds check'
      |0000004Bh: str:='hardware ASSERT'
      |0000004Ch: str:='E-stack over/underflow'
      |0000004Dh: str:='ABORT'
      |0000004Eh: str:='no memory in heap'
      |0000004Fh: str:='illegal parameter'
      |00000050h: str:='BREAK (^C)'
      |00000051h: str:='unimplemented procedure'
      |00000052h: str:='obsolete procedure'
      |00000053h: str:='resource exhausted'
    ELSE str:='unexpected exception'
    END;
  END;
  l:=0;
  WHILE str[l]#0c DO INC(l) END;
  NEW(data,l+1);
  IF data^.ADR=NIL THEN no_mem(data); RETURN END;
  FOR l:=0 TO HIGH(data) DO data[l]:=str[l] END;
END get;

PROCEDURE dispose(VAR data: STRING);
BEGIN
  done:=TRUE;
  IF (data^.ADR=NIL) OR (data^.ADR=SYSTEM.ADR(no_memory)) THEN
    data^.ADR:=NIL; data^.HIGH:=-1; RETURN
  END;                            ------
  DISPOSE(data)
END dispose;

PROCEDURE sprint(VAR res: ARRAY OF CHAR;
                     lex: LEX;
                    code: INTEGER;
                  format: ARRAY OF CHAR;
                SEQ args: SYSTEM.WORD);
  VAR bump: STRING;
      data: STRING;
BEGIN
  done:=TRUE;
  NEW(bump,HIGH(res)+1);
  IF bump^.ADR=NIL THEN
    str.print(res,"%s %s.%04hh",no_memory,lex,code); RETURN
  END;
  get(lex,code,data);
  str.print(bump,format,args);
  str.print(res,bump,data);
  DISPOSE(bump);
  dispose(data)
END sprint;

---------------------------------------------------------------


PROCEDURE perror(VAR res: ARRAY OF CHAR;
                    code: INTEGER;
                  format: ARRAY OF CHAR;
                SEQ args: SYSTEM.WORD);

  PROCEDURE set_error;
    VAR i: BITSET;
        s: ARRAY [0..127] OF CHAR;
        x: ARRAY [0.. 63] OF CHAR;
      m,w: BITSET;
  BEGIN
    s:="";
    i:={8};
    m:=BITSET(code)-{8..30};
    w:=BITSET(code)-m;
    WHILE w#{} DO
      IF i*w#{} THEN
        sprint(x,sysmsg,INTEGER(i+m),"%%s");
        IF s#"" THEN str.append(s,", %s",x)
        ELSE         str.append(s,"%s"  ,x)
        END;
        w:=w-i
      END;
      i:=i<<1
    END;
    str.print(x,format,args); str.print(res,x,s)
  END set_error;

BEGIN
  IF sysmsg=null THEN
    str.print(res," NO ERRORS HANDLER, error: %04#h\n",code);
  ELSIF (BITSET(code)*{31}#{}) & (BITSET(code)*{8..30}#{}) THEN
    set_error
  ELSE
    sprint(res,sysmsg,code,format,args)
  END
END perror;

PROCEDURE change_sysmsg(lex: LEX);
BEGIN
  close(sysmsg); done:=TRUE; sysmsg:=lex
END change_sysmsg;

BEGIN
  null:=0;
  no_memory:=" NO MEMORY ";
  sysmsg:=1;
END Lexicon.
