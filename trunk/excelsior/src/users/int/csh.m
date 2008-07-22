MODULE csh; (* 04-Jun-88. (c) KRONOS *)

IMPORT Terminal;
FROM StdIO      IMPORT  print, File?, StdOut;
FROM BIO        IMPORT  OpenOnDir, CD, bRead, Close, checkHALT, GetEof;
FROM FsPublic   IMPORT  File, FileName;
FROM SYSTEM     IMPORT  ADR;
FROM KRONOS     IMPORT  MOVE, SHL;

VAR nm: FileName;
    fl: File;
    buf: ARRAY [0..1023] OF INTEGER;
    blk: INTEGER;
    ptr: INTEGER;
    adr: INTEGER;
    width,depth: INTEGER;

TYPE slice=ARRAY [0..31] OF INTEGER;
VAR rd,wr,mem: INTEGER;
    wsp: ARRAY [0..255] OF slice;
    tim: ARRAY [0..255] OF slice;
    cnt: INTEGER;

PROCEDURE cash1;
  VAR p,t: POINTER TO slice; i,min: INTEGER;
BEGIN
  INC(cnt);
  IF 31 IN BITSET(adr) THEN
    INC(wr);
    adr:=INTEGER(BITSET(adr)-{31});
    p:=ADR(wsp[adr MOD depth]);
    t:=ADR(tim[adr MOD depth]); min:=0;
    FOR i:=0 TO width-1 DO
      IF p^[i]=adr THEN t^[i]:=cnt; RETURN END;
      IF t^[i]<t^[min] THEN min:=i END;
    END;
    p^[min]:=adr; t^[min]:=cnt;
  ELSE
    INC(rd);
    p:=ADR(wsp[adr MOD depth]);
    t:=ADR(tim[adr MOD depth]); min:=0;
    FOR i:=0 TO width-1 DO
      IF p^[i]=adr THEN t^[i]:=cnt; RETURN END;
      IF t^[i]<t^[min] THEN min:=i END;
    END;
    p^[min]:=adr; t^[min]:=cnt;
    INC(mem);
  END;
END cash1;

VAR i,j,k: INTEGER;

BEGIN
  nm:='ADR.ST';
  checkHALT(OpenOnDir(CD(),fl,nm),nm);
  FOR i:=2 TO 5 DO
    width:=SHL(1,i);
    FOR j:=7-i TO 7-i DO
      depth:=SHL(1,j);
      rd:=0; wr:=0; mem:=0; cnt:=0;
      wsp[0][0]:=-1; MOVE(ADR(wsp[0][1]),ADR(wsp[0][0]),SIZE(wsp)-1);
      tim[0][0]:=-1; MOVE(ADR(tim[0][1]),ADR(tim[0][0]),SIZE(tim)-1);
      blk:=600; ptr:=0;
      checkHALT(bRead(fl,blk,ADR(buf),4096),nm);
      FOR k:=blk*1024 TO GetEof(fl)DIV 4 -1 DO
        IF ptr>=1024 THEN
          ptr:=0; INC(blk);
          checkHALT(bRead(fl,blk,ADR(buf),4096),nm);
          IF blk MOD 10=0 THEN Terminal.print('%4d\r',blk) END;
        END;
        adr:=buf[ptr]; INC(ptr);
        cash1;
      END;
      IF File?(StdOut)>=0 THEN
        Terminal.print('width=%2d depth=%3d wr=%6d rd=%6d mem=%6d\n',
                       width,depth,wr,rd,mem);
      END;
      print('width=%2d depth=%3d wr=%6d rd=%6d mem=%6d\n',
             width,depth,wr,rd,mem);
    END;
  END;
  checkHALT(Close(fl),nm);
END csh.
