MODULE mainWStest; (* Leo 30-Dec-87. (c) KRONOS *)

IMPORT       SYSTEM;
IMPORT  spo: SPOws;
IMPORT  cod: defCodes;
IMPORT  dry: dryStone;
IMPORT  mts: memTest;

TYPE WORD = SYSTEM.WORD;
  ADDRESS = SYSTEM.ADDRESS;

PROCEDURE getm(): BITSET;  CODE cod.getm END getm;
PROCEDURE setm(m: BITSET); CODE cod.setm END setm;
PROCEDURE dt  (); CODE cod.getm cod.copt cod.li2 cod.bic cod.setm cod.slw 3 END dt;
PROCEDURE et  (); CODE cod.llw 3 cod.setm END et;

(*
01234567890123456789012345678901234567890123456789012345678901234567890123456789

 address       write         read     after invert 01234567..01234567
01234567 :  01234567  |  01234567  |  10987654321098765432109876543210
write    :  01234567 XOR xxxxxxxx     ________________________________
read     :                            ________________________________
changes  :                            ___|_____________|____|_________

*)

VAR ln: INTEGER;
 errco: INTEGER;

PROCEDURE Home;
BEGIN
  dt; spo.setpos(0,0); et
END Home;

PROCEDURE ws(VAL s: ARRAY OF CHAR);
BEGIN
  dt;
  spo.setpos(ln,0);   spo.print("%s",s); INC(ln); spo.eraseline;
  spo.setpos(ln+1,0); spo.eraseline; Home;
  IF ln>34 THEN ln:=9 END;
  et
END ws;

PROCEDURE clearnext(n: INTEGER);
  VAR i: INTEGER;
BEGIN
  dt;
  FOR i:=ln TO ln+n-1 DO spo.setpos(i,0); spo.eraseline END; Home;
  et
END clearnext;

PROCEDURE info(ln: INTEGER; VAL s: ARRAY OF CHAR);
BEGIN
  dt;
  spo.setpos(ln,0); spo.print(" %s",s);
  spo.eraseline; Home;
  et
END info;

VAR head: ARRAY [0..6] OF ARRAY [0..31] OF CHAR;

PROCEDURE mark(n: INTEGER);
BEGIN
  dt;
  spo.setpos(n,0); spo.print("*"); Home;
  et
END mark;

PROCEDURE unmark(n: INTEGER);
BEGIN
  dt;
  spo.setpos(n,0); spo.print(" "); Home;
  et
END unmark;

PROCEDURE header;
  VAR i: INTEGER;
BEGIN
  head[0]:="TEST #0: Read After Write      ";
  head[1]:="TEST #1: Write All and Read    ";
  head[2]:="TEST #2: Crowl Write and Read  ";
  head[3]:="TEST #3: Fast Addresses Up     ";
  head[4]:="TEST #4: Fast Addresses Down   ";
  head[5]:="TEST #5: Overlap   Test        ";
  head[6]:="TEST #6: Addresses Test        ";
  FOR i:=0 TO 6 DO
    info(i+2,head[i]);
  END;
END header;

PROCEDURE bits(VAR s: ARRAY OF CHAR; VAR pos: INTEGER; z,o: CHAR; w: WORD);
  VAR i: INTEGER;
BEGIN
  FOR i:=31 TO 0 BY -1 DO
    IF i IN BITSET(w) THEN s[pos]:=o ELSE s[pos]:=z END; INC(pos);
  END;
  s[pos]:=0c;
  IF z=' ' THEN INC(errco) END
END bits;

PROCEDURE hex8(VAR s: ARRAY OF CHAR; VAR pos: INTEGER; w: WORD);
  VAR i,d: INTEGER;
BEGIN
  FOR i:=0 TO 7 DO
    w:=w<<4; d:=INTEGER(BITSET(w)*{0..3});
    IF d>=10 THEN s[pos]:=CHAR( ORD('A')-10+d )
    ELSE          s[pos]:=CHAR( ORD("0")+d )
    END; INC(pos);
  END; s[pos]:=0c;
END hex8;

PROCEDURE rep(VAR s: ARRAY OF CHAR; VAR pos: INTEGER; VAL t: ARRAY OF CHAR);
  VAR i: INTEGER;
BEGIN
  i:=0;
  WHILE (i<=HIGH(t)) & (t[i]#0c) DO
    s[pos]:=t[i]; INC(pos); INC(i)
  END
END rep;

PROCEDURE app(VAR s: ARRAY OF CHAR; VAR pos: INTEGER; VAL t: ARRAY OF CHAR);
BEGIN
  rep(s,pos,t); s[pos]:=0c
END app;

VAR p: BITSET;
   co: INTEGER;
   lo: ADDRESS;
   hi: ADDRESS;

PROCEDURE break;
BEGIN
  ws("----------------------10987654321098765432109876543210-+");
  clearnext(5);
END break;

PROCEDURE err0(VAR s: ARRAY OF CHAR; VAR pos: INTEGER; adr: WORD);
BEGIN pos:=0; hex8(s,pos,adr); app(s,pos," /  ") END err0;

PROCEDURE errR(VAR s: ARRAY OF CHAR; VAR pos: INTEGER; dt: WORD);
BEGIN pos:=0;
  app(s,pos,"READ     :  "); hex8(s,pos,dt); app(s,pos,"  ");
  bits(s,pos,"0","1",dt);
END errR;

PROCEDURE errW(VAR s: ARRAY OF CHAR; VAR pos: INTEGER; dt: WORD);
BEGIN pos:=0;
  app(s,pos,"WRITE    :  "); hex8(s,pos,dt); app(s,pos,"  ");
  bits(s,pos,"0","1",dt);
END errW;

PROCEDURE errCH(VAR s: ARRAY OF CHAR; VAR pos: INTEGER;
                rd,wr: BITSET);
BEGIN pos:=0;
  app(s,pos,"CHANGES  :            ");  bits(s,pos," ","*",rd/wr);
END errCH;

PROCEDURE errorS(adr: ADDRESS; rd,wr: WORD; VAL rem: ARRAY OF CHAR): BOOLEAN;
  VAR s: ARRAY [0..127] OF CHAR; ps: INTEGER;
BEGIN
  err0(s,ps,adr);     app(s,ps,rem); ws(s); ps:=0;
  errR(s,ps,rd);      ws(s); ps:=0;
  errW(s,ps,wr);      ws(s); ps:=0;
  errCH(s,ps,rd,wr);  ws(s); ps:=0;
  break;
  RETURN FALSE;
END errorS;

PROCEDURE ErrorRAW(adr: ADDRESS; rd,wr: WORD): BOOLEAN;
BEGIN RETURN errorS(adr,rd,wr,"TEST # 0") END ErrorRAW;

PROCEDURE ErrorWAAR(adr: ADDRESS; rd,wr: WORD): BOOLEAN;
BEGIN RETURN errorS(adr,rd,wr,"TEST # 1") END ErrorWAAR;

PROCEDURE ErrorCWAR(adr: ADDRESS; rd,wr: WORD): BOOLEAN;
BEGIN RETURN errorS(adr,rd,wr,"TEST # 2") END ErrorCWAR;

PROCEDURE ErrorAdr(adr: ADDRESS; rd,wr: WORD; rel: ADDRESS): BOOLEAN;
  VAR s: ARRAY [0..127] OF CHAR; ps: INTEGER;
BEGIN
  err0(s,ps,adr);
  app(s,ps,"TEST #6  after invert contence at ");  hex8(s,ps,rel); ws(s); ps:=0;

  errR(s,ps,rd);      ws(s); ps:=0;
  errW(s,ps,wr);      ws(s); ps:=0;
  errCH(s,ps,rd,wr);  ws(s); ps:=0;

  break;
  RETURN FALSE;
END ErrorAdr;

PROCEDURE errorA(adr: ADDRESS; rd,wr: WORD; rem: ARRAY OF CHAR): BOOLEAN;
  VAR s: ARRAY [0..127] OF CHAR; ps: INTEGER; pat: BITSET;
BEGIN
  err0(s,ps,adr); app(s,ps,rem); ws(s); ps:=0;

  pat:=BITSET(adr)/BITSET(wr);

  errR(s,ps,rd);   app(s,ps,"  ");
  hex8(s,ps,BITSET(rd)/BITSET(pat)); app(s,ps," XOR "); hex8(s,ps,pat);
  ws(s); ps:=0;

  errW(s,ps,wr);   app(s,ps,"  ");
  hex8(s,ps,adr);  app(s,ps," XOR "); hex8(s,ps,pat);
  ws(s); ps:=0;

  errCH(s,ps,rd,wr); ws(s); ps:=0;
  break;
  RETURN FALSE;
END errorA;

PROCEDURE ErrorAF(adr: ADDRESS; rd,wr: WORD): BOOLEAN;
BEGIN
  RETURN errorA(adr,rd,wr,"TEST #3");
END ErrorAF;

PROCEDURE ErrorAB(adr: ADDRESS; rd,wr: WORD): BOOLEAN;
BEGIN
  RETURN errorA(adr,rd,wr,"TEST #4");
END ErrorAB;

PROCEDURE ErrorO(adr: ADDRESS; rd,wr: WORD; relLo,relHi: ADDRESS): BOOLEAN;
  VAR s: ARRAY [0..127] OF CHAR; ps: INTEGER;
BEGIN
  err0(s,ps,adr);
  app(s,ps,"TEST #5  after invert contence at ");
  hex8(s,ps,relLo); app(s,ps,".."); hex8(s,ps,relHi);
  ws(s); ps:=0;

  errR(s,ps,rd);      ws(s); ps:=0;
  errW(s,ps,wr);      ws(s); ps:=0;
  errCH(s,ps,rd,wr);  ws(s); ps:=0;

  break;
  RETURN FALSE;
END ErrorO;

----------------------------------------------------------------

PROCEDURE AllOrder(lo,hi: ADDRESS; pat: WORD; a: BOOLEAN; no: INTEGER);
  VAR i: INTEGER;
BEGIN
  IF lo>hi THEN RETURN END;
  FOR i:=0 TO no-1 DO
    mark(2); mts.ReadAfterWrite   (lo,hi,pat,ErrorRAW);     unmark(2);
    mark(3); mts.WriteAllAndRead  (lo,hi,pat,ErrorWAAR);    unmark(3);
    mark(4); mts.CrowlWriteAndRead(lo,hi,pat,ErrorCWAR);    unmark(4);
    mark(5); mts.FastAddresses    (lo,hi,pat,+1,ErrorAF);   unmark(5);
    mark(6); mts.FastAddresses    (lo,hi,pat,-1,ErrorAB);   unmark(6);
    mark(7); mts.Overlap          (lo,hi,pat,ErrorO);       unmark(7);
    IF a THEN
      mark(8); mts.Addresses      (lo,hi,pat,ErrorAdr);     unmark(8)
    END
  END
END AllOrder;

VAR H: ARRAY [0..79] OF CHAR;
  Hps: INTEGER;
  Aps: INTEGER;
  Eps: INTEGER;


PROCEDURE show;
  VAR d: ARRAY [0..15] OF CHAR;
      i: INTEGER;
BEGIN
  IF dry.tick MOD 64 # 0 THEN RETURN END;
  i:=0; hex8(d,i,mts.checked);  i:=Aps; rep(H,i,d);
  i:=0; hex8(d,i,errco);        i:=Eps; rep(H,i,d);
  info(1,H);
END show;

PROCEDURE TEST(VAL head: ARRAY OF CHAR; no: INTEGER);
BEGIN
  dt;
  Hps:=0;
  app(H,Hps,"[");  hex8(H,Hps,p);    app(H,Hps,"]       ");
  hex8(H,Hps,lo);  app(H,Hps,"..");  hex8(H,Hps,hi);
  app(H,Hps,"  "); Aps:=Hps;         hex8(H,Hps,0);
  app(H,Hps,"  ERRORS: "); Eps:=Hps; hex8(H,Hps,errco);
  app(H,Hps,"  "); app(H,Hps,head);
  info(1,H);
  et;
  AllOrder(lo,hi,p,(no>1) OR (co MOD 16=15),no)
END TEST;

PROCEDURE present(adr: ADDRESS): BOOLEAN;
  VAR save,read: INTEGER;
BEGIN
  save:=adr^;
  adr^:=12345678h;
  read:=adr^;
  adr^:=save;
  RETURN read=12345678h
END present;

PROCEDURE memtop(): ADDRESS;
  VAR B: ADDRESS;
  PROCEDURE move(t,f: ADDRESS; sz: INTEGER); CODE cod.move END move;
BEGIN
  B:=20000h;
  LOOP
    IF B>=400000h THEN EXIT END;
    B^:=-1;
    IF B^#-1 THEN EXIT END;
    INC(B,20000h );
  END;
  DEC(B);
  move(ADDRESS(20000h),ADDRESS(1FFFFh),INTEGER(B)-1FFFFh);
  RETURN B;
END memtop;

VAR top: ADDRESS;  n,m,l,scr,i: INTEGER;

PROCEDURE vers(): INTEGER; CODE cod.sys cod.sys_vers END vers;

BEGIN
  errco:=0;
  Home;
  IF getm()*{0..1}#{} THEN
    spo.print("Can`t be runned under Excelsior OS\n\n"
              "May be bootstrapped only!\n\n"
              'Reboot and bootstrap "memWStest.boot"');
    setm({});
    LOOP END
  END;
  top:=memtop();

  spo.erase;
  spo.setpos(43,0);
  spo.print("KRONOS-2.6  MAIN SYSTEM TEST   (c) KRONOS. Hacked by Leopold 17-Aug-91");
  dry.dry(n,m,l);
  spo.setpos(35,0);
  spo.print("KRONOS-2.6WS [%d].\n\n%d drystones/second.\n\nClock = %d/%d nanosec.\n\nMEMORY=%d KB"
            ,vers(),n,m,l,(top+1) DIV 256);

  ln:=9;  p:={0};  co:=0;  scr:=0;
  header;
  dry.action:=show;
  setm(getm()+{1});
  i:=50;
  REPEAT DEC(i) UNTIL present(134000h) OR (i=0);
  LOOP
(*
    IF present(1F8000h) THEN
      dt;
      IF co MOD 2 = 1 THEN
        spo.plane(scr); Home; ln:=9;
        scr:=(scr+1) MOD 4
      END;
      et;
      lo:=1F8000h+scr*512*16; hi:=lo+512*16-1;
      TEST("bitmap",2)
    END;

    IF present(800800h) THEN
      lo:=800800h; hi:=lo+400h-1; TEST("SCSI buffer",8);
    END;
*)
    IF present(134000h) THEN
      lo:=134000h; hi:=lo+512-1; TEST("ARC buffer",1);
    END;
(*
    IF present(900000h) THEN
      lo:=900000h; hi:=lo+128*7-1; TEST("WD buffer",8)
    END;

    lo:=2000h; hi:=top;  TEST("main memory",1);
*)
    lo:=060000h; hi:=lo+2000h-1;  TEST("main memory",8);
    IF ODD(co) THEN p:=p<<1 ELSE p:={0..31}/p END;
    INC(co)
  END
END mainWStest.

%h512*1024 / 4 = 20000h
