MODULE int; (* John 18-Feb-88. (c) KRONOS *)

FROM Terminal   IMPORT  Peek, print, BusyRead, Pressed, Read, Write;
FROM SYSTEM     IMPORT  ADR, ADDRESS, WORD;
FROM KRONOS     IMPORT  SETM, GETM, SHL, SHR;
FROM FsPublic   IMPORT  File, FileName;
FROM BIO        IMPORT  OpenOnDir, CD, Close, GetEof, SetEof, bRead, bWrite,
                        checkHALT;
FROM intRAM     IMPORT  CoreWR, CoreRD, GetCode, ShowStat;
FROM Args       IMPORT  TakeWord;
FROM Resource   IMPORT  Final;
IMPORT mCode;

TYPE I=INTEGER;

CONST Nil = 07FFFFF80h;
      ExternalBit = 31;
      ChangeMaskBit = 30;

VAR PC      : INTEGER;
    IR      : INTEGER;
    F       : INTEGER;
    G       : INTEGER;
    L       : INTEGER;
    S       : INTEGER;
    H       : INTEGER;
    P       : INTEGER;
    M       : BITSET;
    Ipt     : BOOLEAN;
    noIpt   : INTEGER;
    Executed: INTEGER;
    AStack  : ARRAY [0..7] OF INTEGER;
    sp      : INTEGER;
    trap    : POINTER TO INTEGER;
    code    : POINTER TO ARRAY [0..0FFFFh] OF CHAR;
    disk    : File;
    diskName: FileName;

PROCEDURE ShowRegs;
  VAR i: INTEGER; cmd: ARRAY [0..31] OF CHAR;
BEGIN
  mCode.VisCommand(ORD(code^[PC]),cmd);
  print('%11d P=%$8h L=%$8h S=%$8h H=%$8h G=%$8h\nF=%$8h PC=%$8h M=%$8h cmd=%s',
        Executed,P,L,S,H,G,F,PC,M,cmd);
  i:=mCode.CmdLen[ORD(code^[PC])];
  FOR i:=1 TO i    DO print('%$2h ',code^[PC+i]) END; print('\n');
  FOR i:=0 TO sp-1 DO print('%$8h ',AStack[i]) END; print('\n');
END ShowRegs;

PROCEDURE t_reg(): ADDRESS;
CODE 0h 60h 6h 88h END t_reg;

PROCEDURE Next(): INTEGER;
BEGIN
  INC(PC); RETURN ORD(code^[PC-1]);
END Next;

PROCEDURE Next2(): INTEGER;
BEGIN
  RETURN Next()+Next()*100h;
END Next2;

PROCEDURE Next4(): INTEGER;
BEGIN
  RETURN INTEGER( BITSET(Next2())+BITSET(Next2()<<16) )
END Next4;

PROCEDURE push(w: INTEGER);
BEGIN
  IF sp<=HIGH(AStack) THEN AStack[sp]:=w; INC(sp); RETURN END;
  Ipt:=TRUE; noIpt:=4Ch;
END push;

PROCEDURE pop(): INTEGER;
BEGIN
  IF sp>0 THEN DEC(sp); RETURN AStack[sp] END;
  Ipt:=TRUE; noIpt:=4Ch; RETURN 0
END pop;

PROCEDURE Empty(): BOOLEAN;
BEGIN RETURN sp=0 END Empty;

PROCEDURE Mark(x: INTEGER; Extern: BOOLEAN);
  VAR i: INTEGER;
BEGIN
  i:=S;
  CoreWR(S,x); INC(S);
  CoreWR(S,L); INC(S);
  IF Extern THEN
    CoreWR(S,BITSET(PC)+{ExternalBit});
  ELSE
    CoreWR(S,PC);
  END;
  INC(S,2); L:=i;
END Mark;

PROCEDURE SaveAStack;
  VAR i: INTEGER;
BEGIN
  i:=S; WHILE NOT Empty() DO CoreWR(S,pop()); INC(S) END;
  CoreWR(S,S-i); INC(S);
END SaveAStack;

PROCEDURE RestoreAStack;
  VAR i: INTEGER;
BEGIN
  DEC(S); i:=CoreRD(S); IF i>SIZE(AStack) THEN i:=SIZE(AStack) END;
  WHILE i>0 DO DEC(i); DEC(S); push(CoreRD(S)) END;
END RestoreAStack;

PROCEDURE SaveRegs;
BEGIN
  CoreWR(1,P);
  SaveAStack;
  CoreWR(P,G);
  CoreWR(P+1,L);
  CoreWR(P+2,PC);
  CoreWR(P+3,M);
  CoreWR(P+4,S);
END SaveRegs;

PROCEDURE RestoreRegs;
BEGIN
  CoreWR(0,P);
  G :=CoreRD(P);
  F :=CoreRD(G);
  code:=GetCode(F);
  L :=CoreRD(P+1);
  PC:=CoreRD(P+2);
  M :=CoreRD(P+3);
  S :=CoreRD(P+4);
  H :=CoreRD(P+5);
  DEC(H,16);
  RestoreAStack;
END RestoreRegs;

PROCEDURE Transfer(p_to,p_from: INTEGER);
  VAR i: INTEGER;
BEGIN
  i:=CoreRD(p_to); CoreWR(p_from,P);
  SaveRegs; P:=i; RestoreRegs;
END Transfer;

PROCEDURE Trap(no: INTEGER);
  VAR i: INTEGER;
BEGIN
  IF (no#47h)&(no#1) THEN print('Trap %2$h\n',no); HALT(1) END;
  noIpt:=no;
  IF no>=3Fh THEN CoreWR(P+6,no); no:=3Fh  END;
  IF (no = 0h) THEN Trap(6); RETURN END;
  IF (no>=0Ch) & (no<3Fh) & NOT (00 IN M) THEN RETURN END;
  IF (no>= 2h) & (no<0Ch) & NOT (no IN M) THEN CoreWR(P+6,no); RETURN END;
  IF (no = 1h)            & NOT (01 IN M) THEN RETURN END;
  IF (no =3Fh)            & NOT (31 IN M) THEN RETURN END;
  Transfer(no*2,CoreRD(no*2+1));
END Trap;

PROCEDURE Fuck(no: INTEGER);
BEGIN
  DEC(PC); Trap(no)
END Fuck;

PROCEDURE unpack(VAR a: INTEGER; n: INTEGER): INTEGER; CODE 40h END unpack;
PROCEDURE pack(VAR a: INTEGER; n,v: INTEGER); CODE 50h END pack;

PROCEDURE Int2();
  VAR i,j,k,sz : INTEGER;
      w,v: BITSET;
      a,b: INTEGER;
      hi,low,Adr: INTEGER;
BEGIN
  CASE IR OF
     |0B8h    : IF (S+2>H) THEN Fuck(40h) ELSE
                  i:=Next(); hi:=pop(); low:=pop(); Adr:=pop();
                  j:=Next2() + PC;
                  IF ((i=0) & (low<=hi)) OR ((i#0) & (low>=hi)) THEN
                    CoreWR(Adr,low);
                    CoreWR(S,Adr); INC(S);
                    CoreWR(S,hi); INC(S);
                  ELSE
                    PC:=j
                  END
                END
     |0B9h    : hi:=CoreRD(S-1); Adr:=CoreRD(S-2);
                sz:=Next(); j:=-Next2()+PC;
                IF 7 IN BITSET(sz) THEN sz:=sz-256 END;
                i:=CoreRD(Adr); INC(i,sz);
                IF ((sz>=0)&(i>hi)) OR ((sz<=0)&(i<hi)) THEN
                  DEC(S,2)
                ELSE
                  CoreWR(Adr,i); PC:=j;
                END;
     |0BAh    : IF (S+1>H) THEN Fuck(40h) ELSE
                  INC(PC,Next2()); j:=pop(); low:=Next2(); hi:=Next2();
                  i:=PC+2*(hi-low)+4; CoreWR(S,i); INC(S);
                  IF (j>=low)&(j<=hi) THEN INC(PC,2*(j-low+1)) END;
                  DEC(PC,Next2());
                END;
     |0BBh    : DEC(S); PC:=CoreRD(S);
     |0BCh    : IF (S+1>H) THEN Fuck(40h) ELSE
                  CoreWR(S,Next2()+PC); INC(S)
                END;
     |0BEh    : IF (pop()#0) THEN push(1); PC:=Next()+PC ELSE INC(PC) END
     |0BFh    : IF (pop()=0) THEN push(0); PC:=Next()+PC ELSE INC(PC) END
     |0C0h    : sz:=pop(); j:=pop(); i:=pop();
                WHILE sz>0 DO
                  CoreWR(i,CoreRD(j)); INC(i); INC(j); DEC(sz)
                END;
     |0C1h    : Adr:=pop(); sz:=Next();
                WHILE sz>0 DO CoreWR(Adr,Next4()); INC(Adr); DEC(sz) END;
     |0C2h    : push(I(CoreRD(G+1))+Next2());
     |0C3h    : i:=pop()*4; j:=pop()*4;
                REPEAT
                  k:=CoreRD(i DIV 4); a:=unpack(k,i MOD 4);
                  k:=CoreRD(j DIV 4); b:=unpack(k,j MOD 4);
                  INC(i); INC(j);
                UNTIL (a=0) OR (b=0) OR (a#b);
                push(a); push(b);
     |0C4h    : i:=L; j:=Next();
                WHILE j>0 DO  i:=CoreRD(i); DEC(j) END;
                push(i);
     |0C5h    : push(CoreRD(L));
     |0C6h    : hi:=pop(); low:=pop(); i:=pop(); push(i);
                IF (i<low)OR(i>hi) THEN push(low); push(hi); Trap(4Ah) END;
     |0C7h    : hi:=pop(); i:=pop(); push(i);
                IF (i<0) OR (i>hi) THEN push(hi); Trap(4Ah) END;
     |0C8h    : sz:=pop();
                IF (S+sz>H) THEN push(sz); Fuck(40h);
                ELSE push(S); INC(S,sz) END;
     |0C9h    : sz:=Next();
                IF (S+sz>H) THEN DEC(PC); Fuck(40h);
                ELSE INC(S,sz) END;
     |0CAh    : S:=L;
                L:=CoreRD(S+1); i:=CoreRD(S+2);
                PC:=INTEGER(BITSET(i)*{0..0Fh});
                IF ExternalBit IN BITSET(i) THEN
                  G:=CoreRD(S); F:=CoreRD(G); code:=GetCode(F);
                END;
                IF ChangeMaskBit IN BITSET(i) THEN M:=CoreRD(S+3) END;
     |0CBh    :
     |0CCh    : IF (S+4>H) THEN Fuck(40h)
                ELSE
                  k:=CoreRD(G-Next()-1);
                  j:=INTEGER(BITSET(k)*{0..21});
                  i:=Next(); Mark(G,TRUE);
                  G:=CoreRD(j);
                  F:=CoreRD(G); code:=GetCode(F);
                  PC:=i*4; PC:=Next4()
                END
     |0CDh    : IF (S+4>H) THEN Fuck(40h)
                ELSE i:=Next(); Mark(pop(),FALSE); PC:=i*4; PC:=Next4();
                END
     |0CEh    : IF (S+3>H) THEN Fuck(40h)
                ELSE
                  DEC(S); i:=CoreRD(S);
                  Mark(G,TRUE);
                  j:=unpack(i,3); i:=I(BITSET(i)*{0..23});
                  G:=CoreRD(i); F:=CoreRD(G); code:=GetCode(F);
                  PC:=j*4; PC:=Next4()
                END
     |0CFh    : IF (S+4>H) THEN Fuck(40h)
                ELSE i:=Next(); Mark(L,FALSE); PC:=i*4; PC:=Next4() END;
     |0D0h..0DFh: IF (S+4>H) THEN Fuck(40h)
                ELSE Mark(L,FALSE); PC:=(IR MOD 10h)*4; PC:=Next4() END;
     |0E0h    : i:=pop();
                IF (i<0) OR (i>1Fh) THEN
                  push(i); Trap(4Ah)
                ELSE
                  j:=pop(); CoreWR(j,BITSET(CoreRD(j))+{i});
                END
     |0E1h    : i:=pop();
                IF (i<0) OR (i>1Fh) THEN
                  push(i); Trap(4Ah)
                ELSE
                  j:=pop(); CoreWR(j,BITSET(CoreRD(j))-{i});
                END
     |0E2h    : w:=BITSET(pop());v:=BITSET(pop()); push(I(v<=w))
     |0E3h    : w:=BITSET(pop());v:=BITSET(pop()); push(I(v>=w))
     |0E4h    : i:=pop(); CoreWR(i,I(CoreRD(i))+1);
     |0E5h    : i:=pop(); CoreWR(i,I(CoreRD(i))-1);
     |0E6h    : i:=pop(); j:=pop(); CoreWR(j,I(CoreRD(j))+i);
     |0E7h    : i:=pop(); j:=pop(); CoreWR(j,I(CoreRD(j))-i);
     |0E8h    : IF (S+1>H) THEN Fuck(40h) ELSE CoreWR(S,pop()); INC(S) END;
     |0E9h    : DEC(S); push(CoreRD(S));
     |0EAh    : sz:=pop(); i:=pop(); push(pop()+i*sz);
     |0EBh    : i:=Next(); j:=Next(); i:=CoreRD(G-i-1); pack(i,3,j); push(i);
     |0F0h    : i:=pop(); j:=pop(); push(i); push(j);
     |0F1h    : push(L-Next()-1);
     |0F2h    : push(CoreRD(L-Next()-1));
     |0F3h    : CoreWR(L-Next()-1,pop());
     |0F4h    : i:=pop(); CoreWR(pop(),i); push(i);
     |0FAh    : push(P)
     |0FEh    : print('\n%$8h',pop())
     |0FFh    : Trap(49h)
  ELSE  Trap(7h)
  END
END Int2;

VAR buf: ARRAY [0..1023] OF INTEGER;

PROCEDURE IO(no: INTEGER);
  VAR i,j,k,Adr: INTEGER;
BEGIN
  CASE no OF
       0      : Adr:=pop() MOD 1000h;
                IF    Adr=0FB8h THEN
                  IF Pressed()#0 THEN push(200b) ELSE push(0) END
                ELSIF Adr=0FB9h THEN
                  push(ORD(BusyRead()));
                ELSIF Adr=0FBAh THEN
                  push(200b)
                ELSE
                  push(0)
                END;
     | 1      : i:=pop(); Adr:=pop() MOD 1000h;
                IF Adr=0FBBh THEN Write(CHAR(i)) END;
     | 2      : i:=pop(); Adr:=pop(); j:=pop();
                IF i MOD 4 # 0 THEN buf[i DIV 4]:=CoreRD(Adr+i DIV 4) END;
                push(I(bRead(disk,j,ADR(buf),i)));
                FOR k:=0 TO (i+3) DIV 4 -1 DO CoreWR(Adr+k,buf[k]) END;
     | 3h     : i:=pop(); Adr:=pop(); j:=pop();
                FOR k:=0 TO (i+3) DIV 4 -1 DO buf[k]:=CoreRD(Adr+k) END;
                push(I(bWrite(disk,j,ADR(buf),i)));
     | 4h     : push(INTEGER(OpenOnDir(CD(),disk,diskName)));
     | 5h     : push(INTEGER(Close(disk)))
  ELSE
  END;
END IO;

PROCEDURE lxb;
  VAR i,j,s: INTEGER;
BEGIN
  i:=pop(); j:=pop(); s:=CoreRD(j+i DIV 4); push(unpack(s,i MOD 4));
END lxb;

PROCEDURE sxb;
  VAR i,j,s,k: INTEGER;
BEGIN
  k:=pop(); i:=pop(); j:=pop();
  s:=CoreRD(j+i DIV 4); pack(s,i MOD 4,k); CoreWR(j+i DIV 4,s);
END sxb;

PROCEDURE Interpreter;
  VAR i,j: INTEGER;
      a,b: CHAR;
      Time: INTEGER;
BEGIN
  P:=CoreRD(1); RestoreRegs;
  Time:=0;
  Ipt:=FALSE;
  LOOP
--  IF Executed>31000 THEN ShowRegs END;
    IF Time>500 THEN Trap(1); Time:=0 END;
    IR:=Next(); INC(Executed); INC(Time);
    CASE IR OF
     |0h..0Fh : push(IR  MOD 16)
     |10h     : push(Next())
     |11h     : push(Next2())
     |12h     : push(Next4())
     |13h     : push(Nil)
     |14h     : push(L+Next())
     |15h     : push(G+Next())
     |16h     : push(pop()+Next())
     |17h     : push(I(CoreRD(CoreRD(G-Next()-1)))+Next());
     |18h     : IF pop()=0 THEN INC(PC,Next2()) ELSE INC(PC,2) END;
     |19h     : INC(PC,Next2())
     |1Ah     : IF pop()=0 THEN INC(PC,Next()) ELSE INC(PC) END;
     |1Bh     : INC(PC,Next())
     |1Ch     : IF pop()=0 THEN INC(PC,-Next2()) ELSE INC(PC,2) END;
     |1Dh     : DEC(PC,Next2())
     |1Eh     : IF pop()=0 THEN INC(PC,-Next()) ELSE INC(PC) END;
     |1Fh     : DEC(PC,Next())
     |20h     : push(CoreRD(L+Next()));
     |21h     : push(CoreRD(G+Next()));
     |22h     : push(CoreRD(I(CoreRD(CoreRD(G-Next()-1)))+Next()));
     |23h     : push(CoreRD(pop()+Next()));
     |24h..2Fh: push(CoreRD(L+IR MOD 16));
     |30h     : CoreWR(L+Next(),pop());
     |31h     : CoreWR(G+Next(),pop());
     |32h     : CoreWR(I(CoreRD(CoreRD(G-Next()-1)))+Next(),pop());
     |33h     : i:=pop(); CoreWR(pop()+Next(),i);
     |34h..3Fh: CoreWR(L+IR MOD 16,pop());
     |40h     : lxb;
     |41h     : push(CoreRD(pop()+pop()));
     |42h..4Fh: push(CoreRD(G+IR MOD 16));
     |50h     : sxb;
     |51h     : i:=pop(); CoreWR(pop()+pop(),i);
     |52h..5Fh: CoreWR(G+IR MOD 16,pop());
     |60h..6Fh: push(CoreRD(pop()+IR MOD 16));
     |70h..7Fh: i:=pop(); CoreWR(pop()+IR MOD 16,i);
     |80h     :
     |81h     : CoreWR(1,P); EXIT;
     |82h     : push(INTEGER(M))
     |83h     : i:=CoreRD(L+2);
                IF NOT (ChangeMaskBit IN BITSET(i)) THEN
                  CoreWR(L+2,BITSET(i)+{ChangeMaskBit});
                  CoreWR(L+3,M);
                END;
                M:=BITSET(pop())
     |84h     : Trap(pop())
     |85h     : Transfer(pop(),pop())
     |86h     : i:=pop(); push(CoreRD(i)); CoreWR(i,0);
     |87h     : DEC(PC);
     |88h     : push(pop()+pop());
     |89h     : i:=pop(); push(pop()-i);
     |8Ah     : push(pop()*pop())
     |8Bh     : i:=pop(); push(pop() DIV i)
     |8Ch     : i:=pop(); push(SHL(pop(),i));
     |8Dh     : i:=pop(); push(SHR(pop(),i));
     |8Eh     : i:=pop(); push(pop()<<i)
     |8Fh     : i:=pop(); push(pop()>>i)
     |90h..95h: IO(IR MOD 16)
     |0A0h    : i:=pop(); j:=pop(); push(INTEGER(j<i))
     |0A1h    : i:=pop(); j:=pop(); push(INTEGER(j<=i))
     |0A2h    : i:=pop(); j:=pop(); push(INTEGER(j>i))
     |0A3h    : i:=pop(); j:=pop(); push(INTEGER(j>=i))
     |0A4h    : i:=pop(); j:=pop(); push(INTEGER(j=i))
     |0A5h    : i:=pop(); j:=pop(); push(INTEGER(j#i))
     |0A6h    : push(ABS(pop()))
     |0A7h    : push(-pop())
     |0A8h    : push(INTEGER(BITSET(pop())+BITSET(pop())))
     |0A9h    : push(INTEGER(BITSET(pop())*BITSET(pop())))
     |0AAh    : i:=pop(); push(INTEGER(BITSET(pop())/BITSET(i)))
     |0ABh    : i:=pop(); push(INTEGER(BITSET(pop())-BITSET(i)))
     |0ACh    : i:=pop(); push(INTEGER(pop() IN BITSET(i)))
     |0ADh    : i:=pop(); j:=0; INCL(BITSET(j),i); push(j)
     |0AEh    : push(INTEGER(pop()=0))
     |0AFh    : i:=pop(); push(pop() MOD i)
     |0B0h    : DEC(S,pop())
     |0B1h    : i:=pop()
     |0B2h    : i:=pop(); RestoreAStack; push(i)
     |0B3h    : IF (S+8>H) THEN Fuck(40h) ELSE SaveAStack END;
     |0B4h    : IF (S+8>H) THEN Fuck(40h) ELSE
                  i:=pop(); SaveAStack; CoreWR(S,i); INC(S);
                END
     |0B5h    : i:=pop(); push(i); push(i)
     |0B6h    : i:=pop(); j:=(i+5) DIV 4;               --CPCOP
                IF (j+S>H) THEN push(i); Fuck(40h)
                ELSE
                  CoreWR(L+Next(),S); i:=pop();
                  WHILE j>0 DO
                    CoreWR(S,CoreRD(i));
                    INC(S); INC(i); DEC(j)
                  END
                END;
     |0B7h    : j:=pop();                               --PCOP
                IF (j+S>H) THEN push(j); Fuck(40h);
                ELSE
                  CoreWR(L+Next(),S); i:=pop();
                  WHILE j>0 DO
                    CoreWR(S,CoreRD(i));
                    INC(S); INC(i); DEC(j)
                  END
                END
    ELSE Int2()
    END;
  END;
  SaveRegs;
END Interpreter;

VAR nm: FileName;
    fl: File;

PROCEDURE Crash;
BEGIN
  print('\nИсполнено %d команд.\n',Executed);
  ShowStat;
  SaveRegs;
--nm:='DUMP';
--checkHALT(OpenOnDir(CD(),fl,nm),nm);
--checkHALT(bWrite(fl,0,GetCode(0),4096*64),nm);
--checkHALT(Close(fl),nm);
  print('Kronos P4.1 quited.\n');
END Crash;

BEGIN
  Final(Crash);
  diskName:='INT.DISK'; nm:=diskName;
  checkHALT(OpenOnDir(CD(),fl,nm),nm);
  checkHALT(bRead(fl,0,GetCode(0),4096),nm);
  checkHALT(Close(fl),nm);
  trap:=t_reg();
  Executed:=0;
  sp:=0;
  Ipt:=FALSE;
  Interpreter;
END int.
