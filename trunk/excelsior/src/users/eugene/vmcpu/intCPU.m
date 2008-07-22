IMPLEMENTATION MODULE intCPU; (* Sem 05-Jun-88. (c) KRONOS *)

FROM SYSTEM     IMPORT  WORD;
FROM Terminal   IMPORT  print;
FROM intRAM     IMPORT  CoreRD, CoreWR, FailPH, SetAdr, CheckPage,
                        CheckWrPage, FailAdr, FailWR, FailDat, GetCode;

CONST                    --  Mask
      TIMEvector = 01h;  --  0
      VMvector   = 02h;
      INVLDvector= 03h;
      RESvector  = 04h;
      STKvector  = 05h;
      RNGvector  = 06h;  --  1
      SHvector   = 07h;

VAR IR     : INTEGER;
    time   : INTEGER;
    code   : POINTER TO ARRAY [0..0FFFFh] OF CHAR;

    trapMM : BOOLEAN;
    trapSH : BOOLEAN;
    trapRNG: BOOLEAN;
    trapSTK: BOOLEAN;

    regsFL : BOOLEAN;

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
  trapSTK:=TRUE;
END push;

PROCEDURE pop(): INTEGER;
BEGIN
  IF sp>0 THEN DEC(sp); RETURN AStack[sp] END;
  trapSTK:=TRUE; RETURN 0;
END pop;

PROCEDURE Empty(): BOOLEAN;
BEGIN RETURN sp=0 END Empty;

PROCEDURE Mark(x: INTEGER; Extern: BOOLEAN);
  VAR i: INTEGER;
BEGIN
  i:=S;
  CoreWRS(x); INC(S);
  CoreWRS(L); INC(S);
  IF Extern THEN
    CoreWRS(BITSET(PC)+{ExternalBit});
  ELSE
    CoreWRS(PC);
  END;
  INC(S,2); L:=i;
END Mark;


PROCEDURE unpack(VAR a: INTEGER; n: INTEGER): INTEGER; CODE 40h END unpack;

PROCEDURE pack(VAR a: INTEGER; n,v: INTEGER); CODE 50h END pack;

PROCEDURE CalcPH(a: INTEGER; w: BOOLEAN): INTEGER;
  VAR i,j,segPhAdr,segAdr,pagePhAdr: INTEGER;
BEGIN
  ASSERT(a=INTEGER(BITSET(a)-{29..31}));
  IF 0Ch IN M THEN
    page:=a DIV 1024; segment:=page DIV 1024; page:=page MOD 1024;
    ASSERT(segment<512);
    IF segment > ST MOD 1024 THEN
      -- особый случай использования сегмента:
      --   выход за пределы таблицы сегментов
      RETURN 0Ch
    END;
    i:=INTEGER(BITSET(ST)-{0..9})+segment; segAdr:=CoreRDph(i);
    IF 31 IN BITSET(segAdr) THEN
      -- особый случай использования сегмента:
      --   сегмент не доступен
      RETURN 0Ch;
    END;
    IF page > segAdr MOD 1024 THEN
      -- особый случай использования страницы:
      --   выход за пределы сегмента
      RETURN 0Dh;
    END;
    j:=INTEGER(BITSET(segAdr)-{0..9})+page; pageAdr:=CoreRDph(j);
    IF 31 IN BITSET(pageAdr) THEN
      -- особый случай использования страницы:
      --   страница не доступна
      RETURN 0Dh;
    END;
    IF w THEN
      IF 30 IN BITSET(pageAdr) THEN
        -- попытка нарушить защиту записи страницы
        RETURN 0Eh;
      END;
      IF 29 IN BITSET(pageAdr) THEN
        -- первая запись в страницу
        IF 30 IN BITSET(segAdr) THEN
          -- попытка нарушить защиту записи сегмента
          RETURN 0Eh;
        END;
        IF 29 IN BITSET(segAdr) THEN
          -- первая запись в сегмент
          EXCL(BITSET(segAdr),29); CoreWRph(i,segAdr);
        END;
        EXCL(BITSET(pageAdr),29); CoreWRph(j,pageAdr);
      END;
    END;
    SetAdr(a,pageAdr,29 IN BITSET(pageAdr)); RETURN 0;
  ELSE
    SetAdr(a,INTEGER(BITSET(a)-{0..9}),FALSE); RETURN 0;
  END;
END CalcPH;

PROCEDURE CheckAdr(a: INTEGER);
-- Проверяет наличие страницы в буфере быстрой переадресации
-- Если ее там нет то вычисляет ее физический адрес и заносит в буфер
-- Если страницы нет в физической памяти, то заносит ее номер в
--      таблицу недостающих страниц и возвращает trapMM=TRUE

BEGIN
  IF FailPH THEN RETURN END;

END CheckAdr;

PROCEDURE SaveRegs;
  VAR i,j: INTEGER;
BEGIN
  ASSERT(NOT FailPH); ASSERT(NOT trapMM); ASSERT(regsFL);
  CheckWrAdr(P); CheckWrAdr(P+15);
  IF trapMM THEN
    print('Дискриптор процесса %$8h исчез из физической памяти!\n',P); HALT(1);
  END;
  i:=P+8; j:=0;
  WHILE NOT Empty() DO CoreWR(i,pop()); INC(i); INC(j) END;
  CoreWR(P+7,j);  CoreWR(P,G);
  CoreWR(P+1,L);  CoreWR(P+2,PC);
  CoreWR(P+3,M);  CoreWR(P+4,S);
  ASSERT(NOT FailPH);
  regsFL:=FALSE;
END SaveRegs;

PROCEDURE RestoreRegs;
  VAR i,j: INTEGER;
BEGIN
  ASSERT(NOT FailPH); ASSERT(NOT trapMM); ASSERT(NOT regsFL);
  CheckAdr(P); CheckAdr(P+15); IF trapMM THEN RETURN END;
  G :=CoreRD(P);
  F :=CoreRD(G); code:=GetCode(F);
  D :=CoreRD(G+2); L :=CoreRD(P+1);
  PC:=CoreRD(P+2); M :=CoreRD(P+3);
  S :=CoreRD(P+4); H :=CoreRD(P+5);
  i :=CoreRD(P+7); j:=P+8+i;
  IF i>SIZE(AStack) THEN i:=SIZE(AStack) END;
  WHILE i>0 DO DEC(i); DEC(j); push(CoreRD(j)) END;
  ASSERT(NOT FailPH);
END RestoreRegs;

PROCEDURE VecTrap(no: INTEGER);
  VAR seg,tag: INTEGER;
BEGIN
  ASSERT((no>=0)&(no<256));
  SaveRegs;
  ASSERT(NOT (trapSH OR trapRNG OR trapSTK));
  seg:=CoreRDph(0); tag:=Tag?();
  CoreWRph(0,CoreRDph(1)); SetTag(0);
  CheckWrAdr(0);
  IF trapMM THEN
    print('Векторов прерываний нет в физической памяти!\n'); HALT(1);
  END;
  CoreWR(no*4+1,P);
  CoreWR(no*4+2,seg);
  CoreWR(no*4+3,tag);
  P:=CoreRD(no*4);
  RestoreRegs;
  IF trapMM THEN
    trapMM:=FALSE;
    seg:=CoreRDph(0); tag:=Tag?();
    CheckWrAdr(0);
    ASSERT(NOT trapMM);
    no:=VMvector;
    CoreWR(no*4+1,P);
    CoreWR(no*4+2,seg);
    CoreWR(no*4+3,tag);
    P:=CoreRD(no*4);
    RestoreRegs;
    IF trapMM THEN
      print('Виртуализатора нет в физической памяти!\n'); HALT(1);
    END;
  END;
END VecTrap;

PROCEDURE Transfer(to: INTEGER);
  VAR to,seg,tag,no: INTEGER;
BEGIN
  SaveRegs;
  P:=to;
  RestoreRegs;
  IF trapMM THEN
    trapMM:=FALSE;
    seg:=CoreRDph(0); tag:=Tag?();
    CoreWRph(0,CoreRDph(1)); SetTag(0);
    CheckWrAdr(0);
    IF trapMM THEN
      print('Векторов прерываний нет в физической памяти!\n'); HALT(1);
    END;
    no:=VMvector;
    CoreWR(no*4+1,P);
    CoreWR(no*4+2,seg);
    CoreWR(no*4+3,tag);
    P:=CoreRD(no*4);
    RestoreRegs;
    IF trapMM THEN
      print('Виртуализатора нет в физической памяти!\n'); HALT(1);
    END;
  END;
END Transfer;

PROCEDURE TryExecCmd;
-- Пытается исполнить одну команду текущего процесса
-- В результате этой попытки могут возникнуть запросы прерываний:
-- FailPH, trapSTK, trapRNG, и т.д.

  VAR i,j: INTEGER;
BEGIN
  IR:=Next();
  CASE IR OF
   |0h..0Fh : push(IR  MOD 16);
   |10h     : push(Next());
   |11h     : push(Next2());
   |12h     : push(Next4());
   |13h     : push(Nil);
   |14h     : push(L+Next());
   |15h     : push(D+Next());
   |16h     : push(pop()+Next());
   |17h     : push(CoreRD(G+Next())+Next());
              IF FailPH THEN i:=pop(); DEC(PC,3) END;
   |18h     : IF pop()=0 THEN INC(PC,Next2()) ELSE INC(PC,2) END;
   |19h     : INC(PC,Next2());
   |1Ah     : IF pop()=0 THEN INC(PC,Next()) ELSE INC(PC) END;
   |1Bh     : INC(PC,Next());
   |1Ch     : IF pop()=0 THEN INC(PC,-Next2()) ELSE INC(PC,2) END;
   |1Dh     : DEC(PC,Next2());
   |1Eh     : IF pop()=0 THEN INC(PC,-Next()) ELSE INC(PC) END;
   |1Fh     : DEC(PC,Next());
   |20h     : push(CoreRD(L+Next()));
              IF FailPH THEN i:=pop(); DEC(PC,2) END;
   |21h     : push(CoreRD(G+Next()));
              IF FailPH THEN i:=pop(); DEC(PC,2) END;
   |22h     : push(CoreRD(pop()+Next()));
              IF FailPH THEN i:=pop(); DEC(PC,2) END;
   |23h..2Fh: push(CoreRD(L+IR MOD 16));
              IF FailPH THEN i:=pop(); DEC(PC) END;
   |30h     : CoreWR(L+Next(),pop());
              IF FailPH THEN push(FailDat); DEC(PC,2) END;
   |31h     : CoreWR(G+Next(),pop());
              IF FailPH THEN push(FailDat); DEC(PC,2) END;
   |32h     : i:=pop(); j:=Next(); k:=pop(); CoreWR(j+k,i);
              IF FailPH THEN push(k); push(i); DEC(PC,2) END;
   |33h..3Fh: CoreWR(L+IR MOD 16,pop());
              IF FailPH THEN push(FailDat); DEC(PC) END;
   |40h     : push(CoreRD(pop()+pop()));
              IF FailPH THEN i:=pop(); push(FailAdr); push(0); DEC(PC) END;
   |41h..4Fh: push(CoreRD(G+IR MOD 16));
              IF FailPH THEN i:=pop(); DEC(PC) END;
   |50h     : i:=pop(); CoreWR(pop()+pop(),i);
              IF FailPH THEN push(FailAdr); push(0); push(i); DEC(PC) END;
   |51h..5Fh: CoreWR(G+IR MOD 16,pop());
              IF FailPH THEN push(FailDat); DEC(PC) END;
   |60h..6Fh: i:=pop(); push(CoreRD(i+IR MOD 16));
              IF FailPH THEN j:=pop(); push(i); DEC(PC) END;
   |70h..7Fh: i:=pop(); j:=pop(); CoreWR(j+IR MOD 16,i);
              IF FailPH THEN push(j); push(i); DEC(PC) END;
   |80h     : push(P);
   |81h     : -- SYS
   |82h     : -- EI
   |83h     : -- DI
   |84h     : TrapVec(pop() MOD 128 +128);
   |85h     : Transfer(pop());
   |86h     : i:=pop(); push(CoreRD(i)); CoreWR(i,0);
              IF FailPH THEN j:=pop(); push(i); DEC(PC) END;
   |87h     : DEC(PC);
   |88h     : di; push(pop()+pop()); ei;
   |89h     : di; i:=pop(); push(pop()-i); ei;
   |8Ah     : di; push(pop()*pop()); ei;
   |8Bh     : di; i:=pop(); push(pop() DIV i); ei;
   |8Ch     : di; i:=pop(); push(SHL(pop(),i)); ei;
   |8Dh     : di; i:=pop(); push(SHR(pop(),i)); ei;
   |8Eh     : i:=pop(); push(pop()<<i);
   |8Fh     : i:=pop(); push(pop()>>i);
   |0A0h    : i:=pop(); push(INTEGER(pop()<i));
   |0A1h    : i:=pop(); push(INTEGER(pop()<=i));
   |0A2h    : i:=pop(); push(INTEGER(pop()>i));
   |0A3h    : i:=pop(); push(INTEGER(pop()>=i));
   |0A4h    : i:=pop(); push(INTEGER(pop()=i));
   |0A5h    : i:=pop(); push(INTEGER(pop()#i));
   |0A6h    : di; push(ABS(pop())); ei;
   |0A7h    : di; push(-pop()); ei;
   |0A8h    : push(INTEGER(BITSET(pop())+BITSET(pop())));
   |0A9h    : push(INTEGER(BITSET(pop())*BITSET(pop())));
   |0AAh    : i:=pop(); push(INTEGER(BITSET(pop())/BITSET(i)));
   |0ABh    : i:=pop(); push(INTEGER(BITSET(pop())-BITSET(i)));
   |0ACh    : i:=pop(); push(INTEGER(pop() IN BITSET(i)));
   |0ADh    : i:=pop();
              IF (i<0)OR(i>=32) THEN trapRNG:=TRUE; push(0)
              ELSE push(INTEGER({i})) END;
   |0AEh    : push(INTEGER(pop()=0))
   |0AFh    : di; i:=pop(); push(pop() MOD i); ei;
   |0B0h    : i:=pop();
              IF S-i>H THEN trapSH:=TRUE; DEC(PC); push(i) ELSE DEC(S,i) END;
   |0B1h    : i:=pop()
   |0B2h    : CheckAdr(S-1); CheckAdr(S-SIZE(AStack));
              IF NOT FailPH THEN
                i:=pop(); DEC(S); j:=CoreRD(S);
                IF j>=SIZE(AStack) THEN
                  trapSTK:=TRUE;
                ELSE
                  WHILE j>0 DO DEC(S); push(CoreRD(S)); DEC(j) END; push(i);
                END;
              ELSE
                DEC(PC);
              END;
   |0B3h    : IF S+SIZE(AStack)+1>H THEN
                DEC(PC); trapSH:=TRUE;
              ELSE
                CheckWrAdr(S); CheckWrAdr(S+SIZE(AStack));
                IF NOT FailPH THEN
                  i:=0;
                  WHILE NOT Empty DO CoreWR(S,pop()); INC(i); INC(S) END;
                  CoreWR(S,i); INC(S); ASSERT(NOT FailPH);
                ELSE
                  DEC(PC);
                END;
              END;
   |0B4h    : IF S+SIZE(AStack)+1>H THEN
                DEC(PC); trapSH:=TRUE;
              ELSE
                CheckWrAdr(S); CheckWrAdr(S+SIZE(AStack));
                IF NOT FailPH THEN
                  i:=0; j:=pop();
                  WHILE NOT Empty DO CoreWR(S,pop()); INC(i); INC(S) END;
                  CoreWR(S,i); INC(S); CoreWR(S,j); INC(S);
                  ASSERT(NOT FailPH);
                ELSE
                  DEC(PC);
                END;
              END;
   |0B5h    : i:=pop(); push(i); push(i)
   |0B6h    : push(pop()*pop()+pop());
   |0B7h    : i:=pop(); pack(i,3,Next()); push(i);
   |0B8h    : i:=pop(); j:=pop(); INC(j,i DIV 4); i:=i MOD 4;
              k:=CoreRD(j); push(unpack(k,i))
              IF FailPH THEN k:=pop(); push(j); push(i); DEC(PC) END;
   |0B9h    : l:=pop(); i:=pop(); j:=pop(); INC(j,i DIV 4); i:=i MOD 4;
              k:=CoreRD(j); pack(k,i,l); CoreWR(j,k);
              IF FailPH THEN push(j); push(i); push(l); DEC(PC) END;
   |0BAh    : IF S=H THEN
                trapSH:=TRUE; DEC(PC);
              ELSE
                CoreWR(S,pop()); INC(S);
                IF FailPH THEN DEC(PC); DEC(S); push(FailDat) END;
              END;
   |0BBh    : DEC(S); push(CoreRD(S));
              IF FailPH THEN DEC(PC); INC(S); i:=pop() END;
   |0BCh    : push(pop()+PC);
   |0BDh    : PC:=pop();
   |0BEh    : IF pop()#0 THEN push(1); INC(PC,Next()) ELSE INC(PC) END;
   |0BFh    : IF pop()=0 THEN push(0); INC(PC,Next()) ELSE INC(PC) END;
   |0C0h    : i:=pop(); fr:=pop(); to:=pop();
              LOOP
                IF i=0 THEN EXIT END;
                CoreWR(to,CoreRD(fr));
                IF FailPH THEN DEC(PC); push(to); push(fr); push(i); EXIT END;
                INC(fr); INC(to); DEC(i);
              END;
   |0C6h    : i:=pop(); j:=pop(); k:=pop(); push(k);
              IF (k<j)OR(k>i) THEN trapRNG:=TRUE END;
   |0C7h    : i:=pop(); k:=pop(); push(k);
              IF (k<0)OR(k>i) THEN trapRNG:=TRUE END;
   |0C8h    : i:=pop();
              IF S+i>H THEN
                trapSH:=TRUE; DEC(PC); push(i);
              ELSE
                push(S); INC(S,i);
              END;
   |0C9h    : i:=pop();
              IF S+i>H THEN trapSH:=TRUE; DEC(PC); push(i) ELSE INC(S,i) END;
   |0CAh    : i:=CoreRD(L+2);
              IF 31 IN i THEN j:=CoreRD(L); k:=CoreRD(j) END;
              IF FailPH THEN
                DEC(PC);
              ELSE
                S:=L; L:=CoreRD(S+1); PC:=i;
                IF 31 IN BITSET(i) THEN
                  EXCL(BITSET(PC),31); G:=j; F:=k;
                END;
                ASSERT(NOT FailPH);
              END;
   |0CBh    :
   |0CCh    : -- CX
   |0CDh    : -- CI
   |0CEh    : -- CF
   |0CFh    : -- CL
   |0D0h..0DFh: -- CL0
   |0E0h    : i:=pop();
              IF (i<0)OR(i>32) THEN
                trapRNG:=TRUE;
              ELSE
                k:=pop(); INCL(BITSET(k),i); push(k);
              END;
   |0E1h    : i:=pop();
              IF (i<0)OR(i>32) THEN
                trapRNG:=TRUE;
              ELSE
                k:=pop(); EXCL(BITSET(k),i); push(k);
              END;
   |0E2h    : i:=pop(); j:=pop(); INC(j,i DIV 32); i:=i MOD 32;
              CheckWrAdr(j);
              IF FailPH THEN
                DEC(PC); push(j); push(i);
              ELSE
                k:=CoreRD(j); INCL(BITSET(k),i); CoreWR(j,k);
              END;
   |0E3h    : i:=pop(); j:=pop(); INC(j,i DIV 32); i:=i MOD 32;
              CheckWrAdr(j);
              IF FailPH THEN
                DEC(PC); push(j); push(i);
              ELSE
                k:=CoreRD(j); EXCL(BITSET(k),i); CoreWR(j,k);
              END;
   |0E4h    : di; i:=pop(); CoreWR(i,CoreRD(i)+1); ei;
              IF FailPH THEN DEC(PC); push(i) END;
   |0E5h    : di; i:=pop(); CoreWR(i,CoreRD(i)-1); ei;
              IF FailPH THEN DEC(PC); push(i) END;
   |0E6h    : di; j:=pop(); i:=pop(); CoreWR(i,CoreRD(i)+j); ei;
              IF FailPH THEN DEC(PC); push(i); push(j) END;
   |0E7h    : di; j:=pop(); i:=pop(); CoreWR(i,CoreRD(i)-j); ei;
              IF FailPH THEN DEC(PC); push(i); push(j) END;
   |0F0h    : i:=pop(); j:=pop(); push(i); push(j);
   |0F1h    : push(L-Next()-1);
   |0F2h    : push(CoreRD(L-Next()-1));
              IF FailPH THEN i:=pop(); DEC(PC,2) END;
   |0F3h    : CoreWR(L-Next()-1),pop());
              IF FailPH THEN push(FailDat); DEC(PC,2) END;
   |0F4h    : i:=pop(); j:=pop(); CoreWR(j,i); push(j);
              IF FailPH THEN push(i); DEC(PC) END;
   |0FFh    : VecTrap(INVLDvector);
  ELSE VecTrap(RESvector);
  END;
  trapRNG:=trapRNG OR trapRNG?();
END TryExecCmd;

PROCEDURE Execute;
BEGIN
  INC(time);
  IF (time>100)&(0 IN M) THEN
    DEC(time,100); VecTrap(TIMEvector)
  ELSE
    TryExecCmd;
    IF FailPH THEN
      CalcPH; FailPH:=FALSE;
      trapRNG:=FALSE; trapSH:=FALSE;
    END;
    IF trapSTK THEN
      trapSTK:=FALSE; trapMM:=FALSE; trapRNG:=FALSE; trapSH:=FALSE;
      VecTrap(STKvector);
    ELSIF trapMM THEN
      trapMM:=FALSE; trapRNG:=FALSE; trapSH:=FALSE;
      VecTrap(VMvector);
    ELSIF trapRGN THEN
      trapRNG:=FALSE; trapSH:=FALSE;
      IF 1 IN M THEN VecTrap(RNGvector) END;
    ELSIF trapSH  THEN
      trapSH:=FALSE;
      VecTrap(SHvector);
    END;
  END;
END Execute;

BEGIN
  time:=0;
END intCPU.m
