MODULE mem25tst; (*$T- Leo 27-Aug-88. (c) KRONOS *)
                 (*$I- Leo 22-Feb-89. (c) KRONOS *)
                 (*$N- Leo 22-Feb-89. (c) KRONOS *)

FROM SYSTEM    IMPORT   ADDRESS, WORD, ADR;

CONST KB=256;

----------------------------------------------------------------

CONST trb=093h;
     jbsc=01Eh;
     drop=0B1h;

PROCEDURE getm(): BITSET;  CODE 82h END getm;
PROCEDURE setm(m: BITSET); CODE 83h END setm;
PROCEDURE di; CODE 82h 03h 0ABh 83h END di;
                        -- bic
PROCEDURE ei; CODE 82h 02h 0A8h 83h END ei;
                        -- or

PROCEDURE wait(flag: INTEGER); CODE 0B5h trb jbsc 04 drop END wait;
                                 -- copt


CONST BASE=80000h;
      HALF=10000h;


VAR put_in : INTEGER;           MASK16_31: BITSET; (* :={16..31} *)
    put_end: INTEGER;
    put_beg: INTEGER;
    put_out: INTEGER;

    get_in : INTEGER;           MASK00_15: BITSET; (* :={00..15} *)
    get_end: INTEGER;
    get_beg: INTEGER;
    get_out: INTEGER;


        BCB: POINTER TO ARRAY [0..0FFFFh] OF CHAR;
       wBCB: POINTER TO ARRAY [0..0FFFFh] OF INTEGER;
     F0000h: POINTER TO ARRAY [0..0FFFFh] OF CHAR;
     inFLAG: INTEGER; (* byte pointers *)
    outFLAG: INTEGER; (* relative 0    *)

PROCEDURE VDU;
  CONST channel=9;
  VAR adr: ADDRESS;
BEGIN
  MASK16_31:={16..31};
  MASK00_15:={00..15};
  F0000h:=ADDRESS( BASE + 0F0000h DIV 4 );

  adr:=BASE+(0F8010h+channel*4) DIV 4;
  adr:=INTEGER(adr^) MOD HALF;
  BCB:=ADDRESS( F0000h ) + adr DIV 4; wBCB:=ADDRESS(BCB);
  outFLAG:=( INTEGER(BCB)*4 )+12;
   inFLAG:=( INTEGER(BCB)*4 )+00;

  put_in :=INTEGER(wBCB^[3]>>16) MOD HALF; (* 14,15 bytes BCB *)
  put_end:=INTEGER(wBCB^[5]>>16) MOD HALF; (* 22,23 bytes BCB *)
  put_beg:=        wBCB^[5] MOD HALF;      (* 20,21 bytes BCB *)

  get_out:=        wBCB^[1]      MOD HALF;  (*  4, 5 bytes BCB *)
  get_end:=INTEGER(wBCB^[2]>>16) MOD HALF;  (* 10,11 bytes BCB *)
  get_beg:=        wBCB^[2]      MOD HALF;  (*  8, 9 bytes BCB *)

END VDU;

PROCEDURE puts(VAL s: ARRAY OF CHAR; VAR Pos,Len: INTEGER);
  VAR pos,len,new: INTEGER; ch,nextch: CHAR;
BEGIN (* used only in write_str where ipts are disabled *)
  pos:=Pos;     len:=Len;
  wait(outFLAG);
    put_out:=INTEGER( BITSET(wBCB^[4])*MASK00_15 );
  BCB^[12]:=1c;
  nextch:=s[pos];
  LOOP
    ch:=nextch;
    IF len=0          THEN         EXIT END;
    IF ch=0c          THEN len:=0; EXIT END;
    IF put_in=put_end THEN new:=put_beg ELSE new:=put_in+1 END;
    IF new=put_out    THEN         EXIT END;
    INC(pos);  DEC(len);        nextch:=s[pos];
    F0000h^[put_in]:=ch;        put_in:=new;
  END;
  Pos:=pos;     Len:=len;
  wait(outFLAG);
  wBCB^[3]:=INTEGER( BITSET(wBCB^[3])-MASK16_31+BITSET(put_in<<16)+{0} );
END puts;

PROCEDURE gets(VAR s: ARRAY OF CHAR; VAR Pos,Len: INTEGER);
  VAR pos,len: INTEGER;
BEGIN
  pos:=Pos;     len:=Len;
  wait(inFLAG);
    get_in:=INTEGER( BITSET(wBCB^[0]>>16)*MASK00_15 );
  BCB^[0]:=1c;
  IF get_in=get_out THEN RETURN END;
  LOOP
    IF len=0  THEN EXIT END;
    s[pos]:=F0000h^[get_out]; INC(pos); DEC(len);
    IF get_out=get_end THEN get_out:=get_beg ELSE INC(get_out) END;
    IF get_in =get_out THEN EXIT END;
  END;
  Pos:=pos;
  Len:=len;
  wait(inFLAG);
  wBCB^[1]:=INTEGER( BITSET(wBCB^[1]) - MASK00_15 + BITSET(get_out) );
   BCB^[0]:=1c;
END gets;

PROCEDURE ready(): INTEGER;
  VAR size: INTEGER;
BEGIN
  size:=get_end-get_beg+1;
  wait(inFLAG);
    get_in:=INTEGER( BITSET(wBCB^[0]>>16)*MASK00_15 );
  BCB^[0]:=1c;
  RETURN (get_in-get_out+size) MOD size;
END ready;

PROCEDURE write_str(VAL s: ARRAY OF CHAR);
  VAR p,l: INTEGER;
BEGIN
  di;
  p:=0; l:=HIGH(s)+1;
  REPEAT puts(s,p,l) UNTIL l=0;
  ei;
END write_str;

----------------------------------------------------------------

CONST
  FDC_NOP   = 00h; -- no operation
  FDC_TRY   = 01h; -- retry
  FDC_ABORT = 02h; -- abort
  FDC_RESET = 03h; -- reset buffering
  FDC_FLUSH = 04h; -- flush all buffers
  FDC_RD    = 05h; -- read a sector
  FDC_WR    = 06h; -- write a sector
  FDC_RT    = 07h; -- read a track
  FDC_WT    = 08h; -- write a track
  FDC_FMT   = 09h; -- format a track
  FDC_RSP   = 0Ah; -- read drive specs
  FDC_WSP   = 0Bh; -- write drive specs
  FDC_RID   = 0Ch; -- establish drive specs by reading drive
  FDC_SEEK  = 0Dh; -- seeks the head to the specified track
  FDC_RD_RT = 0Eh; -- raw read track

TYPE fdcBCB=
  RECORD
    flag  : INTEGER;
    cmd   : INTEGER;
    drv   : INTEGER;
    cyl_lo: INTEGER;
    cyl_hi: INTEGER;
    sec   : INTEGER;
    len   : INTEGER;
    addr  : INTEGER;
    error : INTEGER;
  END;

CONST -- offsets in BCB (in bytes)
  _flag  = 00;   _cmd   = 01;   _sta   = 01;  _drv   = 02;
  _trk   = 03;   _side  = 04;   _sec   = 05;  _len   = 06;
  _chng  = 23;   _ready = 24;   _err   = 16;

CONST
     FDC_CLR=0;         FDC_ATTN=4;

VAR dBCB: POINTER TO ARRAY [0..0FFh] OF CHAR;
     MAD: POINTER TO INTEGER;

PROCEDURE outb(where,what: INTEGER);
  VAR p: POINTER TO ARRAY [0..0] OF CHAR;
BEGIN
  p:=ADDRESS( (0CFFF0h+10h) DIV 4 ) + BASE;
  REPEAT UNTIL p^[0]=0c;
  p^[0]:=CHAR(where+1);
  REPEAT UNTIL p^[0]=0c;
END outb;

PROCEDURE fdc_cmd(VAR bcb: fdcBCB);
BEGIN
  IF dBCB^[_flag]#0c THEN RETURN END;
  outb(FDC_CLR,0);
  dBCB^[_flag]:= CHAR(000h);
  dBCB^[_cmd] := CHAR(bcb.cmd);
  dBCB^[_drv] := CHAR(bcb.drv);
  dBCB^[_trk] := CHAR(bcb.cyl_lo);
  dBCB^[_side]:= CHAR(bcb.cyl_hi);
  dBCB^[_sec] := CHAR(bcb.sec);
  dBCB^[_len] := CHAR(bcb.len);
  MAD^:=bcb.addr;
  dBCB^[_flag]:=CHAR(0FFh);
  outb(FDC_ATTN,0);
END fdc_cmd;

VAR boot_drv: INTEGER;

PROCEDURE Z80FDC;
  VAR adr: ADDRESS;
BEGIN
  adr :=( 0F8010h+1*4 ) DIV 4 + BASE;
  dBCB:=ADDRESS( ( INTEGER( BITSET(adr^)*{0..15} ) + 0F0000h ) DIV 4 + BASE );
  MAD :=ADDRESS(dBCB)+2;
  boot_drv:=ORD(dBCB^[_drv] );
END Z80FDC;

PROCEDURE try_read_track(adr: ADDRESS);
  VAR bcb: fdcBCB;
BEGIN
  bcb.cmd   := FDC_RD; -- read sectors
  bcb.drv   := boot_drv;
  bcb.cyl_lo:= 0;
  bcb.cyl_hi:= 0;
  bcb.sec   := 1;
  bcb.len   := 16*KB;
  bcb.addr  := (adr-BASE)*4;
  fdc_cmd(bcb);
END try_read_track;

----------------------------------------------------------------

VAR current_address: ADDRESS;
   optional_address: ADDRESS;

TYPE                      -- err adr  read  writen
  SimpleError  = PROCEDURE ( ADDRESS, WORD, WORD );
                          -- err adr  read  writen invert
  AddressError = PROCEDURE ( ADDRESS, WORD, WORD,  ADDRESS );
                          -- err adr  read  writen invLOW   invHIGH
  OverlapError = PROCEDURE ( ADDRESS, WORD, WORD,  ADDRESS, ADDRESS );

PROCEDURE pop(): ADDRESS; CODE END pop;
PROCEDURE push(w: WORD) ; CODE END push;
PROCEDURE copt;           CODE 0B5h END copt;
PROCEDURE store;          CODE 0B3h END store;
PROCEDURE lodfv;          CODE 0B2h END lodfv;
PROCEDURE lsw0;           CODE 060h END lsw0;
PROCEDURE ssw0;           CODE 070h END ssw0;

PROCEDURE WritePeace(lo,hi: ADDRESS; pat: WORD);
  VAR a,x: ADDRESS; pat1: WORD; min: INTEGER;
BEGIN
  IF lo>hi THEN RETURN END;
  x:=lo;
  REPEAT
    current_address:=x;
    pat1:=pat; min:=x+32;
    IF min>hi THEN min:=hi END;
    push(x);                   (*1*)
    REPEAT
      copt;                    (*2*)  (* x,x                      *)
      push(pat1<<1);           (*3*)  (* x,x,pat1<<1              *)
      copt;                    (*4*)  (* x,x,pat1<<1,pat1<<1      *)
      pat1:=pop();             (*3*)  (* x,x,pat1<<1              *)
      ssw0;                    (*1*)  (* x                        *)
      push(pop()+1);           (*1*)  (* x                        *)
      copt;                    (*2*)  (* x,x                      *)
    UNTIL pop()>min;           (*1*)
    IF pop()#0 THEN END;
    x:=x+33;
  UNTIL x>hi;
END WritePeace;

PROCEDURE ReadPeace(lo,hi: ADDRESS; pat: WORD; error: SimpleError);
  VAR a,x: ADDRESS; pat1,rd: WORD; min: INTEGER;
BEGIN
  IF lo>hi THEN RETURN END;
  x:=lo;
  REPEAT
    current_address:=x;
    pat1:=pat; min:=x+32;
    IF min>hi THEN min:=hi END;
    push(x);              (*1*)
    REPEAT
      copt;               (*2*)
      lsw0;               (*2*)
      copt;               (*3*)
      rd:=pop();          (*2*)
      push(pat1<<1);      (*3*)
      copt;               (*4*)
      pat1:=pop();        (*3*)
      IF pop()#pop()      (*1*)
      THEN
        a:=pop();         (*0*)
        error(a,rd,pat1);
        push(a);          (*1*)
      END;
      push(pop()+1);      (*1*)
      copt;               (*2*)
    UNTIL pop()>min;      (*1*)
    IF pop()#0 THEN END;
    x:=x+33
  UNTIL x>hi;
END ReadPeace;

(********************* ReadAfterWrite *************************)
(* По каждому адресу пишется образец и немедленно читается.   *)
(* Ошибка показывает несовпадение записанного и прочитанного  *)
(* значений, и может являться следствием неисправного бита в  *)
(* кристале памяти. При переходе к следующему адресу образец  *)
(* сдвигается циклически влево на один бит,                   *)
(**************************************************************)

PROCEDURE ReadAfterWrite(lo,hi: ADDRESS; pat: WORD; error: SimpleError);
  VAR a,x: ADDRESS; pat1,rd: WORD; min: INTEGER;
BEGIN
  IF lo>hi THEN RETURN END;
  x:=lo;
  REPEAT
    current_address:=x;
    pat1:=pat; min:=x+32;
    IF min>hi THEN min:=hi END;
    push(x);                    (*1*)
    REPEAT
      copt;                     (*2*)  (*               x,x                 *)
      push(pat1<<1);            (*3*)  (*               x,x,pat1<<1         *)
      copt;                     (*4*)  (*               x,x,pat1<<1,pat1<<1 *)
      pat1:=pop();              (*3*)  (* pat1:=pat1<<1 x,x,pat1<<1         *)
      ssw0;                     (*1*)  (* x^:=pat1<<1   x                   *)
      copt;                     (*2*)  (*               x,x                 *)
      lsw0;                     (*2*)  (*               x,x^                *)
      copt;                     (*3*)  (*               x,x^,x^             *)
      rd:=pop();                (*2*)  (*               x,x^                *)
      IF WORD(pop())#pat1 THEN  (*1*)  (* x^#pat1       x                   *)
        a :=pop();              (*0*)  (* a:=x;                             *)
        error(a,rd,pat1);
        push(a);                (*1*)  (*               x                   *)
      END;                      (*1*)  (*               x                   *)
      push(pop()+1);            (*1*)  (* x:=x+1        x                   *)
      copt;                     (*2*)  (*               x,x                 *)
    UNTIL pop()>min;            (*1*)  (* x>min         x                   *)
    IF pop()#0 THEN END;
    x:=x+33
  UNTIL x>hi
END ReadAfterWrite;

(********************* WriteAllAndRead  ***********************)
(* По каждому адресу пишется образец. При переходе к          *)
(* следующему адресу образец сдвигается циклически влево на   *)
(* один бит. После того как вся область прописана, вся она    *)
(* читается и сравнивается с тем же образцом.                 *)
(* Ошибка показывает несовпадение записанного и прочитанного  *)
(* значений, и может являться следствием неисправного бита в  *)
(* кристале памяти или искажения адреса при чтении записи.    *)
(**************************************************************)


PROCEDURE WriteAllAndRead(lo,hi: ADDRESS; pat: WORD; error: SimpleError);
BEGIN
  WritePeace(lo,hi,pat);
  ReadPeace (lo,hi,pat,error);
END WriteAllAndRead;

(************************ FastAddress *************************)
(* По каждому адресу записывает значение побитового XOR       *)
(* самого адреса и образца. Запись производится в прямом или  *)
(* обратном направлении. После заполнение всей области        *)
(* производится чтение и проверка.                            *)
(* Ошибка показывает несовпадение записанного и прочитанного  *)
(* значений, и может являться следствием искажения адреса при *)
(* чтении/записи, перепутывания адресов, или неисправного     *)
(* бита в кристале памяти.                                    *)
(**************************************************************)

PROCEDURE FastAddresses(lo,hi: ADDRESS; pat: BITSET; dir: INTEGER;
                        error: SimpleError);
  VAR a,x: ADDRESS; min: ADDRESS; rd: BITSET;
BEGIN
  IF lo>hi THEN RETURN END;
  current_address:=lo;
  IF dir=+1 THEN
    push(lo);                       (*1*)
    REPEAT
      copt;                         (*2*)
      copt;                         (*3*)
      push(BITSET(pop()) / pat);    (*3*)
      ssw0;                         (*1*)
      push(pop()+1);                (*1*)
      copt;                         (*2*)
    UNTIL pop()>hi;                 (*1*)
    IF pop()#0 THEN END;
  END;
  IF dir=-1 THEN
    push(hi);                       (*1*)
    REPEAT
      copt;                         (*2*)
      copt;                         (*3*)
      push(BITSET(pop()) / pat);    (*3*)
      ssw0;                         (*1*)
      push(pop()-1);                (*1*)
      copt;                         (*2*)
    UNTIL pop()<lo;                 (*1*)
    IF pop()#0 THEN END;
  END;
  x:=lo;
  WHILE x<=hi DO min:=x+33;
    current_address:=x;
    IF min>hi THEN min:=hi END;
    push(x);                         (*1*)
    REPEAT
      copt;                          (*2*)
      copt;                          (*3*)
      lsw0;                          (*3*)
      rd:=BITSET(pop());             (*2*)
      IF BITSET(pop())/pat # rd THEN (*1*)
        a:=pop();                    (*0*)
        error(a,rd,BITSET(a)/pat);
        push(a);                     (*1*)
      END;
      push(pop()+1);                 (*1*)
      copt;                          (*2*)
    UNTIL pop()>min;                 (*1*)
    IF pop()#0 THEN END;
    x:=min+1;
  END;
END FastAddresses;

(************************** Addresses *************************)
(* По каждому адресу записывает значение образца. При переходе*)
(* к следующему адресу значение образца сдвигается циклически *)
(* влево на один бит. Немедленно после записи образца по всем *)
(* адресам отличающегося от текущего в одном бите производится*)
(* побитовое инвертирование их содержимого и при каждом       *)
(* инвертировании проверяется значение по текущему адресу.    *)
(* Ошибка показывает несовпадение записанного и прочитанного  *)
(* значений, и может являться следствием "слипания" адресов   *)
(* чтения/записи, перепутывания адресов, или неисправного     *)
(* бита в кристале памяти.                                    *)
(**************************************************************)

PROCEDURE Addresses(lo,hi: ADDRESS; pat: WORD; error: AddressError);

  TYPE bool=BITSET;  CONST false={};

  VAR    a,x,b: ADDRESS;        min: INTEGER;
        barrel: BITSET;          rd: WORD;

BEGIN   barrel:={0};
  WritePeace(lo,hi,pat);
  REPEAT barrel:=barrel<<1 UNTIL ADDRESS(barrel)>hi;
  IF lo>hi THEN RETURN END;
  x:=lo;
  WHILE x<=hi DO
    current_address:=x;
    min:=x+33;
    IF min>hi THEN min:=hi END;
    a:=x;
    REPEAT pat:=pat<<1;
      a^:=pat;
      push(barrel);                             (*1*)
      REPEAT
        push(pop()>>1);                         (*1*)
        copt;                                   (*2*)
        b:=ADDRESS(BITSET(pop())/BITSET(a));    (*1*)
        IF bool(b>=lo)*bool(b<=hi)*bool(b#a)#false THEN
          push(b);                              (*2*)  (* barrel,b *)
          copt;                                 (*3*)  (* barrel,b,b *)
          lsw0;                                 (*3*)  (* barrel,b,b^ *)
          push(b);                              (*4*)  (* barrel,b,b^,b *)
          copt;                                 (*5*)  (* barrel,b,b^,b,b *)
          lsw0;                                 (*5*)  (* barrel,b,b^,b,b^ *)
          push( BITSET(pop())/{0..31} );        (*5*)
          ssw0;                                 (*3*)  (* barrel,b,b^ *)
          rd:=a^;
          IF rd#pat THEN                        (*3*)
            store;
            error(a,rd,pat,b);
            lodfv
          END;                                  (*3*)
          ssw0;                                 (*1*)
        END;
        copt;                                   (*2*)
      UNTIL BITSET(pop())={0};                  (*1*)
      IF pop()#0 THEN END;
      a:=a+1;
    UNTIL a>min;
    x:=min+1;
  END;
END Addresses;

(*********************** Crowl Test ***************************)
(* Аналогичен Read/Write тестам но работает кусочками по 33   *)
(* слова.                                                     *)
(**************************************************************)

PROCEDURE CrowlWriteAndRead(lo,hi: ADDRESS; pat: WORD; error: SimpleError);
  VAR a,x: ADDRESS; pat1,rd: WORD; min: INTEGER;
BEGIN
  IF lo>hi THEN RETURN END;
  x:=lo;
  REPEAT
    current_address:=x;
    pat1:=pat; min:=x+32;
    IF min>hi THEN min:=hi END;
    push(x);             (*1*)
    REPEAT
      copt;              (*2*)
      push(pat1<<1);     (*3*)
      copt;              (*4*)
      pat1:=pop();       (*3*)
      ssw0;              (*1*)
      push(pop()+1);     (*1*)
      copt;              (*2*)
    UNTIL pop()>min;     (*1*)
    IF pop()#0 THEN END;
    pat1:=pat;
    push(x);              (*1*)
    REPEAT
      copt;               (*2*)
      lsw0;               (*2*)
      copt;               (*3*)
      rd:=pop();          (*2*)
      push(pat1<<1);      (*3*)
      copt;               (*4*)
      pat1:=pop();        (*3*)
      IF pop()#pop()      (*1*)
      THEN
        a:=pop();         (*0*)
        error(a,rd,pat1);
        push(a);          (*1*)
      END;
      push(pop()+1);      (*1*)
      copt;               (*2*)
    UNTIL pop()>min;      (*1*)
    IF pop()#0 THEN END;
    x:=x+33
  UNTIL x>hi;
END CrowlWriteAndRead;

(************************** Overlap ***************************)
(* Записывает инвертированным образцом участок в 33 слова и   *)
(* проверяет что значения в остальной области при этом не     *)
(* исказились. Двигает такой участок от начала до конца       *)
(* области, по 33 слова.                                      *)
(* Ошибка показывает несовпадение записанного и прочитанного  *)
(* значений, и может являться следствием взаимного влияния    *)
(* смежных областей памяти, "слипания" адресов                *)
(* чтения/записи, перепутывания адресов, или неисправного     *)
(* бита в кристале памяти.                                    *)
(**************************************************************)

PROCEDURE Overlap(lo,hi: ADDRESS; pat: WORD; error: OverlapError);

  VAR OverlapLoInv, OverlapHiInv: ADDRESS;   break: BOOLEAN;

  PROCEDURE ReadPeace(lo,hi: ADDRESS; pat: WORD);
    VAR a,x: ADDRESS; pat1,rd: WORD; min: INTEGER;
  BEGIN
    IF lo>hi THEN RETURN END;
    x:=lo;
    REPEAT
      optional_address:=x;
      pat1:=pat; min:=x+32;
      IF min>hi THEN min:=hi END;
      push(x);              (*1*)
      REPEAT
        copt;               (*2*)
        lsw0;               (*2*)
        copt;               (*3*)
        rd:=pop();          (*2*)
        push(pat1<<1);      (*3*)
        copt;               (*4*)
        pat1:=pop();        (*3*)
        IF pop()#pop() THEN (*1*)
          a:=pop();         (*0*)
          error(a,rd,pat1,OverlapLoInv,OverlapHiInv);
          push(a);          (*1*)
        END;
        push(pop()+1);      (*1*)
        copt;               (*2*)
      UNTIL pop()>min;      (*1*)
      IF pop()#0 THEN END;
      x:=x+33
    UNTIL x>hi
  END ReadPeace;

  VAR x: ADDRESS; min: INTEGER;

BEGIN x:=lo;
  optional_address:=x;
  WritePeace(lo,hi,pat);
  WHILE x<=hi DO min:=x+(33*33*3-1);
    current_address:=x;
    IF min>hi THEN min:=hi END;
    OverlapLoInv:=x;  OverlapHiInv:=min;
    WritePeace(x,min,{0..31}/BITSET(pat));
    ReadPeace(lo,x-1,pat);
    ReadPeace(min+1,hi,pat);
    ReadPeace(x,min,{0..31}/BITSET(pat));
    WritePeace(x,min,pat);
    ReadPeace(x,hi,pat);
    x:=min+1;
  END;
END Overlap;

----------------------------------------------------------------

TYPE process = POINTER TO
       RECORD
         G: ADDRESS;
         L: ADDRESS;
        PC: INTEGER;
         M: BITSET;
         S: ADDRESS;
         H: ADDRESS;
         T: INTEGER;
       END;


PROCEDURE newprocess(proc: PROC;  a: ADDRESS; sz: INTEGER; VAR p: process);
  VAR pr: process; n,pc: INTEGER; F: ADDRESS;
BEGIN
  pr:=a;
  n :=INTEGER( BITSET(proc<<8)*{0..7} );
  a :=INTEGER( BITSET(proc)*{0..23});
  pr^.G :=a^;  F:=INTEGER(pr^.G^)+n;
  pr^.PC:=F^;
  pr^.M :=getm();
  pr^.L :=ADDRESS(pr)+10h;
  a     :=ADDRESS(pr)+14h; a^:=0;
  pr^.S :=a+1;
  pr^.H :=ADDRESS(pr)+sz-1;
  pr^.T :=0;
  p:=process(pr);
END newprocess;

PROCEDURE transfer(VAR from,to: process); CODE 85h END transfer;


----------------------------------------------------------------

VAR bump: ARRAY [0..127] OF CHAR;

PROCEDURE pos(l,c: INTEGER);
BEGIN
  bump:="" 33c "=  "; bump[2]:=CHAR(l+40b); bump[3]:=CHAR(c+40b);
  write_str(bump);
END pos;

PROCEDURE clear; BEGIN write_str("" 33c "[K") END clear;

PROCEDURE inv(on: BOOLEAN);
BEGIN
  IF on THEN write_str("" 33c "[7m") ELSE write_str("" 33c "[27m") END;
END inv;

PROCEDURE hint(on: BOOLEAN);
BEGIN
  IF on THEN write_str("" 33c "[33m") ELSE write_str("" 33c "[32m") END;
END hint;

CONST hex_dig="0123456789ABCDEF";

PROCEDURE app_hex(VAR str: ARRAY OF CHAR; a: WORD; pos,len: INTEGER);
  VAR i: INTEGER;
BEGIN i:=0;
  REPEAT
    a:=(a<<4); str[i+pos]:=hex_dig[INTEGER(BITSET(a)*{0..3})];
    i:=i+1;
  UNTIL i=len;
END app_hex;

PROCEDURE app_adr(VAR str: ARRAY OF CHAR; a: ADDRESS; pos: INTEGER);
BEGIN
  IF a<BASE THEN
    app_hex(str,a,pos,8); str[pos+8]:=' ';
  ELSE
    a:=(INTEGER(BITSET(current_address)-{0..7}) - BASE)*4;
    app_hex(str,(BITSET(a)-{0..15})<<12,pos,4);
    str[pos+4]:=":";
    app_hex(str,(BITSET(a)*{0..15})<<16,pos+5,4);
  END;
END app_adr;

PROCEDURE write_hex(v: WORD; VAL tail: ARRAY OF CHAR);
BEGIN
  app_hex(bump,v,0,8); bump[8]:=0c;
  write_str(bump);     write_str(tail);
END write_hex;

VAR testno: INTEGER;
       pat: BITSET;
     cycle: INTEGER;

----------------------------------------------------------------

VAR state: BITSET;              mBusLo,mBusHi: ADDRESS;

PROCEDURE quit; CODE 81h END quit;

VAR   str: ARRAY [0..127] OF CHAR;

CONST o=ORD("0");

PROCEDURE info;
BEGIN
  str:=
-- 0         1         2         3         4         5         6         7
-- 0123456789012345678901234567890123456789012345678901234567890123456789012
  "TEST 0     PASS 00     ADDRESS 00000000           ";
  str[ 5]:=CHAR(o + testno);
  str[16]:=CHAR(o + cycle  DIV 10 MOD 10);
  str[17]:=CHAR(o + cycle  MOD 10);
  app_adr(str,ADDRESS(BITSET(current_address)-{0..7}),31);
  IF {15}*state#{} THEN str[41]:="D"; str[42]:="M"; str[43]:="A"
  ELSIF testno=5   THEN
    app_adr(str,ADDRESS(BITSET(optional_address)-{0..7}),40);
  END;
  pos(0,17); write_str(str); clear;
END info;

PROCEDURE opts;
  PROCEDURE on_off(bit,pos: INTEGER);
  BEGIN
    IF bit IN state THEN str[pos]:="O"; str[pos+1]:="N"; str[pos+2]:=" " END;
  END on_off;
BEGIN
  str:=
-- 0         1         2         3         4         5         6         7
-- 01234567890123456789012345678901234567890123456789012345678901234567890123456
  "   <<<   debug OFF   wait OFF   bell OFF   mBus OFF   dma OFF   loc OFF   >>>";
                 on_off(10,26); on_off(14,37);
  on_off(11,48); on_off(12,58); on_off(13,68);
  pos(1,0); write_str(str); clear;
END opts;

PROCEDURE header; FORWARD;
PROCEDURE test_name(n: INTEGER; inv: BOOLEAN); FORWARD;

PROCEDURE key;
  VAR ch: CHAR;
     p,l: INTEGER;
BEGIN
  p:=0; l:=1;
  gets(str,p,l);
  ch:=str[0];
  IF ch=3c  THEN pos(24,0); write_str(""12c); pos(24,0); quit; header
  ELSIF ("0"<=ch) & (ch<="6") THEN l:=ORD(ch)-o;
    state:=state/{l}; test_name(l,testno=l);
  END;
  ch:=CAP(ch);
  IF    ch="B" THEN state:=state/{14};
  ELSIF ch="M" THEN state:=state/{11}
  ELSIF ch="D" THEN state:=state/{12}
  ELSIF ch="L" THEN state:=state/{13}
  ELSIF ch="W" THEN state:=state/{10}
  END;
  opts;
END key;

VAR driver,ipted: process;  time,second: INTEGER;

PROCEDURE configer;
BEGIN
  LOOP
    time:=time+1;
    WHILE ready()#0 DO key END;
    IF time > second THEN info; second:=time+50;
      IF {15}*state#{} THEN try_read_track(mBusLo) END;
    END;
    transfer(driver,ipted);
  END;
END configer;

PROCEDURE config;
  VAR a: ADDRESS;
BEGIN
  time:=0; second:=50;
  di;
  newprocess(configer,BASE+(0D8000h DIV 4),1024,driver);
  a:=1*2;      a^:=driver;
  a:=a+1;      a^:=ADR(ipted);
  driver^.M:=driver^.M-{0,1};
END config;

----------------------------------------------------------------

PROCEDURE write_test_no;
BEGIN
  bump:="| 0 | "; bump[2]:=CHAR(ORD("0")+testno);
  write_str(bump);
END write_test_no;

VAR err_co: INTEGER;

PROCEDURE rollup;
BEGIN
  IF {14}*state#{} THEN write_str("" 7c ) END;
  IF ({10}*state#{}) & (err_co>7) THEN
    pos(22,0);
    write_str("HIT ANY KEY");
    di;
    WHILE ready()=0 DO END;
    WHILE ready()#0 DO key END;
    pos(22,0); clear;
  END;
  pos(12,0); write_str("" 33c "[M");
  pos(21,0); write_str("" 33c "[L");
  INC(err_co);
  ei;
END rollup;

PROCEDURE test_name(no: INTEGER; mark: BOOLEAN);

  PROCEDURE w(VAL s: ARRAY OF CHAR);
    VAR i: INTEGER;
  BEGIN
    pos(3+no,0); bump:="TEST 0  "; bump[5]:=CHAR(ORD("0") + no MOD 10);
    IF no IN state THEN bump[6]:='+' END;
    write_str(bump); write_str(s);
    IF mark THEN i:=0;
      REPEAT i:=i+1 UNTIL s[i]=0c; i:=71-i;  bump[i+1]:=0c;
      REPEAT bump[i]:=' '; i:=i-1 UNTIL i<0; write_str(bump);
    ELSE
      clear;
    END;
  END w;

BEGIN
  di;
  inv(mark);
  CASE no OF
  |0: w("write piece; then check it by reading");
  |1: w("write all; then check by reading");
  |2: w("write all by address xor pattern forward");
  |3: w("write all by address xor pattern backward");
  |4: w("write  crawl piece; then check all");
  |5: w("invert crawl piece; then check all");
  |6: w("invert word at addresses differing in one bit with being checked");
  END;
  inv(FALSE);
  ei;
END test_name;

PROCEDURE write_adr(a: ADDRESS);
BEGIN
  bump:="00000000  | "; app_adr(bump,a,0); write_str(bump);
END write_adr;

PROCEDURE error0(adr: INTEGER; rd,wr: WORD);
BEGIN
  di;
  rollup;
  write_test_no; write_adr(adr);
  write_hex(wr   ," | ");
  write_hex(rd   ," | ");
  write_hex(BITSET(rd)/BITSET(wr)," |");
  ei;
END error0;

PROCEDURE error1(adr: INTEGER; rd,wr: WORD; inv: INTEGER);
BEGIN
  di;
  error0(adr,rd,wr);
  write_str(" after invert @ "); write_hex(inv,"");
  ei;
END error1;

PROCEDURE error2(adr: INTEGER; rd,wr: WORD; iLo,iHi: INTEGER);
BEGIN
  di;
  error1(adr,rd,wr,iLo);
  write_str(".."); write_hex(iHi,"");
  ei;
END error2;

PROCEDURE tester(lo,hi: INTEGER);

  PROCEDURE beg(no: INTEGER): BOOLEAN;
    VAR x: BOOLEAN;
  BEGIN x:=no IN state;
    IF x THEN testno:=no; test_name(testno,TRUE) END; RETURN x
  END beg;

  PROCEDURE end;
  BEGIN test_name(testno,FALSE); testno:=testno+1; current_address:=lo END end;

BEGIN
  IF beg(0) THEN ReadAfterWrite   (lo,hi,pat,error0);    end END;
  IF beg(1) THEN WriteAllAndRead  (lo,hi,pat,error0);    end END;
  IF beg(2) THEN FastAddresses    (lo,hi,pat,+1,error0); end END;
  IF beg(3) THEN FastAddresses    (lo,hi,pat,-1,error0); end END;
  IF beg(4) THEN CrowlWriteAndRead(lo,hi,pat,error0);    end END;
  IF beg(5) THEN Overlap          (lo,hi,pat,error2);    end END;
  IF beg(6) THEN Addresses        (lo,hi,pat,error1);    end END;
END tester;

PROCEDURE test(lo,hi: ADDRESS);
BEGIN
  setm(getm()+{1});
  pat:={0};
  REPEAT
    IF (state*{0..6}#{}) & (state*{11..13}#{}) THEN
      IF {13}*state#{} THEN tester(lo,hi)         END;
      IF {11}*state#{} THEN tester(mBusLo,mBusHi) END;
      IF {12}*state#{} THEN
         INCL(state,15); tester(mBusLo+16*KB,mBusHi); EXCL(state,15);
      END;
      pat:={0..31}/pat;
      IF ODD(cycle) THEN pat:=pat<<1 END;
      cycle:=cycle+1;
    END;
  UNTIL cycle=64;
END test;

----------------------------------------------------------------

PROCEDURE header;

  PROCEDURE blank;
    VAR i: INTEGER;
  BEGIN i:=20; REPEAT write_str("----"); i:=i-1 UNTIL i=0 END blank;

  PROCEDURE ws(VAL s: ARRAY OF CHAR);
    VAR i: INTEGER;
        b: ARRAY [0..3] OF CHAR;
  BEGIN b:=" "; i:=0;
    WHILE s[i]#0c DO
      IF s[i]="`" THEN hint(TRUE)
      ELSE b[0]:=s[i]; write_str(b); hint(FALSE);
      END; i:=i+1;
    END;
    hint(FALSE);
  END ws;

  VAR i: INTEGER;

BEGIN
  write_str("" 33c "?2;0T" );  -- cursor off
  write_str("" 33c "[H" );     -- home
  write_str("" 33c "[J" );     -- clear
  info; opts;
  FOR i:=0 TO 6 DO test_name(i,FALSE) END;
  pos(11,0); blank;
  pos(23,0); blank;
  pos(24,2);
  ws("`Wait on error  `Bell  `MultiBus  `DMA  "
     "`Local memory  `0..`6 on/off test  `^`C stop"
  );
END header;

BEGIN
  di;
  mBusLo:=BASE + (080000h DIV 4);
  mBusHi:=BASE + (0CFF00h DIV 4);
  current_address:=0; cycle:=0; testno:=0;
  state:={0..5,11,12,13,14}; err_co:=0;
  Z80FDC;
  VDU;
  header;
  try_read_track(mBusLo);
  config;
  LOOP test(4,512*KB-1) END;
END mem25tst.
