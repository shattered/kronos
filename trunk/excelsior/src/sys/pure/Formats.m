IMPLEMENTATION MODULE Formats[1]; (* Sem  07-Aug-88. (c) KRONOS *)
                                  (* Leo  03-Nov-88. (c) KRONOS *)
                                  (* Hady 28-Jul-89. (c) KRONOS *)

FROM SYSTEM     IMPORT  WORD, ADDRESS;

(*$T-*)

PROCEDURE format
    (p: WORD; write: write_proc; VAL fmt: ARRAY OF CHAR; SEQ args: WORD);

  TYPE flags  = SET OF (lj,sg,zr,bs,sp,cap,w1,w2,w3,cn);

  CONST CAP_DIG = "0123456789ABCDEF";
        SML_DIG = "0123456789abcdef";

  CONST max=127;

  VAR out        : ARRAY [0..max] OF CHAR;
      buf        : ARRAY [0..255] OF CHAR;
      bcnt,bptr  : INTEGER;
      ocnt,fcnt  : INTEGER;
      acnt       : INTEGER;
      ch         : CHAR;
      base       : CHAR;
      val        : WORD;
      wd1,wd2,wd3: INTEGER;
      flg        : flags;


  PROCEDURE Ovr;
  BEGIN
    buf:="*** print_error: more then 256 bytes for argument ***";
    bptr:=0; bcnt:=53;
  END Ovr;

  PROCEDURE appInt;
    CONST min_int = "2147483648";
    VAR i,j: INTEGER; sig: BOOLEAN;
  BEGIN
    sig:=INTEGER(val)<0; bcnt:=HIGH(buf)+1; bptr:=bcnt;
    IF NOT (w2 IN flg) THEN wd2:=1 END;
    IF bptr-wd2<1 THEN wd2:=bptr-1 END;
    IF INTEGER(val)=MIN(INTEGER) THEN
      i:=9;
      WHILE (wd2>0) OR (i>=0) DO
        DEC(bptr); buf[bptr]:=min_int[i];
        DEC(wd2);  DEC(i)
      END
    ELSE
      IF sig THEN val:=ABS(INTEGER(val)) END;
      WHILE (wd2>0) OR (INTEGER(val)#0) DO
        j:=ABS(INTEGER(val) MOD 10);
        DEC(bptr); buf[bptr]:=CHAR(j+ORD('0'));
        val:=INTEGER(val) DIV 10; DEC(wd2);
      END
    END;
    IF (zr IN flg) & (w1 IN flg) THEN j:=bcnt-wd1;
      IF j<0 THEN j:=0 END;
      IF ( flags{sp,sg}*flg # flags{} ) OR sig THEN INC(j) END;
      WHILE j<bptr DO DEC(bptr); buf[bptr]:='0' END;
    END;
    IF sp IN flg THEN
      DEC(bptr);
      IF sig THEN buf[bptr]:='-' ELSE buf[bptr]:=' ' END
    ELSIF sg IN flg THEN
      DEC(bptr);
      IF sig THEN buf[bptr]:='-' ELSE buf[bptr]:='+' END
    ELSIF sig THEN
      DEC(bptr); buf[bptr]:='-'
    END
  END appInt;

  PROCEDURE appCar(n: INTEGER);
    VAR j: INTEGER; m: BITSET;
      dig: ARRAY [0..16] OF CHAR;
  BEGIN
    IF cap IN flg THEN dig:=CAP_DIG ELSE dig:=SML_DIG END;
    m:=BITSET(INTEGER(1<<n)-1);
    bcnt:=HIGH(buf)+1; bptr:=bcnt;
    IF bs IN flg THEN DEC(bptr); buf[bptr]:=base END;
    IF NOT (w2 IN flg) THEN wd2:=1 END;
    IF bptr-wd2<1 THEN wd2:=bptr-1 END;
    WHILE (wd2>0) OR (INTEGER(val)#0) DO
      j:=INTEGER(BITSET(val)*m);
      DEC(bptr); buf[bptr]:=dig[j];
      val:=(BITSET(val)-m)>>n; DEC(wd2);
    END;
    IF (zr IN flg) & (w1 IN flg) THEN
      j:=bcnt-wd1; IF j<0 THEN j:=0 END;
      WHILE j<bptr DO DEC(bptr); buf[bptr]:='0' END;
    END;
  END appCar;

  PROCEDURE appStr;
    VAR ptr: POINTER TO ARRAY [0..0FFFFFh] OF CHAR;
          i: INTEGER;
  BEGIN
    ptr:=val; bptr:=0; bcnt:=0; i:=0;
    IF ptr=NIL THEN RETURN END;
    IF flags{w1,w2,w3}*flg=flags{} THEN
      WHILE ptr^[i]#0c DO
        IF ocnt>=max THEN out[max]:=0c; write(p,out,0,max); ocnt:=0 END;
        out[ocnt]:=ptr^[i]; INC(ocnt); INC(i);
      END;
      RETURN
    END;
    IF w3 IN flg THEN
      WHILE (ptr^[i]#0c) & (wd3>0) DO INC(i); DEC(wd3) END;
      IF ptr^[i]=0c THEN RETURN END;
    END;
    IF w2 IN flg THEN
      WHILE (bcnt<=HIGH(buf)) & (ptr^[i]#0c) & (wd2>0) DO
        buf[bcnt]:=ptr^[i]; INC(bcnt); INC(i); DEC(wd2);
      END;
    ELSE
      WHILE (bcnt<=HIGH(buf)) & (ptr^[i]#0c) DO
        buf[bcnt]:=ptr^[i]; INC(bcnt); INC(i);
      END;
    END;
  END appStr;

  PROCEDURE appChar(Ch: CHAR);
  BEGIN
    bptr:=0; bcnt:=0;
    IF w2 IN flg THEN ELSE wd2:=1 END;
    WHILE (wd2>0) & (bcnt<HIGH(buf)) DO
      buf[bcnt]:=Ch; INC(bcnt); DEC(wd2)
    END;
  END appChar;

  PROCEDURE appSet;

    VAR  i,Cou: INTEGER;
         Emp  : BOOLEAN;

    PROCEDURE appNum(n: INTEGER);
    BEGIN
      IF n DIV 10 > 0 THEN appNum(n DIV 10) END;
      buf[bcnt]:=CHAR(n MOD 10 + ORD('0')); INC(bcnt);
    END appNum;

  BEGIN
    Cou:=0;  Emp:=TRUE;   bcnt:=1; bptr:=0; buf[0]:='{';
    i:=0;
    REPEAT
      IF i IN BITSET(val) THEN
        INC(Cou)
      ELSIF Cou>0 THEN
        IF NOT Emp THEN buf[bcnt]:=','; INC(bcnt) END;
        IF Cou=1 THEN
          appNum(i-1);
        ELSE
          appNum(i-Cou);
          buf[bcnt]:='.'; INC(bcnt); buf[bcnt]:='.'; INC(bcnt);
          appNum(i-1);
        END;
        Cou:=0; Emp:=FALSE;
      END;
      i:=i+1;
    UNTIL i>32;
    buf[bcnt]:='}'; INC(bcnt);
  END appSet;

  PROCEDURE appFloatF;
    VAR i,j,e: INTEGER; sig: BOOLEAN; r: REAL;
  BEGIN
    sig:=31 IN BITSET(val); val:=BITSET(val)-{31};
    IF NOT (w1 IN flg) THEN INCL(flg,w1); wd1:=6 END;
    IF (w2 IN flg) & (wd2=0) THEN
      IF bs IN flg THEN
        bptr:=HIGH(buf); bcnt:=bptr+1; buf[bptr]:='.';
      ELSE
        bptr:=HIGH(buf)+1; bcnt:=bptr;
      END;
    ELSE
      IF NOT (w2 IN flg) THEN wd2:=3; INCL(flg,w2) END;
      bptr:=HIGH(buf)-wd2; bcnt:=HIGH(buf)+1;
      IF bptr<1 THEN Ovr; RETURN END;
      buf[bptr]:='.'; r:=REAL(val);
      IF r>1.e+9 THEN r:=0. ELSE r:=r-FLOAT(TRUNC(r)) END;
      FOR i:=1 TO wd2 DO
        r:=r*10.0; j:=TRUNC(r); ASSERT((j>=0)&(j<=9));
        buf[bptr+i]:=CHAR(ORD('0')+j);
        r:=r-FLOAT(j);
      END
    END;
    r:=REAL(val);
    IF r<1.0 THEN
      DEC(bptr); IF bptr<1 THEN Ovr; RETURN END;
      buf[bptr]:='0';
    ELSIF r>FLOAT(MAX(INTEGER)) THEN
      e:=0;
      REPEAT r:=r/10.; DEC(bptr); INC(e) UNTIL r<1.0;
      IF bptr<1 THEN Ovr; RETURN END;
      FOR i:=0 TO e-1 DO
        r:=r*10.0; j:=TRUNC(r); r:=r-FLOAT(j);
        buf[bptr+i]:=CHAR(ORD('0')+j);
      END;
    ELSE
      i:=TRUNC(r); ASSERT(i>=0);
      WHILE i#0 DO
        IF bptr<1 THEN Ovr; RETURN END;
        DEC(bptr); buf[bptr]:=CHAR(ORD('0')+(i MOD 10));
        i:=i DIV 10
      END
    END;
    IF (zr IN flg) & (w1 IN flg) THEN
      j:=bcnt-wd1;
      IF j<1 THEN j:=1 END;
      WHILE j<bptr DO DEC(bptr); buf[bptr]:='0' END;
    END;
    ASSERT(bptr>=1); -- оставлено место для знака
    IF sg IN flg THEN
      DEC(bptr);
      IF sig THEN buf[bptr]:='-' ELSE buf[bptr]:='+' END;
    ELSIF sp IN flg THEN
      DEC(bptr);
      IF sig THEN buf[bptr]:='-' ELSE buf[bptr]:=' ' END;
    ELSIF sig THEN
      DEC(bptr);  buf[bptr]:='-';
    END;
  END appFloatF;

  PROCEDURE appFloatE;
    VAR sig: BOOLEAN; r: REAL; i,j,e: INTEGER;
  BEGIN
    sig:=31 IN BITSET(val); val:=BITSET(val)-{31};
    bptr:=0; bcnt:=0;
    IF NOT (w1 IN flg) THEN INCL(flg,w1); wd1:=10 END;
    IF sg IN flg THEN
      IF sig THEN buf[bcnt]:='-' ELSE buf[bcnt]:='+' END; INC(bcnt);
    ELSIF sp IN flg THEN
      IF sig THEN buf[bcnt]:='-' ELSE buf[bcnt]:=' ' END; INC(bcnt);
    ELSIF sig THEN
      buf[bcnt]:='-'; INC(bcnt);
    END;
    r:=REAL(val); e:=0;
    WHILE r>=10. DO INC(e); r:=r/10. END;
    WHILE (r<1.0) & (e>-50) DO DEC(e); r:=r*10. END;
    IF e<=-50 THEN e:=0; r:=0. END;
    i:=TRUNC(r); r:=r-FLOAT(i);
    buf[bcnt]:=CHAR(ORD('0')+i); INC(bcnt);
    IF (w2 IN flg) & (wd2=0) THEN
      IF bs IN flg THEN buf[bcnt]:='.'; INC(bcnt) END;
    ELSE
      IF NOT (w2 IN flg) THEN wd2:=3; INCL(flg,w2) END;
      IF wd2>(HIGH(buf)-bcnt-8) THEN wd2:=HIGH(buf)-bcnt-8 END;
      buf[bcnt]:='.'; INC(bcnt);
      FOR i:=1 TO wd2 DO
        r:=r*10.0; j:=TRUNC(r); r:=r-FLOAT(j);
        ASSERT((j>=0)&(j<=9)); buf[bcnt]:=CHAR(ORD('0')+j); INC(bcnt);
      END;
    END;
    IF cap IN flg THEN buf[bcnt]:='E' ELSE buf[bcnt]:='e' END; INC(bcnt);
    IF e<0 THEN buf[bcnt]:='-'; e:=ABS(e) ELSE buf[bcnt]:='+' END; INC(bcnt);
    buf[bcnt]:=CHAR(ORD('0')+(e DIV 10)); INC(bcnt);
    buf[bcnt]:=CHAR(ORD('0')+(e MOD 10)); INC(bcnt);
    ASSERT(bcnt<=HIGH(buf)+1);
  END appFloatE;

  PROCEDURE appFloatG;
    VAR r: REAL; e: INTEGER;
  BEGIN
    r:=ABS(REAL(val)); e:=0;
    IF r#0.0 THEN
      WHILE r>=10. DO INC(e); r:=r/10. END;
      WHILE (r<1.0) & (e>-40) DO DEC(e); r:=r*10. END;
    END;
    IF NOT (w2 IN flg) THEN wd2:=3; INCL(flg,w2) END;
    IF (e<-4) OR (e>=wd2) THEN
      DEC(wd2); appFloatE;
    ELSE
      wd2:=wd2-e-1; appFloatF;
      IF NOT (bs IN flg) & (wd2#0) THEN
        WHILE buf[bcnt-1]='0' DO DEC(bcnt) END;
        IF buf[bcnt-1]='.' THEN DEC(bcnt) END;
      END;
    END;
  END appFloatG;

  PROCEDURE Next;
  BEGIN
    IF fcnt<=HIGH(fmt) THEN ch:=fmt[fcnt]; INC(fcnt) ELSE ch:=0c END;
  END Next;

  PROCEDURE SubFormat(): BOOLEAN;

    PROCEDURE width(n: INTEGER);
    BEGIN
      IF    w3 IN flg THEN wd3:=n
      ELSIF w2 IN flg THEN wd2:=n
      ELSE INCL(flg,w1);   wd1:=n
      END;
    END width;

    PROCEDURE scan_format;
      VAR n: INTEGER; done: BOOLEAN;
    BEGIN
      flg :=flags{};
      done:=FALSE;      wd1:=-1; wd2:=-1; wd3:=-1;
      REPEAT n:=-1;
        CASE ch OF
        |'#': INCL(flg,bs);            |'+': INCL(flg,sg);
        |'$': INCL(flg,zr);            |' ': INCL(flg,sp);
        |'-': INCL(flg,lj);
        |'.': IF w2 IN flg THEN INCL(flg,w3)
              ELSE INCL(flg,w2);
              END;
        |'|': INCL(flg,cn);
        |'*': IF acnt<=HIGH(args) THEN width(args[acnt]); INC(acnt) END;
        |'0'..'9':
              n:=ORD(ch)-ORD('0'); Next;
              IF (n=0) & ('0'<=ch) & (ch<='9') & NOT (w2 IN flg) THEN
                INCL(flg,zr)
              END;
              WHILE ('0'<=ch) & (ch<='9') DO
                n:=n*10+ORD(ch)-ORD('0'); Next
              END;
              width(n); fcnt:=fcnt-1;
        ELSE  base:=ch; done:=TRUE
        END;
        IF fcnt<=HIGH(fmt) THEN ch:=fmt[fcnt]; INC(fcnt) ELSE ch:=0c END
      UNTIL done;
      IF wd1<0 THEN EXCL(flg,w1); wd1:=0 END;
      IF wd2<0 THEN EXCL(flg,w2); wd2:=0 END;
      IF wd3<0 THEN EXCL(flg,w3); wd3:=0 END;
    END scan_format;

    VAR spaces: INTEGER;

    PROCEDURE left_justify;
    BEGIN
      WHILE bptr<bcnt DO
        IF ocnt>=max THEN out[max]:=0c; write(p,out,0,max); ocnt:=0 END;
        out[ocnt]:=buf[bptr]; INC(ocnt); INC(bptr);
      END;
      WHILE spaces>0 DO
        IF ocnt>=max THEN out[max]:=0c; write(p,out,0,max); ocnt:=0 END;
        out[ocnt]:=' '; INC(ocnt); DEC(spaces)
      END;
    END left_justify;

    PROCEDURE right_justify;
    BEGIN
      WHILE spaces>0 DO
        IF ocnt>=max THEN out[max]:=0c; write(p,out,0,max); ocnt:=0 END;
        out[ocnt]:=' '; INC(ocnt); DEC(spaces);
      END;
      WHILE bptr<bcnt DO
        IF ocnt>=max THEN out[max]:=0c; write(p,out,0,max); ocnt:=0 END;
        out[ocnt]:=buf[bptr]; INC(ocnt); INC(bptr);
      END;
    END right_justify;

    PROCEDURE center;
      VAR tail: INTEGER;
    BEGIN
      tail:=spaces DIV 2; spaces:=spaces-tail;
      WHILE spaces>0 DO
        IF ocnt>=max THEN out[max]:=0c; write(p,out,0,max); ocnt:=0 END;
        out[ocnt]:=' '; INC(ocnt); DEC(spaces);
      END;
      WHILE bptr<bcnt DO
        IF ocnt>=max THEN out[max]:=0c; write(p,out,0,max); ocnt:=0 END;
        out[ocnt]:=buf[bptr]; INC(ocnt); INC(bptr);
      END;
      WHILE tail>0 DO
        IF ocnt>=max THEN out[max]:=0c; write(p,out,0,max); ocnt:=0 END;
        out[ocnt]:=' '; INC(ocnt); DEC(tail);
      END;
    END center;

  BEGIN
    scan_format;
    (* Take argument *)
    IF acnt<=HIGH(args) THEN
      val:=args[acnt]; INC(acnt)
    ELSE (* misstake *)
      RETURN TRUE
    END;
    IF (base>='A') & (base<='Z') THEN
      base:=CHAR(ORD(base)+40b)
    ELSE
      INCL(flg,cap)
    END;
    CASE base OF
     |'d': appInt;      |'h': appCar(4);
     |'f': appFloatF;   |'x': appCar(4);
     |'e': appFloatE;   |'b': appCar(3);
     |'g': appFloatG;   |'o': appCar(3);
     |'i': appCar(1);
     |'s': appStr;
     |'c': appChar(CHAR(val)); -- bcnt:=1; bptr:=0; buf[0]:=CHAR(val);
     |'{': IF ch='}' THEN Next; appSet ELSE DEC(acnt); RETURN TRUE END;
    ELSE (* illegal base, unget argument *)
      DEC(acnt); RETURN TRUE;
    END;
    ASSERT(bcnt<=HIGH(buf)+1);
    ASSERT(bptr>=0);
    IF    w1 IN flg THEN spaces:=wd1-bcnt+bptr ELSE spaces:=0 END;
    IF    lj IN flg THEN left_justify
    ELSIF cn IN flg THEN center
    ELSE                 right_justify
    END;
    RETURN FALSE;
  END SubFormat;

  PROCEDURE Format(): BOOLEAN;
    VAR f,a: INTEGER;
  BEGIN
    f:=fcnt; a:=acnt;
    IF SubFormat() THEN fcnt:=f; acnt:=a; ch:=fmt[fcnt-1]; RETURN TRUE END;
    RETURN FALSE;
  END Format;

BEGIN
  IF HIGH(fmt)<=0 THEN RETURN END;
  ch:=fmt[0]; fcnt:=1; ocnt:=0; acnt:=0;
  REPEAT
    IF ch='\' THEN
      (*Next:*)
      IF fcnt<=HIGH(fmt) THEN ch:=fmt[fcnt]; INC(fcnt) ELSE ch:=0c END;
      IF ocnt>=max-1 THEN out[ocnt]:=0c; write(p,out,0,ocnt); ocnt:=0 END;
      IF     ch='n' THEN ch:=36c;
      ELSIF  ch='r' THEN ch:=15c;
      ELSIF  ch='l' THEN ch:=12c;
      ELSIF  ch='e' THEN ch:=33c;
      ELSIF  ch='g' THEN ch:=07c;
      ELSIF  ch='\' THEN ch:='\';
      ELSIF  ch=0c  THEN ch:='\';
      ELSE
        out[ocnt]:='\'; INC(ocnt);
      END;
      out[ocnt]:=ch; INC(ocnt);
      (*Next:*)
      IF fcnt<=HIGH(fmt) THEN ch:=fmt[fcnt]; INC(fcnt) ELSE ch:=0c END;
    ELSIF ch='%' THEN
      (*Next:*)
      IF fcnt<=HIGH(fmt) THEN ch:=fmt[fcnt]; INC(fcnt) ELSE ch:=0c END;
      IF ocnt>=max THEN out[max]:=0c; write(p,out,0,max); ocnt:=0 END;
      IF (ch=0c) OR (ch='%') THEN
        out[ocnt]:='%'; INC(ocnt); Next;
      ELSIF Format() THEN
        out[ocnt]:='%'; INC(ocnt);
      END;
    ELSE
      IF ocnt>=max THEN out[max]:=0c; write(p,out,0,max); ocnt:=0 END;
      out[ocnt]:=ch; INC(ocnt);
      IF fcnt<=HIGH(fmt) THEN ch:=fmt[fcnt]; INC(fcnt) ELSE ch:=0c END;
    END;
  UNTIL ch=0c;
  out[ocnt]:=0c; write(p,out,0,ocnt);
END format;

END Formats.
