IMPLEMENTATION MODULE ForIO; (* by Nsv, 17-Mar-89.  *)

FROM SYSTEM     IMPORT  ADDRESS, WORD, ADR;
FROM Edit       IMPORT  ReadString;
FROM BIO        IMPORT  CD,UnLink;
FROM FsPublic   IMPORT  FileName;
FROM Streams    IMPORT  Stream, Seek, Close, Open, Create, Read, Write,
                        PutS, GetS, GetC, Why?, File?;
FROM Heap       IMPORT  ALLOCATE, DEALLOCATE;
FROM Image      IMPORT  image0, image, PeekReal, PeekNum;
FROM ASCII      IMPORT  NL,SPACE;
FROM Strings    IMPORT  App, AppStr, Len, Str0, Str2;

FROM StdIO      IMPORT  Show, print, WriteLn;
--FROM Resource   IMPORT  Final;

TYPE
     formatEn   = (nothing,stPos,brLeft,brRight,slash,Pa,
                   eofForm,
                   Iw,Iwm, Fwd,Ewd,Ewde,Dwd,Gwd,Gwde,Lw,A0,Aw,
                   colon,
                   Hw,Tc,TLs,TRs,Xw,S0,SP,SS,BN,BZ);

     formatSet = SET OF formatEn;

(*   EnStatus = (old,new,scratch,unknown,keep,delete,
                 formatted,unformatted,direct,seq,null,zero,
         internal,freeformat,read,write,const,text,int,real,logic,slsh,nul,
         sysIO, open, close, rewind, backspace, endfile);
*)
     StatusSet = SET OF EnStatus;

     fList =
       RECORD
         Fn:            ADDRESS;  -- ADR(sDescriptor);
         STATE:         StatusSet;
         Iostat:        ADDRESS;
         Recl:          INTEGER;     -- record size
         NextRec:       INTEGER;
         STream:        Stream;
       END;

     fpList = POINTER TO fList;

     sDescriptor = RECORD sAd: ADDRESS; lo,ln: INTEGER END;

CONST
      OLD="OLD"; NEW="NEW"; SCRATCH="SCRATCH"; UNKNOWN="UNKNOWN";
      KEEP="KEEP"; DELETE="DELETE"; FORMATTED="FORMATTED";
      UNFORMATTED="UNFORMATTED"; DIRECT="DIRECT"; SEQU="SEQUENTIAL";
      NUL="NULL"; ZERO="ZERO";
--      blank=" ";
(*
VAR
  exchPar:    RECORD
                ioStat: ADDRESS;
                file:   ADDRESS; -- ADR(descriptor)
                status: ADDRESS; --EnStatus;
                access: ADDRESS; --EnStatus;
                form:   ADDRESS; --EnStatus;
                blank:  ADDRESS; --EnStatus;
                recl:   INTEGER;
              END;
*)

VAR
  frInx    : INTEGER;  -- carry format index
  beforVal : BOOLEAN;
  strInx,lastInx: INTEGER;
  symb     : formatEn;
  cState   : StatusSet;
  cRec     : ARRAY [0..1023] OF CHAR;
  cnt      : INTEGER;  -- format repeat counter
  cLen     : INTEGER;
  carryList: fpList;
  units    : ARRAY [0..100] OF fpList;
  fName,cName: FileName;
  formatAdr: POINTER TO ARRAY [0..1023] OF CHAR;
  myDesc   : sDescriptor;
  formBeg  : INTEGER;  -- carry start format index
  iRec     : ARRAY [0..1023] OF CHAR;
  cErr     : INTEGER;
  constA   : ARRAY [0..255] OF CHAR;
  constW   : WORD;
  stackInx : INTEGER;
  stack    : ARRAY [0..31] OF INTEGER;
  sign     : CHAR;
  IfdAdr   : ADDRESS;

PROCEDURE Min(x,y: INTEGER): INTEGER;
BEGIN  IF x<y THEN RETURN x ELSE RETURN y END;
END Min;

PROCEDURE strDesc(dAdr: ADDRESS; VAR aa: ADDRESS; VAR fr,len: INTEGER);
  VAR ss: POINTER TO sDescriptor;
BEGIN ss:=dAdr;
  fr:=ss^.lo; len:=ss^.ln; aa:=ss^.sAd + (fr DIV 4);
  fr:=fr MOD 4;
END strDesc;

PROCEDURE putStr(da: ADDRESS; VAR s: ARRAY OF CHAR);
VAR i: INTEGER;  f,l: INTEGER; a: POINTER TO ARRAY [0..255] OF CHAR;
BEGIN
  strDesc(da,a,f,l); i:=0;
  WHILE (i<l)&(i<=HIGH(s))&(s[i]#0c)  DO a^[f+i]:=s[i]; INC(i) END;
END putStr;

PROCEDURE getStr(da: ADDRESS; VAR s: ARRAY OF CHAR);
VAR i: INTEGER;  f,l: INTEGER; a: POINTER TO ARRAY [0..0] OF CHAR;
BEGIN
  strDesc(da,a,f,l); i:=0;
  WHILE (i<l)&(a^[f]<=" ") DO INC(f); INC(i) END; DEC(l,i);
  i:=0;
(*$T-*);
  WHILE (i<l)&(i<HIGH(s))&(a^[f+i]>" ") DO s[i]:=a^[f+i]; INC(i) END;
(*$T+*);
  s[i]:=0c; Str2(s);
END getStr;

PROCEDURE getStr1(da: ADDRESS; VAR s: ARRAY OF CHAR);
VAR i: INTEGER;  f,l: INTEGER; a: POINTER TO ARRAY [0..0] OF CHAR;
BEGIN
  strDesc(da,a,f,l); i:=0;
(*$T-*);
  WHILE (i<l)&(i<HIGH(s)) DO s[i]:=a^[f+i]; INC(i) END;
(*$T+*);
  s[i]:=0c; Str2(s);
END getStr1;

PROCEDURE ClearRec;
BEGIN
  strInx:=0; lastInx:=0;
END ClearRec;

PROCEDURE addNumber(w:INTEGER);
  VAR i,lp,ln: INTEGER;
BEGIN
  ln:=Len(constA);
  lp:=strInx+w; IF lp>=cLen THEN cErr:=9; RETURN END;
  IF ln>w THEN cErr:=11; i:=strInx;
    WHILE w>0 DO cRec[i]:="*"; INC(i); DEC(w); END;
  ELSE DEC(w,ln);
    i:=lp-1; WHILE ln>0 DO DEC(ln); cRec[i]:=constA[ln]; DEC(i); END;
    WHILE w>0 DO cRec[i]:=" "; DEC(i); DEC(w); END;
  END;
  strInx:=lp; IF lastInx<strInx THEN lastInx:=strInx END;
END addNumber;

PROCEDURE addStr(w:INTEGER);
  VAR i,lp,ln: INTEGER;
BEGIN
  ln:=Len(constA);
  lp:=strInx+w; IF lp>=cLen THEN cErr:=9; RETURN END;
  IF ln>w THEN ln:=w; END;
  DEC(w,ln);
  i:=lp-1; WHILE ln>0 DO DEC(ln); cRec[i]:=constA[ln]; DEC(i); END;
  WHILE w>0 DO cRec[i]:=" "; DEC(i); DEC(w); END;
  strInx:=lp; IF lastInx<strInx THEN lastInx:=strInx END;
END addStr;

PROCEDURE movePnt(pos:INTEGER; rel:BOOLEAN);
  VAR i: INTEGER;
BEGIN
  IF rel THEN
    IF pos>0 THEN INC(strInx,pos) ELSE DEC(strInx,-pos) END;
  ELSE strInx:=pos;
  END;
  IF (strInx<0) OR (strInx>cLen) THEN cErr:=9;
  ELSE
    IF lastInx<strInx THEN i:=lastInx; lastInx:=strInx;
       WHILE i<lastInx DO cRec[i]:=" "; INC(i); END;
    END;
  END;
END movePnt;

PROCEDURE CreateFileName(chan:INTEGER):INTEGER;
  VAR d:INTEGER;
BEGIN
  IF exchPar.file#NIL THEN RETURN 0 END;
  IF (chan<0) OR (chan>99) THEN RETURN 1 END;
  d:=chan DIV 10;
  fName[3]:=CHAR(ORD('0')+d);
  d:=chan MOD 10;
  fName[4]:=CHAR(ORD('0')+d);
  exchPar.file:=ADR(myDesc);
  RETURN 0;
END CreateFileName;

PROCEDURE initVar;
BEGIN
  WITH exchPar  DO
    ioStat:=NIL; file:=NIL; status:=NIL; access:= NIL;
    form:=NIL;   recl:=132; blank:=NIL;
  END;
END initVar;

PROCEDURE gM(e: EnStatus; ad: ADDRESS): BOOLEAN;
VAR i: INTEGER; s: ARRAY [0..255] OF CHAR; r: BOOLEAN;
BEGIN
  getStr(ad,s);
  CASE e  OF
     old: r:=(s=OLD) | new: r:=(s=NEW) | scratch: r:=(s=SCRATCH)
   | unknown: r:=(s=UNKNOWN) | keep: r:=(s=KEEP) | delete: r:=(s=DELETE)
   | formatted: r:=(s=FORMATTED) | unformatted: r:=(s=UNFORMATTED)
   | direct: r:=(s=DIRECT) | seq: r:=(s=SEQU) | null: r:=(s=NUL)
   | zero: r:=(s=ZERO)
  ELSE r:=FALSE;
  END;
  RETURN r
END gM;

PROCEDURE InitChannel(chan: INTEGER): BOOLEAN;
VAR ei,ej: EnStatus;  r: BOOLEAN;
BEGIN
  WITH units[chan]^  DO
  WITH exchPar       DO
    Iostat:=ioStat;
    Recl:=recl; Fn:=file;
    STATE:=StatusSet{};
    IF status#NIL THEN  r:=FALSE; ei:=old;
      WHILE (NOT r)&(ei<=unknown) DO r:=gM(ei,status); INC(ei) END; DEC(ei);
      IF NOT r THEN RETURN FALSE ELSE INCL(STATE,ei) END;
    ELSE INCL(STATE,unknown);
    END;
    ei:=seq;
    IF access#NIL THEN
      IF gM(seq,access) THEN ei:=seq;
      ELSIF gM(direct,access) THEN ei:=direct ELSE RETURN FALSE END;
    END;
    INCL(STATE,ei);
    IF direct IN STATE THEN ei:=unformatted;
    ELSE ei:=formatted;
    END;
    IF form#NIL THEN
      IF gM(formatted,form) THEN ei:=formatted;
      ELSIF gM(unformatted,form) THEN ei:=unformatted ELSE RETURN FALSE END;
    END;
    INCL(STATE,ei);
    ei:=null;
    IF blank#NIL THEN
      IF gM(null,blank) THEN ei:=null;
      ELSIF gM(zero,blank) THEN ei:=zero ELSE RETURN FALSE END;
    END;
    INCL(STATE,ei);
    NextRec:=1;
  END
  END; RETURN TRUE
END InitChannel;

PROCEDURE EquStr(d1,d2: ADDRESS): BOOLEAN;
  VAR  s1,s2: ARRAY [0..255] OF CHAR;
BEGIN
  getStr(d1,s1); getStr(d2,s2);
  RETURN (s1=s2)
END EquStr;

PROCEDURE ForOpen(chan: INTEGER): INTEGER;
  VAR r: BOOLEAN;  st: Stream; ss: ARRAY [0..15] OF CHAR;
      i: INTEGER; f: fpList; er: INTEGER;
BEGIN f:=units[100]; cState:=StatusSet{open};
  er:=CreateFileName(chan);
  IF er#0 THEN RETURN er END;
LOOP
  IF NOT InitChannel(100) THEN er:=2; EXIT END;
  IF units[chan]#NIL THEN
    WITH units[chan]^ DO
     IF  EquStr(Fn,f^.Fn) THEN
           --except blank all mast be the same
       IF ((STATE/f^.STATE)*StatusSet{old..seq}#StatusSet{})
                      OR(f^.Recl#Recl) THEN er:=3;
       END;
       EXIT
     ELSE  i:=Close(STream); -- r:=(i>=0);
     END;
    END
  ELSE ALLOCATE(units[chan],SIZE(fList));
  END;
  units[chan]^:=f^;
  getStr(units[chan]^.Fn,cName);
  IF scratch IN f^.STATE THEN
     ss[0]:=0c; st:=Create(ss);   -- tmp file, no BIO.link
  ELSIF old IN f^.STATE THEN  st:=Open(cName);
  ELSIF new IN f^.STATE THEN
    st:=Open(cName); -- if exist then error
    IF st>=0 THEN er:=4; st:=Close(st);
    ELSE st:=Create(cName)
    END;
  ELSE  st:=Open(cName);
        IF st<0 THEN st:=Create(cName) END;
  END;
  IF (st>=0) AND (er=0) THEN units[chan]^.STream:=st; --st:=0;
  ELSE DEALLOCATE(units[chan],SIZE(fList)); -- units[chan]:=NIL;
       er:=ABS(st)*200h+er;
  END; EXIT
END; -- LOOP
  IF (exchPar.ioStat#NIL) THEN exchPar.ioStat^:=er END;
  initVar;
  RETURN er;
END ForOpen;

PROCEDURE ForClose(chan: INTEGER; iostat: ADDRESS): INTEGER;
VAR er,res: INTEGER; -- en: EnStatus;
BEGIN
  cState:=StatusSet{close};
  IF (chan<0) OR (chan>99) THEN er:=1;
  ELSE er:=0;
    IF units[chan]#NIL THEN
      IF exchPar.status#NIL THEN
        IF gM(delete,exchPar.status) THEN
          res:=INTEGER(UnLink(CD(),cName));
        ELSE getStr(units[chan]^.Fn,cName);
          res:=ABS(Close(units[chan]^.STream)); -- keep
        END;
      ELSE getStr(units[chan]^.Fn,cName);
        res:=ABS(Close(units[chan]^.STream)); -- keep
      END;
      DEALLOCATE(units[chan],SIZE(fList));
    ELSE er:=5;
    END;
  END;
  IF (res>0) OR (er#0) THEN  er:=ABS(res)*200h+er END;
  IF (iostat#NIL) THEN iostat^:=er END;
  initVar;
  RETURN er;
END ForClose;

PROCEDURE ForRewind(chan: INTEGER; iostat: ADDRESS): INTEGER;
VAR er,res: INTEGER;
BEGIN  res:=0;
  cState:=StatusSet{rewind};
  IF (chan<0) OR (chan>99) THEN er:=1;
  ELSIF units[chan]=NIL THEN  er:=5
  ELSE er:=0;
    WITH  units[chan]^    DO
      res:=Seek(STream,0,0);
      IF res>=0 THEN NextRec:=1; res:=0 END;
    END;
  END;
  er:=ABS(res)*200h+er;
  IF (iostat#NIL) THEN iostat^:=er END;
  RETURN er
END ForRewind;

PROCEDURE ForBackspace(chan: INTEGER; iostat: ADDRESS): INTEGER;
VAR er,res: INTEGER; ch: CHAR;   len: INTEGER;
BEGIN res:=0;
  cState:=StatusSet{backspace};
  IF (chan<0) OR (chan>99) THEN er:=1;
  ELSIF units[chan]=NIL THEN  er:=5;
  ELSE  er:=0;
  WITH  units[chan]^    DO
    IF (formatted IN STATE) THEN
      res:=Seek(STream,-2,1); ch:=GetC(STream);
      WHILE (ch#NL) AND ((res>0) AND (ch#0c))   DO
        res:=Seek(STream,-2,1); ch:=GetC(STream);
      END;
      IF res=0 THEN res:=Seek(STream,-1,1); END;
    ELSE   --  unformatted
      res:=Seek(STream,-2,1); -- 2 bytes first & 2 last means len record
      len:=0;
      res:=Read(STream,ADR(len),2);
      res:=Seek(STream,-len,1);
    END;
    IF direct IN STATE THEN er:=6 END;
  END;
  END;
  IF res>0 THEN res:=0; END;
  er:=ABS(res)*200h+er;
  IF (iostat#NIL) THEN iostat^:=er END;
  RETURN er
END ForBackspace;

PROCEDURE EndFile(chan: INTEGER; iostat: ADDRESS): INTEGER;
VAR er,res: INTEGER;
BEGIN res:=0;
  cState:=StatusSet{endfile};
  IF (chan<0) OR (chan>99) THEN er:=1;
  ELSIF units[chan]=NIL THEN  er:=5;
  ELSE er:=0;
    res:=Seek(units[chan]^.STream,0,1);  -- res = pos end of stream
    IF res>0 THEN
--    SetEof(File?(units[chan]^.STream),res);
--    IF Cut(File?(units[chan]^.STream))THEN  res:=-3; END;
    END;
  END;
  IF res>0 THEN res:=0; END;
  er:=ABS(res)*200h+er;
  IF (iostat#NIL) THEN iostat^:=er END;
  RETURN er
END EndFile;

PROCEDURE readCarry; FORWARD;
-- PROCEDURE PeekLogic(j: INTEGER; VAR b: BOOLEAN): INTEGER; FORWARD;
PROCEDURE writeCarry; FORWARD;

(*$B-*)
(*$R-*)
PROCEDURE getFormat(): formatEn;
BEGIN
  INC(frInx);
  RETURN  formatEn(formatAdr^[frInx-1])
END getFormat;

PROCEDURE putStack(ix,cn: INTEGER);
BEGIN
  stack[stackInx] := ix; INC(stackInx); stack[stackInx] := cn; INC(stackInx)
END putStack;

PROCEDURE getStack(VAR ix,cn: INTEGER);
BEGIN
  IF stackInx#0 THEN
    DEC(stackInx); cn:=stack[stackInx]; DEC(stackInx); ix:=stack[stackInx];
  END;
END getStack;

PROCEDURE getSymb();
VAR i: INTEGER;
BEGIN
  IF NOT beforVal THEN RETURN END;
  IF cnt=0 THEN symb:=getFormat();
     IF 7 IN BITSET(symb) THEN symb:=formatEn(BITSET(symb)-{7});
        cnt:=INTEGER(getFormat()); DEC(cnt);
     END;
     IF symb=brLeft THEN putStack(frInx,cnt); cnt:=0;  getSymb(); RETURN
     ELSIF symb=brRight THEN getStack(i,cnt);
       IF cnt#0 THEN frInx:=i; DEC(cnt); putStack(frInx,cnt); cnt:=0; END;
       getSymb(); RETURN
     END;
  ELSE DEC(cnt);
  END;
END getSymb;
(*$B+*)
(*$R+*)

PROCEDURE PeekLogic(VAR b: BOOLEAN; j,w: INTEGER): INTEGER;
VAR  rf,rt: BOOLEAN;  i: INTEGER; c,ch: CHAR;
BEGIN
  IF w>0 THEN
    i:=cLen-strInx;
    IF i>=w THEN i:=strInx+w; ELSE i:=cLen END;
    ch:=cRec[i]; cRec[i]:=0c;
  END; c:=cRec[j];
  WHILE  (c=" ") DO INC(j); c:=cRec[j]; END;
  IF c=0c THEN cErr:=11;
  ELSE
    IF    (c="T") OR (c=".")&(cRec[j+1]="T")
       OR (c="t") OR (c=".")&(cRec[j+1]="t") THEN b:=TRUE;
    ELSIF (c="F") OR (c=".")&(cRec[j+1]="F")
       OR (c="f") OR (c=".")&(cRec[j+1]="f") THEN b:=FALSE;
    ELSE cErr:=11;
    END;
  END;
  IF w>0 THEN cRec[i]:=ch; j:=i;
  ELSIF cErr#0 THEN
  ELSE
    WHILE  (cRec[j]#" ")&(j<cLen)&(cRec[j]#",") DO INC(j) END;
  END;
  RETURN j
END PeekLogic;

PROCEDURE getNumber(VAR val:WORD; w:INTEGER; int:BOOLEAN);
  VAR i,k: INTEGER; ch: CHAR;
BEGIN
  IF w<=0 THEN
    IF int THEN k:=PeekNum (cRec,strInx,val);
    ELSE        k:=PeekReal(cRec,strInx,val);
    END;
  ELSE
    i:=cLen-strInx;
    IF i>=w THEN i:=strInx+w; ELSE i:=cLen END;
    ch:=cRec[i]; cRec[i]:=0c;
    IF int THEN k:=PeekNum (cRec,strInx,val);
    ELSE        k:=PeekReal(cRec,strInx,val);
    END;
    cRec[i]:=ch;
  END;
  IF k>=0 THEN strInx:=k;
  ELSE cErr:=11;
  END;
END getNumber;

PROCEDURE getVal(val: WORD): BOOLEAN;
 VAR w,d,j,k,i: INTEGER; r,rf,rt: BOOLEAN; old: INTEGER; ad: ADDRESS;
    ps: POINTER TO ARRAY [0..4095] OF CHAR; ww: WORD; a: ADDRESS;
BEGIN  r:=TRUE;
  getSymb();
  IF beforVal & (symb IN formatSet{eofForm..colon}) THEN
     beforVal:=FALSE; RETURN BOOLEAN(100h)
  END;
  old:=frInx;
  CASE symb  OF
        stPos:       formBeg:=frInx
     |  eofForm:     frInx:=formBeg; readCarry;
                     beforVal:=TRUE; r:=getVal(val); r:=getVal(val);

     |  slash:       readCarry
     |  colon:       beforVal:=TRUE; r:=getVal(val); r:=getVal(val);

     |  Iw:          w:=INTEGER(getFormat());
                     getNumber(ww,w,TRUE);
                     a:=val; a^:=ww;
     |  Pa:       -- ???? -- потом
     |  Iwm,Fwd,Ewd,Gwd,Dwd:
                     w:=INTEGER(getFormat());
                     d:=INTEGER(getFormat());
                     IF symb=Iwm THEN rt:=TRUE; ELSE rt:=FALSE; END;
                     getNumber(ww,w,rt);
                     a:=val;
                     a^:=ww;

     | Ewde,Gwde:    w:=INTEGER(getFormat());
                     d:=INTEGER(getFormat());
                     k:=INTEGER(getFormat()); --e
                     getNumber(ww,w,FALSE);
                     a:=val;
                     a^:=ww;

     | Lw:           w:=INTEGER(getFormat()); --w
                     strInx:=PeekLogic(r,strInx,w);
                     a:=val;
                     a^:=WORD(r);

     | Hw:           w:=INTEGER(getFormat()); INC(strInx,w);
                     WHILE w>0 DO k:=INTEGER(getFormat()); END;

     | A0,Aw:        strDesc(val,ps,i,d);
                     IF symb=Aw THEN w:=INTEGER(getFormat());
                     ELSE w:=d;
                     END;
                     IF strInx+w>cLen THEN w:=cLen-strInx; END;
                     j:=Min(d,w);
                     FOR k:=0 TO j-1 DO ps^[i+k]:=cRec[strInx+k] END;
                     IF d>w THEN k:=j;
                       WHILE k<d DO  ps^[i+k]:=" "; INC(k) END;
                     END;
                     INC(strInx,w);

     | Tc:           strInx:=INTEGER(getFormat())-1;
                     IF (strInx<0) THEN strInx:=0
                     ELSIF (strInx>cLen) THEN strInx:=cLen;
                     END;
     | TLs:          i:=INTEGER(getFormat());
                     DEC(strInx,i);
                     IF (strInx<0) THEN strInx:=0 END;
     | TRs,Xw:       i:=INTEGER(getFormat());
                     INC(strInx,i);
                     IF (strInx>cLen) THEN strInx:=cLen; END;

     | S0,SP,SS:
     | BN,BZ:        -- next time  to do
  ELSE  cErr:=12;
  END;
  IF cnt>0 THEN frInx:=old END;
  IF (NOT (symb IN formatSet{eofForm..colon}))
      AND (cErr=0) THEN r:=getVal(val) END;
  RETURN cErr=0;
END getVal;

PROCEDURE putVal(val: WORD): BOOLEAN;
 VAR w,d,k,m,i,j: INTEGER; r,rf,rt: BOOLEAN; old: INTEGER; rl: REAL;
        str: ARRAY [0..31] OF CHAR; ch: CHAR;
        ps: POINTER TO ARRAY [0..4095] OF CHAR;
BEGIN -- r:=TRUE;
  getSymb();
  IF beforVal & (symb IN formatSet{eofForm..colon}) THEN
     beforVal:=FALSE; RETURN BOOLEAN(100h)
  END;
  old:=frInx;
  CASE symb  OF
        stPos:       formBeg:=frInx;
     |  eofForm:     frInx:=formBeg; writeCarry;
                     beforVal:=TRUE; r:=putVal(val); r:=putVal(val);
     |  slash:       writeCarry;
     |  colon:       beforVal:=TRUE; r:=putVal(val); r:=putVal(val);

     |  Iw:          w:=INTEGER(getFormat()); --w
                     image0(str,"%%%c%dd",sign,w);
                     image0(constA,str,val); addNumber(w);

     |  Pa:       -- ???? -- потом
     |  Iwm:         w:=INTEGER(getFormat()); m:=INTEGER(getFormat());
                     Str0(constA);
                     FOR k:=0 TO w-m-1 DO App(constA," ") END;
                     image0(str,"%%$%dd",m);
                     image(constA,str,val); addNumber(w);

     |  Fwd,Ewd,Dwd:
                     IF symb=Fwd THEN ch:="f" ELSE ch:="e" END;
                     w:=INTEGER(getFormat()); --w
                     d:=INTEGER(getFormat()); --d
                     image0(str,"%%%c%d.%d%c",sign,w,d,ch);
                     image0(constA,str,val); addNumber(w);

     |  Gwd:
              w:=INTEGER(getFormat()); --w
              d:=INTEGER(getFormat()); --d
              rl:=0.1; i:=0;
              WHILE (REAL(val)>=rl)&(i<d)  DO rl:=rl*10.; INC(i) END;
              IF (i#0)&(i<d) THEN image0(str,"%%%c%d.%df",sign,w-4,d-i-1);
                  AppStr(str,"    ");
              ELSE image0(str,"%%%c%d.%de",sign,w,d)
              END;
              image0(constA,str,val); addNumber(w);

     | Ewde,Gwde:    -- ????
                     w:=INTEGER(getFormat()); --w
                     d:=INTEGER(getFormat()); --d
                     k:=INTEGER(getFormat()); --e
                     ch:="e";
                     image0(str,"%%%c%d.%d%c",sign,w,d,ch);
                     image0(constA,str,val); addNumber(w);

     | Lw:           w:=INTEGER(getFormat()); --w
                     i:=0; Str0(constA);
                     WHILE (i<w-1) DO App(constA," "); INC(i) END;
                     IF BOOLEAN(val)=FALSE THEN ch:="F" ELSE ch:="T" END;
                     App(constA,ch); addNumber(w);

     | Hw:    w:=INTEGER(getFormat()); Str0(constA);
              FOR i:=0 TO w-1 DO App(constA,CHAR(getFormat())) END;
              addStr(w);
     | A0:           strDesc(val,ps,i,d);
                     k:=0; Str0(constA);
                     WHILE k<d DO  App(constA,ps^[i]); INC(i); INC(k) END;
                     addStr(d);

     | Aw:           w:=INTEGER(getFormat());
                     strDesc(val,ps,i,d);  j:=Min(d,w);
                     k:=0;  Str0(constA);
                     IF w>d THEN
                       WHILE k<(w-d) DO App(constA," "); INC(k) END;
                     END;
                     WHILE k<w DO  App(constA,ps^[i]); INC(i); INC(k) END;
                     addStr(w);

     | Tc:           i:=INTEGER(getFormat())-1; --c; f77 pos [1.. ]
                     movePnt(i,FALSE);
     | TLs:          i:=INTEGER(getFormat()); --s
                     movePnt(-i,TRUE);
     | TRs:          i:=INTEGER(getFormat()); --s
                     movePnt(i,TRUE);
     | Xw:     w:=INTEGER(getFormat()); Str0(constA);
               FOR i:=0 TO w-1 DO App(constA," ") END;
               addStr(w);

     | SP:      sign:="+";
     | S0,SS:   sign:=" ";
     | BN,BZ:        -- next time  to do
  ELSE  cErr:=12;
  END;
  IF cnt>0 THEN frInx:=old END;
  IF (NOT (symb IN formatSet{eofForm..colon}))
      AND (cErr#12) THEN r:=putVal(val) END;
  RETURN cErr=0;
END putVal;

PROCEDURE delim(): BOOLEAN;
VAR i: INTEGER;
BEGIN
  i:=strInx;
  WHILE (i<cLen)&((cRec[i]=" ")OR(cRec[i]=11c)) DO INC(i) END;
  IF (i>=cLen)OR(cRec[i]=0c) THEN RETURN FALSE END;
  strInx:=i;
  RETURN TRUE;
END delim;

PROCEDURE freeForm(c: CHAR; ad: ADDRESS): BOOLEAN;
VAR k,j,i,w: INTEGER;  ch: CHAR; p: POINTER TO ARRAY [0..1023] OF CHAR;
    rl: REAL; fr,ln:INTEGER; r: BOOLEAN;
BEGIN
  IF (slsh IN cState) THEN RETURN TRUE (* ok *) END;
  IF (const IN cState)&(cnt#0) THEN DEC(cnt);
    IF (nul IN cState) THEN RETURN TRUE END; -- null val
    IF (text IN cState)&(c="T") THEN strDesc(ad,p,fr,ln);
      k:=INTEGER(constA[HIGH(constA)]); k:=Min(k,ln);
      FOR i:=0 TO k-1 DO p^[fr]:=constA[i]; INC(fr) END;
    ELSIF (c="I")&((int IN cState)OR(real IN cState)) THEN ad^:=constW;
      IF (real IN cState) THEN ad^:=FLOAT(INTEGER(constW)) END;
    ELSIF (c="R")&((int IN cState)OR(real IN cState)) THEN ad^:=constW;
      IF (int IN cState) THEN ad^:=TRUNC(REAL(constW)) END
    ELSIF (c="L")&(logic IN cState) THEN ad^:=constW
    ELSE RETURN FALSE
    END;
    IF cnt=0 THEN cState:=cState-StatusSet{const..nul} END;
    RETURN TRUE
  END;
  IF NOT delim() THEN RETURN FALSE END;
  ch:=cRec[strInx]; i:=strInx;
  IF (ch=",") THEN strInx:=i+1; RETURN TRUE -- null, no change val
  ELSIF (ch="/") THEN  INCL(cState,slsh); RETURN TRUE
  END;
  k:=i;
  i:=PeekNum(cRec,i,w);
  IF (i>0) THEN
    IF (cRec[i]='*') THEN INC(i); cnt:=w-1; INCL(cState,const);
      IF (cRec[i]=' ')OR(cRec[i]=',') THEN  INCL(cState,nul); --null const r*
      ELSE
        IF (c='I') THEN i:=PeekNum(cRec,i,constW); INCL(cState,int);
          ad^:=constW;
        ELSIF (c='R') THEN i:=PeekReal(cRec,i,constW); INCL(cState,real);
          ad^:=constW;
        ELSIF (c='L') THEN i:=PeekLogic(constW,i,0);
                           INCL(cState,logic);
        ELSE  ch:=cRec[i];  -- " T"
          IF (ch="'")OR(ch='"') THEN INC(i); j:=0;
             LOOP
               IF (j>=HIGH(constA)) THEN RETURN FALSE END;
               IF (cRec[i]#ch)  THEN constA[j]:=cRec[i]; INC(i);
               ELSIF (cRec[i+1]=ch) THEN constA[j]:=cRec[i]; INC(i,2)
               ELSE constA[HIGH(constA)]:=CHAR(j); INC(i); EXIT
               END;
               INC(j);
             END;  INCL(cState,text); INC(cnt); r:=freeForm(c,ad);
          ELSE RETURN FALSE
          END;
        END;
      END
    ELSIF (c="I") THEN
      IF (cRec[i]="E")OR(cRec[i]=".") THEN i:=PeekReal(cRec,k,rl);
        ad^:=TRUNC(rl);
      ELSE  ad^:=w END
    ELSIF (c="R") THEN i:=PeekReal(cRec,k,ad^)
    END;
  ELSIF (c="R") THEN i:=PeekReal(cRec,k,ad^)
  ELSIF (c="T") THEN i:=k; ch:=cRec[i];
     IF (ch="'")OR(ch='"') THEN INC(i);
       strDesc(ad,p,fr,ln); j:=0;
       LOOP
         IF (j>=ln) THEN RETURN FALSE END;
         IF (cRec[i]#ch)  THEN  p^[fr]:=cRec[i]; INC(i);
         ELSIF (cRec[i+1]=ch) THEN  p^[fr]:=cRec[i]; INC(i,2)
         ELSE  INC(i); EXIT
         END;
         INC(fr); INC(j);
       END;
     ELSE RETURN FALSE
     END;
  ELSIF (c="L") THEN i:=PeekLogic(ad^,k,0)
  ELSE RETURN FALSE
  END;
  IF i<0 THEN RETURN FALSE END;
  IF (cRec[i]=".")OR(cRec[i]="E") THEN
    REPEAT  ch:=cRec[i]; INC(i) UNTIL (ch=" ")OR(ch=","); DEC(i);
  END;
  strInx:=i; r:= delim(); IF (cRec[strInx]=",") THEN INC(strInx) END;
  RETURN (strInx<=cLen);
END freeForm;

PROCEDURE InputIRLT(c: CHAR; varAdr: ADDRESS);
VAR i: INTEGER;  r: BOOLEAN; p: POINTER TO ARRAY [0..511] OF CHAR;
    fr,len,j: INTEGER;
BEGIN
  IF cErr#0 THEN RETURN END;
  IF freeformat IN cState THEN
    r:=freeForm(c,varAdr);
  ELSIF formatted IN cState THEN
    r:=getVal(varAdr);
    beforVal:=TRUE;
    r:=getVal(ADR(i));
  ELSE -- unformatted IN cState THEN
    IF c='T' THEN
      strDesc(varAdr,p,fr,len); j:=fr+len-1;
    ELSE p:=varAdr; len:=4; fr:=0; j:=3;
    END;
    IF (strInx+len)>cLen THEN cErr:=9;
    ELSE
      FOR i:=fr TO j DO p^[i]:=cRec[strInx]; INC(strInx) END;
    END;
  END;
--  IF (strInx>cLen) THEN cErr:=INTEGER(BITSET(cErr)+{4,8}) END;
END InputIRLT;

PROCEDURE OutputIRLT(c: CHAR; var: WORD);
VAR i,j: INTEGER; ch: CHAR; ad: POINTER TO ARRAY [0..511] OF CHAR;
    r: BOOLEAN;
VAR fr,len: INTEGER;
BEGIN
  IF cErr#0 THEN RETURN END;
  IF freeformat IN cState THEN
    IF c="R" THEN
      IF REAL(var)=0.0 THEN image0(constA,"%4.1f ",var);
      ELSIF ABS(REAL(var))<0.1 THEN image0(constA,"%e ",var);
      ELSE image0(constA,"%g ",var)
      END
    ELSIF c="I" THEN image0(constA,"%d ",var)
    ELSIF c="T" THEN getStr1(var,constA); i:=Len(constA);
      IF i<HIGH(constA) THEN constA[i]:=" "; constA[i+1]:=0c END;
    ELSE IF BOOLEAN(var) THEN ch:="T" ELSE ch:="F" END;
       image0(constA,"%c ",ch)
    END;
    i:=Len(constA); addNumber(i);
  ELSIF formatted IN cState THEN
    r:= putVal(var);
    beforVal:=TRUE;
    r:= putVal(i);
  ELSE --   unformatted IN cStatus
    IF c='T' THEN
      strDesc(var,ad,fr,len); j:=fr+len-1;
    ELSE ad:=ADR(var); len:=4; fr:=0; j:=3;
    END;
    IF (strInx+len)>cLen THEN cErr:=9;
    ELSE
      FOR i:=fr TO j DO cRec[strInx]:=ad^[i]; INC(strInx) END;
    END;
  END;
END OutputIRLT;

PROCEDURE InputIV(vv: ADDRESS);
BEGIN  InputIRLT("I",vv)
END InputIV;

PROCEDURE InputIA(ad: ADDRESS; ln: INTEGER);
VAR i: INTEGER;
BEGIN
  FOR i:=0 TO ln-1 DO InputIRLT("I",ad); INC(ad) END;
END InputIA;

PROCEDURE InputRA(ad: ADDRESS; ln: INTEGER);
VAR i: INTEGER;
BEGIN
  FOR i:=0 TO ln-1 DO InputIRLT("R",ad); INC(ad) END;
END InputRA;

PROCEDURE InputRV(ad: ADDRESS);
BEGIN  InputIRLT("R",ad)
END InputRV;

PROCEDURE InputLV(vv: ADDRESS);
BEGIN  InputIRLT("L",vv)
END InputLV;

PROCEDURE InputLA(ad: ADDRESS; ln: INTEGER);
VAR i: INTEGER;
BEGIN
  FOR i:=0 TO ln-1 DO InputIRLT("L",ad); INC(ad) END;
END InputLA;

PROCEDURE InputCV(vv: ADDRESS);
BEGIN  InputIRLT("T",vv)
END InputCV;

PROCEDURE InputCA(ad: ADDRESS; cn: INTEGER);
VAR i: INTEGER; sd: POINTER TO sDescriptor; l: INTEGER;
BEGIN
  sd:=ad;
  WITH sd^ DO
    l:=ln;
    FOR i:=0 TO cn-1 DO InputIRLT("T",sd); INC(lo,l); END;
  END;
END InputCA;

PROCEDURE OutputIV(vv: WORD);
BEGIN  OutputIRLT("I",vv)
END OutputIV;

PROCEDURE OutputIA(ad: ADDRESS; ln: INTEGER);
VAR i: INTEGER;
BEGIN
  FOR i:=0 TO ln-1 DO OutputIRLT("I",ad^); INC(ad) END;
END OutputIA;

PROCEDURE OutputRA(ad: ADDRESS; ln: INTEGER);
VAR i: INTEGER;
BEGIN
  FOR i:=0 TO ln-1 DO OutputIRLT("R",ad^); INC(ad) END;
END OutputRA;

PROCEDURE OutputRV(vv: WORD);
BEGIN  OutputIRLT("R",vv)
END OutputRV;

PROCEDURE OutputLA(ad: ADDRESS; ln: INTEGER);
VAR i: INTEGER;
BEGIN
  FOR i:=0 TO ln-1 DO OutputIRLT("L",ad^); INC(ad) END;
END OutputLA;

PROCEDURE OutputLV(vv: WORD);
BEGIN  OutputIRLT("L",vv)
END OutputLV;

PROCEDURE OutputCV(vv: ADDRESS);
BEGIN  OutputIRLT("T",vv)
END OutputCV;

PROCEDURE OutputCA(ad: ADDRESS; cn: INTEGER);
VAR i: INTEGER; sd: POINTER TO sDescriptor; l: INTEGER;
BEGIN
  sd:=ad;
  WITH sd^ DO
    l:=ln;
  FOR i:=0 TO cn-1 DO OutputIRLT("T",sd); INC(lo,l);  END;
  END;
END OutputCA;

PROCEDURE readCarry;
VAR r: INTEGER;
BEGIN
  IF sysIO    IN cState THEN ReadString("f77:",cRec); cLen:=Len(cRec);
    strInx:=0; WriteLn; RETURN
  END;
  IF internal IN cState THEN RETURN END;
  WITH carryList^   DO
  IF seq IN cState THEN
    IF unformatted IN cState THEN
       r:=Read(STream,ADR(cLen),2);
       r:=Read(STream,ADR(cRec),cLen-4);
       r:=Read(STream,ADR(cLen),2); DEC(cLen,4);
    ELSE
      r:=GetS(STream,cRec); cLen:=r;
    END
  ELSE -- direct
    INC(carryList^.NextRec);
    IF unformatted IN cState THEN
       r:=Read(STream,ADR(cLen),2);
       r:=Read(STream,ADR(cRec),cLen-4);
       r:=Read(STream,ADR(cLen),2); DEC(cLen,4);
    ELSE
       r:=GetS(STream,cRec); cLen:=r-1;
    END;
   IF (r>0) AND (cLen#Recl) THEN cErr:=8; END;
-- print(' read direct: r=%d Recl=%d cLen=%d \n',r,Recl,cLen);
  END;
  strInx:=0;
  IF r=0 THEN (*eof*) cErr:=INTEGER(BITSET(cErr)+{31})
  ELSIF r<0 THEN cErr:=ABS(r)*200h;
  END;
  END -- WITH
END readCarry;

PROCEDURE ReadSF(chan: INTEGER;  fmt: ADDRESS);
  VAR r: BOOLEAN; i: INTEGER;
BEGIN
  carryList:=NIL;
  cState:=StatusSet{formatted,seq,read};
  IF chan<0 THEN
    cState:=StatusSet{formatted,seq,read,sysIO};
  ELSIF chan>99 THEN cErr:=1;
    RETURN
  ELSE
    IF(units[chan]=NIL) THEN
      cErr:=ForOpen(chan);
      IF cErr#0 THEN RETURN END;
    END;
    carryList:=units[chan];
    IF (unformatted IN carryList^.STATE)
    OR (direct      IN carryList^.STATE) THEN
      cErr:=7; RETURN
    END;
  END;
  formatAdr:=fmt;
  formBeg:=0; frInx:=0; beforVal:=TRUE;
  cnt:=0;
  readCarry;
  r:=getVal(ADR(i));
END ReadSF;

PROCEDURE ReadSFR(chan: INTEGER;  fmt: ADDRESS);
BEGIN
END ReadSFR;

PROCEDURE ReadSU(chan: INTEGER);
BEGIN
  cState:=StatusSet{unformatted,seq,read};
  IF (chan<0) OR (chan>99) THEN cErr:=1; RETURN END;
  IF units[chan]=NIL THEN
    cErr:=ForOpen(chan);
    cState:=StatusSet{formatted,seq,read};
    IF cErr#0 THEN  RETURN END;
    INCL(units[chan]^.STATE,unformatted); EXCL(units[chan]^.STATE,formatted)
  END;
  carryList:=units[chan];
  IF (formatted IN carryList^.STATE)
  OR (direct    IN carryList^.STATE) THEN
      cErr:=7; RETURN
  END;
  readCarry;
END ReadSU;

PROCEDURE ReadSL(chan: INTEGER);
BEGIN
  cState:=StatusSet{formatted,seq,freeformat,read};
  carryList:=NIL;
  IF chan<0 THEN INCL(cState,sysIO);
  ELSIF chan>99 THEN cErr:=1;
    RETURN
  ELSE
    IF(units[chan]=NIL) THEN
      cErr:=ForOpen(chan);
      cState:=StatusSet{formatted,seq,freeformat,read};
      IF cErr#0 THEN RETURN END;
    END;
    carryList:=units[chan];
    IF (unformatted IN carryList^.STATE)
    OR (direct      IN carryList^.STATE) THEN
      cErr:=7; RETURN
    END;
  END;
  readCarry;
END ReadSL;

PROCEDURE ReadDF(chan: INTEGER; fmt: ADDRESS; recNb: INTEGER);
VAR i: INTEGER; r: BOOLEAN;
BEGIN
  cState:=StatusSet{formatted,direct,read};
  IF (chan<0) OR (chan>99) THEN cErr:=1; RETURN END;
  IF units[chan]=NIL THEN
    cErr:=ForOpen(chan);
    cState:=StatusSet{formatted,direct,read};
    IF cErr#0 THEN  RETURN END;
    INCL(units[chan]^.STATE,direct); EXCL(units[chan]^.STATE,seq)
  END;
  carryList:=units[chan];
  IF (unformatted IN carryList^.STATE)
  OR (seq         IN carryList^.STATE) THEN
      cErr:=7; RETURN
  END;
  formatAdr:=fmt;
  WITH carryList^ DO
    i:=Seek(STream,(Recl+1)*(recNb-1),0); NextRec:=recNb+1;
  END;
  IF (i<0) THEN cErr:=ABS(i)*200h; RETURN  END;
  formBeg:=0; frInx:=0; beforVal:=TRUE; cnt:=0; cLen:=carryList^.Recl+1;
  readCarry;
  r:=getVal(ADR(i));
END ReadDF;

PROCEDURE ReadDFR(chan: INTEGER; fmt: ADDRESS; recNb: INTEGER);
BEGIN
END ReadDFR;

PROCEDURE ReadDU(chan: INTEGER;  recNb: INTEGER);
VAR i: INTEGER;
BEGIN
  cState:=StatusSet{unformatted,direct,read};
  IF (chan<0) OR (chan>99) THEN cErr:=1; RETURN END;
  IF units[chan]=NIL THEN
    cErr:=ForOpen(chan);
    cState:=StatusSet{unformatted,direct,read};
    IF cErr#0 THEN  RETURN END;
    INCL(units[chan]^.STATE,direct); EXCL(units[chan]^.STATE,seq);
    INCL(units[chan]^.STATE,unformatted); EXCL(units[chan]^.STATE,formatted)
  END;
  carryList:=units[chan];
  IF (formatted IN carryList^.STATE)
  OR (seq       IN carryList^.STATE) THEN
      cErr:=7; RETURN
  END;
  WITH carryList^ DO
    i:=Seek(STream,(Recl+4)*(recNb-1),0); NextRec:=recNb+1;
  END;
  cLen:=carryList^.Recl+4;
  IF (i<0) THEN cErr:=ABS(i)*200h; RETURN  END;
  readCarry;
END ReadDU;

PROCEDURE ReadDL(chan: INTEGER;  recNb: INTEGER); -- f77 not use
VAR r: INTEGER; i: INTEGER;
BEGIN
  cState:=StatusSet{formatted,direct,freeformat,read};
  IF (chan<0) OR (chan>99) THEN cErr:=1; RETURN END;
  IF units[chan]=NIL THEN
    cErr:=ForOpen(chan);
    cState:=StatusSet{formatted,direct,freeformat,read};
    IF cErr#0 THEN  RETURN END;
    INCL(units[chan]^.STATE,direct); EXCL(units[chan]^.STATE,seq)
  END;
  carryList:=units[chan];
  IF (unformatted IN carryList^.STATE)
  OR (seq         IN carryList^.STATE) THEN
      cErr:=7; RETURN
  END;
  WITH carryList^ DO
    i:=Seek(STream,(Recl+1)*(recNb-1),0); NextRec:=recNb+1;
  END;
  cLen:=carryList^.Recl;
  IF (i<0) THEN cErr:=ABS(i)*200h; RETURN  END;
  readCarry;
END ReadDL;

PROCEDURE ReadIF(dAdr: ADDRESS; fmt: ADDRESS);
VAR aa: ADDRESS;  i,j: INTEGER; r: BOOLEAN;
BEGIN
  formatAdr:=fmt;
  cState:=StatusSet{internal,formatted,read};
  formBeg:=0; frInx:=0; beforVal:=TRUE;
  cnt:=0;
  strDesc(dAdr,aa,i,j);
  strInx:=i; cLen:=j+strInx;
  --ORIGIN(cRec,aa,cLen);
  r:=getVal(ADR(i));
END ReadIF;

PROCEDURE writeCarry;
VAR r,i: INTEGER; ch: CHAR;
BEGIN
  IF sysIO    IN cState THEN cRec[lastInx]:=0c; Show(cRec);
     ClearRec;  RETURN
  END;
  IF internal IN cState THEN RETURN END;
  WITH carryList^   DO
  IF seq IN STATE THEN
    IF unformatted IN STATE THEN
       IF strInx>=cLen THEN cErr:=10; RETURN END;
       INC(strInx,4);
       r:=Write(STream,ADR(strInx),2);
       r:=Write(STream,ADR(cRec),strInx-4);
       r:=Write(STream,ADR(strInx),2);
    ELSE
       cRec[lastInx]:=NL;
       r:=Write(STream,ADR(cRec),lastInx+1);
       ClearRec;
    END
  ELSE -- direct
    INC(NextRec);
    IF unformatted IN STATE THEN
       i:=Recl-strInx;
       IF i<0 THEN cErr:=9; RETURN ;
       ELSE
         WHILE i>0 DO cRec[strInx]:=0c; INC(strInx); DEC(i); END;
         INC(strInx,4);
         r:=Write(STream,ADR(strInx),2);
         r:=Write(STream,ADR(cRec),strInx-4);
         r:=Write(STream,ADR(strInx),2);
       END;
    ELSE
       i:=Recl-lastInx;
       IF i<0 THEN cErr:=9; RETURN ;
-- print(' write direct: r=%d Recl=%d strInx=%d \n',r,Recl,strInx);
       ELSE
         movePnt(Recl,FALSE);
         cRec[strInx]:=NL;
         r:=Write(STream,ADR(cRec),strInx+1);
         ClearRec;
       END;
    END
  END;
  IF r<0 THEN cErr:=ABS(r)*200h; END;
  END -- WITH
END writeCarry;

PROCEDURE WriteSU(chan: INTEGER);
VAR r: INTEGER;
BEGIN
  cState:=StatusSet{unformatted,seq,write};
  IF (chan<0) OR (chan>99) THEN cErr:=1; RETURN END;
  IF units[chan]=NIL THEN
    cErr:=ForOpen(chan);
    cState:=StatusSet{formatted,seq,write};
    IF cErr#0 THEN  RETURN END;
    INCL(units[chan]^.STATE,unformatted); EXCL(units[chan]^.STATE,formatted)
  END;
  carryList:=units[chan];
  IF (formatted IN carryList^.STATE)
  OR (direct    IN carryList^.STATE) THEN
      cErr:=7; RETURN
  END;
  cLen:=HIGH(cRec);
  strInx:=0;
END WriteSU;

PROCEDURE WriteSF(chan: INTEGER; fmt: ADDRESS);
  VAR r: BOOLEAN; i: INTEGER;
BEGIN
  carryList:=NIL;
  cState:=StatusSet{formatted,seq,write};
  IF chan<0 THEN
    cState:=StatusSet{formatted,seq,write,sysIO};
  ELSIF chan>99 THEN cErr:=1;
    RETURN
  ELSE
    IF(units[chan]=NIL) THEN
      cErr:=ForOpen(chan);
      cState:=StatusSet{formatted,seq,write};
      IF cErr#0 THEN RETURN END;
    END;
    carryList:=units[chan];
    IF (unformatted IN carryList^.STATE)
    OR (direct      IN carryList^.STATE) THEN
      cErr:=7; RETURN
    END;
  END;
  formatAdr:=fmt;
  cLen:=HIGH(cRec);
  ClearRec;
  formBeg:=0; frInx:=0; cnt:=0; beforVal:=TRUE;
  r:=putVal(i);
END WriteSF;

PROCEDURE WriteSFR(chan: INTEGER; fmt: ADDRESS);
BEGIN
END WriteSFR;

PROCEDURE WriteSL(chan: INTEGER);
VAR r: INTEGER;
BEGIN
  cState:=StatusSet{formatted,seq,freeformat,write};
  carryList:=NIL;
  IF chan<0 THEN INCL(cState,sysIO);
  ELSIF chan>99 THEN cErr:=1;
    RETURN
  ELSE
    IF(units[chan]=NIL) THEN
      cErr:=ForOpen(chan);
      cState:=StatusSet{formatted,seq,freeformat,write};
      IF cErr#0 THEN RETURN END;
    END;
    carryList:=units[chan];
    IF (unformatted IN carryList^.STATE)
    OR (direct      IN carryList^.STATE) THEN
      cErr:=7; RETURN
    END;
  END;
  cLen:=HIGH(cRec);
  ClearRec;
END WriteSL;

PROCEDURE WriteDL(chan: INTEGER; recNb: INTEGER);
VAR r: INTEGER; i: INTEGER;
BEGIN
  cState:=StatusSet{formatted,direct,freeformat,write};
  IF (chan<0) OR (chan>99) THEN cErr:=1; RETURN END;
  IF units[chan]=NIL THEN
    cErr:=ForOpen(chan);
    cState:=StatusSet{formatted,direct,freeformat,write};
    IF cErr#0 THEN  RETURN END;
    INCL(units[chan]^.STATE,direct); EXCL(units[chan]^.STATE,seq)
  END;
  carryList:=units[chan];
  IF (unformatted IN carryList^.STATE)
  OR (seq         IN carryList^.STATE) THEN
      cErr:=7; RETURN
  END;
  cLen:= carryList^.Recl+1;
  WITH carryList^ DO
    i:=Seek(STream,(Recl+1)*(recNb-1),0); NextRec:=recNb;
  END;
  ClearRec;
  IF (i<0) THEN cErr:=ABS(i)*200h; END;
END WriteDL;

PROCEDURE WriteDF(chan: INTEGER; fmt: ADDRESS; recNb: INTEGER);
VAR i: INTEGER; r: BOOLEAN;
BEGIN
  cState:=StatusSet{formatted,direct,write};
  IF (chan<0) OR (chan>99) THEN cErr:=1; RETURN END;
  IF units[chan]=NIL THEN
    cErr:=ForOpen(chan);
    cState:=StatusSet{formatted,direct,write};
    IF cErr#0 THEN  RETURN END;
    INCL(units[chan]^.STATE,direct); EXCL(units[chan]^.STATE,seq)
  END;
  carryList:=units[chan];
  IF (unformatted IN carryList^.STATE)
  OR (seq         IN carryList^.STATE) THEN
      cErr:=7; RETURN
  END;
  formatAdr:=fmt;
  cLen:= carryList^.Recl+1;
  WITH carryList^ DO
    i:=Seek(STream,(Recl+1)*(recNb-1),0); NextRec:=recNb+1;
  END;
  ClearRec;
  formBeg:=0; frInx:=0; cnt:=0; beforVal:=TRUE;
  IF (i<0) THEN cErr:=ABS(i)*200h; RETURN END;
  r:=putVal(i);
END WriteDF;

PROCEDURE WriteDFR(chan: INTEGER; fmt: ADDRESS; recNb: INTEGER);
BEGIN
END WriteDFR;

PROCEDURE WriteDU(chan: INTEGER; recNb: INTEGER);
VAR i: INTEGER; r: INTEGER;
BEGIN
  cState:=StatusSet{unformatted,direct,write};
  IF (chan<0) OR (chan>99) THEN cErr:=1; RETURN END;
  IF units[chan]=NIL THEN
    cErr:=ForOpen(chan);
    cState:=StatusSet{unformatted,direct,write};
    IF cErr#0 THEN  RETURN END;
    INCL(units[chan]^.STATE,direct); EXCL(units[chan]^.STATE,seq);
    INCL(units[chan]^.STATE,unformatted); EXCL(units[chan]^.STATE,formatted)
  END;
  carryList:=units[chan];
  IF (formatted IN carryList^.STATE)
  OR (seq       IN carryList^.STATE) THEN
      cErr:=7; RETURN
  END;
  cLen:= carryList^.Recl+4;
  WITH carryList^ DO
    i:=Seek(STream,(Recl+4)*(recNb-1),0); NextRec:=recNb+1;
  END;
  IF (i<0) THEN cErr:=ABS(i)*200h; END;
  strInx:=0;
END WriteDU;

PROCEDURE WriteIF(dAdr: ADDRESS; fmt: ADDRESS);
VAR aa: ADDRESS;  i,j: INTEGER; r: BOOLEAN;
BEGIN
  formatAdr:=fmt;
  cState:=StatusSet{internal,formatted,write};
  cLen:=HIGH(cRec);
  ClearRec;
  formBeg:=0; frInx:=0; cnt:=0; beforVal:=TRUE;
  IfdAdr:=dAdr;
  r:=getVal(i);
END WriteIF;

PROCEDURE moveIF;
  VAR i,j,fr,len: INTEGER;
      p: POINTER TO ARRAY [0..511] OF CHAR;
BEGIN
  strDesc(IfdAdr,p,fr,len);
  j:=len-lastInx;
  IF j<0 THEN cErr:=21;
  ELSE
    cRec[lastInx]:=SPACE;
  END;
  FOR i:=0 TO len-1 DO
    p^[fr]:=cRec[i]; INC(fr);
  END;
END moveIF;

PROCEDURE IOend(ios: ADDRESS): INTEGER; -- <0 -eof, =0 -ok, >= -error
VAR i: INTEGER;
BEGIN
  IF ((cErr=0) OR (cErr=11)) & (write IN cState) THEN writeCarry END;
  IF (internal IN cState) THEN
   IF (write IN cState) & (cErr=0) THEN moveIF;
   ELSIF read IN cState THEN --ORIGIN(cRec,ADR(iRec),SIZE(iRec));
   END;
  END;
  IF ios#NIL THEN    ios^:=cErr END;
  stackInx:=0; i:=cErr; cErr:=0;
  RETURN i
END IOend;

PROCEDURE fin;
BEGIN
  print("\n****%s",cRec);
END fin;

PROCEDURE IOerror(er: INTEGER);
VAR s: ARRAY [0..255] OF CHAR; i,j: INTEGER;
BEGIN
  IF er=0 THEN RETURN  END;
  IF er<0 THEN print("\n Достигнут конец файла\n"); HALT END;
  i:=er MOD 200h; j:=er DIV 200h;
  Str0(s);
  IF i>0 THEN AppStr(s," Ошибка ");
    IF open IN cState THEN
      AppStr(s,"при открытии файла: ");
    ELSIF close IN cState THEN
      AppStr(s,"при закрытии файла: ");
    ELSIF backspace IN cState THEN
      AppStr(s,"при возврате на одну позицию: ");
    ELSIF rewind IN cState THEN
      AppStr(s,"при установке начала файла: ");
    ELSIF endfile IN cState THEN
      AppStr(s,"при установке конца файла: ");
    ELSIF read IN cState THEN
      AppStr(s,"при вводе. Доступ: ");
      IF seq IN cState THEN
        AppStr(s,"последовательный. ");
      ELSIF direct IN cState THEN
        AppStr(s,"прямой. ");
      ELSIF internal IN cState THEN
        AppStr(s,"внутренний файл. ");
      END;
      AppStr(s,"Форматность: ");
      IF freeformat IN cState THEN
        AppStr(s,"свободный формат.");
      ELSIF formatted IN cState THEN
        AppStr(s,"форматный.");
      ELSIF unformatted IN cState THEN
        AppStr(s,"бесформатный.");
      END;
    ELSIF write IN cState THEN
      AppStr(s,"при выводе: Доступ: ");
      IF seq IN cState THEN
        AppStr(s,"последовательный. ");
      ELSIF direct IN cState THEN
        AppStr(s,"прямой. ");
      ELSIF internal IN cState THEN
        AppStr(s,"внутренний файл. ");
      END;
      AppStr(s,"Форматность: ");
      IF freeformat IN cState THEN
        AppStr(s,"свободный формат.");
      ELSIF formatted IN cState THEN
        AppStr(s,"форматный.");
      ELSIF unformatted IN cState THEN
        AppStr(s,"бесформатный.");
      END;
    END;
    print("\n%s",s); Str0(s);
    CASE i OF
      1: AppStr(s,"Канал в/в вне 0..99 диапазона. ");
    | 2: AppStr(s,"Ошибка в спецификациях канала. ");
    | 3: AppStr(s,"Файл открыт с другими параметрами. ");
    | 4: AppStr(s,"Статус NEW: файл с таким именем уже есть.");
    | 5: AppStr(s,"Попытка доступа к не открытому файлу.");
    | 6: AppStr(s,"Backspace для файла прямого доступа.");
    | 7: AppStr(s,"Неправильный метод доступа.");
    | 8: AppStr(s,"Длина записи не совпадает с заданной.");
    | 9: AppStr(s,"Превышена длина записи.");
    |10: AppStr(s,"Превышен максимальный размер записи.");
    |11: AppStr(s,"Ошибка преобразования данных.");
    |12: AppStr(s,"Ошибка в кодах формата."  );
    ELSE AppStr(s,"Неизвестная ошибка");
    END;
  END;
  print("\n%s \n",s);
  IF j#0 THEN
    Why?(-j,s); print(" ОШИБКА ФАЙЛОВОЙ СИСТЕМЫ: %s \n",s);
  END;
  IF i#11 THEN HALT; END; cErr:=0;
END IOerror;

VAR z: INTEGER;

BEGIN
--Final(fin);
  FOR z:=0 TO  99 DO units[z]:=NIL END;
  stackInx:=0; cErr:=0; sign:=" ";
  --ORIGIN(cRec,ADR(iRec),SIZE(iRec));
  fName:="FOR00.DAT"; Str2(fName);
  WITH myDesc DO sAd:=ADR(fName); lo:=0; ln:=Len(fName) END;
  initVar;
  ALLOCATE(units[100],SIZE(fList));
END ForIO.
