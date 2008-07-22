IMPLEMENTATION MODULE exMain; (* Leo  05-Jun-87. (c) KRONOS *)
                              (* Ned  19-Nov-87. (c) KRONOS *)

IMPORT exTex, exMem;
IMPORT  sys: SYSTEM;
IMPORT  face: myEditor;
FROM ASCII      IMPORT  NL;
FROM exScreen   IMPORT  w, ws, leftbound, pos?, pos, leftbound?, clearscr
                      , showinfo, get, getline, setline, linesize, undel
                      , posc?, posl?, posl, posc, stringsize, adr, adrline
                      , refresh, refreshline, stop, PleaseRefresh, dupover
                      , dup, push, pop, rollup, rolldw, swapup, swapdw
                      , ic, dc, dl, il, clearln, fixpos, rubout
                      , delword0, ruboutword0, skipwordleft0, skipwordright0
                      , delword1, ruboutword1, skipwordleft1, skipwordright1
                      , infomode, infomode?, bell, bell?, ins, ins?
                      , pullleft, pullright, maxcol, size, frame, mark
                      , pushandclearinfo, Bell, setinfostr, set_on_off
                      , timepos, fnpos, nocolumns
                      ;
FROM exMem      IMPORT   jump, last;
FROM exSetUp    IMPORT   public, set_up;
FROM exHead     IMPORT   ask, message, alarm;
FROM exMacro    IMPORT   in?, out?, getmacro, peekmacro, intomacro
                       , StartMacro, FinishGold, FinishBronze
                       , GetGoldMacro, GetBronzeMacro;

IMPORT  tty: Terminal;
IMPORT  key: Keyboard;
IMPORT  img: Strings;
IMPORT time: Time;
IMPORT  env: tskEnv;
--IMPORT  std: StdIO;

CONST empty = 0c;

VAR top: INTEGER; (* # in file top line of screen *)

PROCEDURE fix;
BEGIN IF NOT out?() THEN fixpos(top+posl?) END END fix;

PROCEDURE Control?(ch: CHAR): BOOLEAN;
BEGIN RETURN ORD(ch) MOD 128 < 40b END Control?;

VAR pleaseinfo: BOOLEAN; lastTime: INTEGER;

PROCEDURE inform_on(s: ARRAY OF CHAR);
  VAR i: ARRAY [0..15] OF CHAR;
BEGIN
  img.print(i,'%|11.11s',s);
  setinfostr(timepos,i); fix
END inform_on;

PROCEDURE Time;
  VAR hh,mm: INTEGER; pm: BOOLEAN; s: ARRAY [0..15] OF CHAR;
BEGIN
  IF in?() THEN RETURN END;
  IF lastTime DIV 60 = time.time() DIV 60 THEN RETURN END;
  lastTime:=time.time();
  hh:=(lastTime DIV (60*60)) MOD 24; pm:=(hh>=12);
  mm:=(lastTime DIV     60 ) MOD 60;
  IF hh#12 THEN hh:=hh MOD 12 END;
  IF pm THEN img.print(s," %2d:%$2d p.m. ",hh,mm)
  ELSE       img.print(s," %2d:%$2d a.m. ",hh,mm)
  END;
  inform_on(s);
END Time;

PROCEDURE EmitTime;
  CONST monthes = 'JanFebMarAprMayJunJulAugSepOctNovDec';
  VAR s: ARRAY [0..15] OF CHAR; tm,i: INTEGER;
      y,m,d,ho,mn,sc: INTEGER;
BEGIN tm:=time.time();
  time.unpack(tm,y,m,d,ho,mn,sc);
  img.print(s,"%$02d-%3.3.*s-%02d",d,(m-1)*3,monthes,y MOD 100);
  i:=0;
  WHILE (i<=HIGH(s)) & (s[i]#0c) DO w(s[i]); INC(i) END;
END EmitTime;

PROCEDURE inform_off; BEGIN lastTime:=-1; Time END inform_off;

VAR   next: CHAR;
   tobreak: BOOLEAN;
read_again: BOOLEAN;

PROCEDURE break_mode(on: BOOLEAN);
BEGIN tobreak:=on END break_mode;

PROCEDURE break?;
  VAR k: CHAR; for: INTEGER;
BEGIN
  IF NOT tobreak THEN RETURN END;
  IF NOT face.onbreak() THEN RETURN END;
  for:=key.state^.foreign;
  key.set_foreign(0);
  REPEAT
    k:=ask("QUIT WITHOUT WRITING? ");
    IF k>=300c THEN k:='N' ELSE k:=CAP(k) END;
  UNTIL (k='Y') OR (k='N');
  IF key.state^.foreign#for THEN key.set_foreign(1)
  ELSE                           key.set_foreign(0)
  END;
  IF k='Y' THEN FINISH; HALT END;
END break?;

PROCEDURE ReadKey(VAR k: CHAR);
BEGIN
  IF out?() THEN k:=getmacro()
  ELSE
    IF read_again THEN read_again:=FALSE; k:=next; next:=empty
    ELSE key.read(k)
    END;
  END;
  IF in?()  THEN intomacro(k) END;
  IF k=key.break THEN break?; k:=empty END;
  next:=empty
END ReadKey;

PROCEDURE PeekKey(): CHAR;
BEGIN
  IF next#empty THEN RETURN next END;
  IF out?() THEN next:=peekmacro(); RETURN next END;
  IF NOT read_again THEN
    IF key.ready()<=0 THEN next:=0c
    ELSE key.read(next)
    END
  END;
  IF next=key.break THEN next:=empty; break?    END;
  read_again:=(next#empty);
  RETURN next
END PeekKey;

PROCEDURE ShowInfo;
BEGIN
  pleaseinfo:=TRUE;
  IF PeekKey()#empty THEN RETURN END;
  fix; showinfo; pleaseinfo:=FALSE;
END ShowInfo;

PROCEDURE UpdateInfo;
BEGIN fix; showinfo; pleaseinfo:=FALSE END UpdateInfo;

PROCEDURE Pressed?(): BOOLEAN;
BEGIN
  IF out?() THEN RETURN peekmacro()#empty ELSE RETURN key.ready()#0 END
END Pressed?;

PROCEDURE BeginMacro(gold: BOOLEAN);
BEGIN
  IF in?() THEN Bell; RETURN END;
  IF gold THEN
    inform_on("G O L D");
  ELSE
    inform_on("B R O N Z E");
  END;
  StartMacro;
END BeginMacro;

PROCEDURE EndMacro(gold: BOOLEAN; del: INTEGER);
  VAR c: CHAR; i: INTEGER;
BEGIN
  IF NOT in?() THEN Bell; RETURN END;
  REPEAT
    c:=ask(" КНОПКА ДЛЯ МАКРОСА -> ");
  UNTIL NOT Control?(c);
  IF infomode?() THEN ShowInfo ELSE pushandclearinfo; pop END;
  IF gold THEN FinishGold(c,del) ELSE FinishBronze(c,del) END;
  inform_off;
END EndMacro;

PROCEDURE PopMacro(gold: BOOLEAN; c: CHAR);
BEGIN
  IF in?() THEN RETURN END;
  IF NOT Control?(c) THEN
    IF gold THEN GetGoldMacro(c) ELSE  GetBronzeMacro(c) END;
  END;
END PopMacro;

(*-------------------------------------------------------------------*)

TYPE String=ARRAY [0..256] OF CHAR;
     StrPtr=POINTER TO String;

VAR
   (* buffer lines for simple action   *)
   (* Note! One additional char for 0c *)
   bump: String;
   bmpt: String;

(*-------------------------------------------------------------------*)


VAR frmBegL,frmBegC: INTEGER;
    frmEndL,frmEndC: INTEGER;

PROCEDURE frame?(VAR fl,fc,tl,tc: INTEGER);
  VAR l,c: INTEGER;
BEGIN
  IF (frmEndL<0) OR (frmBegL<0) THEN
    fl:=-1; fc:=fl; tl:=fc; tc:=tl; RETURN
  END;
  IF (frmBegL>frmEndL) THEN tl:=frmBegL; fl:=frmEndL;
  ELSE                      fl:=frmBegL; tl:=frmEndL;
  END;
  IF (frmBegC>frmEndC) THEN tc:=frmBegC; fc:=frmEndC;
  ELSE                      fc:=frmBegC; tc:=frmEndC;
  END;
END frame?;

PROCEDURE ResetFrame;
BEGIN
  frmBegL:=-1; frmBegC:=-1;
  frmEndL:=-1; frmEndC:=-1;
END ResetFrame;

PROCEDURE frmErr;
BEGIN message(TRUE,"NO BEGIN/END MARKER") END frmErr;

PROCEDURE frmOver;
BEGIN message(TRUE,"AREAS OVERLAP, INSERTION IMPOSSIBLE") END frmOver;

PROCEDURE frmImpl;
BEGIN message(TRUE,"EXCUSE, NOT IMPLEMENTED") END frmImpl;

PROCEDURE resh0; BEGIN END resh0;

VAR pleaseunmark: BOOLEAN;
    pleasemark  : BOOLEAN;

PROCEDURE JumpBegin;
BEGIN
  IF frmBegL<0 THEN frmErr; RETURN END;
  JumpTo(frmBegL,frmBegC); pleasemark:=TRUE;
END JumpBegin;

PROCEDURE JumpEnd;
BEGIN
  IF frmEndL<0 THEN frmErr; RETURN END;
  JumpTo(frmEndL,frmEndC); pleasemark:=TRUE;
END JumpEnd;

PROCEDURE GetBegin(VAR l,c: INTEGER); VAR i: INTEGER;
BEGIN frame?(l,c,i,i) END GetBegin;

PROCEDURE GetEnd  (VAR l,c: INTEGER); VAR i: INTEGER;
BEGIN frame?(i,i,l,c) END GetEnd;

PROCEDURE mark?;
  VAR l0,c0,l1,c1: INTEGER;
BEGIN
  pleasemark:=FALSE;
  frame?(l0,c0,l1,c1);
  IF (l0<0) OR (l1<0) THEN RETURN END;
  mark(l0-top,c0,l1-top,c1,1);
  pleaseunmark:=TRUE;
END mark?;

PROCEDURE unmark;
  VAR l0,c0,l1,c1: INTEGER;
BEGIN
  pleaseunmark:=FALSE;
  frame?(l0,c0,l1,c1);
  IF (l0<0) OR (l1<0) THEN RETURN END;
  mark(l0-top,c0,l1-top,c1,0);
END unmark;

PROCEDURE MarkBegin;
BEGIN
  Where?(frmBegL,frmBegC); pleasemark:=TRUE;
END MarkBegin;

PROCEDURE MarkEnd;
BEGIN
  Where?(frmEndL,frmEndC); pleasemark:=TRUE;
END MarkEnd;

PROCEDURE MarkLine;
BEGIN
  Where?(frmBegL,frmBegC); frmBegC:=0; pleasemark:=TRUE;
  Where?(frmEndL,frmEndC); frmEndC:=exMem.size?();
END MarkLine;

PROCEDURE RestoreLine;
BEGIN refreshline END RestoreLine;

PROCEDURE frmDelLine;
  VAR fL,fC,tL,tC: INTEGER;
BEGIN
  frame?(fL,fC,tL,tC);
  IF (fL>=0) & (exMem.cur<=fL) THEN
    DEC(frmBegL); DEC(frmEndL); RETURN
  END;
  IF (tL>=0) & (exMem.cur<=tL) THEN
    IF tL=frmBegL THEN DEC(frmBegL)
    ELSE               DEC(frmEndL)
    END;
    DEC(tL);
    IF (tL<fL) OR (tL=fL) & (tC<fC) THEN ResetFrame END; RETURN
  END;
END frmDelLine;

PROCEDURE frmInsLine;
  VAR fL,fC,tL,tC: INTEGER;
BEGIN
  frame?(fL,fC,tL,tC);
  IF (fL>=0) & (exMem.cur<=fL) THEN
    INC(frmBegL); INC(frmEndL); RETURN
  END;
  IF (tL>=0) & (exMem.cur<=tL) THEN
    IF tL=frmBegL THEN INC(frmBegL)
    ELSE               INC(frmEndL)
    END; INC(tL); RETURN
  END;
END frmInsLine;

PROCEDURE frmresh; BEGIN RefreshScreen(TRUE) END frmresh;

PROCEDURE Jump(line,col: INTEGER; resh?: BOOLEAN); FORWARD;

PROCEDURE DelRect;
  VAR l,c: INTEGER;
     line: INTEGER;     fL,fC,tL,tC: INTEGER;
   sz,i,j: INTEGER;
BEGIN frame?(fL,fC,tL,tC);
  l:= fL;  c := fC;
  FOR line:=l TO tL DO
    jump(line); sz:=exMem.size?();
    IF sz>fC THEN exMem.get(bump,sz);
      IF sz<=tC THEN
        bump[fC]:=0c; exMem.put(bump,fC)
      ELSE j:=fC;
        FOR i:=tC+1 TO sz DO
          bump[j]:=bump[i]; INC(j);
        END; exMem.put(bump,j-1);
      END;
    END;
  END;
  Jump(l,c,TRUE);
END DelRect;

PROCEDURE DelFrame(rect: BOOLEAN);
  VAR line: INTEGER;     fL,fC,tL,tC: INTEGER;
    sz,i,j: INTEGER;
BEGIN
  frame?(fL,fC,tL,tC);
  IF (fL<0) OR (tL<0) THEN frmErr; RETURN END;
  IF rect THEN DelRect; RETURN END;
  JumpTo(fL,fC);
  exMem.delete(tL-fL+1,public.high*2,frmresh);
  ResetFrame;
END DelFrame;

PROCEDURE PutRect;
  VAR line: INTEGER;
      step: INTEGER;
     cstep: INTEGER;    fL,fC,tL,tC: INTEGER;
  no,fr,to: INTEGER;
 szt,szf,i: INTEGER;
     c0,c1: INTEGER;
BEGIN
  frame?(fL,fC,tL,tC);
  IF (fL=exMem.cur) & (fC=posc?) THEN RETURN END;
  no:=tL-fL+1;
  IF (fL>exMem.cur) THEN
    step:=+1; fr:=fL; to:=exMem.cur;
  ELSE
    step:=-1; fr:=tL; to:=exMem.cur+no-1;
  END;
  IF (fC>posc?) THEN cstep:=+1 ELSE cstep:=-1 END;
  WHILE no>0 DO
    c0:=posc?;  c1:=c0+tC-fC+1;
    jump(fr); exMem.get(bump,szf);
    jump(to); exMem.get(bmpt,szt);
    FOR i:=szf TO tC DO bump[i]:=' ' END;
    IF cstep>0 THEN
      FOR i:=szt TO c1-1 DO bmpt[i]:=' ' END;
      FOR i:=fC TO tC DO bmpt[c0]:=bump[i]; INC(c0) END;
    ELSE c0:=c1; DEC(c1);
      FOR i:=tC TO fC BY -1 DO bmpt[c1]:=bump[i]; DEC(c1) END;
      FOR i:=szt TO c1 DO bmpt[i]:=' ' END;
    END;
    IF c0>szt THEN bmpt[c0]:=0c; szt:=c0 END;
    exMem.put(bmpt,szt);
    DEC(no); INC(fr,step); INC(to,step);
  END;
END PutRect;

PROCEDURE PutFrame(rect: BOOLEAN);
  VAR line: INTEGER;
      step: INTEGER;
     cstep: INTEGER;    fL,fC,tL,tC: INTEGER;
  no,fr,to: INTEGER;
      sz,i: INTEGER;
BEGIN
  frame?(fL,fC,tL,tC);
  IF (fL<0) OR (tL<0) THEN frmErr; RETURN END;
  IF rect THEN PutRect
  ELSE
    IF (fL=exMem.cur) THEN RETURN END;
    no:=tL-fL+1;
    IF fL>exMem.cur THEN
      step:=+1; fr:=fL; to:=exMem.cur;
    ELSE
      step:=-1; fr:=tL; to:=exMem.cur+no-1;
    END;
    WHILE no>0 DO
      jump(fr); exMem.get(bump,sz);
      jump(to); exMem.put(bump,sz);
      DEC(no); INC(fr,step); INC(to,step);
    END;
  END;
  RefreshScreen(TRUE);
END PutFrame;

PROCEDURE EraRect;
  VAR line: INTEGER;
     no,fr: INTEGER;    fL,fC,tL,tC: INTEGER;
     i,szf: INTEGER;
BEGIN
  frame?(fL,fC,tL,tC);
  no:=tL-fL+1;
  fr:=fL;
  WHILE no>0 DO
    jump(fr); exMem.get(bump,szf);
    IF szf>tC THEN
      FOR i:=fC TO tC DO bump[i]:=' ' END;
    ELSIF szf>fC THEN szf:=fC; bump[fC]:=0c
    END;
    exMem.put(bump,szf);
    DEC(no); INC(fr);
  END;
END EraRect;

PROCEDURE EraFrame(rect: BOOLEAN);
  VAR line: INTEGER;
     no,fr: INTEGER;    fL,fC,tL,tC: INTEGER;
        sz: INTEGER;
BEGIN
  frame?(fL,fC,tL,tC);
  IF (fL<0) OR (tL<0) THEN frmErr; RETURN END;
  IF rect THEN EraRect
  ELSE
    no:=tL-fL+1;
    fr:=fL;
    WHILE no>0 DO
      jump(fr); exMem.get(bump,sz); exMem.put(bump,0);
      DEC(no); INC(fr);
    END;
  END;
  RefreshScreen(TRUE);
END EraFrame;

PROCEDURE InsRect(vert: BOOLEAN);
  VAR line: INTEGER;
      step: INTEGER;
     cstep: INTEGER;    fL,fC,tL,tC: INTEGER;
  no,fr,to: INTEGER;
 szt,szf,i: INTEGER;
       m,j: INTEGER;
     c0,c1: INTEGER;
BEGIN
  frame?(fL,fC,tL,tC);
  c0:=posc?;
  no:=tL-fL+1;
  IF vert THEN
    IF (exMem.cur>=fL) & (exMem.cur<=tL) THEN frmOver; RETURN END;
    exMem.insert(no); IF alarm THEN RETURN END;
    IF exMem.cur<fL THEN
      INC(frmBegL,no); INC(frmEndL,no); frame?(fL,fC,tL,tC);
    END;
  END;
  IF (fL>exMem.cur) THEN
    step:=+1; fr:=fL; to:=exMem.cur;
  ELSE
    step:=-1; fr:=tL; to:=exMem.cur+no-1;
  END;
  IF (fC>posc?) THEN cstep:=+1 ELSE cstep:=-1 END;
  WHILE (no>0) & NOT alarm DO
    c0:=posc?;  c1:=c0+tC-fC+1;
    jump(fr); exMem.get(bump,szf);
    jump(to); exMem.get(bmpt,szt);
    IF NOT vert & (szt>c0) THEN m:=tC-fC+1; i:=szt+m;
      IF i>HIGH(bmpt) THEN i:=HIGH(bmpt) END;
      j:=i-m; szt:=i;
      WHILE i>=c0+m DO
        bmpt[i]:=bmpt[j]; DEC(i); DEC(j);
      END;
    END;
    FOR i:=szf TO tC DO bump[i]:=' ' END;
    IF cstep>0 THEN
      FOR i:=szt TO c1-1 DO bmpt[i]:=' ' END;
      FOR i:=fC TO tC DO bmpt[c0]:=bump[i]; INC(c0) END;
    ELSE c0:=c1; DEC(c1);
      FOR i:=tC TO fC BY -1 DO bmpt[c1]:=bump[i]; DEC(c1) END;
      FOR i:=szt TO c1 DO bmpt[i]:=' ' END;
    END;
    IF c0>szt THEN bmpt[c0]:=0c; szt:=c0 END;
    exMem.put(bmpt,szt);
    DEC(no); INC(fr,step); INC(to,step);
  END;
END InsRect;

PROCEDURE InsFrame(rect,vert: BOOLEAN);
  VAR line: INTEGER;
      step: INTEGER;
     cstep: INTEGER;    fL,fC,tL,tC: INTEGER;
  no,fr,to: INTEGER;
      sz,i: INTEGER;
BEGIN
  frame?(fL,fC,tL,tC);
  IF (fL<0) OR (tL<0) THEN frmErr; RETURN END;
  IF rect THEN InsRect(vert);
  ELSE
    IF (exMem.cur>=fL) & (exMem.cur<=tL) THEN frmOver; RETURN END;
    no:=tL-fL+1;
    exMem.insert(no); IF alarm THEN RETURN END;
    IF exMem.cur<fL THEN
      INC(frmBegL,no); INC(frmEndL,no); frame?(fL,fC,tL,tC);
    END;
    IF fL>exMem.cur THEN
      step:=+1; fr:=fL; to:=exMem.cur;
    ELSE
      step:=-1; fr:=tL; to:=exMem.cur+no-1;
    END;
    WHILE (no>0) & NOT alarm DO
      jump(fr); exMem.get(bump,sz);
      jump(to); exMem.put(bump,sz);
      DEC(no); INC(fr,step); INC(to,step);
    END;
  END;
  RefreshScreen(TRUE);
END InsFrame;

PROCEDURE MovFrame(rect,vert: BOOLEAN);
  VAR line: INTEGER;
      step: INTEGER;
     cstep: INTEGER;    fL,fC,tL,tC: INTEGER;
  no,fr,to: INTEGER;        newBegL: INTEGER;
      sz,i: INTEGER;
BEGIN
  frame?(fL,fC,tL,tC);
  IF (fL<0) OR (tL<0) THEN frmErr; RETURN END;
  IF rect OR vert THEN frmImpl; RETURN END;
  IF (exMem.cur>=fL) & (exMem.cur<=tL) THEN frmOver; RETURN END;
  no:=tL-fL+1;
  exMem.insert(no); IF alarm THEN RETURN END;
  IF exMem.cur<fL THEN
    INC(frmBegL,no); INC(frmEndL,no); frame?(fL,fC,tL,tC);
  END;
  newBegL:=exMem.cur;
  IF fL>exMem.cur THEN
    step:=+1; fr:=fL; to:=exMem.cur;
  ELSE
    step:=-1; fr:=tL; to:=exMem.cur+no-1;
  END;
  WHILE (no>0) & NOT alarm DO
    jump(fr); exMem.get(bump,sz);
    jump(to); exMem.put(bump,sz);
    DEC(no); INC(fr,step); INC(to,step);
  END;
  no:=tL-fL+1;
  jump(fL); exMem.delete(no,-1,resh0);
  IF fL<newBegL THEN DEC(newBegL,no) END;
  IF frmBegL>=frmEndL THEN
    frmBegL:=newBegL;
    frmEndL:=newBegL+no;
  ELSE
    frmBegL:=newBegL+no;
    frmEndL:=newBegL;
  END;
  RefreshScreen(TRUE);
END MovFrame;

PROCEDURE FmtFrame(rect: BOOLEAN);
  VAR l0,c0,l1,c1,par: INTEGER;
      s: ARRAY [0..15] OF CHAR;
BEGIN
  frame?(l0,c0,l1,c1);
  IF (l0<0) OR (l1<0) THEN frmErr; RETURN END;
  WITH public DO
    IF NOT rect THEN c0:=lmargin; c1:=rmargin END;
    par:=c0+fmargin;
    IF    par<0 THEN par:=0
    ELSIF par>rmargin THEN par:=rmargin
    END;
  END;
  inform_on('FORMATING');
  exTex.form(par,l0,c0,l1,c1);
  inform_off;
  frmEndL:=l1;
--jump(l0);
  Jump(l0,c0,TRUE); -- with refresh
END FmtFrame;

PROCEDURE FmtPara;

  PROCEDURE spaces(): INTEGER;
    VAR pStr: StrPtr;  i: INTEGER;
  BEGIN
    IF exMem.size?()=0 THEN RETURN 999999 END;
    pStr:=exMem.adr();  i:=0;
    WHILE pStr^[i]=' ' DO INC(i) END;
    RETURN i
  END spaces;

  PROCEDURE begin(VAR b: INTEGER);
    VAR sps: INTEGER;
  BEGIN
    b:=exMem.cur;
    WHILE (b<last) & (exMem.size?()=0) DO INC(b); jump(b) END;
    IF b=0 THEN RETURN END;
    sps:=spaces();
    jump(b-1);
    IF exMem.size?()=0 THEN RETURN END;
    IF spaces()<sps    THEN RETURN END;
    LOOP
      IF b=0 THEN EXIT END;
      jump(b-1);
      IF exMem.size?()=0 THEN EXIT END;
      DEC(b);
      IF spaces()>sps    THEN EXIT END;
    END;
  END begin;

  PROCEDURE end(VAR e: INTEGER);
    VAR sps: INTEGER;
  BEGIN
    e:=exMem.cur;
    WHILE (e<last) & (exMem.size?()=0) DO INC(e); jump(e) END;
    IF e=last THEN RETURN END;
    jump(e+1);
    IF exMem.size?()=0 THEN RETURN END;
    IF spaces()>sps    THEN RETURN END;
    sps:=spaces();
    INC(e);
    LOOP
      IF e>=last THEN EXIT END;
      jump(e+1);
      IF exMem.size?()=0 THEN EXIT END;
      IF spaces()#sps    THEN EXIT END;
      INC(e);
    END;
  END end;

  VAR l0,c0,l1,c1,par,sav: INTEGER;

BEGIN
  c0:=0; c1:=0; sav:=exMem.cur;
  begin(l0); jump(sav); end(l1); jump(sav);
  WITH public DO
    c0:=lmargin; c1:=rmargin;
    par:=c0+fmargin;
    IF    par<0 THEN par:=0
    ELSIF par>rmargin THEN par:=rmargin
    END;
  END;
  inform_on('FORMATING');
  exTex.form(par,l0,c0,l1,c1);
  inform_off;
  Jump(l1+1,0,TRUE); -- with refresh
END FmtPara;

PROCEDURE Centre(rect: BOOLEAN);
  VAR s: StrPtr; l0,c0,l1,c1: INTEGER;
BEGIN
  IF rect THEN frame?(l0,c0,l1,c1);
    IF (l0<0) OR (l1<0) THEN frmErr; RETURN END;
    exTex.centre(c0,c1);
  ELSE
    exTex.centre(public.lmargin,public.rmargin)
  END;
  s:=exMem.adr();
  ws(s^,exMem.size?());
END Centre;

PROCEDURE setlmargin;
  VAR l,c: INTEGER; s: ARRAY [0..15] OF CHAR;
BEGIN Where?(l,c);
  IF c<public.rmargin THEN public.lmargin:=c END;
  img.print(s,'LEFT = %d',public.lmargin); inform_on(s);
END setlmargin;

PROCEDURE setrmargin;
  VAR l,c: INTEGER; s: ARRAY [0..15] OF CHAR;
BEGIN Where?(l,c);
  IF c>public.lmargin THEN public.rmargin:=c END;
  img.print(s,'RIGHT = %d',public.rmargin); inform_on(s);
END setrmargin;

PROCEDURE setfmargin;
  VAR l,c: INTEGER; s: ARRAY [0..15] OF CHAR;
BEGIN Where?(l,c);
  public.fmargin:=c-public.lmargin;
  img.print(s,'DELTA = %d',public.fmargin); inform_on(s);
END setfmargin;

PROCEDURE JumpToPattern(VAL pat: ARRAY OF CHAR;
                        top?,infrm?,rect?: BOOLEAN);
  VAR l,c,lf,cf: INTEGER;
      fL,fC,tL,tC: INTEGER;
BEGIN
  Where?(l,c);
  lf:=l; cf:=c+1;
  IF infrm? THEN frame?(fL,fC,tL,tC);
    IF (fL<0) OR (tL<0) THEN frmErr; RETURN END;
    exMem.findframe(fL,fC,tL,tC);
    IF (lf<fL) OR (lf>tL) THEN lf:=fL; cf:=0 END;
  ELSE
    exMem.findframe(0,0,last,999999);
  END;
  IF top? THEN lf:=0; cf:=0 END;
  IF exMem.find(lf,cf,rect?,pat) THEN
    JumpTo(lf,cf)
  ELSE
    message(TRUE,"Pattern not found ");
    JumpTo(l,c)
  END;
END JumpToPattern;

PROCEDURE setbound(col: INTEGER); FORWARD;
PROCEDURE sync; FORWARD;

PROCEDURE writestr(VAL s: ARRAY OF CHAR);
BEGIN ws(s,stringsize(s)) END writestr;

PROCEDURE replace(VAL pat,rep: ARRAY OF CHAR; VAR fromL,fromC: INTEGER;
                                         rect?: BOOLEAN): BOOLEAN;
  VAR     lf,cf: INTEGER;     all: BOOLEAN;
     co, pred,i: INTEGER;     bot: INTEGER;
     sz,szP,szR: INTEGER;      ch: CHAR;     k: CHAR;
     P: STRING;

  PROCEDURE sz?(VAL s: ARRAY OF CHAR): INTEGER;
    VAR i: INTEGER;
  BEGIN i:=0;
    WHILE (i<HIGH(s)) & (s[i]#0c) DO INC(i) END; RETURN i
  END sz?;

  PROCEDURE replaceone;
    VAR i: INTEGER;
  BEGIN
    IF (cf+szR>=HIGH(bump)) OR
       (cf+szP>=HIGH(bump)) OR
       (sz+szR>=HIGH(bump)) OR
       (sz+szP>=HIGH(bump)) THEN RETURN END;
    FOR i:=0  TO cf-1     DO bmpt[i]:=bump[i]    END;
    FOR i:=cf TO cf+szR-1 DO bmpt[i]:=rep[i-cf]  END;
    INC(sz,szR-szP);
    IF sz>HIGH(bmpt) THEN sz:=HIGH(bmpt) END;
    FOR i:=cf+szR TO sz-1 DO bmpt[i]:=bump[i-szR+szP] END;
    bmpt[sz]:=0c;
  END replaceone;

  PROCEDURE AskForReplace(): CHAR;
    VAR ch: CHAR;
  BEGIN
    IF leftbound?()>cf THEN setbound(cf)
    ELSE
      setbound(cf+(szP+szR+ABS(szR-szP)) DIV 2);
    END;
    pos(bot,0);   writestr(bump);
    pos(bot+1,0); writestr(bmpt);
    pos(bot+2,0); clearln; pos(bot+2,cf); w("^");
    REPEAT
      k:=ask("ЗАМЕНИТЬ (Y/N)  |  ПРЕКРАТИТЬ (Q)  |  ВСЕ ОСТАЛЬНЫЕ (A) ? ");
      ch:=CAP(k);
      all:=(ch='A');
    UNTIL (ch='Y') OR (ch='N') OR (ch='Q') OR all;
    RETURN ch;
  END AskForReplace;

BEGIN
  szP:=sz?(pat);
  szR:=sz?(rep);
(*$<*) (*$U+*)
  P^.ADR:=sys.ADR(pat); P^.HIGH:=szP;
(*$>*)
  IF (szP=0) THEN RETURN FALSE END;
  lf:=fromL; cf:=fromC;
  IF NOT exMem.find(lf,cf,rect?,pat) THEN
    message(TRUE,"Pattern not found"); RETURN FALSE
  END;
  bot:=public.high-2-public.dwmargin-2;
  FOR i:=bot-2 TO public.high DO pos(i,0); clearln END;
  FOR i:=0 TO HIGH(bump) DO bump[i]:='-' END;
  bump[HIGH(bump)]:=0c;
  pos(bot-3,0); writestr(bump);

  all:=FALSE; pred:=lf; co:=0;
  WHILE exMem.find(lf,cf,rect?,pat) DO
    IF (lf#pred) & (co>0) THEN
      pos(bot,0); writestr(bmpt); co:=0; pred:=lf;
      rollup("",0); pleaseinfo:=TRUE;
    END;
    jump(lf); exMem.get(bump,sz); replaceone;
    IF NOT all THEN
      ch:=AskForReplace();
      IF (ch='Q') THEN RETURN TRUE END;
    END;
    IF all OR (ch='Y') THEN
      exMem.put(bmpt,sz); fromL:=lf; fromC:=cf; INC(co);
    ELSE bmpt:=bump
    END;
    IF all OR (ch#'N') THEN INC(cf,szR) ELSE INC(cf,szP) END;
    IF all THEN ch:=CAP(PeekKey());
      IF ch='Q' THEN ReadKey(ch); RETURN TRUE END;
    END;
  END; RETURN TRUE
END replace;

PROCEDURE Replace(VAL pat,rep: ARRAY OF CHAR; top?,infrm?,rect?: BOOLEAN);
  VAR b,lf,cf: INTEGER;  im: BOOLEAN;
  fL,fC,tL,tC: INTEGER;
BEGIN
  Where?(lf,cf); b:=leftbound?();
  IF infrm? THEN frame?(fL,fC,tL,tC);
    IF (fL<0) OR (tL<0) THEN frmErr; RETURN END;
    exMem.findframe(fL,fC,tL,tC);
    IF (lf<fL) OR (lf>tL) THEN lf:=fL; cf:=0 END;
  ELSE
    exMem.findframe(0,0,last,999999);
  END;
  IF top? THEN lf:=0; cf:=0 END;

  im:=infomode?(); infomode(FALSE);
  IF replace(pat,rep,lf,cf,rect?) THEN
    leftbound(b); top:=exMem.maxline(); sync; JumpTo(lf,cf);
  END;
  infomode(im);
END Replace;

(*-------------------------------------------------------------------*)


VAR filename: ARRAY [0..63] OF CHAR;

PROCEDURE windowname;
  VAR i: INTEGER; s: ARRAY [0..63] OF CHAR;
BEGIN
  i:=nocolumns-4-fnpos;
  IF img.len(filename)<i THEN
    img.print(s,'%|*.*s',i,i,filename);
  ELSE
    img.print(s,'%|*.*s ...',i-4,i-4,filename);
  END;
  setinfostr(fnpos,s);
END windowname;

PROCEDURE SetName(VAL s: ARRAY OF CHAR);
BEGIN img.print(filename,'%s',s); windowname END SetName;

PROCEDURE GetName(VAR s: ARRAY OF CHAR);
BEGIN img.print(s,'%s',filename) END GetName;

PROCEDURE sync;
BEGIN jump(top+posl?) END sync;

PROCEDURE FillScreen(k: CHAR);
  VAR i,sav,sz: INTEGER;  pStr: StrPtr;
BEGIN
  sav:=exMem.cur;
  push;
  tty.set_cursor(0);
  pos(0,0);
  FOR i:=0 TO public.high-2 DO
    posl(i); jump(top+i); pStr:=exMem.adr(); ws(pStr^,exMem.size?());
    IF ODD(i DIV 2) & (k#empty) & (PeekKey()=k) THEN
      jump(sav); pop; sync;
      tty.set_cursor(1);
      RETURN
    END;
  END;
  pop; jump(sav); sync;
  tty.set_cursor(1)
END FillScreen;

PROCEDURE SilentScreen;
  VAR i,sav,sz: INTEGER;  pStr: StrPtr;
BEGIN
  sav:=exMem.cur;
  push; pos(0,0);
  FOR i:=0 TO public.high-2 DO
    jump(top+i); pStr:=exMem.adr(); setline(i,pStr^,exMem.size?());
  END;
  pop; jump(sav);
END SilentScreen;

VAR stopkey: CHAR;

PROCEDURE Stop(): BOOLEAN;  BEGIN RETURN PeekKey()=stopkey END Stop;

PROCEDURE stopwhen(k: CHAR);
BEGIN stopkey:=k; stop:=Stop END stopwhen;

PROCEDURE otherthan(k: CHAR): BOOLEAN;
  VAR ch: CHAR;
BEGIN
  IF k=empty THEN RETURN TRUE END;
  IF PeekKey()#k THEN RETURN TRUE ELSE ReadKey(ch); RETURN ch#k END
END otherthan;

PROCEDURE Left(left: CHAR);
  VAR l,c,nc: INTEGER;
BEGIN
  REPEAT c:=posc?;
    LOOP DEC(c);
      IF c<0 THEN
        Up(empty); c:=size(); stopwhen(left); setbound(c); posc(c);
      ELSE
        posc(c); nc:=posc?;
        IF  posc?=c THEN EXIT END;
        stopwhen(left); leftbound(leftbound?()-public.adjust); posc(c); fix;
      END;
      IF otherthan(left) THEN EXIT END;
    END;
    IF PleaseRefresh() THEN stopwhen(left); refresh END;
  UNTIL otherthan(left);
END Left;

PROCEDURE Right(right: CHAR);
  VAR c,nc: INTEGER;
BEGIN
  REPEAT c:=posc?;
    LOOP
      INC(c); posc(c); nc:=posc?;
      IF c-1=maxcol THEN RETURN END;
      IF nc=c       THEN EXIT END;
      stopwhen(right); leftbound(leftbound?()+public.adjust); posc(c); fix;
      IF otherthan(right) THEN EXIT END;
    END;
    IF PleaseRefresh() THEN stopwhen(right); refresh END;
  UNTIL otherthan(right);
END Right;

PROCEDURE Rtab(rtab: CHAR);
  VAR c: INTEGER;
BEGIN
  c:=(posc? DIV 8)*8 + 8;  posc(c);
  IF posc?#c THEN
    REPEAT
      IF posc?=maxcol THEN RETURN END;
      stopwhen(rtab); leftbound(leftbound?()+8); posc(c); fix;
    UNTIL otherthan(rtab);
  END;
END Rtab;

PROCEDURE Ltab(ltab: CHAR);
  VAR c,nc: INTEGER;
BEGIN
  c:=(posc?+7) DIV 8 * 8 - 8; posc(c);
  IF posc?#c THEN
    REPEAT
      IF posc?=0 THEN RETURN END;
      stopwhen(ltab); leftbound(leftbound?()-8); posc(c); fix;
    UNTIL otherthan(ltab);
  END;
END Ltab;

PROCEDURE Del(del: CHAR);
BEGIN
  REPEAT
    REPEAT
      IF posc?=0 THEN RETURN END;
      stopwhen(del);  rubout;
    UNTIL otherthan(del);
    IF PleaseRefresh()  THEN stopwhen(del); refresh END;
  UNTIL otherthan(del);
END Del;

PROCEDURE writeStop(): BOOLEAN;
  VAR k: CHAR;
BEGIN k:=PeekKey(); RETURN NOT Control?(k) END writeStop;

(*------------------------------------------------------------*)

PROCEDURE save;
  VAR p: StrPtr;
BEGIN
  sync;
  IF (size()=0) & (exMem.size?()=0) THEN RETURN END;
  p:=adr(); exMem.put(p^,size())
END save;

PROCEDURE rolling?;
  VAR l: INTEGER; ptr: StrPtr;
BEGIN
  l:=posl?;
  IF l>public.high-2-public.dwmargin THEN posl(l-1);
    INC(top); jump(top+public.high-2); ptr:=exMem.adr();
    rollup(ptr^,exMem.size?());
    pleaseinfo:=TRUE; sync;
  END;
  IF (top>0) & (l<public.upmargin) THEN posl(l+1);
    DEC(top); jump(top); ptr:=exMem.adr();
    rolldw(ptr^,exMem.size?()); pleaseinfo:=TRUE; sync;
  END;
END rolling?;

PROCEDURE Up(up: CHAR);
BEGIN
  REPEAT
    IF (posl?=0) & (top=0) THEN RETURN END; posl(posl?-1); rolling?;
  UNTIL otherthan(up);
END Up;

PROCEDURE Dw(dw: CHAR);
BEGIN
  REPEAT posl(posl?+1); rolling? UNTIL otherthan(dw);
END Dw;

VAR lastDLln,lastDLcl: INTEGER;

PROCEDURE DelLn(delln: CHAR; wrap: BOOLEAN);
  VAR ptr: StrPtr; i: INTEGER;
BEGIN
  IF (exMem.cur>last) THEN RETURN END;
  REPEAT
    IF wrap THEN
      push; posl(posl?+1); pullright; pullleft; pop;
    END;
    stopwhen(delln);
    jump(top+public.high-1); ptr:=exMem.adr();
    dl(ptr^,exMem.size?());
    sync; frmDelLine;
    Where?(lastDLln,lastDLcl);
    exMem.delete(1,-1,resh0); save;
  UNTIL (exMem.cur>last) OR otherthan(delln); pleaseinfo:=TRUE;
END DelLn;

PROCEDURE UnDel;
  VAR szu,szd: INTEGER;
BEGIN
  IF lastDLln<0 THEN RETURN END;
  JumpTo(lastDLln,lastDLcl);
  undel(bump,bmpt); il; pleaseinfo:=TRUE;
  exMem.insert(1);
  szu:=stringsize(bump); ws(bump,szu);
  exMem.put(bump,szu);
  exMem.jump(exMem.cur+1);     posl(posl?+1);
  szd:=stringsize(bmpt); ws(bmpt,szd);
  exMem.put(bmpt,szd);   posl(posl?-1); sync;
  lastDLln:=-1;
END UnDel;

VAR old: String;
   old?: BOOLEAN;
  oldno: INTEGER;

PROCEDURE SwapOld;
BEGIN
  IF old? & (oldno=exMem.cur) THEN
    bump:=old; get(old); ws(bump,stringsize(bump));
  END;
END SwapOld;

PROCEDURE updatenext;
  VAR l: INTEGER; p: StrPtr;
BEGIN l:=posl?; sync; p:=adrline(l+1);
  jump(exMem.cur+1); exMem.put(p^,linesize(l+1));  sync;
END updatenext;

PROCEDURE updateprev;
  VAR l: INTEGER; p: StrPtr;
BEGIN l:=posl?; sync; p:=adrline(l-1);
  jump(exMem.cur-1); exMem.put(p^,linesize(l-1));  sync;
END updateprev;

PROCEDURE setbou(c: INTEGER): BOOLEAN;
  VAR lb: INTEGER;
BEGIN lb:=leftbound?();
  IF c<0 THEN c:=0 END;
  IF c<leftbound?() THEN leftbound(c DIV 8 * 8); fix;
    RETURN leftbound?()#lb
  END;
  IF c>leftbound?()+public.width-8 THEN
    c:=c-(public.width-8); leftbound(c DIV 8 * 8); fix;
    RETURN leftbound?()#lb
  END;
  RETURN FALSE;
END setbou;

PROCEDURE setbound(c: INTEGER);
BEGIN IF setbou(c) THEN END END setbound;

PROCEDURE InsLn(insln: CHAR; wrap: BOOLEAN);
  VAR i,sz: INTEGER;
BEGIN
  IF exMem.cur>last THEN RETURN END;
  REPEAT stopwhen(insln); il; frmInsLine;
    exMem.insert(1); save;
    IF wrap THEN
      get(bump); sz:=size(); i:=0;
      WHILE (i<sz) & (bump[i]=' ') DO INC(i) END;
      IF i>=sz THEN i:=0 END;
      setbound(i); pos(posl?+1,i); pullleft; save; rolling?;
    ELSE
      updatenext;
    END;
  UNTIL (exMem.cur>last) OR otherthan(insln);  pleaseinfo:=TRUE;
END InsLn;

PROCEDURE DupLn(dupln: CHAR);
BEGIN
  IF exMem.cur>last THEN RETURN END;
  REPEAT
    stopwhen(dupln); dup;  sync; frmInsLine;
    exMem.insert(1); save; rolling?;
  UNTIL (exMem.cur>last) OR otherthan(dupln);  pleaseinfo:=TRUE;
END DupLn;

PROCEDURE DupOver;
BEGIN
  IF exMem.cur>last THEN RETURN END;
  dupover; sync; save; rolling?;
END DupOver;

PROCEDURE SwapUp(upswp: CHAR);
BEGIN
  IF exMem.cur=0 THEN RETURN END;
  REPEAT
    stopwhen(upswp); swapup; updatenext; rolling?
  UNTIL  otherthan(upswp);
END SwapUp;

PROCEDURE SwapDw(dwswp: CHAR);
BEGIN
  REPEAT
    stopwhen(dwswp); swapdw; updateprev; rolling?
  UNTIL  otherthan(dwswp);
END SwapDw;

PROCEDURE PullLeft;  BEGIN pullleft  END PullLeft;
PROCEDURE PullRight; BEGIN pullright END PullRight;

PROCEDURE middle(): INTEGER;
BEGIN
  WITH public DO RETURN upmargin+(high-2-upmargin-dwmargin) DIV 2 END
END middle;

PROCEDURE PageUp(pg: CHAR);
  VAR l,ncur: INTEGER;  ptr: StrPtr;

  PROCEDURE  pageroll(newcur: INTEGER);
  BEGIN
    posl(public.upmargin); sync;
    push; pos(0,0); sync;
    WHILE (exMem.cur+public.upmargin>newcur) & (top>0) DO
      DEC(top); jump(top); ptr:=exMem.adr();
      rolldw(ptr^,exMem.size?()); pleaseinfo:=TRUE;
      IF (newcur>public.upmargin) & (top MOD 4 = 0)
       & (key.ready()#0) & (PeekKey()=pg) THEN
        pop; sync; RETURN
      END;
    END;
    pop; sync;
    posl(posl?-(exMem.cur-newcur)); sync
  END pageroll;

  PROCEDURE pageskip(newcur: INTEGER);
  BEGIN
    posl(public.upmargin); sync;
    top:=top-(exMem.cur-newcur); sync;
    IF top<=0 THEN top:=0; sync;
      FillScreen(empty);
    ELSE
      fix;
      IF pleaseinfo THEN showinfo; pleaseinfo:=FALSE END;
      IF PeekKey()=pg THEN sync; RETURN END;
      FillScreen(pg)
    END;
    posl(posl?-(exMem.cur-newcur)); sync
  END pageskip;

BEGIN
  l:=posl?;
  IF exMem.cur=0 THEN RETURN END;
  IF exMem.cur>=last+public.high*2 THEN
    posl(public.upmargin); DEC(top,public.pagesize); sync; RETURN
  ELSIF (l-public.pagesize>=public.upmargin) OR (top=0) THEN
    posl(l-public.pagesize); sync;
    IF otherthan(pg) THEN RETURN END;
  END;
  ncur:=exMem.cur-public.pagesize;  IF ncur<0 THEN ncur:=0 END;
  pageroll(ncur);
  IF otherthan(pg) THEN RETURN END;
  REPEAT
    DEC(ncur,public.pagesize);  IF ncur<0 THEN ncur:=0 END;
    IF top=0 THEN RETURN END;
    pageskip(ncur);
  UNTIL otherthan(pg);
END PageUp;

PROCEDURE PageDw(pg: CHAR);
  VAR l,ncur,max,bot: INTEGER;  ptr: StrPtr;

  PROCEDURE pageroll(newcur: INTEGER);
  BEGIN
    posl(bot);
    push; pos(public.high,0); sync;
    WHILE exMem.cur-public.dwmargin<newcur DO
      INC(top);
      jump(top+public.high-2); ptr:=exMem.adr();
      rollup(ptr^,exMem.size?()); pleaseinfo:=TRUE;
      IF (newcur<max) & (top MOD 4 # 0)
       & (key.ready()#0) & (PeekKey()=pg) THEN
        pop; sync; RETURN
      END;
    END;
    pop; sync;
  END pageroll;

  PROCEDURE pageskip(newcur: INTEGER);
  BEGIN
    top:=top+(newcur-exMem.cur); sync;
    IF top>last THEN FillScreen(empty); RETURN END;
    fix;
    IF pleaseinfo THEN showinfo; pleaseinfo:=FALSE END;
    IF PeekKey()=pg THEN sync; RETURN END;
    FillScreen(pg);
  END pageskip;

BEGIN max:=exMem.maxline()-2*public.high;
  IF exMem.cur>=max THEN RETURN END;
  l:=posl?; bot:=public.high-2-public.dwmargin;
  IF    exMem.cur>last+public.high*2 THEN
    INC(top,public.pagesize); sync; RETURN
  ELSIF (l+public.pagesize<=bot) THEN
    posl(l+public.pagesize); sync;
    IF otherthan(pg) THEN RETURN END;
  END;
  ncur:=exMem.cur+public.pagesize;
  IF ncur>=max THEN ncur:=max END;
  pageroll(ncur);
  IF otherthan(pg) THEN RETURN END;
  REPEAT
    IF (exMem.cur>=max) OR (top>last) THEN RETURN END;
    ncur:=ncur+public.pagesize;
    IF ncur>=max THEN ncur:=max END;
    pageskip(ncur);
  UNTIL otherthan(pg);
END PageDw;

PROCEDURE CarriageReturn(cr: CHAR);
BEGIN
  REPEAT
    IF ins?() THEN
      InsLn(empty,TRUE); fix;
    ELSE
      IF posc?=leftbound?() THEN leftbound(0) END; posc(0); RETURN
    END;
  UNTIL otherthan(cr);
END CarriageReturn;

PROCEDURE LineFeed(lf: CHAR);
  VAR l: INTEGER;
BEGIN
  REPEAT
    IF size()>posc? THEN clearln END; (*!!!*)
    save; leftbound(0); pos(posl?+1,0); rolling?;
  UNTIL otherthan(lf);
END LineFeed;

PROCEDURE NewLine(newln: CHAR);
BEGIN
  REPEAT leftbound(0); posc(0); Dw(newln) UNTIL otherthan(newln);
END NewLine;

PROCEDURE ClearTail;
BEGIN IF size()>posc? THEN clearln END END ClearTail;

PROCEDURE ValidChar(k: CHAR);
  VAR l,c,lb: INTEGER;
BEGIN
  IF Control?(k) THEN
    IF k=7c THEN bell(NOT bell?()) END;
    RETURN
  END;
  LOOP
    LOOP
      IF ins?() & (size()=maxcol+1) THEN
        push; InsLn(empty,FALSE); lb:=leftbound?(); leftbound(0); posc(0);
        Dw(empty); pullleft; save; leftbound(lb); pop; sync; fix;
      END;
      lb:=leftbound?();
      w(k);
      IF lb#leftbound?() THEN fix END;
      IF c=maxcol THEN
        Dw(empty); leftbound(0); posc(0); sync; fix;
      END;
      k:=PeekKey();
      IF Control?(k) THEN EXIT END;
      ReadKey(k);
    END;
    IF PleaseRefresh() THEN stop:=writeStop; refresh END;
    k:=PeekKey();
    IF Control?(k) THEN EXIT END;
    ReadKey(k);
  END;
END ValidChar;

PROCEDURE InsCh(ch: CHAR);
  VAR lb: INTEGER;
BEGIN
  IF (size()<maxcol+1) OR NOT ins?() THEN ic(ch); RETURN END;
  push; InsLn(empty,FALSE); lb:=leftbound?(); leftbound(0); posc(0);
  Dw(empty); pullleft; save; leftbound(lb); pop; sync; fix;
  ic(ch);
END InsCh;

PROCEDURE DelCh; BEGIN dc END DelCh;

PROCEDURE DelWord0; BEGIN delword0    END DelWord0;
PROCEDURE DelWord1; BEGIN delword1    END DelWord1;
PROCEDURE RubWord0; BEGIN ruboutword0 END RubWord0;
PROCEDURE RubWord1; BEGIN ruboutword1 END RubWord1;

PROCEDURE WordLeft1;
BEGIN skipwordleft1  END WordLeft1;

PROCEDURE WordRight1;
BEGIN skipwordright1 END WordRight1;

PROCEDURE WordLeft0;
BEGIN skipwordleft0 END WordLeft0;

PROCEDURE WordRight0;
BEGIN skipwordright0 END WordRight0;

PROCEDURE setup;
BEGIN
  frame(public.high,public.width);
  infomode(public.info); ins(public.ins); bell(public.bell);
  windowname;
END setup;

PROCEDURE SETUP;
BEGIN
  save;
  public.info:=infomode?(); public.ins:=ins?(); public.bell:=bell?();
  pushandclearinfo;
  set_up; setup; pop; RefreshScreen(TRUE);
  inform_off;
  pleaseinfo:=TRUE;
END SETUP;

PROCEDURE Peep;
BEGIN
END Peep;

PROCEDURE InsRep(mode: INTEGER);
BEGIN
  CASE mode+1 OF
  |1: ins(NOT ins?())
  |0: ins(FALSE)
  |2: ins(TRUE)
  ELSE
  END;
END InsRep;

PROCEDURE GetKey(): CHAR;
  VAR k: CHAR; t: INTEGER;
BEGIN
  sync; Time;
  IF exMem.cur#oldno THEN old?:=FALSE END;
  IF NOT old? THEN old?:=TRUE; get(old); oldno:=exMem.cur END;
  IF NOT alarm THEN save ELSE SilentScreen; refresh; alarm:=FALSE END;
  IF PleaseRefresh() THEN refresh END;
  IF pleasemark THEN mark? END;
  IF exMem.cur>=exMem.maxline()-public.high THEN PageUp(empty) END;
  IF NOT out?() & pleaseinfo THEN
    key.wait(750);
    IF NOT read_again & (key.ready()=0) THEN ShowInfo END
  ELSE
    IF NOT read_again & (key.ready()=0) THEN fix END
  END;
  ReadKey(k);
  IF pleaseunmark THEN unmark END;
  RETURN k
END GetKey;

PROCEDURE RefreshScreen(mem: BOOLEAN);
BEGIN
  IF mem THEN FillScreen(empty) ELSE refresh END;
END RefreshScreen;

PROCEDURE Jump(line,col: INTEGER; resh: BOOLEAN);
  VAR l,c: INTEGER;
BEGIN
  l:=posl?; c:=posc?;
  IF (exMem.cur=line) & (c=col) THEN
    IF resh THEN RefreshScreen(TRUE) END;
    RETURN
  END;
  IF line<0 THEN line:=0 END;
  IF line>exMem.maxline()-2*public.high THEN
     line:=exMem.maxline()-2*public.high
  END;
  WITH public DO  l:=top+upmargin;
    IF top=0 THEN l:=0 END;
    IF (line>=l) & (line<=top+high-2-dwmargin) THEN
      IF resh THEN SilentScreen END;
      IF NOT setbou(col) & resh THEN refresh END;
      l:=posl?; c:=posc?;  pos(l+(line-exMem.cur),col); sync;
      RETURN
    END;
    IF line<middle() THEN top:=0
    ELSE top:=line-middle()
    END;
    SilentScreen;
    IF NOT setbou(col) THEN refresh END;
    pos(line-top,col); sync;
  END;
END Jump;

PROCEDURE JumpTo(line,col: INTEGER);
BEGIN Jump(line,col,FALSE) END JumpTo;

PROCEDURE Where?(VAR line,col: INTEGER);
BEGIN
  sync; line:=top+posl?; col:=posc?
END Where?;

PROCEDURE FileTop;
BEGIN JumpTo(0,0) END FileTop;

PROCEDURE FileBottom;
  VAR c: INTEGER;
BEGIN
  exMem.jump(last);  c:=exMem.size?(); sync;  JumpTo(last,c);
END FileBottom;

PROCEDURE LineLeft;
  VAR i,sz: INTEGER;
BEGIN
  sz:=size(); i:=0;
  IF sz>0 THEN get(bump);
    WHILE (i<sz) & (bump[i]=' ') DO INC(i) END;
    IF i=sz THEN i:=0 END;
  END;
  IF posc?=i THEN RETURN END;
  setbound(i); posc(i)
END LineLeft;

PROCEDURE LineRight;
  VAR c: INTEGER;
BEGIN
  IF posc?=size() THEN RETURN END;
  c:=size();
  setbound(c); posc(c)
END LineRight;


VAR ALARM: BOOLEAN;

PROCEDURE alarm_mode(on: BOOLEAN);
BEGIN ALARM:=on END alarm_mode;

PROCEDURE Fault;
  VAR p: StrPtr; i,sos: INTEGER;
BEGIN
  IF NOT ALARM THEN FINISH; RETURN END;
  FINISH;
(*
  StdIO.print("FAULT in EDITOR!!!\n");
  IF NOT StdIO.QueryLn("try save text?") THEN RETURN END;
  sos:=StdIO.Create("EX.SOS");
  FOR i:=0 TO last DO
    exMem.jump(i); p:=exMem.adr(); StdIO.PutS(sos,p^); StdIO.PutC(sos,NL);
    IF i MOD 32 = 0 THEN StdIO.print("%d\r",i) END;
  END;
  sos:=StdIO.Close(sos);
  StdIO.print("Text saved in file ./EX.SOS  \n");
*)
END Fault;

PROCEDURE FINISH;
BEGIN
  ALARM:=FALSE;
  infomode(FALSE); pushandclearinfo;
  tty.set_cursor(1);
  tty.set_color(0);
  tty.set_reverse(0);
  tty.up(2);
  tty.erase(0)
END FINISH;

BEGIN
  read_again:=FALSE;
  break_mode(FALSE);
  alarm_mode(FALSE);
  infomode(FALSE); SetName(""); inform_off;
  ResetFrame;
  key.set_break(0);
  top:=0; clearscr; setup; pleaseinfo:=FALSE;
  lastDLln:=-1; lastDLcl:=-1; oldno:=-1; old?:=FALSE; old[0]:=0c;
  next:=empty;
  pleaseunmark:=FALSE;
  pleasemark:=FALSE;
  env.final(Fault);
END exMain.
