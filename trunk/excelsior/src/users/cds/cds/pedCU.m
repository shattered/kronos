IMPLEMENTATION MODULE pedCU;(*$U+ Sem 06-Mar-87. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADR, ADDRESS, WORD;

IMPORT  cod : defCodes;
IMPORT  mem : libHeap;
IMPORT  err : libCrash;
IMPORT  mod : pedModel;

FROM pedModel    IMPORT signal, board, seg_rec;

CONST
  Clearens=8; Overlay=2;
  mem_fail = 'Не хватило памяти.';

VAR mask : BITSET;

(* bits
  0 - fixed
  1 - main direction
*)

PROCEDURE unpack(s: signal; n: INTEGER; VAR sg: segment);
BEGIN
  WITH sg DO
    IF (n<0)OR(n>HIGH(s^.cu)) THEN size:=0; RETURN END;
    WITH s^.cu[n] DO
      IF c=0 THEN size:=0; RETURN END;
      size:=INTEGER(BITSET(c<<12)*{0..11});
      IF 1 IN BITSET(c) THEN
        x1:=INTEGER(BITSET(a)*mask)+size-8000h;
        y1:=INTEGER(BITSET(a>>16)*mask)+size-8000h;
        x2:=INTEGER(BITSET(b)*mask)-size-8000h;
        y2:=INTEGER(BITSET(b>>16)*mask)-size-8000h;
      ELSE
        x1:=INTEGER(BITSET(a)*mask)+size-8000h;
        y2:=INTEGER(BITSET(a>>16)*mask)+size-8000h;
        x2:=INTEGER(BITSET(b)*mask)-size-8000h;
        y1:=INTEGER(BITSET(b>>16)*mask)-size-8000h;
      END;
      lays:=BITSET(c>>12)*{0..7};
      pipe:=INTEGER(BITSET(c>>4)*{0..7});
      fix :=0 IN BITSET(c);
    END;
  END;
END unpack;

PROCEDURE pack_seg(VAL sg: segment; VAR s: seg_rec);
  VAR x1,x2,y1,y2: INTEGER;
BEGIN
  WITH s DO
    c:=0;
    IF sg.x1>sg.x2 THEN
      x1:=sg.x2-sg.size+8000h; x2:=sg.x1+sg.size+8000h;
      IF sg.y1>sg.y2 THEN
        y1:=sg.y2-sg.size+8000h; y2:=sg.y1+sg.size+8000h;
        c:=INTEGER(BITSET(c)+{1});
      ELSE
        y1:=sg.y1-sg.size+8000h; y2:=sg.y2+sg.size+8000h;
      END;
    ELSE
      x1:=sg.x1-sg.size+8000h; x2:=sg.x2+sg.size+8000h;
      IF sg.y1>sg.y2 THEN
        y1:=sg.y2-sg.size+8000h; y2:=sg.y1+sg.size+8000h;
      ELSE
        y1:=sg.y1-sg.size+8000h; y2:=sg.y2+sg.size+8000h;
        c:=INTEGER(BITSET(c)+{1});
      END;
    END;
    IF x1<0 THEN x1:=0 END;
    IF x2<0 THEN x2:=0 END;
    IF y1<0 THEN y1:=0 END;
    IF y2<0 THEN y2:=0 END;
    IF x1>0FFFFh THEN x1:=0FFFFh END;
    IF x2>0FFFFh THEN x2:=0FFFFh END;
    IF y1>0FFFFh THEN y1:=0FFFFh END;
    IF y2>0FFFFh THEN y2:=0FFFFh END;
    IF sg.fix THEN c:=INTEGER(BITSET(c)+{0}) END;
    INC(c,INTEGER(sg.size>>12)+INTEGER(sg.lays<<12)+INTEGER(sg.pipe<<4));
    a:=x1+INTEGER(y1<<16);
    b:=x2+INTEGER(y2<<16);
  END;
END pack_seg;

PROCEDURE pack(s: signal; n: INTEGER; VAL sg: segment);
BEGIN
  pack_seg(sg,s^.cu[n]);
END pack;

PROCEDURE app_n(s: signal; VAL sg: segment): INTEGER;
  VAR i,n: INTEGER; t: mem.pAREA;
  CONST delta=4;
BEGIN
  n:=0; t:=mem.tag?(s);
  IF HIGH(s^.cu)<0 THEN
    mem.alloc_tag(s^.cu^.ADR,SIZE(seg_rec)*delta,t^);
    IF s^.cu^.ADR=NIL THEN err.raise(mem_fail) END;
    s^.cu^.HIGH:=delta-1;
    FOR i:=0 TO delta-1 DO s^.cu[i].c:=0 END;
  ELSE
    LOOP
      IF s^.cu[n].c=0 THEN EXIT END;
      INC(n);
      IF n>HIGH(s^.cu) THEN
        IF NOT mem.realloc(s^.cu^.ADR,SIZE(seg_rec)*(s^.cu^.HIGH+1+delta)) THEN
          err.raise(mem_fail);
        END;
        s^.cu^.HIGH:=s^.cu^.HIGH+delta;
        FOR i:=HIGH(s^.cu)-delta+1 TO HIGH(s^.cu) DO s^.cu[i].c:=0 END;
        EXIT;
      END;
    END;
  END;
  pack_seg(sg,s^.cu[n]);
  RETURN n;
END app_n;

PROCEDURE app(s: signal; VAL sg: segment);
BEGIN
  IF app_n(s,sg)=0 THEN END;
END app;

PROCEDURE del(s: signal; n: INTEGER);
BEGIN
  WITH s^.cu[n] DO a:=0; b:=0; c:=0 END;
END del;

PROCEDURE extend(s: signal; n: INTEGER);
  VAR t: mem.pAREA; i: INTEGER;
BEGIN
  IF HIGH(s^.cu)+1>=n THEN RETURN END;
  t:=mem.tag?(s);
  IF HIGH(s^.cu)<0 THEN
    mem.alloc_tag(s^.cu^.ADR,SIZE(seg_rec)*n,t^);
    IF s^.cu^.ADR=NIL THEN err.raise(mem_fail) END;
    s^.cu^.HIGH:=n-1;
    FOR i:=0 TO n-1 DO s^.cu[i].c:=0 END;
  ELSE
    IF NOT mem.realloc(s^.cu^.ADR,SIZE(seg_rec)*n) THEN
      err.raise(mem_fail);
    END;
    FOR i:=HIGH(s^.cu)+1 TO n-1 DO s^.cu[i].c:=0 END;
    s^.cu^.HIGH:=n-1;
  END;
END extend;

PROCEDURE truncate(s: signal);
  VAR
    i,j,k: INTEGER;
    sa: POINTER TO ARRAY [0..0FFFFFh] OF seg_rec;
    t: mem.pAREA;
BEGIN
  FOR i:=0 TO HIGH(s^.cu) DO IF s^.cu[i].c#0 THEN INC(j) END END;
  IF j=HIGH(s^.cu)+1 THEN RETURN END;
  t:=mem.tag?(s);
  mem.alloc_tag(sa,j*SIZE(seg_rec),t^); k:=0;
  IF sa=NIL THEN err.raise(mem_fail) END;
  FOR i:=0 TO HIGH(s^.cu) DO
    IF s^.cu[i].c#0 THEN sa^[k]:=s^.cu[i]; INC(k) END;
  END;
  ASSERT(k=j);
  mem.dealloc_adr(s^.cu^.ADR);
  s^.cu^.ADR:=sa; s^.cu^.HIGH:=j-1;
END truncate;

PROCEDURE OnLine(ln: range; x1,y1,x2,y2: INTEGER): BOOLEAN;
  VAR dx,dy: INTEGER;
BEGIN
  dx:=ln.x2-ln.x1; dy:=ln.y2-ln.y1;
  RETURN ((x1-ln.x1)*dy=(y1-ln.y1)*dx)&((x2-ln.x1)*dy=(y2-ln.y1)*dx);
END OnLine;

PROCEDURE Side(x1,y1,x2,y2,x,y: INTEGER): INTEGER;
  VAR dx,dy,z1,z2,r: INTEGER;
BEGIN
  dx:=x2-x1; dy:=y2-y1;
  IF (dx=0)&(dy=0) THEN
    IF (x=x1)&(y=y1) THEN RETURN 0 ELSE RETURN 1 END;
  END;
  r:=x*dx+y*dy;
  z1:=r-x1*dx-y1*dy;
  z2:=x2*dx+y2*dy-r;
  IF (z1>=0)&(z2>=0) THEN RETURN 0
  ELSIF z1<0 THEN RETURN -1
  ELSE RETURN 1 END;
END Side;

PROCEDURE StrongSide(x1,y1,x2,y2,x,y: INTEGER): INTEGER;
  VAR dx,dy,z1,z2,r: INTEGER;
BEGIN
  dx:=x2-x1; dy:=y2-y1;
  IF (dx=0)&(dy=0) THEN RETURN 1 END;
  r:=x*dx+y*dy;
  z1:=r-x1*dx-y1*dy;
  z2:=x2*dx+y2*dy-r;
  IF (z1>0)&(z2>0) THEN RETURN 0
  ELSIF z1<=0 THEN RETURN -1
  ELSE RETURN 1 END;
END StrongSide;

PROCEDURE SQ(r: REAL): REAL;
CODE cod.copt cod.fmul END SQ;

PROCEDURE ISQ(i: INTEGER): INTEGER;
CODE cod.copt cod.mul END ISQ;

PROCEDURE Len(x1,y1,x2,y2,x,y: INTEGER): INTEGER;
  VAR sd,A,B,C: INTEGER;
BEGIN
  B:=x2-x1; A:=y1-y2;
  IF NOT BOOLEAN(BITSET(A)+BITSET(B)) THEN RETURN ISQ(x1-x)+ISQ(y1-y) END;
  sd:=Side(x1,y1,x2,y2,x,y);
  IF sd<0 THEN RETURN ISQ(x1-x)+ISQ(y1-y) END;
  IF sd>0 THEN RETURN ISQ(x2-x)+ISQ(y2-y) END;
  C:=-x1*A-y1*B;
  RETURN TRUNC(SQ(FLOAT(A*x+B*y+C))/FLOAT(ISQ(A)+ISQ(B)));
END Len;

PROCEDURE SkipSegments(s: signal; VAL b: range; VAR n: INTEGER);
  PROCEDURE chk_box(VAR p1,p2: seg_rec): BOOLEAN; CODE 0F8h END chk_box;
  VAR l: seg_rec; p,e: POINTER TO seg_rec;
BEGIN
  IF n<0 THEN n:=0 END;
  IF n>HIGH(s^.cu) THEN RETURN END;
  l.a:=b.x1+8000h+INTEGER((b.y1+8000h)<<16);
  l.b:=b.x2+8000h+INTEGER((b.y2+8000h)<<16);
  p:=ADR(s^.cu[n]);
  e:=ADR(s^.cu[HIGH(s^.cu)])+SIZE(seg_rec);
  REPEAT
    IF (p^.c#0) & chk_box(p^,l) THEN RETURN END;
    p:=ADDRESS(p)+SIZE(seg_rec); INC(n);
  UNTIL p=e;
END SkipSegments;

PROCEDURE SeekInBox(mdl: board; rn: range; ip: IterProc);
  TYPE b=BITSET; i=INTEGER;
  VAR n,j,x1,x2,y1,y2: INTEGER; p: POINTER TO seg_rec;
      s: signal;
BEGIN
  IF rn.x1>rn.x2 THEN n:=rn.x1; rn.x1:=rn.x2; rn.x2:=n END;
  IF rn.y1>rn.y2 THEN n:=rn.y1; rn.y1:=rn.y2; rn.y2:=n END;
  x1:=rn.x1; x2:=rn.x2;
  y1:=rn.y1; y2:=rn.y2;
  FOR n:=0 TO HIGH(mdl^.sigs) DO
    s:=mdl^.sigs[n]; p:=ADR(s^.cu);
    FOR j:=0 TO HIGH(s^.cu) DO
      IF p^.c#0 THEN
        IF BOOLEAN(b(i(b(p^.a)*mask)<=x2)*b(i(b(p^.a>>16)*mask)<=y2)*
                   b(i(b(p^.b)*mask)>=x1)*b(i(b(p^.b>>16)*mask)>=y1)) THEN
          IF ip(s,j) THEN RETURN END;
        END;
        p:=ADDRESS(p)+SIZE(seg_rec);
      END;
    END;
  END;
END SeekInBox;

PROCEDURE Shoted?(mdl: board; sig: signal; rn: range; sz,lay: INTEGER;
                  VAR shot: signal; VAR shn: INTEGER): BOOLEAN;
  TYPE b=BITSET; i=INTEGER;
  VAR x1,y1,x2,y2,dxc,dyc,dx,dy,n,j,r: INTEGER;
      s: signal;
      p: POINTER TO seg_rec;
      sg: segment;
BEGIN
  shot:=NIL; shn:=0;
  IF mod.fantom IN sig^.type THEN RETURN FALSE END;
  IF rn.x1>rn.x2 THEN n:=rn.x1; rn.x1:=rn.x2; rn.x2:=n END;
  IF rn.y1>rn.y2 THEN n:=rn.y1; rn.y1:=rn.y2; rn.y2:=n END;
  sz:=sz+Clearens;
  x1:=rn.x1-sz; x2:=rn.x2+sz;
  y1:=rn.y1-sz; y2:=rn.y2+sz;
  dxc:=rn.x2-rn.x1; dyc:=rn.y2-rn.y1;
  FOR n:=0 TO HIGH(mdl^.sigs) DO
    s:=mdl^.sigs[n];
    IF (s#sig) & NOT (mod.fantom IN s^.type) THEN
      p:=ADR(s^.cu);
      FOR j:=0 TO HIGH(s^.cu) DO
        IF p^.c#0 THEN
          IF BOOLEAN(b(i(b(p^.a)*mask)<=x2)*b(i(b(p^.a>>16)*mask)<=y2)*
                     b(i(b(p^.b)*mask)>=x1)*b(i(b(p^.b>>16)*mask)>=y1)) THEN
            unpack(s,j,sg);
            IF (sg.size#0) & (lay IN sg.lays) THEN
              r:=ISQ(sz+sg.size);
              dx:=sg.x2-sg.x1; dy:=sg.y2-sg.y1;
              IF
               ((dxc*(sg.y1-rn.y1)-dyc*(sg.x1-rn.x1)>=0)#
                (dxc*(sg.y2-rn.y1)-dyc*(sg.x2-rn.x1)>=0))
               &((dx*(rn.y1-sg.y1)-dy*(rn.x1-sg.x1)>=0)#
                (dx*(rn.y2-sg.y1)-dy*(rn.x2-sg.x1)>=0))
               OR (Len(rn.x1,rn.y1,rn.x2,rn.y2,sg.x1,sg.y1)<r)
               OR (Len(rn.x1,rn.y1,rn.x2,rn.y2,sg.x2,sg.y2)<r)
               OR (Len(sg.x1,sg.y1,sg.x2,sg.y2,rn.x1,rn.y1)<r)
               OR (Len(sg.x1,sg.y1,sg.x2,sg.y2,rn.x2,rn.y2)<r)
              THEN
                shot:=s; shn:=j;
                RETURN TRUE
              END;
            END;
          END;
        END;
        p:=ADDRESS(p)+SIZE(seg_rec);
      END;
    END;
  END;
  RETURN FALSE;
END Shoted?;

PROCEDURE InsertRange(mdl: board; sig: signal; pp: e_proc; xx: WORD;
                      rn: range; sz,lay: INTEGER; fix: BOOLEAN): BOOLEAN;
  VAR i: INTEGER; DelMarks: ARRAY [0..499] OF BITSET; Del?: BOOLEAN;
  PROCEDURE Mark;
  BEGIN INCL(DelMarks[i DIV 32],i MOD 32) END Mark;
  PROCEDURE Mark?(): BOOLEAN;
  BEGIN RETURN (i MOD 32) IN DelMarks[i DIV 32] END Mark?;
  VAR l0,l1,k: INTEGER; shot: signal; shotn: INTEGER; sg: segment;
BEGIN
  IF (rn.x1=rn.x2)&(rn.y1=rn.y2) THEN RETURN FALSE END;
  FOR k:=0 TO HIGH(DelMarks) DO DelMarks[k]:={} END;
  i:=0;
  WHILE i<=HIGH(sig^.cu) DO
    unpack(sig,i,sg);
    Del?:=FALSE;
    IF (sg.size#0) & (lay IN sg.lays) & (sg.pipe=0) & (sg.fix=fix) &
        NOT Mark?() & OnLine(rn,sg.x1,sg.y1,sg.x2,sg.y2) THEN
      l0:=Side(sg.x1,sg.y1,sg.x2,sg.y2,rn.x1,rn.y1);
      l1:=Side(sg.x1,sg.y1,sg.x2,sg.y2,rn.x2,rn.y2);
      IF (l0=0)&(l1=0) THEN
        IF sg.size>=sz THEN RETURN FALSE END;
      ELSIF l0=0 THEN
        IF sg.size=sz THEN
          IF l1>0 THEN
            rn.x1:=sg.x1; rn.y1:=sg.y1;
          ELSE
            rn.x1:=sg.x2; rn.y1:=sg.y2;
          END;
          Del?:=TRUE;
        ELSIF sg.size>sz THEN
          IF l1>0 THEN
            rn.x1:=sg.x2; rn.y1:=sg.y2;
          ELSE
            rn.x1:=sg.x1; rn.y1:=sg.y1;
          END;
        END;
      ELSIF l1=0 THEN
        IF sg.size=sz THEN
          IF l0>0 THEN
            rn.x2:=sg.x1; rn.y2:=sg.y1;
          ELSE
            rn.x2:=sg.x2; rn.y2:=sg.y2;
          END;
          Del?:=TRUE;
        ELSIF sg.size>sz THEN
          IF l1>0 THEN
            rn.x2:=sg.x1; rn.y2:=sg.y1;
          ELSE
            rn.x2:=sg.x2; rn.y2:=sg.y2;
          END;
        END;
      ELSIF (l0<0)#(l1<0) THEN
        IF sg.size<=sz THEN Del?:=TRUE END;
      END;
    END;
    IF Del? THEN Mark; i:=0 ELSE INC(i) END;
  END;
  IF Shoted?(mdl,sig,rn,sz,lay,shot,shotn) THEN RETURN TRUE END;
  FOR i:=0 TO HIGH(sig^.cu) DO
    IF Mark?() THEN pp(xx,sig,i,FALSE); del(sig,i) END
  END;
  sg.x1:=rn.x1; sg.x2:=rn.x2;
  sg.y1:=rn.y1; sg.y2:=rn.y2;
  sg.lays:={lay}; sg.size:=sz;
  sg.pipe:=0; sg.fix:=fix;
  pp(xx,sig,app_n(sig,sg),TRUE);
  RETURN FALSE;
END InsertRange;

PROCEDURE DeleteRange(sig: signal; pp: e_proc; xx: WORD;
                      rn: range; l: INTEGER);
  VAR sg,sg1: segment; i,r1,r2,l0,l1: INTEGER;
BEGIN
  IF (rn.x1=rn.x2)&(rn.y1=rn.y2) THEN RETURN END;
  FOR i:=0 TO HIGH(sig^.cu) DO
    unpack(sig,i,sg);
    IF (sg.size#0) & (l IN sg.lays) & OnLine(rn,sg.x1,sg.y1,sg.x2,sg.y2) THEN
      l0:=StrongSide(sg.x1,sg.y1,sg.x2,sg.y2,rn.x1,rn.y1);
      l1:=StrongSide(sg.x1,sg.y1,sg.x2,sg.y2,rn.x2,rn.y2);
      IF (l0=0) & (l1=0) THEN
        r1:=ISQ(sg.x1-rn.x1)+ISQ(sg.y1-rn.y1);
        r2:=ISQ(sg.x1-rn.x2)+ISQ(sg.y1-rn.y2);
        IF r1<r2 THEN
          sg1:=sg; pp(xx,sig,i,FALSE); del(sig,i);
          sg.x2:=rn.x1; sg.y2:=rn.y1;
          sg1.x1:=rn.x2; sg1.y1:=rn.y2;
          pp(xx,sig,app_n(sig,sg),TRUE);
          pp(xx,sig,app_n(sig,sg1),TRUE);
        ELSE
          sg1:=sg; pp(xx,sig,i,FALSE); del(sig,i);
          sg.x2:=rn.x2; sg.y2:=rn.y2;
          sg1.x1:=rn.x1; sg1.y1:=rn.y1;
          pp(xx,sig,app_n(sig,sg),TRUE);
          pp(xx,sig,app_n(sig,sg1),TRUE);
        END;
      ELSIF l0=0 THEN
        IF l1>0 THEN
          sg.x2:=rn.x1; sg.y2:=rn.y1
        ELSE
          sg.x1:=rn.x1; sg.y1:=rn.y1
        END;
        pp(xx,sig,i,FALSE); del(sig,i);
        pp(xx,sig,app_n(sig,sg),TRUE);
      ELSIF l1=0 THEN
        IF l0>0 THEN
          sg.x2:=rn.x2; sg.y2:=rn.y2
        ELSE
          sg.x1:=rn.x2; sg.y1:=rn.y2
        END;
        pp(xx,sig,i,FALSE); del(sig,i);
        pp(xx,sig,app_n(sig,sg),TRUE);
      ELSIF (l0<0)#(l1<0) THEN
        IF (sg.x1#sg.x2) OR (sg.y1#sg.y2) THEN
          pp(xx,sig,i,FALSE); del(sig,i);
        END;
      END;
    END;
  END;
END DeleteRange;

PROCEDURE InsertVias
  (mdl: board; sig: signal; pp: e_proc; xx: WORD;
   x,y,sz,vsz: INTEGER; lays: BITSET; fix: BOOLEAN): BOOLEAN;
  VAR l: INTEGER; DelMarks: ARRAY [0..499] OF BITSET;
  PROCEDURE Mark;
  BEGIN INCL(DelMarks[l DIV 32],l MOD 32) END Mark;
  PROCEDURE Mark?(): BOOLEAN;
  BEGIN RETURN (l MOD 32) IN DelMarks[l DIV 32] END Mark?;
  VAR sg: segment; shot: signal; shotn: INTEGER; rn: range;
BEGIN
  FOR l:=0 TO HIGH(DelMarks) DO DelMarks[l]:={} END;
  l:=0; IF sz<vsz THEN sz:=vsz END;
  WHILE l<=HIGH(sig^.cu) DO
    unpack(sig,l,sg);
    IF (sg.size#0) & sg.fix & NOT Mark?() &
       (sg.x1=x) & (sg.x2=x) & (sg.y1=y) & (sg.y2=y) THEN
      IF sg.size>sz THEN sz:=sg.size END;
      IF sg.pipe>vsz THEN vsz:=sg.pipe END;
      IF (sg.lays=lays)&(sg.size=sz)&(sg.pipe=vsz) THEN RETURN FALSE END;
      Mark; l:=0;
    ELSE
      INC(l);
    END;
  END;
  rn.x1:=x; rn.y1:=y;
  rn.x2:=x; rn.y2:=y;
  FOR l:=0 TO 31 DO
    IF (l IN lays) & Shoted?(mdl,sig,rn,sz,l,shot,shotn) THEN RETURN TRUE END;
  END;
  FOR l:=0 TO HIGH(sig^.cu) DO
    IF Mark?() THEN pp(xx,sig,l,FALSE); del(sig,l) END
  END;
  sg.x1:=x; sg.x2:=x;
  sg.y1:=y; sg.y2:=y;
  sg.size:=sz; sg.pipe:=vsz;
  sg.lays:=lays; sg.fix:=fix;
  pp(xx,sig,app_n(sig,sg),TRUE);
  RETURN FALSE;
END InsertVias;

PROCEDURE DeleteVias(sig: signal; pp: e_proc; xx: WORD; x,y: INTEGER);
  VAR i: INTEGER; sg: segment;
BEGIN
  FOR i:=0 TO HIGH(sig^.cu) DO
    unpack(sig,i,sg);
    IF (sg.size#0)&(sg.x1=x)&(sg.x2=x)&(sg.y1=y)&(sg.y2=y) THEN
      pp(xx,sig,i,FALSE); del(sig,i);
    END;
  END;
END DeleteVias;

PROCEDURE FindSignal
        (mdl: board; sig: signal; x,y,l,size: INTEGER; VAR sn: INTEGER): signal;
  TYPE b=BITSET; i=INTEGER;
  VAR x1,x2,y1,y2,n,j: INTEGER; s: signal; sg: segment;
      p: POINTER TO seg_rec;
BEGIN
  x1:=x-size+8000h; x2:=x+size+8000h;
  y1:=y-size+8000h; y2:=y+size+8000h;
  FOR n:=0 TO HIGH(mdl^.sigs) DO
    s:=mdl^.sigs[n];
    IF s#sig THEN
      p:=ADR(s^.cu);
      FOR j:=0 TO HIGH(s^.cu) DO
        IF p^.c#0 THEN
          IF BOOLEAN(b(i(b(p^.a)*mask)<=x2)*b(i(b(p^.a>>16)*mask)<=y2)*
                     b(i(b(p^.b)*mask)>=x1)*b(i(b(p^.b>>16)*mask)>=y1)) THEN
            unpack(s,j,sg);
            IF (l IN sg.lays)&
               (Len(sg.x1,sg.y1,sg.x2,sg.y2,x,y)<=ISQ(size+sg.size)) THEN
              sn:=j;
              RETURN s;
            END;
          END;
        END;
        p:=ADDRESS(p)+SIZE(seg_rec);
      END;
    END;
  END;
  RETURN NIL;
END FindSignal;

PROCEDURE Areas(sig: signal): INTEGER;
  PROCEDURE Min(x,y: INTEGER): INTEGER;
  BEGIN IF x<y THEN RETURN x ELSE RETURN y END END Min;
  PROCEDURE Max(x,y: INTEGER): INTEGER;
  BEGIN IF x>y THEN RETURN x ELSE RETURN y END END Max;
  TYPE Range=RECORD
               minX,minY,maxX,maxY: INTEGER;
               Id,x1,x2,y1,y2,size: INTEGER;
               layer: BITSET
             END;
       pRange=POINTER TO Range;
  VAR Idents: POINTER TO ARRAY [0..0FFFFh] OF Range;
      IdCnt : INTEGER;
      IdPtr : INTEGER;
  PROCEDURE Connect(x1,y1,x2,y2,sz: INTEGER; ls: BITSET; p: pRange): BOOLEAN;
    VAR r,r1,dx,dy,cdx,cdy: INTEGER;
  BEGIN
    r1:=p^.size+sz-Overlay;
    r:=r1*r1;
    dx:=p^.x2-p^.x1; dy:=p^.y2-p^.y1;
    cdx:=x2-x1; cdy:=y2-y1;
    RETURN
      ((cdx*(p^.y1-y1)-cdy*(p^.x1-x1)>=0)#(cdx*(p^.y2-y1)-cdy*(p^.x2-x1)>=0))
      & ((dx*(y1-p^.y1)-dy*(x1-p^.x1)>=0)#(dx*(y2-p^.y1)-dy*(x2-p^.x1)>=0))
      OR (Len(x1,y1,x2,y2,p^.x1,p^.y1)<r)
      OR (Len(x1,y1,x2,y2,p^.x2,p^.y2)<r)
      OR (Len(p^.x1,p^.y1,p^.x2,p^.y2,x1,y1)<r)
      OR (Len(p^.x1,p^.y1,p^.x2,p^.y2,x2,y2)<r);
  END Connect;
  VAR size,from,to,i,j: INTEGER; p,q,r: pRange; sg: segment;
BEGIN
  IdCnt:=0; IdPtr:=1;
  size:=0;
  FOR i:=0 TO HIGH(sig^.cu) DO IF sig^.cu[i].c#0 THEN INC(size) END END;
  IF size<=1 THEN RETURN size END;
  mem.ALLOCATE(Idents,size*SIZE(Range));
  j:=0;
  FOR i:=0 TO HIGH(sig^.cu) DO
    unpack(sig,i,sg);
    IF sg.size#0 THEN
      WITH Idents^[j] DO
        x1:=sg.x1; x2:=sg.x2; y1:=sg.y1; y2:=sg.y2;
        size:=sg.size; layer:=sg.lays;
        minX:=Min(x1,x2)-size; maxX:=Max(x1,x2)+size;
        minY:=Min(y1,y2)-size; maxY:=Max(y1,y2)+size;
        Id:=IdPtr; INC(IdPtr); INC(IdCnt);
      END;
      INC(j);
    END;
  END;
  r:=ADR(Idents^[size]);
  FOR i:=0 TO size-1 DO
    WITH Idents^[i] DO
      p:=ADR(Idents^[i+1]);
      WHILE p<r DO
        IF (Id#p^.Id)&(layer*p^.layer#{}) THEN
          IF NOT ( (minX>=p^.maxX)OR(p^.minX>=maxX)OR
                   (minY>=p^.maxY)OR(p^.minY>=maxY) ) &
                   Connect(x1,y1,x2,y2,size,layer,p) THEN
            from:=Id; to:=p^.Id;
            q:=ADR(Idents^[0]);
            REPEAT
              IF q^.Id=from THEN q^.Id:=to END;
              q:=ADDRESS(q)+SIZE(Range);
            UNTIL q>=r;
            DEC(IdCnt);
          END;
        END;
        p:=ADDRESS(p)+SIZE(Range);
      END;
    END;
  END;
  mem.DEALLOCATE(Idents,size*SIZE(Range));
  RETURN IdCnt;
END Areas;

BEGIN
  mask:={0..15};
END pedCU.
