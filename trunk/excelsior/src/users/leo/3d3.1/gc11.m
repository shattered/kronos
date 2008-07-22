MODULE gc11; (*    Leo  06-Feb-90. (c) KRONOS *)
             (*    nick 01-Aug-90. (c) KRONOS *)
             (*$U+ nick 27-Mar-91. (c) KRONOS *)

IMPORT  ASCII,SYSTEM;           IMPORT  cod: defCodes;
IMPORT  bio: BIO;               IMPORT  std: StdIO;
IMPORT  arg: tskArgs;           IMPORT  str: Strings;
IMPORT  tty: Terminal;          IMPORT  key: Keyboard;
IMPORT  mat: realMath;          IMPORT  vio: CGA88;
IMPORT  mem: Heap;              IMPORT  def: def31dg;
IMPORT  vec: Vectors;           IMPORT  tm : Time;
IMPORT  fot: Foto;

WITH STORAGE: mem;

CONST RUMBS = 16; (* 16 for 22.5 grad *)
      a = 70;  SS = 180;  maxI = vio.Imax;  iSCALE = def.iSCALE;
      b = 10;  SW = 240;  minI = vio.Imin;  oSCALE = 2*1024;
               sC = SS DIV 2;               xSCALE =   1024;

VAR FRAME,WRITE: BOOLEAN;  VP: INTEGER;
    OBJCT,LIGHT: BOOLEAN;  VL: vec.VECTOR;
   TRAIN,CHANGE: BOOLEAN;  tri_no: INTEGER;

VAR number: INTEGER;
   picture: ARRAY [0..63] OF CHAR;

TYPE WORD = SYSTEM.WORD;
  ADDRESS = SYSTEM.ADDRESS;

-------------------------  LOW/LEVEL  --------------------------
                         -------------
(*$<$T-*)

PROCEDURE move(d,s: ADDRESS; size: INTEGER); CODE cod.move END move;

PROCEDURE fill(VAR a: ARRAY OF WORD; w: WORD);
BEGIN a[0]:=w; move(SYSTEM.ADR(a[1]),SYSTEM.ADR(a[0]),HIGH(a)) END fill;

PROCEDURE zero(VAR a: ARRAY OF WORD);
BEGIN a[0]:=0; move(SYSTEM.ADR(a[1]),SYSTEM.ADR(a[0]),HIGH(a)) END zero;

PROCEDURE swap(VAR a,b: WORD);
  VAR i: INTEGER;
BEGIN i:=a; a:=b; b:=i END swap;

TYPE
  ITER_LINE = RECORD
                s ,p ,co,sn,pn: INTEGER;
                Ds,Dp,ds,dp,sl: INTEGER;
                Gx,Cs         : BOOLEAN;
                pbeg,pend     : INTEGER;
              END;


PROCEDURE iline(VAR edge: ITER_LINE);
CODE cod.bmg cod.bmg_ftri END iline;

(*$>*)

----------------------------  I/O  -----------------------------
                            -------
CONST CANT = "can't";

VAR inp: STRING;                cou,outY: INTEGER;
    out: DYNARR OF INTEGER;

VAR file: bio.FILE;
    path: ARRAY [0..127] OF CHAR;

PROCEDURE create(VAL name,ext: ARRAY OF CHAR);
BEGIN
  str.print(path,"%s.%s",name,ext);
  bio.create(file,path,'m',128*1024);
  IF NOT bio.done THEN
    std.perror(bio.error,'%s open file "%s": %%s\n',CANT,path); HALT(1)
  END;
  cou:=0; outY:=0
END create;

PROCEDURE write;
BEGIN
  bio.write(file,out^.ADR,cou*4);
  IF NOT bio.done THEN
    std.perror(bio.error,'%s write file "%s": %%s\n',CANT,path); HALT(1)
  END;
  cou:=0;
END write;

PROCEDURE close;
BEGIN
  IF cou>0 THEN write END;
  bio.close(file);
  IF NOT bio.done THEN
    std.perror(bio.error,'%s close file "%s": %%s\n',CANT,path); HALT(1)
  END
END close;

-----------------------  OUTPUT FORMAT  ------------------------
                       -----------------

PROCEDURE putw(word: WORD);
BEGIN
  IF cou>HIGH(out) THEN RESIZE(out,HIGH(out)+1+1024) END;
  out[cou]:=word; INC(cou);
  IF cou=8*1024 THEN write; cou:=0 END
END putw;

PROCEDURE put_line(x0,x1,y,c: INTEGER);
  TYPE set=BITSET;
BEGIN
  IF y#outY THEN putw({30}+set(y MOD 1024)); outY:=y END;
  putw( set(x0 MOD 1024) + set(x1 MOD 1024) << 10 + set(c MOD 256)>>12 );
END put_line;

PROCEDURE put_rumb(ksi,fi: INTEGER);
  TYPE set=BITSET;
BEGIN
  ksi:=-INTEGER( set(fi MOD 256)<<8 + set(ksi MOD 256) );
  putw(ksi);
END put_rumb;


----------------------  DATA STRUCTURES  -----------------------
                      -------------------

CONST
  X = def.X;  Y = def.Y;  Z = def.Z;  W = def.W;

TYPE
  XYZ       = def.TOP;
  TRIANGLE  = def.POLYGON;
  OBJECT    = def.OBJECT;

-------------------------- READ OBJECT -------------------------
                          -------------
PROCEDURE read(VAL name: ARRAY OF CHAR): ADDRESS;
  VAR a: ADDRESS;
    eof: INTEGER;
   size: INTEGER;
    obj: POINTER TO OBJECT;
   path: ARRAY [0..127] OF CHAR;
   file: bio.FILE;
      i: INTEGER;
BEGIN
  str.print(path,"%s.objg1",name);
  bio.open(file,path,'r');
  IF NOT bio.done THEN
    std.perror(bio.error,'%s open file "%s": %%s\n',CANT,path); HALT(1)
  END;
  eof:=bio.eof(file);
  IF eof<=0 THEN
    std.print('empty file "%s"\n',path); HALT(1)
  END;
  mem.ALLOCATE(a,(eof+3) DIV 4);
  bio.read(file,a,eof);
  IF NOT bio.done THEN
    std.perror(bio.error,'%s read file "%s": %%s\n',CANT,path); HALT(1)
  END;
  bio.close(file);
  obj:=a;
  INC(a,SIZE(obj^));
  WITH obj^ DO
    ASSERT(SIZE(name)+SIZE(poly)+SIZE(body)+SIZE(ints)+SIZE(obj^)<=eof*4,111h);
    name^.ADR:=a; INC(a,SIZE(obj^.name));
    poly^.ADR:=a; INC(a,SIZE(obj^.poly));
    body^.ADR:=a; INC(a,SIZE(obj^.body));
    ints^.ADR:=a; INC(a,SIZE(obj^.ints));
    NEW(image,top);
    norms^.ADR:=a;
    tty.print('%s: polygons=%d tops=%d\n',name,tri,top);
  END;
  RETURN obj
END read;

---------------------------  VIEWS  ----------------------------
                           ---------
TYPE
  M4 = ARRAY [0..3] OF XYZ;

PROCEDURE MxM(VAR r: M4; a,b: M4);
  VAR i,j,k: INTEGER; s: INTEGER;
BEGIN
  FOR i:=0 TO 3 DO
    FOR j:=0 TO 3 DO
      s:=0;
      FOR k:=0 TO 3 DO s:=s+a[i,k]*b[k,j] END;
      r[i,j]:=s DIV oSCALE
    END
  END
END MxM;

PROCEDURE VxM(VAR r: XYZ; v: XYZ; VAL a: M4);
  VAR j,k: INTEGER; s: INTEGER;
BEGIN
  FOR j:=0 TO 3 DO
    s:=0;
    FOR k:=0 TO 3 DO s:=s+v[k]*a[k,j] END;
    r[j]:=s DIV oSCALE
  END
END VxM;

PROCEDURE max(SEQ a: INTEGER): INTEGER;
  VAR b: INTEGER; i: INTEGER;
BEGIN
  i:=0; b:=a[0];
  WHILE i<=HIGH(a) DO
    IF a[i]>b THEN b:=a[i] END; INC(i)
  END;
  RETURN b
END max;

PROCEDURE min(SEQ a: INTEGER): INTEGER;
  VAR b: INTEGER; i: INTEGER;
BEGIN
  i:=0; b:=a[0];
  WHILE i<=HIGH(a) DO
    IF a[i]<b THEN b:=a[i] END; INC(i)
  END;
  RETURN b
END min;

VAR sin: ARRAY [0..RUMBS-1] OF INTEGER;
    cos: ARRAY [0..RUMBS-1] OF INTEGER;

PROCEDURE frame;
BEGIN
  vio.line(15,0   ,0   ,SW-1,0   );
  vio.line(15,0   ,0   ,0   ,SS-1);
  vio.line(15,SW-1,0   ,SW-1,SS-1);
  vio.line(15,0   ,SS-1,SW-1,SS-1);
END frame;

PROCEDURE tria(o: OBJECT; t: TRIANGLE; c: INTEGER);
  VAR y,c1,c2,c3: INTEGER;
BEGIN
  IF c=7 THEN c1:= 7; c2:= 7; c3:= 7
  ELSE        c1:= 9; c2:=10; c3:=11
  END;
  WITH o DO
    WITH t DO
      y:=image[tops[1]][Y];
      vio.line(c1,image[tops[0]][X],image[tops[0]][Y],
                  image[tops[1]][X],y);
      y:=image[tops[2]][Y];
      vio.line(c2,image[tops[0]][X],image[tops[0]][Y],
                  image[tops[2]][X],y);
      y:=image[tops[1]][Y];
      vio.line(c3,image[tops[2]][X],image[tops[2]][Y],
                  image[tops[1]][X],y);
    END
  END
END tria;


----------------------------------------------------------------


VAR display: ARRAY [0..SW] OF INTEGER;
    Zbuffer: ARRAY [0..SW] OF INTEGER;

PROCEDURE output_line(Y: INTEGER);
  VAR i,j,c,l: INTEGER;
BEGIN
  i:=0;
  (*$<$T-*)
  WHILE (i<HIGH(display)) DO
    c:=display[i]; j:=i+1;
    IF c#0 THEN display[SW]:=-c ELSE display[SW]:=-1 END;
    IF display[j]=c THEN
      REPEAT j:=j+1 UNTIL display[j]#c
    END;
    IF c>=0 THEN
      vio.hline(c DIV 10000h,c MOD 10000h,i,Y,j-1)
    END;
    i:=j
  END
  (*$>*)
END output_line;

----------------------------------------------------------------

PROCEDURE init_edge(VAR con: ITER_LINE; s0,p0,s1,p1: INTEGER);
  VAR t: INTEGER;
BEGIN
  ASSERT(p1>=p0);
  WITH con DO
    sl:=s1; pbeg:=p0; pend:=p1;
    s :=s0; sn:=s; Ds:=s1-s0; Cs:=TRUE;
    p :=p0; pn:=p; Dp:=p1-p0; Gx:=TRUE;
    IF Ds< 0  THEN Ds:=-Ds; ds:=-1 ELSE Cs:=FALSE; ds:=1        END;
    IF Dp< 0  THEN Dp:=-Dp; dp:=-1 ELSE            dp:=1        END;
    IF Ds>=Dp THEN co:=Ds DIV 2    ELSE Gx:=FALSE; co:=Dp DIV 2 END
  END
END init_edge;

TYPE Top = POINTER TO XYZ;

PROCEDURE eq(e0,e1: ITER_LINE): BOOLEAN;
  VAR   i: INTEGER;
    a0,a1: ADDRESS;
    res  : BOOLEAN;
BEGIN
  a0:=SYSTEM.ADR(e0);
  a1:=SYSTEM.ADR(e1);
  res:=TRUE; i:=0;
  WHILE (i<=SIZE(e0)-1) & res DO
    res:=NOT (a0^=a1^); INC(a0); INC(a1); INC(i)
  END;
  RETURN res
END eq;

TYPE Edges  = POINTER TO EDGES;
     EDGES  = RECORD
                next: Edges;
                yent,yout : INTEGER;
                color,case: INTEGER;
                e0,e1,e2  : ITER_LINE;
                z0,z1,z2  : ITER_LINE;
                i0,i1,i2  : ITER_LINE;
              END;

VAR scan_line: Edges;

PROCEDURE form_edges(VAR o: OBJECT; VAR e: Edges; t: INTEGER);
  VAR p0,p1,p2: Top;
      t0,t1,t2: INTEGER;
      I0,I1,I2: INTEGER;
BEGIN
  NEW(e);
  WITH o.poly[t] DO
    t0:=tops[0];
    t1:=tops[1];
    t2:=tops[2];
    e^.color:=color;
    e^.case:=case;
  END;
  WITH o DO
    p0:=SYSTEM.ADR(image[t0]);
    p1:=SYSTEM.ADR(image[t1]);
    p2:=SYSTEM.ADR(image[t2]);
    I0:=ints[t0].val;
    I1:=ints[t1].val;
    I2:=ints[t2].val;
  END;
  WITH e^ DO
    next:=NIL;
    yent:=p0^[Y];
    yout:=p2^[Y];
    CASE case OF
      |0: init_edge(e0,p0^[X],p0^[Y],p1^[X],p1^[Y]);
          init_edge(e1,p0^[X],p0^[Y],p2^[X],p2^[Y]);
          init_edge(e2,p1^[X],p1^[Y],p2^[X],p2^[Y]);

          init_edge(z0,p0^[Z],p0^[Y],p1^[Z],p1^[Y]);
          init_edge(z1,p0^[Z],p0^[Y],p2^[Z],p2^[Y]);
          init_edge(z2,p1^[Z],p1^[Y],p2^[Z],p2^[Y]);

          init_edge(i0,I0,p0^[Y],I1,p1^[Y]);
          init_edge(i1,I0,p0^[Y],I2,p2^[Y]);
          init_edge(i2,I1,p1^[Y],I2,p2^[Y])

      |1: init_edge(e0,p0^[X],p0^[Y],p1^[X],p1^[Y]);
          init_edge(e1,p0^[X],p0^[Y],p2^[X],p2^[Y]);

          init_edge(z0,p0^[Z],p0^[Y],p1^[Z],p1^[Y]);
          init_edge(z1,p0^[Z],p0^[Y],p2^[Z],p2^[Y]);

          init_edge(i0,I0,p0^[Y],I1,p1^[Y]);
          init_edge(i1,I0,p0^[Y],I2,p2^[Y])

      |2: init_edge(e0,p0^[X],p0^[Y],p2^[X],p2^[Y]);
          init_edge(e1,p1^[X],p1^[Y],p2^[X],p2^[Y]);

          init_edge(z0,p0^[Z],p0^[Y],p2^[Z],p2^[Y]);
          init_edge(z1,p1^[Z],p1^[Y],p2^[Z],p2^[Y]);

          init_edge(i0,I0,p0^[Y],I2,p2^[Y]);
          init_edge(i1,I1,p1^[Y],I2,p2^[Y])

      |3: init_edge(e0,p0^[X],p0^[Y],p1^[X],p1^[Y]);
          init_edge(e1,p1^[X],p1^[Y],p2^[X],p2^[Y]);
          init_edge(e2,p0^[X],p0^[Y],p2^[X],p2^[Y]);

          init_edge(z0,p0^[Z],p0^[Y],p1^[Z],p1^[Y]);
          init_edge(z1,p1^[Z],p1^[Y],p2^[Z],p2^[Y]);
          init_edge(z2,p0^[Z],p0^[Y],p2^[Z],p2^[Y]);

          init_edge(i0,I0,p0^[Y],I1,p1^[Y]);
          init_edge(i1,I1,p1^[Y],I2,p2^[Y]);
          init_edge(i2,I0,p0^[Y],I2,p2^[Y])

      |4: init_edge(e0,p0^[X],p0^[Y],p2^[X],p2^[Y]);
          init_edge(e1,p0^[X],p0^[Y],p1^[X],p1^[Y]);
          init_edge(e2,p1^[X],p1^[Y],p2^[X],p2^[Y]);

          init_edge(z0,p0^[Z],p0^[Y],p2^[Z],p2^[Y]);
          init_edge(z1,p0^[Z],p0^[Y],p1^[Z],p1^[Y]);
          init_edge(z2,p1^[Z],p1^[Y],p2^[Z],p2^[Y]);

          init_edge(i0,I0,p0^[Y],I2,p2^[Y]);
          init_edge(i1,I0,p0^[Y],I1,p1^[Y]);
          init_edge(i2,I1,p1^[Y],I2,p2^[Y])
    ELSE
      ASSERT(FALSE);
    END
  END
END form_edges;

PROCEDURE iter_0(e: Edges);
BEGIN
  WITH e^ DO
    LOOP
      CASE case OF
        |0  : IF e0.pn=e0.pend THEN e0:=e2; z0:=z2; i0:=i2 END;
        |4  : IF e1.pn=e1.pend THEN e1:=e2; z1:=z2; i1:=i2 END;
      ELSE
      END;
      iline(e0); iline(z0); iline(i0);
      iline(e1); iline(z1); iline(i1);
      IF e0.pn=0 THEN EXIT END
    END
  END
END iter_0;

PROCEDURE new_edge(VAR o: OBJECT; t: INTEGER);
  VAR e: Edges;
BEGIN
  form_edges(o,e,t);
  IF e^.yent<0 THEN iter_0(e) END;
  e^.next:=scan_line;
  scan_line:=e;
END new_edge;

VAR Yenter: ARRAY [0..SS-1] OF INTEGER;

PROCEDURE y_enter(VAR o: OBJECT; t,y_m,y_M: INTEGER);
BEGIN
  IF y_m>0 THEN
    IF y_m<SS THEN
      o.poly[t].next:=Yenter[y_m];
      Yenter[y_m]:=t
    END
  ELSE
    IF y_M>=0 THEN new_edge(o,t) END
  END
END y_enter;

PROCEDURE sort_tops(VAR o: OBJECT);
  VAR p0,p1,p2: Top;
      base,tmp: ADDRESS;
      i,s     : INTEGER;
BEGIN
  base:=SYSTEM.ADR(o.image); s:=SIZE(o.image[0]);
  FOR i:=0 TO o.tri-1 DO
    WITH o.poly[i] DO
      p0:=base+tops[0]*s;
      p1:=base+tops[1]*s;
      p2:=base+tops[2]*s;
      IF p1^[Y]>p2^[Y] THEN tmp:=p1; p1:=p2; p2:=tmp END;
      IF p0^[Y]>p1^[Y] THEN tmp:=p0; p0:=p1; p1:=tmp END;
      IF p1^[Y]>p2^[Y] THEN tmp:=p1; p1:=p2; p2:=tmp END;
      case:=0;
      IF p1^[Y]=p2^[Y] THEN  case:=1;
        IF p1^[X]>p2^[X] THEN tmp:=p1; p1:=p2; p2:=tmp END
      END;
      IF p0^[Y]=p1^[Y] THEN  case:=case+2;
        IF p0^[X]>p1^[X] THEN tmp:=p1; p1:=p0; p0:=tmp END
      END;
      IF (case=0) & (p1^[X]>p2^[X]) THEN case:=case+4 END;
      tops[0]:=INTEGER(p0-base) DIV s;
      tops[1]:=INTEGER(p1-base) DIV s;
      tops[2]:=INTEGER(p2-base) DIV s;
      y_enter(o,i,p0^[Y],p2^[Y]);
    END
  END
END sort_tops;

PROCEDURE put_buffer(color,Z0,I0,X0,Z1,I1,X1: INTEGER);
  CONST
    range = maxI-minI+1;

  VAR t,I: INTEGER;
      z,i: ITER_LINE;
BEGIN
  IF X0>X1 THEN
    t:=X0; X0:=X1; X1:=t;
    t:=Z0; Z0:=Z1; Z1:=t;
    t:=I0; I0:=I1; I1:=t;
  END;
  IF (X1<0) OR (X0>=SW) THEN RETURN END;
  IF X0=X1 THEN
    Z0:=(Z1+Z0) DIV 2; I0:=(I1+I0) DIV 2;
    IF Zbuffer[X0]>Z0 THEN
      Zbuffer[X0]:=Z0;
      I0:=I0*range DIV xSCALE + minI;
      IF I0>maxI THEN I0:=maxI END;
      display[X0]:=color*10000h+I0
    END;
    RETURN
  END;
  init_edge(z,Z0,X0,Z1,X1);
  init_edge(i,I0,X0,I1,X1);
  LOOP
    IF z.ds<0 THEN iline(z); iline(i) END;
    IF (z.pn>=0) & (z.pn<SW) THEN
      IF Zbuffer[z.pn]>z.sn THEN
        Zbuffer[z.pn]:=z.sn;
        I:=i.sn*range DIV xSCALE + minI;
        IF I>maxI THEN I:=maxI END;
        display[z.pn]:=color*10000h + I
      END
    END;
    IF z.ds>0 THEN iline(z); iline(i) END;
    IF z.pn=z.pend THEN EXIT END;
  END
END put_buffer;

(*
PROCEDURE iter_tria(e: Edges);
BEGIN
  WITH e^ DO
    CASE case OF
      |0: IF e0.pn<e2.pbeg THEN
            IF e0.ds<0 THEN iline(e0); iline(z0); iline(i0) END;
            IF e1.ds>0 THEN iline(e1); iline(z1); iline(i1) END;
            put_buffer(color,z0.sn,i0.sn,e0.sn,z1.sn,i1.sn,e1.sn);
            IF e0.ds>0 THEN iline(e0); iline(z0); iline(i0) END;
            IF e1.ds<0 THEN iline(e1); iline(z1); iline(i1) END;
          ELSE
            IF e2.ds<0 THEN iline(e2); iline(z2); iline(i2) END;
            IF e1.ds>0 THEN iline(e1); iline(z1); iline(i1) END;
            put_buffer(color,z2.sn,i2.sn,e2.sn,z1.sn,i1.sn,e1.sn);
            IF e2.pn=e2.pend THEN RETURN END;
            IF e2.ds>0 THEN iline(e2); iline(z2); iline(i2) END;
            IF e1.ds<0 THEN iline(e1); iline(z1); iline(i1) END;
          END

      |1: IF e0.ds<0 THEN iline(e0); iline(z0); iline(i0) END;
          IF e1.ds>0 THEN iline(e1); iline(z1); iline(i1) END;
          put_buffer(color,z0.sn,i0.sn,e0.sn,z1.sn,i1.sn,e1.sn);
          IF e0.pn=e0.pend THEN RETURN END;
          IF e0.ds>0 THEN iline(e0); iline(z0); iline(i0) END;
    а     IF e1.ds<0 THEN iline(e1); iline(z1); iline(i1) END;

      |2: IF e0.ds>0 THEN iline(e0); iline(z0); iline(i0) END;
          IF e1.ds<0 THEN iline(e1); iline(z1); iline(i1) END;
          put_buffer(color,z0.sn,i0.sn,e0.sn,z1.sn,i1.sn,e1.sn);
          IF e0.pn=e0.pend THEN RETURN END;
          IF e0.ds<0 THEN iline(e0); iline(z0); iline(i0) END;
          IF e1.ds>0 THEN iline(e1); iline(z1); iline(i1) END;

      |3: put_buffer(color,z0.s,i0.s,e0.s,z2.s,i2.s,e2.s);
          put_buffer(color,z0.s,i0.s,e0.s,z1.s,i1.s,e1.s);
          put_buffer(color,z1.s,i1.s,e1.s,z2.s,i2.s,e2.s);
      |4: IF e0.pn<e2.pbeg THEN
            IF e0.ds<0 THEN iline(e0); iline(z0); iline(i0) END;
            IF e1.ds>0 THEN iline(e1); iline(z1); iline(i1) END;
            put_buffer(color,z0.sn,i0.sn,e0.sn,z1.sn,i1.sn,e1.sn);
            IF e0.ds>0 THEN iline(e0); iline(z0); iline(i0) END;
            IF e1.ds<0 THEN iline(e1); iline(z1); iline(i1) END;
          ELSE
            IF e0.ds<0 THEN iline(e0); iline(z0); iline(i0) END;
            IF e2.ds>0 THEN iline(e2); iline(z2); iline(i2) END;
            put_buffer(color,z0.sn,i0.sn,e0.sn,z2.sn,i2.sn,e2.sn);
            IF e0.pn=e0.pend THEN RETURN END;
            IF e0.ds>0 THEN iline(e0); iline(z0); iline(i0) END;
            IF e2.ds<0 THEN iline(e2); iline(z2); iline(i2) END;
          END
    ELSE
     ASSERT(FALSE)
    END
  END
END iter_tria;
*)

PROCEDURE iter_tria(e: Edges);
BEGIN
  WITH e^ DO
    CASE case OF
      |0  : put_buffer(color,z0.sn,i0.sn,e0.sn,z1.sn,i1.sn,e1.sn);
            IF e0.pn=e0.pend THEN e0:=e2; z0:=z2; i0:=i2 END;
      |1,2: put_buffer(color,z0.sn,i0.sn,e0.sn,z1.sn,i1.sn,e1.sn);
      |3  : put_buffer(color,z0.s,i0.s,e0.s,z2.s,i2.s,e2.s);
            put_buffer(color,z0.s,i0.s,e0.s,z1.s,i1.s,e1.s);
            put_buffer(color,z1.s,i1.s,e1.s,z2.s,i2.s,e2.s);
            RETURN
      |4  : put_buffer(color,z0.sn,i0.sn,e0.sn,z1.sn,i1.sn,e1.sn);
            IF e1.pn=e1.pend THEN e1:=e2; z1:=z2; i1:=i2 END;
    ELSE
      ASSERT(FALSE);
    END;
    iline(e0); iline(z0); iline(i0);
    iline(e1); iline(z1); iline(i1);
  END
END iter_tria;

PROCEDURE update_scan_line(VAR o: OBJECT; y: INTEGER);
  VAR node,nxt: Edges; tri: INTEGER;
BEGIN
(* remove old *)
  WHILE (scan_line#NIL) & (scan_line^.yout<y) DO
    nxt:=scan_line^.next;
    DISPOSE(scan_line);
    scan_line:=nxt;
  END;
  node:=scan_line;
  WHILE node#NIL DO
    WITH node^ DO
      IF (next#NIL) & (next^.yout<y) THEN
        nxt:=next^.next;
        DISPOSE(next);
        next:=nxt
      ELSE node:=next
      END
    END
  END;

  IF y>=SS THEN RETURN END;
(* insert new *)
  tri:=Yenter[y];
  WHILE tri>=0 DO
    new_edge(o,tri);
    tri:=o.poly[tri].next;
  END;
END update_scan_line;

PROCEDURE scan(VAR o: OBJECT);
  VAR Y: INTEGER; node: Edges;
BEGIN
  fill(Yenter,-1);
  scan_line:=NIL;
  sort_tops(o);
  Y:=0;
  REPEAT
    IF scan_line=NIL THEN
      WHILE (Y<SS) & (Yenter[Y]<0) DO INC(Y) END;
      DEC(Y);
    END;
    fill(Zbuffer,MAX(INTEGER));
    fill(display,-1);
    node:=scan_line;
    WHILE node#NIL DO
      iter_tria(node);
      node:=node^.next;
    END;
    output_line(Y);
    INC(Y);
    update_scan_line(o,Y);
  UNTIL (Y=SS);
  update_scan_line(o,MAX(INTEGER));
  ASSERT(scan_line=NIL);
END scan;

VAR NOR: BOOLEAN;

PROCEDURE light(VAR o: OBJECT);
  CONST range = FLOAT(maxI-minI);
  VAR   i,j: INTEGER; t: vec.VECTOR;
BEGIN
  WITH o DO
    FOR i:=0 TO top-1 DO
      FOR j:=X TO Z DO t[j]:=FLOAT(body[i][j]) END;
      vec.sub(t,VL,t);
      IF NOR THEN
        ints[i].val:=TRUNC(vec.sml(t,norms[i])*
                     FLOAT(xSCALE DIV 2)/vec.len(t))+xSCALE DIV 2;
      ELSE
        ints[i].val:=ABS(TRUNC(vec.sml(t,norms[i])*FLOAT(xSCALE)/vec.len(t)))
      END
    END
  END
END light;

PROCEDURE view(VAR o: OBJECT);
  CONST f = a*SS DIV b;
       sW =   SW DIV 2;
  VAR z: INTEGER;
    i,j: INTEGER;
    col: INTEGER;
BEGIN
  IF FRAME THEN vio.erase; frame ELSE light(o) END;
  WITH o DO
    FOR i:=0 TO top-1 DO
      z:=VP+image[i][Z];
      image[i][X]:= f*image[i][X] DIV z+sW;
      image[i][Y]:=-f*image[i][Y] DIV z+sC;
--    image[i][X]:= f*image[i][X] DIV VP+sW;
--    image[i][Y]:=-f*image[i][Y] DIV VP+sC;
    END;
    IF FRAME THEN
      FOR i:=0 TO tri-1 DO tria(o,poly[i],7) END;
      IF TRAIN THEN tria(o,poly[tri_no],9) END;
      RETURN
    END
  END;
  vio.erase; frame; scan(o);
END view;

VAR E: M4;

VAR name: ARRAY [0..127] OF CHAR;
    ftno: INTEGER;

PROCEDURE foto;
  VAR f: bio.FILE;
   path: ARRAY [0..127] OF CHAR;
BEGIN
  str.print(path,'%s.%d.igd',name,ftno);
  bio.create(f,path,'w',128*1024);
  bio.write(f,1F8000h,128*1024);
  bio.close(f);
END foto;

PROCEDURE compute_views(o: OBJECT);

  VAR R,RY,RZ: M4;  dir: INTEGER;


  PROCEDURE light_pos(dir,lr,ud: INTEGER);
  BEGIN
    CASE dir OF
      | X : VL[Y]:=VL[Y]+FLOAT(VP*lr);
            VL[Z]:=VL[Z]+FLOAT(VP*ud);
      | Y : VL[X]:=VL[X]-FLOAT(VP*lr);
            VL[Z]:=VL[Z]+FLOAT(VP*ud);
      | Z : VL[Y]:=VL[Y]-FLOAT(VP*ud);
            VL[X]:=VL[X]-FLOAT(VP*lr);
    ELSE
    END
  END light_pos;

  PROCEDURE image;
    VAR i,j: INTEGER;  t: INTEGER;
  BEGIN
    WITH o DO
      CASE dir OF
        |X: FOR i:=0 TO top-1 DO
              image[i][Z]:=body[i][X];
              image[i][Y]:=body[i][Y];
              image[i][X]:=body[i][Z];
            END;
        |Y: FOR i:=0 TO top-1 DO
              image[i][Z]:=body[i][Y];
              image[i][X]:=body[i][X];
              image[i][Y]:=body[i][Z];
            END
        |Z: FOR i:=0 TO top-1 DO
              o.image[i]:=o.body[i]
            END
      END
    END
  END image;

  PROCEDURE rotate(fi,ksi,tau: INTEGER);
    VAR i,j: INTEGER;  RX,RY: M4;
  BEGIN
    RX:=E;
    RX[1,1]:=cos[fi ]; RX[1,2]:=-sin[fi ];
    RX[2,1]:=sin[fi ]; RX[2,2]:= cos[fi ];
    RY:=E;
    RY[0,0]:= cos[ksi]; RY[0,2]:=sin[ksi];
    RY[2,0]:=-sin[ksi]; RY[2,2]:=cos[ksi];
    RZ:=E;
    RZ[0,0]:= cos[tau]; RZ[0,1]:=sin[tau];
    RZ[1,0]:=-sin[tau]; RZ[1,1]:=cos[tau];
    MxM(R,RY,RX);
    MxM(R,R,RZ);
    FOR i:=0 TO o.top-1 DO VxM(o.image[i],o.image[i],R) END
  END rotate;

---------------------------- NORMALS ---------------------------
                            ---------
  PROCEDURE show_normal(VAR o: OBJECT; t: INTEGER);
    VAR cnt: def.TOP;  cos,sin: INTEGER;
        i,j: INTEGER;  v1,v2,N: vec.VECTOR;
        len: REAL;     Rx,Ry  : M4;
  BEGIN
    dir:=Z; image;  cnt:=o.image[o.poly[t].tops[0]]; cnt[W]:=1;
    WITH o DO
      FOR i:=0 TO top-1 DO
        FOR j:=X TO Z DO image[i][j]:=image[i][j]-cnt[j] END
      END
    END;
    WITH o.poly[t] DO
      FOR i:=X TO Z DO
        v1[i]:=FLOAT(o.image[tops[1]][i]);
        v2[i]:=FLOAT(o.image[tops[2]][i]);
      END
    END;
    vec.vml(N,v1,v2);  vec.normal(N);
    Ry:=E;  Rx:=E;  len:=mat.sqrt(N[X]*N[X]+N[Z]*N[Z]);

    cos:=mat.round(N[Z]/len*FLOAT(oSCALE));  Ry[0,0]:= cos;  Ry[2,2]:= cos;
    sin:=mat.round(N[X]/len*FLOAT(oSCALE));  Ry[2,0]:=-sin;  Ry[0,2]:= sin;

    cos:=mat.round(N[Y]*FLOAT(oSCALE));      Rx[1,1]:= cos;  Rx[2,2]:= cos;
    sin:=mat.round(len *FLOAT(oSCALE));      Rx[2,1]:= sin;  Rx[1,2]:=-sin;

    MxM(R,Ry,Rx);

    FOR i:=0 TO o.top-1 DO VxM(o.image[i],o.image[i],R) END;

    FRAME:=TRUE;
    TRAIN:=TRUE;
    tri_no:=t;
    view(o);
  END show_normal;

  PROCEDURE normals(VAR o: OBJECT);
    VAR i,j,k,c: INTEGER;  v ,v0: def.VECTOR;
         list,I: INTEGER;  v1,v2: def.VECTOR;
         L: REAL;           case: INTEGER;
  BEGIN
   WITH o DO
     FOR k:=0 TO top-1 DO norms[k]:=vec.null END;
     FOR i:=0 TO tri-1 DO
       WITH poly[i] DO
          FOR j:=X TO Z DO
            v0[j]:=FLOAT(image[tops[0]][j] DIV def.iSCALE);
            v1[j]:=FLOAT(image[tops[1]][j] DIV def.iSCALE);
            v2[j]:=FLOAT(image[tops[2]][j] DIV def.iSCALE);
          END;
          case:=0;
          IF vec.equal(v0,v1) THEN case:=1      END;
          IF vec.equal(v0,v2) THEN case:=case+2 END;
          IF vec.equal(v2,v1) THEN case:=case+4 END;
          CASE case OF
            |0: vec.sub(v1,v1,v0); vec.sub(v2,v2,v0); vec.vml(norm,v1,v2);
            |1: vec.sub(norm,v2,v0)
            |2: vec.sub(norm,v1,v0)
            |4: vec.sub(norm,v0,v1)
            |7: norm:=vec.null; norm[Z]:=1.;
          ELSE
            ASSERT(FALSE)
          END;
          vec.normal(norm);
          FOR k:=0 TO 2 DO vec.add(norms[tops[k]],norms[tops[k]],norm) END
        END
      END;
      FOR k:=0 TO top-1 DO
        IF NOT vec.equal(norms[k],vec.null) THEN vec.normal(norms[k]) END
      END
    END
  END normals;

----------------------------- write ----------------------------
                             -------
  VAR file: bio.FILE;
      path: ARRAY [0..127] OF CHAR;

  PROCEDURE wr(adr: SYSTEM.ADDRESS; sz: INTEGER);
  BEGIN
    bio.write(file,adr,sz*4);
    IF NOT bio.done THEN
      std.perror(bio.error,'%s write file "%s": %%s\n',CANT,path); HALT(1)
    END
  END wr;

  PROCEDURE write(VAL o: def.OBJECT);
    VAR size: INTEGER;
         obj: def.OBJECT;
  BEGIN
    str.print(path,"%s.%s",o.name,'objg1');
    size:=SIZE(o)+SIZE(o.name)+SIZE(o.body)+SIZE(o.poly);
    bio.create(file,path,'m',size*4);
    IF NOT bio.done THEN
      std.perror(bio.error,'%s open file "%s": %%s\n',CANT,path); HALT(1)
    END;
    obj:=o;
    WITH obj DO
      image^.ADR:=NIL;  image^.HIGH:=-1;
      norms^.ADR:=NIL;
      name^.ADR:=NIL;   poly^.ADR:=NIL;
      body^.ADR:=NIL;   ints^.ADR:=NIL;
    END;
    wr(SYSTEM.ADR(obj    ),SIZE(obj    ));
    wr(SYSTEM.ADR(o.name ),SIZE(o.name ));
    wr(SYSTEM.ADR(o.poly ),SIZE(o.poly ));
    wr(SYSTEM.ADR(o.body ),SIZE(o.body ));
    wr(SYSTEM.ADR(o.ints ),SIZE(o.ints ));
    wr(SYSTEM.ADR(o.norms),SIZE(o.norms));
    bio.close(file);
    IF NOT bio.done THEN
      std.perror(bio.error,'%s close file "%s": %%s\n',CANT,path); HALT(1)
    END
  END write;

-------------------------- END NORMALS -------------------------
                          -------------

  PROCEDURE generate;
    VAR f,fi,ksi,hi: INTEGER;
  BEGIN
    FRAME:=FALSE;

    FOR f:=-(RUMBS DIV 4) TO RUMBS DIV 4 DO
      IF ABS(f)=RUMBS DIV 4 THEN hi:=0 ELSE hi:=RUMBS - 1 END;
      fi:=(f+RUMBS) MOD RUMBS;
      FOR ksi:=0 TO hi DO
        put_rumb(ksi,fi); image; rotate(fi,ksi,0);  view(o)
      END;
    END
  END generate;

  VAR ch: CHAR;
     i,j: INTEGER;
  fi,ksi: INTEGER;
     tau: INTEGER;

BEGIN
  OBJCT:=TRUE;
  LIGHT:=FALSE;
  WITH o DO
    scale:=max(o.box[X],o.box[Y],o.box[Z]);
--    VP:=(a*oSCALE*5 DIV b) DIV 4;  (* 7/4=1.75 ~=~ sqrt(3) *)
    VP:=(a*oSCALE*5 DIV b) DIV 4 - oSCALE*2;
    VL[X]:=0.; VL[Y]:=0.; VL[Z]:=FLOAT(VP);
    FOR i:=0 TO top-1 DO
      body[i][X]:=body[i][X]*oSCALE DIV scale;
      body[i][Y]:=body[i][Y]*oSCALE DIV scale;
      body[i][Z]:=body[i][Z]*oSCALE DIV scale;
    END
  END;

  fi:=0; ksi:=0; tau:=0; dir:=Z; ftno:=0;
  LOOP
    IF OBJCT THEN
      image; rotate(fi,ksi,tau); view(o); FRAME:=TRUE; OBJCT:=FALSE
    END;
    key.read(ch);
    CASE ch OF
      |'x','X'  : dir:=X;                OBJCT:=TRUE
      |'y','Y'  : dir:=Y;                OBJCT:=TRUE
      |'z','Z'  : dir:=Z;                OBJCT:=TRUE
      |'f','F'  : FRAME:=FALSE;          OBJCT:=TRUE; LIGHT:=FALSE
      |'+','='  : VP:=VP-oSCALE;
      |'-','_'  : VP:=VP+oSCALE;
      |'l','L'  : LIGHT:=TRUE;
      |'o','O'  : OBJCT:=TRUE; LIGHT:=FALSE;
      |"<"      : tau:=(tau+RUMBS+1) MOD RUMBS
      |">"      : tau:=(tau+RUMBS-1) MOD RUMBS
      |key.right:
                  IF LIGHT THEN light_pos(dir,-1,0)
                  ELSE ksi:=(ksi+RUMBS+1) MOD RUMBS
                  END;
      |key.left : IF LIGHT THEN light_pos(dir, 1,0)
                  ELSE ksi:=(ksi+RUMBS-1) MOD RUMBS
                  END;
      |key.dw   : IF LIGHT THEN light_pos(dir,0, 1)
                  ELSE  fi:=(fi+RUMBS+1) MOD RUMBS
                  END;
      |key.up   : IF LIGHT THEN light_pos(dir,0,-1)
                  ELSE fi:=(fi+RUMBS-1) MOD RUMBS
                  END;
      |key.exit : WRITE:=TRUE;     FRAME:=FALSE;
                  create(name,"pic");
                  image; rotate(fi,ksi,tau);  view(o);
                  close;
                  WRITE:=FALSE;    FRAME:=TRUE;
      |'g','G'  : WRITE:=TRUE;
                  create(name,"o3j");
                  generate;
                  close;
                  WRITE:=FALSE; RETURN
      |006c     : foto; INC(ftno);
      |"0"      : fi:=0; ksi:=0; tau:=0; OBJCT:=TRUE
      |"t","T"  : TRAIN:=NOT TRAIN
      |"n","N"  : WITH o DO
                    tria(o,poly[tri_no],7);
                    tri_no:=(tri_no+1) MOD tri;
                    tria(o,poly[tri_no],9);
                    WITH poly[tri_no] DO
                      tty.print('%d: [%d %d %d]\n',tri_no
                                                  ,ints[tops[0]].val
                                                  ,ints[tops[1]].val
                                                  ,ints[tops[2]].val);
                    END
                  END
      |"s","S"  : show_normal(o,tri_no); CHANGE:=TRUE
      |"c","C"  : WITH o.poly[tri_no] DO
                    i:=tops[1]; tops[1]:=tops[2]; tops[2]:=i;
                  END;
                  CHANGE:=FALSE
      |"w","W"  : normals(o); --write(o);
      |"u","U"  : str.print(picture,"%s.%02d",name,number);
                  fot.foto(vio.bmd,2,2,476,356,"3d.sld",picture);
                  vio.erase;
                  fot.show(vio.bmd,"3d.sld",picture);
                  INC(number);
                  key.read(ch);
      |33c      : RETURN
    ELSE
      OBJCT:=TRUE
    END
  END
END compute_views;

CONST PIx2=3.1415926535*2.;

PROCEDURE init_views;
  VAR i: INTEGER;
    x,d: REAL;
BEGIN
  fill(E,0);
  FOR i:=0 TO 3 DO E[i,i]:=oSCALE END;
  vio.erase;
  i:=0; x:=0.0;
  d:=PIx2/FLOAT(RUMBS);
  WHILE i<=HIGH(sin) DO
    cos[i]:=TRUNC(mat.cos(x)*FLOAT(oSCALE));
    sin[i]:=TRUNC(mat.sin(x)*FLOAT(oSCALE));
    x:=x+d; INC(i)
  END;
  vio.erase;
END init_views;

----------------------------  BODY  ----------------------------
                            --------
VAR o_ptr: POINTER TO OBJECT;

BEGIN
  IF HIGH(arg.words)<0 THEN HALT END;
  str.copy(name,arg.words[0]);
  out^.ADR:=NIL;  out^.HIGH:=-1;
  number:=0;
  FRAME:=TRUE;
  WRITE:=FALSE;
  CHANGE:=FALSE;
  init_views;
  o_ptr:=read(name);
  NOR:=    arg.flag('-','n');
  TRAIN:=  arg.flag('-','t');
  IF NOT arg.number('tri',tri_no) THEN tri_no:=0 END;
  compute_views(o_ptr^);
END gc11.
