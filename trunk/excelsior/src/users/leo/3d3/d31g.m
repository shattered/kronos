MODULE d31g; (* Leo  06-Feb-90. (c) KRONOS *)
             (* nick 01-Aug-90. (c) KRONOS *)

IMPORT  ASCII,SYSTEM;           IMPORT  cod: defCodes;
IMPORT  bio: BIO;               IMPORT  std: StdIO;
IMPORT  arg: tskArgs;           IMPORT  str: Strings;
IMPORT  tty: Terminal;          IMPORT  mat: realMath;
IMPORT  vec: Vectors;           IMPORT  low: lowLevel;
IMPORT  mem: Heap;              IMPORT  def: def31dg;

WITH STORAGE (NEW    : mem.ALLOCATE;
              DISPOSE: mem.DEALLOCATE;
              RESIZE : mem.REALLOCATE);

VAR WRITE: BOOLEAN;


-------------------------  LOW/LEVEL  --------------------------
                         -------------

(*$<$T-*)

PROCEDURE move(d,s: SYSTEM.ADDRESS; size: INTEGER); CODE cod.move END move;

PROCEDURE fill(VAR a: ARRAY OF SYSTEM.WORD; w: SYSTEM.WORD);
BEGIN a[0]:=w; move(SYSTEM.ADR(a[1]),SYSTEM.ADR(a[0]),HIGH(a)) END fill;

PROCEDURE zero(VAR a: ARRAY OF SYSTEM.WORD);
BEGIN a[0]:=0; move(SYSTEM.ADR(a[1]),SYSTEM.ADR(a[0]),HIGH(a)) END zero;

PROCEDURE swap(VAR a,b: SYSTEM.WORD);
  VAR i: INTEGER;
BEGIN i:=a; a:=b; b:=i END swap;

(*$>*)

----------------------------  I/O  -----------------------------
                            -------
CONST CANT = "can't";

VAR inp: STRING;

    pos: INTEGER;
    col: INTEGER;
   line: INTEGER;
   next: CHAR;

    out: DYNARR OF INTEGER;
    cou: INTEGER;
   outY: INTEGER;

PROCEDURE read(VAL name: ARRAY OF CHAR);
  VAR eof: INTEGER;
     path: ARRAY [0..127] OF CHAR;
     file: bio.FILE;
BEGIN
  str.print(path,"%s.3db",name);
  bio.open(file,path,'r');
  IF NOT bio.done THEN
    std.perror(bio.error,'%s open file "%s": %%s\n',CANT,path); HALT(1)
  END;
  eof:=bio.eof(file);
  IF eof<=0 THEN
    std.print('empty file "%s"\n',path); HALT(1)
  END;
  NEW(inp,eof);
  bio.read(file,inp.ADR,eof);
  IF NOT bio.done THEN
    std.perror(bio.error,'%s read file "%s": %%s\n',CANT,path); HALT(1)
  END;
  bio.close(file);
  pos:=0; line:=0; col:=0; next:=inp[0]; cou:=0;
END read;

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
     image.ADR:=NIL;  image.HIGH:=-1;
     norms.ADR:=NIL;
     name.ADR:=NIL;   poly.ADR:=NIL;
     body.ADR:=NIL;   ints.ADR:=NIL;
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

---------------------------  SCANER  ---------------------------
                           ----------

PROCEDURE error(VAL s: ARRAY OF CHAR);
BEGIN
  std.print('ERROR: syntax. At line=%3d col=%3d ***\n%s\n',line,col,s); HALT(1)
END error;

VAR ch: CHAR;

PROCEDURE getch;
BEGIN
  IF pos>HIGH(inp) THEN error('unexpceted end of file') END;
  ch:=next;
  INC(pos); INC(col);
  IF pos<=HIGH(inp) THEN next:=inp[pos] END;
  IF ch=ASCII.NL THEN col:=0; INC(line) END
END getch;

VAR sy: CHAR;
    id: ARRAY [0..15] OF CHAR;
   val: INTEGER;

PROCEDURE ident;
  VAR i: INTEGER;
BEGIN
  sy:='I';
  i:=0;
  WHILE ('A'<=CAP(ch)) & (CAP(ch)<='Z') OR ('0'<=ch) & (ch<='9') DO
    IF i<HIGH(id) THEN id[i]:=ch; INC(i) END;
    getch
  END;
  id[i]:=0c
END ident;

PROCEDURE number;
  VAR v,d,r: REAL;  sign: INTEGER;
BEGIN
  sy:='N';
  v:=0.0;
  IF    ch='-' THEN sign:=-1; getch
  ELSIF ch='+' THEN sign:=+1; getch
  ELSE              sign:=+1
  END;
  WHILE ('0'<=ch) & (ch<='9') DO
    d:=FLOAT(ORD(ch)-ORD('0'));
    v:=v*10.0+d; getch
  END;
  IF ch='.' THEN
    r:=0.1; getch;
    WHILE ('0'<=ch) & (ch<='9') DO
      d:=FLOAT(ORD(ch)-ORD('0'));
      v:=v+d*r; r:=r*0.1; getch
    END
  END;
  IF sign<0 THEN val:=-TRUNC(v*FLOAT(def.iSCALE))
  ELSE           val:=+TRUNC(v*FLOAT(def.iSCALE))
  END
END number;

PROCEDURE getsy;
BEGIN
  LOOP
    WHILE (ch=ASCII.NL) OR (ch=' ') DO getch END;
    IF (ch='-') & (next='-') THEN
      REPEAT getch UNTIL ch=ASCII.NL;
    ELSE
      EXIT
    END;
  END;
  IF ('A'<=CAP(ch)) & (CAP(ch)<='Z')       THEN ident
  ELSIF (ch='-') OR ('0'<=ch) & (ch<='9')  THEN number
  ELSE
    sy:=ch; getch
  END;
END getsy;

----------------------  DATA STRUCTURES  -----------------------
                      -------------------

CONST
  X = def.X;  Y = def.Y;  Z = def.Z;  W = def.W;

TYPE
  XYZ      = def.TOP;
  TRIANGLE = def.POLYGON;
  OBJECT   = def.OBJECT;

--------------------------  COMPILER  --------------------------
                          ------------


PROCEDURE get_xyz(VAR o: OBJECT; VAR i: INTEGER);
  VAR not_found: BOOLEAN;
      Top      : XYZ;
BEGIN
  Top[W]:=1;
  IF sy#'(' THEN error('"(" expected') END;           (* "(" *)        getsy;
  IF sy#'N' THEN error('X-coordinate expected') END;  Top[X]:=val; getsy;
  IF sy#'N' THEN error('Y-coordinate expected') END;  Top[Y]:=val; getsy;
  IF sy#'N' THEN error('Z-coordinate expected') END;  Top[Z]:=val; getsy;
  IF sy#')' THEN error('")" expected') END;           (* ")" *)        getsy;

  i:=0; not_found:=TRUE;
  WITH o DO
    IF top>HIGH(body) THEN RESIZE(body,top+8); RESIZE(ints,top+8) END;
    WHILE (i<top) & not_found DO
      not_found:=(Top[X]#body[i][X]) OR
                 (Top[Y]#body[i][Y]) OR
                 (Top[Z]#body[i][Z]);
      INC(i)
    END;
    IF not_found THEN
      body[top]:=Top;
      ints[top].val:=-1;
      ints[top].ref:=-1;
      INC(top); RETURN
    END;
    DEC(i);
  END
END get_xyz;

PROCEDURE set_T(VAR o: OBJECT; top,tri,ctop: INTEGER);
  VAR list,i,j: INTEGER;
BEGIN
  list:=o.ints[top].ref;
  IF list<0 THEN
    o.ints[top].ref:=tri*4+ctop; RETURN
  END;
  WHILE list#-1 DO
    i:=list DIV 4;
    j:=list MOD 4;
    list:=o.poly[i].refs[j]
  END;
  o.poly[i].refs[j]:=tri*4+ctop
END set_T;

PROCEDURE triangle(VAR o: OBJECT);      --- чтение треугольника
  VAR i: INTEGER;
BEGIN
  getsy; (* "TRIA" *)
  WITH o DO
    IF tri>HIGH(poly) THEN RESIZE(poly,tri+8)  END;
    IF (sy#'N') THEN error('"color" expected') END;
    WITH poly[tri] DO
      norm:=vec.null;
      color:=val DIV def.iSCALE; getsy;
      FOR i:=0 TO 2 DO
        refs[i]:=-1; get_xyz(o,tops[i]);
        set_T(o,tops[i],tri,i);
      END;
      next:=-1; case:=-1; ref:=-1;
      IF (tops[0]=tops[1]) & (tops[0]=tops[2]) THEN norm[Z]:=1. END
    END;
    INC(tri)
  END
END triangle;

PROCEDURE line2tri(VAR o: OBJECT);
  VAR i,m,t: INTEGER;
BEGIN
  getsy; (* "LINE" *)
  WITH o DO
    IF tri>HIGH(body) THEN RESIZE(poly,tri+8)  END;
    IF (sy#'N') THEN error('"color" expected') END;
    WITH poly[tri] DO
      norm:=vec.null;
      color:=val DIV def.iSCALE; getsy;
      FOR i:=0 TO 1 DO
        refs[i]:=-1; get_xyz(o,tops[i]);
        set_T(o,tops[i],tri,i)
      END;
      refs[2]:=-1; tops[2]:=tops[1];
      next:=-1; case:=-1; ref:=-1;
      norm[Z]:= 1.
    END;
    INC(tri)
  END
END line2tri;

PROCEDURE rect(VAR o: OBJECT);
  VAR i,j: INTEGER;
BEGIN
  getsy; (* "RECT" *)
  WITH o DO
    IF tri+1>HIGH(poly) THEN RESIZE(poly,tri+8) END;
    IF (sy#'N') THEN error('"color" expected') END;
    poly[tri].color:=val DIV def.iSCALE; getsy;
    WITH poly[tri] DO
      norm:=vec.null;
      FOR i:=0 TO 2 DO
        refs[i]:=-1; get_xyz(o,tops[i]);
        set_T(o,tops[i],tri,i)
      END;
      IF (tops[0]=tops[1]) & (tops[0]=tops[2]) THEN norm[Z]:=1. END;
      next:=-1; case:=-1; ref:=-1
    END;
    INC(tri);
    poly[tri]:=poly[tri-1];
    WITH poly[tri] DO
      get_xyz(o,tops[1]); set_T(o,tops[1],tri,1);
      IF (tops[0]=tops[1]) & (tops[0]=tops[2]) THEN norm[Z]:=1. END;
    END;
    INC(tri)
  END
END rect;

PROCEDURE normals(VAR o: OBJECT);
  VAR i,j,k,c: INTEGER;  v ,v0: def.VECTOR;
       list,I: INTEGER;  v1,v2: def.VECTOR;
       L: REAL;
BEGIN
  WITH o DO
    FOR i:=0 TO tri-1 DO
      WITH poly[i] DO
        FOR j:=X TO Z DO
          v0[j]:=FLOAT(body[tops[0]][j] DIV def.iSCALE);
          v1[j]:=FLOAT(body[tops[1]][j] DIV def.iSCALE);
          v2[j]:=FLOAT(body[tops[2]][j] DIV def.iSCALE);
        END;
        IF vec.equal(norm,vec.null) THEN
          IF NOT vec.equal(v0,v1) & NOT vec.equal(v0,v2) THEN
            vec.sub(v1,v1,v0);
            vec.sub(v2,v2,v0);
            vec.vml(norm,v1,v2);
          ELSIF vec.equal(v0,v1) THEN
            FOR j:=X TO Z DO norm[j]:=v2[j]-v0[j] END
          ELSIF vec.equal(v0,v2) THEN
            FOR j:=X TO Z DO norm[j]:=v1[j]-v0[j] END
          ELSE
            norm[X]:=0.; norm[Y]:=0.; norm[Z]:=1.
          END;
--          vec.normal(norm)
        END
      END
    END;
    FOR k:=0 TO top-1 DO
      list:=ints[k].ref;
      c:=0; I:=0; L:=0.;
      low.fill(norms[k],0.);
      REPEAT
        i:=list DIV 4;
        j:=list MOD 4;
        vec.add(v,norms[k],poly[i].norm);
        IF vec.len(v) >= L THEN  norms[k]:=v
        ELSE  vec.sub(norms[k],norms[k],poly[i].norm)
        END;
        L:=vec.len(norms[k]);
        list:=poly[i].refs[j]
      UNTIL list=-1;
      FOR c:=X TO Z DO norms[k][c]:=norms[k][c]/L END
    END
  END
END normals;

PROCEDURE center(VAR o: OBJECT);
  VAR i,j: INTEGER;
BEGIN
  WITH o DO
    FOR i:=0 TO top-1 DO
      FOR j:=X TO Z DO
       IF body[i][j]<o.cen[j] THEN o.cen[j]:=body[i][j] END; --- min coor
       IF body[i][j]>o.box[j] THEN o.box[j]:=body[i][j] END; --- max coor
      END
    END;
    FOR j:=X TO Z DO DEC(box[j],cen[j]) END;                 --- real size
    FOR i:=0 TO top-1 DO
      FOR j:=X TO Z DO DEC(body[i][j],cen[j]+box[j] DIV 2) END
    END;
    FOR j:=X TO Z DO cen[j]:=box[j] DIV 2 END                --- real center
  END
;tty.print('STATISTICS:\n  Object "%s" triangles=%d tops=%d\n',
           o.name,o.tri,o.top);
END center;

PROCEDURE show(VAL o: OBJECT);
  VAR i: INTEGER; list,tri,top: INTEGER;
BEGIN
  IF NOT arg.flag('+','v') THEN RETURN END;
  FOR i:=0 TO o.top-1 DO
tty.print('*******\n');
    list:=o.ints[i].ref;
    REPEAT
      tri:=list DIV 4;
      top:=list MOD 4;
tty.print('tri=%d top=%d list=%d\n',tri,top,list);
      list:=o.poly[tri].refs[top];
    UNTIL list=-1;
  END;
END show;

PROCEDURE object_dcl;
  VAR o: OBJECT;
BEGIN
  fill(o.cen,MAX(INTEGER)); o.cen[W]:=1;
  fill(o.box,MIN(INTEGER)); o.cen[W]:=1;
  getsy; (* OBJECT *)
  IF sy#'I' THEN error('ident expected') END;
  NEW(o.name,str.len(id)+1);
  str.copy(o.name,id);
  getsy; (*id*)
  NEW(o.poly);  o.tri:=0;
  NEW(o.body);  o.top:=0;
  NEW(o.ints);
  NEW(o.norms);
  NEW(o.image);
  LOOP
    IF sy#'I' THEN error('ident expected') END;
    IF    id='TRIA' THEN triangle(o)
    ELSIF id='LINE' THEN line2tri(o)
    ELSIF id='RECT' THEN rect(o)
    ELSIF id='END'  THEN
      getsy;
      IF (sy#'I') OR (id#o.name) THEN error("must be object's name") END;
      getsy; EXIT
    ELSE error('"TRIA" expected')
    END
  END;
  RESIZE(o.body ,o.top);
  RESIZE(o.ints ,o.top);
  RESIZE(o.norms,o.top);
  center(o);
  show(o);
  normals(o);
  write(o);
  DISPOSE(o.body);  DISPOSE(o.ints);
  DISPOSE(o.poly);  DISPOSE(o.name);
  DISPOSE(o.norms)
END object_dcl;

PROCEDURE compile;
BEGIN
  getsy;
  WHILE (sy='I') & (id="OBJECT") DO
    object_dcl
  END;
  IF sy#'.' THEN error('"." or "OBJECT" expected') END
END compile;


VAR name: ARRAY [0..63] OF CHAR;

BEGIN
  IF HIGH(arg.words)<0 THEN HALT END;
  str.copy(name,arg.words[0]);
  inp.ADR:=NIL;  inp.HIGH:=-1;
  out.ADR:=NIL;  out.HIGH:=-1;
  id:=""; val:=0; ch:=' ';
  WRITE:=FALSE;
  read(name);
  compile
END d31g.
