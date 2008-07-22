IMPLEMENTATION MODULE cdsImp; (* Sem 28-Dec-87. (c) KRONOS *)

-- Внимание!!!
-- Не реализован случай пересечения сегментов!

IMPORT  tty: Terminal;
IMPORT  mm : ModelMisc;
IMPORT  mcd: defCodes;

FROM Model      IMPORT  Object, Objects;
FROM pedEditor  IMPORT  Sheet;
FROM pedTopology IMPORT Del, App, Len, Side;

TYPE point=RECORD x,y,l: INTEGER END;

PROCEDURE Tag(o: Object): Objects;
CODE 0 mcd.lxb END Tag;

PROCEDURE SQ(x: REAL): REAL; CODE mcd.copt mcd.fmul END SQ;
PROCEDURE ISQ(x: INTEGER): INTEGER; CODE mcd.copt mcd.mul END ISQ;

PROCEDURE delta(x1,y1,x2,y2,x,y: INTEGER): INTEGER;
-- квадрат расстояния от точки до прямой
  VAR A,B,C: INTEGER;
BEGIN
  A:=y1-y2; B:=x2-x1; C:=-x1*A-y1*B;
  RETURN TRUNC(SQ(FLOAT(A*x+B*y+C))/FLOAT(ISQ(A)+ISQ(B)));
END delta;

PROCEDURE normal(x1,y1,x2,y2,x,y: INTEGER);
-- normX, normY - точка пересечения нормали с прямой
  VAR A,B,C: INTEGER;
BEGIN
  A:=y1-y2; B:=x2-x1; C:=-x1*A-y1*B;
  normX:=TRUNC(
           (FLOAT(ISQ(B))*FLOAT(x)-FLOAT(A*B)*FLOAT(y)-FLOAT(C)*FLOAT(A))/
            FLOAT(ISQ(A)+ISQ(B))
              );
  normY:=TRUNC(
           (FLOAT(ISQ(A))*FLOAT(y)-FLOAT(A*B)*FLOAT(x)-FLOAT(C)*FLOAT(B))/
            FLOAT(ISQ(A)+ISQ(B))
              );
END normal;

VAR pnt: ARRAY [0..99] OF point;
    pntcnt: INTEGER;

PROCEDURE Pnt(x,y: INTEGER; ls: BITSET);
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO 31 DO
    IF i IN ls*mm.Layer THEN
      pnt[pntcnt].x:=x; pnt[pntcnt].y:=y; pnt[pntcnt].l:=i;
      INC(pntcnt);
--print('point %d %d layer %d, ident %d\n',x,y,i,Ident);
    END;
  END;
END Pnt;

PROCEDURE clip(x1,y1,x2,y2: INTEGER);
  VAR i,j: INTEGER;
BEGIN
  ASSERT((x2-x1#0)OR(y2-y1#0));
  FOR i:=0 TO pntcnt-1 DO
    WITH pnt[i] DO
      IF delta(x1,y1,x2,y2,x,y)#0 THEN
        normal(x1,y1,x2,y2,x,y);
        x:=normX; y:=normY;
      END;
      j:=Side(x1,y1,x2,y2,x,y);
      IF j<0 THEN x:=x1; y:=y1 END;
      IF j>0 THEN x:=x2; y:=y2 END;
    END;
  END;
END clip;

PROCEDURE Seek(ExtPin: BOOLEAN);
  VAR x1,y1,x2,y2,sz: INTEGER; ls: BITSET;
      dx,dy,DX,DY,p1,p2,p3,p4,r,i,j,id: INTEGER;
BEGIN
  x1:=mm.X1; x2:=mm.X2; y1:=mm.Y1; y2:=mm.Y2;
  sz:=mm.Size; ls:=mm.Layer; pntcnt:=0; id:=mm.Ident;
  mm.StartConductor(mm.Signal,ExtPin);
  WHILE NOT mm.Empty DO
    IF (mm.Ident#id)&(ls*mm.Layer#{}) THEN
      r:=ISQ(mm.Size+sz);
      DX:=mm.X2-mm.X1; DY:=mm.Y2-mm.Y1;
      dx:=x2-x1; dy:=y2-y1;
      p1:=dx*(mm.Y1-y1)-dy*(mm.X1-x1);
      p2:=dx*(mm.Y2-y1)-dy*(mm.X2-x1);
      p3:=DX*(y1-mm.Y1)-DY*(x1-mm.X1);
      p4:=DX*(y2-mm.Y1)-DY*(x2-mm.X1);
      IF    ((dx#0)OR(dy#0))&(p1=0)&(p2=0) THEN
        -- второй отрезок лежит на осевой линии первого
        i:=Side(x1,y1,x2,y2,mm.X1,mm.Y1);
        j:=Side(x1,y1,x2,y2,mm.X2,mm.Y2);
        IF    (i<0)&(j<0) THEN
          IF Len(mm.X1,mm.Y1,mm.X2,mm.Y2,x1,y1)<r THEN Pnt(x1,y1,ls) END;
        ELSIF (i>0)&(j>0) THEN
          IF Len(mm.X1,mm.Y1,mm.X2,mm.Y2,x2,y2)<r THEN Pnt(x2,y2,ls) END;
        ELSE
          IF    i<0 THEN Pnt(x1,y1,ls)
          ELSIF i=0 THEN Pnt(mm.X1,mm.Y1,ls)
          ELSE           Pnt(x2,y2,ls)
          END;
          IF    j<0 THEN Pnt(x1,y1,ls)
          ELSIF j=0 THEN Pnt(mm.X2,mm.Y2,ls)
          ELSE           Pnt(x2,y2,ls)
          END;
        END;
      ELSIF ((DX#0)OR(DX#0))&(p3=0)&(p4=0) THEN
        -- первый отрезок лежит на осевой линии второго
        i:=Side(mm.X1,mm.Y1,mm.X2,mm.Y2,x1,y1);
        j:=Side(mm.X1,mm.Y1,mm.X2,mm.Y2,x2,y2);
        IF    (i<0)&(j<0) THEN
          IF Len(x1,y1,x2,y2,mm.X1,mm.Y1)<r THEN Pnt(mm.X1,mm.Y1,ls) END;
        ELSIF (i>0)&(j>0) THEN
          IF Len(x1,y1,x2,y2,mm.X2,mm.Y2)<r THEN Pnt(mm.X2,mm.Y2,ls) END;
        ELSE
          IF    i<0 THEN Pnt(mm.X1,mm.Y1,ls)
          ELSIF i=0 THEN Pnt(x1,y1,ls)
          ELSE           Pnt(mm.X2,mm.Y2,ls)
          END;
          IF    j<0 THEN Pnt(mm.X1,mm.Y1,ls)
          ELSIF j=0 THEN Pnt(x2,y2,ls)
          ELSE           Pnt(mm.X2,mm.Y2,ls)
          END;
        END;
      ELSIF ((dx#0)OR(dy#0))&(p1=0) THEN
        -- касание началом второго отрезка
        IF Len(x1,y1,x2,y2,mm.X1,mm.Y1)<r THEN Pnt(mm.X1,mm.Y1,ls) END;
      ELSIF ((dx#0)OR(dy#0))&(p2=0) THEN
        -- касание концом второго отрезка
        IF Len(x1,y1,x2,y2,mm.X2,mm.Y2)<r THEN Pnt(mm.X2,mm.Y2,ls) END;
      ELSIF ((DX#0)OR(DY#0))&(p3=0) THEN
        -- касание началом первого отрезка
        IF Len(mm.X1,mm.Y1,mm.X2,mm.Y2,x1,y1)<r THEN Pnt(x1,y1,ls) END;
      ELSIF ((DX#0)OR(DY#0))&(p4=0) THEN
        -- касание концом первого отрезка
        IF Len(mm.X1,mm.Y1,mm.X2,mm.Y2,x2,y2)<r THEN Pnt(x2,y2,ls) END;
      ELSIF ((p1>0)#(p2>0))&((p3>0)#(p4>0)) THEN
        -- отрезки пересекаются
        Pnt(x1,y1,ls); Pnt(x2,y2,ls);
      ELSIF (dx=0)&(dy=0) THEN
        IF Len(mm.X1,mm.Y1,mm.X2,mm.Y2,x1,y1)<ISQ(mm.Size+sz) THEN Pnt(x1,y1,ls) END;
      ELSIF (DX=0)&(DY=0) THEN
        i:=Side(x1,y1,x2,y2,mm.X1,mm.Y1);
        IF    i<0 THEN IF ISQ(mm.X1-x1)+ISQ(mm.Y1-y1)<r THEN Pnt(x1,y1,ls) END;
        ELSIF i>0 THEN IF ISQ(mm.X1-x2)+ISQ(mm.Y1-y2)<r THEN Pnt(x2,y2,ls) END;
        ELSIF delta(x1,y1,x2,y2,mm.X1,mm.Y1)<r THEN
          normal(x1,y1,x2,y2,mm.X1,mm.Y1); Pnt(normX,normY,ls);
        END;
      ELSE
        IF (Len(x1,y1,x2,y2,mm.X1,mm.Y1)<r) OR
           (Len(x1,y1,x2,y2,mm.X2,mm.Y2)<r) OR
           (Len(mm.X1,mm.Y1,mm.X2,mm.Y2,x1,y1)<r) OR
           (Len(mm.X1,mm.Y1,mm.X2,mm.Y2,x2,y2)<r) THEN Pnt(x1,y1,ls); Pnt(x2,y2,ls) END;
      END;
    END;
    mm.NextConductor;
  END;
END Seek;

PROCEDURE Bril(s: Object; sht: Sheet);
  VAR i,delcnt,min,minj,max,maxj,minX,minY,maxX,maxY,j,cnt: INTEGER;
      ls: BITSET;
BEGIN
  IF Tag(s)#signal THEN RETURN END;
tty.print('signal %s\n',s^.Name);
tty.print('Epin %d\n',sht^.PublicContext^.ExtPin);
  REPEAT
    delcnt:=0;
    mm.StartConductor(s,sht^.PublicContext^.ExtPin);
    WHILE NOT mm.Empty DO
      IF NOT mm.Fixed THEN
        i:=mm.Ident;
tty.print('bril %d %d --- %d %d ident %d\n',mm.X1,mm.Y1,mm.X2,mm.Y2,mm.Ident);

        Seek(sht^.PublicContext^.ExtPin);

        mm.FindConductor(i);
        IF (mm.X1=mm.X2)&(mm.Y1=mm.Y2) THEN
          ls:={}; cnt:=0;
          FOR j:=0 TO pntcnt-1 DO
            IF pnt[j].l IN mm.Layer THEN INCL(ls,pnt[j].l) END;
          END;
          FOR j:=0 TO 31 DO IF j IN ls THEN INC(cnt) END END;
          IF cnt<2 THEN Del(sht); INC(delcnt) END;
        ELSE
          IF pntcnt<2 THEN
            Del(sht); INC(delcnt);
          ELSE
            clip(mm.X1,mm.Y1,mm.X2,mm.Y2);
            minX:=pnt[0].x; maxX:=pnt[0].x;
            minY:=pnt[0].y; maxY:=pnt[0].y;
            min:=(mm.X1-minX)*(mm.X1-minX)+(mm.Y1-minY)*(mm.Y1-minY);
            max:=(mm.X2-maxX)*(mm.X2-maxX)+(mm.Y2-maxY)*(mm.Y2-maxY);
            FOR j:=1 TO pntcnt-1 DO
              minj:=(mm.X1-pnt[j].x)*(mm.X1-pnt[j].x)+(mm.Y1-pnt[j].y)*(mm.Y1-pnt[j].y);
              maxj:=(mm.X2-pnt[j].x)*(mm.X2-pnt[j].x)+(mm.Y2-pnt[j].y)*(mm.Y2-pnt[j].y);
              IF min>minj THEN min:=minj; minX:=pnt[j].x; minY:=pnt[j].y END;
              IF max>maxj THEN max:=maxj; maxX:=pnt[j].x; maxY:=pnt[j].y END;
            END;
            IF (minX#mm.X1)OR(minY#mm.Y1)OR(maxX#mm.X2)OR(maxY#mm.Y2) THEN
              Del(sht); mm.X1:=minX; mm.X2:=maxX; mm.Y1:=minY; mm.Y2:=maxY; App(sht); INC(delcnt);
            END;
          END;
        END;
      END;
      mm.NextConductor;
    END;
  UNTIL delcnt=0;
END Bril;

END cdsImp.
