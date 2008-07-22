PROCEDURE Vector(x,y: CARDINAL);
  VAR dx,dy,Xcnt,Ycnt,adx,ady,i: CARDINAL;
BEGIN
  IF X=x THEN
    LOOP
      Point(X,Y);
      IF y=Y THEN RETURN END;
      IF y<Y THEN DEC(Y) ELSE INC(Y) END;
    END;
  END;
  IF Y=y THEN
    LOOP
      Point(X,Y);
      IF x=X THEN RETURN END;
      IF x<X THEN DEC(X) ELSE INC(X) END;
    END;
  END;
  dx:=x-X; dy:=y-Y;
  adx:=ABS(dx); ady:=ABS(dy);
  Xcnt:=adx DIV 2; Ycnt:=ady DIV 2;
  IF dx>dy THEN
    LOOP
      Point(X,Y);
      IF X=x THEN Point(x,y); RETURN END;
      IF dx>0 THEN INC(X) ELSE DEC(X) END;
      INC(Ycnt,ady);
      IF Ycnt>=adx THEN
        DEC(Ycnt,adx);
        IF dy>0 THEN INC(Y) ELSE DEC(Y) END;
      END;
    END;
  ELSE
    LOOP
      Point(X,Y);
      IF Y=y THEN Point(x,y); RETURN END;
      IF dy>0 THEN INC(Y) ELSE DEC(Y) END;
      INC(Xcnt,adx);
      IF Xcnt>=ady THEN
        DEC(Xcnt,ady);
        IF dx>0 THEN INC(X) ELSE DEC(X) END;
      END;
    END;
  END;
END Vector;

