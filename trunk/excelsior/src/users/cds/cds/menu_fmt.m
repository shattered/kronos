PROCEDURE menu(VAL f: ARRAY OF CHAR; SEQ a: WORD);
  VAR c: CHAR; i,j: INTEGER; w: wnd.window; prop: BOOLEAN;
  PROCEDURE num(): INTEGER;
    VAR n: INTEGER; p: POINTER TO ARRAY [0..0FFFFh] OF CHAR;
  BEGIN
    n:=0;
    LOOP
      IF (c=0c) OR (c='|') THEN RETURN n END;
      IF c=',' THEN INC(i); c:=f[i]; RETURN n END;
      IF c='*' THEN
        INC(i); c:=f[i];
        IF j<=HIGH(a) THEN n:=a[j]; INC(j) END;
      ELSIF (c>='0') & (c<='9') THEN
        n:=n*10+(ORD(c)-ORD('0')); INC(i); c:=f[i];
      ELSIF c='?' THEN
        INC(i); c:=f[i];
        IF j<=HIGH(a) THEN
          p:=a[j]; INC(j);
          IF w#NIL THEN n:=vg.string_len(prop,p^,0) END;
        END;
      ELSE
        INC(i); c:=f[i];
      END;
    END;
  END num;
  PROCEDURE next;
  BEGIN
    WHILE (c#'|') & (c#0c) DO INC(i); c:=f[i] END;
    IF c='|' THEN INC(i); c:=f[i] END;
  END next;
  PROCEDURE mode(): vg.modes;
    VAR m: vg.modes;
  BEGIN
    IF c='b' THEN m:=vg.bic
    ELSIF c='o' THEN m:=vg.or
    ELSIF c='x' THEN m:=vg.xor
    ELSE m:=vg.rep
    END;
    LOOP
      IF (c=0c) OR (c='|') THEN RETURN m END;
      IF c=',' THEN INC(i); c:=f[i]; RETURN m END;
      INC(i); c:=f[i];
    END;
  END mode;
  PROCEDURE str(VAR s: ARRAY OF CHAR);
    VAR br: CHAR; n: INTEGER;
  BEGIN
    n:=0;
    IF c=0c THEN s:=''; RETURN END;
    IF c='*' THEN
      IF j>HIGH(a) THEN s:=''; RETURN END;
      p:=a[j]; INC(j);
      WHILE (n<HIGH(s)) & (p^[n]#0c) DO s[n]:=p^[n]; INC(n) END;
      s[n]:=0c; INC(i); c:=f[i];
    ELSE
      br:=c; INC(i); c:=f[i];
      LOOP
        IF c=0c THEN s[n]:=0c; RETURN END;
        IF c=br THEN INC(i); c:=f[i]; s[n]:=0c; EXIT END;
        IF n<HIGH(s) THEN s[n]:=c; INC(n) END;
        INC(i); c:=f[i];
      END;
    END;
    LOOP
      IF (c=0c) OR (c='|') THEN RETURN END;
      IF c=',' THEN INC(i); c:=f[i]; RETURN END;
      INC(i); c:=f[i];
    END;
  END str;
  VAR x,y: INTEGER; buf: ARRAY [0..255] OF CHAR;
BEGIN
  i:=0; j:=0; c:=f[i]; w:=NIL; prop:=FALSE; cl:=0;
  LOOP
    CASE c OF
      | 0c: IF w#NIL THEN wnd.open(w) END; RETURN
      |'w': INC(i); c:=f[i];
            w:=wnd.create(num(),num()); w^.x:=num(); w^.y:=num(); next;
            vg.color(w,1); vg.frame(w,0,0,w^.sx-1,w^.sy-1);
            vg.color(w,2); vg.box(w,0,1,w^.sx-2,w^.sy-1);
      |'f': INC(i); c:=f[i]; prop:=BOOLEAN(num());
            IF w#NIL THEN vg.prop_font(w,prop) END; next;
      |'t': INC(i); c:=f[i];
            IF w#NIL THEN
              x:=num(); y:=num(); str(buf); vg.write_string(w,x,y,buf);
            END;
            next;
      |'b': INC(i); c:=f[i];
            x:=num(); y:=num(); sx:=num(); sy:=num(); next;
            IF w#NIL THEN
              vg.color(w,15);
            END;
      |'c': INC(i); c:=f[i]; IF w#NIL THEN cl:=num(); vg.color(w,cl) END; next;
      |'m': INC(i); c:=f[i]; IF w#NIL THEN vg.mode(w,mode()) END; next;
    ELSE INC(i); c:=f[i]; next;
    END;
  END;
END menu;



BEGIN
  menu('w200,200,10,10|c15|mx|t15,15,+asdfgh+|');
