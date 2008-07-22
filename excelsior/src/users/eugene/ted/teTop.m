IMPLEMENTATION MODULE teTop; (* 25-Oct-89. (c) KRONOS *)

CONST
  max_xy=10000h;

PROCEDURE Allocate(VAR a: ADDRESS; s: INTEGER);
BEGIN
  ALLOCATE(a,s);
  ASSERT(a#NIL);
END Allocate;

PROCEDURE app_seg(VAR t: seg_tree; s: segment_rec);
  VAR
    p : seg_tree;
    ap: POINTER TO seg_tree;

  PROCEDURE insert;
    CONST app_no=8;
    VAR a: ADDRESS; ps: segment;
  BEGIN
    IF p^.seg_fr=0 THEN
      Allocate(a,(p.^seg_no+app_no)*SIZE(segment_rec));
      move(a+app_no*SIZE(segment_rec),p^.seg,p^.seg_no*SIZE(segment_rec));
      Deallocate(p^.seg,p^.seg_no*SIZE(segment_rec));
      p^.seg_fr:=app_no; INC(p^.seg_no,app_no); p^.seg:=a;
    END;
    DEC(p^.seg_fr);
    ps:=ADDRESS(p^.seg)+p^.seg_fr*SIZE(segment_rec);
    ps^:=s;
  END insert;

  PROCEDURE cat(h: seg_tree; x,y,d: INTEGER);
  BEGIN
    IF t=NIL THEN RETURN END;

  END cat;

BEGIN
  cat(t);
  p:=t; ap:=ADR(t); x:=0; y:=0; d:=max_xy;
  LOOP
    IF p=NIL THEN
      Allocate(p,SIZE(p^));
      p^.up:=NIL; p^.dw:=NIL;
      p^.seg_fr:=0; p^.seg_no:=0; p^.seg:=NIL;
      ap^:=p;
    END;
    IF s.x>x THEN ap:=ADR(p^.up); p:=ap^; x:=x+d;
    ELSIF s.x+s.sx<=x THEN ap:=ADR(p^.dw); p:=ap^; x:=x-d;
    ELSE insert; RETURN
    END;
    IF p=NIL THEN
      Allocate(p,SIZE(p^));
      p^.up:=NIL; p^.dw:=NIL;
      p^.seg_fr:=0; p^.seg_no:=0; p^.seg:=NIL;
      ap^:=p;
    END;
    IF s.y>y THEN ap:=ADR(p^.up); p:=ap^; y:=y+d;
    ELSIF s.y+s.sy<=y THEN ap:=ADR(p^.dw); p:=ap^; y:=y-d;
    ELSE insert; RETURN
    END;
    IF d=0 THEN insert; RETURN END;
    d:=d DIV 2;
  END;
END app_seg;

END teTop.
