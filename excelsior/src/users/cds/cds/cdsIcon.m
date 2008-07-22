IMPLEMENTATION MODULE cdsIcon; (*  25-Feb-91. (c) KRONOS *)

FROM libWindows  IMPORT window;
FROM SYSTEM      IMPORT ADR;

IMPORT  vg  : cdsGrafic;
IMPORT  wnd : libWindows;
IMPORT  mem : libHeap;
IMPORT  str : Strings;

WITH STORAGE : mem;

TYPE
  icon_kind = (ik_number,ik_pcb_num,ik_on_off_ik_close);

  ICON = POINTER TO icon;
  icon = RECORD
    nxt  : ICON;
    ww   : window;
    x,y  : INTEGER;
    sx,sy: INTEGER;
    CASE kind : icon_kind OF
      |ik_on_off:
        o_val: POINTER TO BOOLEAN;
      |ik_number:
        n_val: POINTER TO INTEGER;
        n_fr : INTEGER;
        n_to : INTEGER;
      |ik_pcb_num:
        p_val: POINTER TO INTEGER;
      |ik_close:
        c_ww : POINTER TO window;
        c_ic : POINTER TO ICON;
    END;
  END;

PROCEDURE close(VAR w: window; VAR i: ICON; x,y: INTEGER);
BEGIN
END close;

PROCEDURE drow_on_off(w: window; i: ICON);
BEGIN
  vg.prop_font(w,TRUE); w^.patt:={0..31};
  vg.mode(w,vg.bic); vg.color(w,15);
  vg.box(w,i^.x,i^.y,i^.x+i^.sx-1,i^.y+i^.sy-1);
  vg.mode(w,vg.rep); vg.color(w,2);
  vg.box(w,i^.x+1,i^.y,i^.x+i^.sx-1,i^.y+i^.sy-2);
  vg.mode(w,vg.rep); vg.color(w,1);
  vg.frame(w,i^.x,i^.y,i^.x+i^.sx-1,i^.y+i^.sy-1);
  vg.mode(w,vg.bic); vg.color(w,3);
  IF i^.o_val^ THEN
    vg.write_string(w,i^.x+6,i^.y+2,'on');
  ELSE
    vg.write_string(w,i^.x+3,i^.y+2,'off');
  END;
END drow_on_off;

PROCEDURE do_on_off(w: window; i: ICON; x,y: INTEGER; ch: CHAR): BOOLEAN;
BEGIN
  IF ch#wnd.l_on THEN RETURN FALSE END;
  i^.o_val^:=NOT i^.o_val^;
  drow_on_off(w,i);
  wnd.ref_box(w,i^.y,i^.sy);
  RETURN TRUE;
END do_on_off;

PROCEDURE on_off(w: window; VAR i: ICON; x,y: INTEGER; VAR v: BOOLEAN);
  CONST x_size=31; y_size=15;
  VAR ii: ICON;
BEGIN
  NEW(ii); ii^.nxt:=i; i:=ii; i^.ww:=w;
  i^.x:=x; i^.y:=y;
  i^.sx:=x_size; i^.sy:=y_size;
  i^.kind:=ik_on_off;
  i^.o_val:=ADR(v);
  drow_on_off(w,i);
END on_off;

PROCEDURE do_number(w: window; i: ICON; x,y: INTEGER; ch: CHAR): BOOLEAN;
  PROCEDURE print_num(VAR n: INTEGER; color: INTEGER);
    VAR l: INTEGER; ln: ARRAY [0..79] OF CHAR;
  BEGIN
    vg.prop_font(w,FALSE); w^.patt:={0..31};
    vg.mode(w,vg.bic); vg.color(w,15);
    vg.box(w,i^.x,i^.y,i^.x+i^.sx-1,i^.y+i^.sy-1);
    vg.mode(w,vg.rep); vg.color(w,2);
    vg.box(w,i^.x+1,i^.y,i^.x+i^.sx-1,i^.y+i^.sy-2);
    vg.color(w,1); vg.frame(w,i^.x,i^.y,i^.x+i^.sx-1,i^.y+i^.sy-1);
    LOOP
      str.print(ln,'%d',n); l:=vg.string_len(FALSE,ln,0);
      IF l<=i^.sx-4 THEN EXIT END;
      n:=n/10;
    END;
    vg.mode(w,vg.xor); vg.color(w,INTEGER({1}/BITSET(color)));
    vg.write_string(w,i^.x+i^.sx-l-1,i^.y+(i^.sy-vg.char_h) DIV 2,ln);
  END print_num;
  VAR n,xx,yy: INTEGER;
BEGIN
  IF ch=1c THEN print_num(i^.n_val^,0); RETURN TRUE END;
  IF ch#wnd.l_on THEN RETURN FALSE END;
  wnd.del_crs; n:=0;
  LOOP
    print_num(n,8); wnd.ref_box(w,i^.y,i^.sy);
    REPEAT wnd.wait; wnd.first(xx,yy,ch); wnd.drop UNTIL ch#0c;
    IF (ch=15c) OR (ch=wnd.l_on) THEN i^.n_val^:=n; EXIT END;
    IF (ch=33c) OR (ch=wnd.r_on) THEN EXIT END;
    IF ch=10c THEN n:=n/10
    ELSIF ch='-' THEN IF n#MIN(INTEGER) THEN n:=-n END;
    ELSIF ch=' ' THEN n:=0;
    ELSIF (ch>='0') & (ch<='9') THEN
      IF n>=0 THEN
        IF n < (MAX(INTEGER)-10)/10 THEN n:=n*10+(ORD(ch)-ORD('0')) END;
      ELSE
        IF n > (MIN(INTEGER)+10)/10 THEN n:=n*10-(ORD(ch)-ORD('0')) END;
      END;
    END;
  END;
  IF i^.n_val^>i^.n_to THEN i^.n_val^:=i^.n_to
  ELSIF i^.n_val^<i^.n_fr THEN i^.n_val^:=i^.n_fr
  END;
  print_num(i^.n_val^,0);
  wnd.ref_box(w,i^.y,i^.sy);
  RETURN TRUE;
END do_number;

PROCEDURE number(w: window; VAR i: ICON; x,y,sx,fr,to: INTEGER;
                 VAR v: INTEGER);
  VAR ii: ICON;
BEGIN
  NEW(ii); ii^.nxt:=i; i:=ii; i^.ww:=w;
  i^.x:=x; i^.y:=y; i^.sx:=sx; i^.sy:=15;
  i^.kind:=ik_number;
  i^.n_val:=ADR(v); i^.n_fr:=fr; i^.n_to:=to;
  IF do_number(w,i,0,0,1c) THEN END;
END number;

PROCEDURE pcb_number_proc(w: window; x,y,cx,cy: INTEGER; ch: CHAR;
                          VAR num: INTEGER): BOOLEAN;
  PROCEDURE print_num(n,x,color: INTEGER);
    VAR l: INTEGER; ln: ARRAY [0..79] OF CHAR;
  BEGIN
    vg.prop_font(w,FALSE); w^.patt:={0..31};
    vg.mode(w,vg.bic); vg.color(w,15); vg.box(w,x,y,x+74,y+14);
    vg.mode(w,vg.rep); vg.color(w,1); vg.frame(w,x,y,x+74,y+14);
    vg.mode(w,vg.xor); vg.color(w,2); vg.box(w,x+1,y,x+74,y+13);
    vg.color(w,INTEGER({1}/BITSET(color)));
    str.print(ln,'%d.%$2d',n/100,ABS(n REM 100));
    l:=vg.string_len(FALSE,ln,0);
    IF l>72 THEN vg.write_string(w,x+2,y+1,'*******');
    ELSE vg.write_string(w,x+73-l,y+1,ln)
    END;
  END print_num;
  PROCEDURE edit_num(VAR n: INTEGER; x: INTEGER): BOOLEAN;
     VAR i,j: INTEGER; ch: CHAR; pnt: BOOLEAN;
  BEGIN
    pnt:=FALSE; wnd.del_crs;
    LOOP
      print_num(n,x,8); wnd.ref_box(w,y,15);
      REPEAT wnd.wait; wnd.first(i,j,ch); wnd.drop UNTIL ch#0c;
      IF (ch=15c) OR (ch=wnd.l_on) THEN RETURN TRUE END;
      IF (ch=33c) OR (ch=wnd.r_on) THEN RETURN FALSE END;
      IF ch=10c THEN n:=n/10
      ELSIF ch='-' THEN IF n#MIN(INTEGER) THEN n:=-n END;
      ELSIF ch=' ' THEN n:=0;
      ELSIF ch='.' THEN
        pnt:=NOT pnt;
        IF pnt THEN n:=n/100*100 END;
      ELSIF (ch>='0') & (ch<='9') THEN
        IF pnt THEN
          i:=ABS(n REM 100);
          i:=(i*10+ORD(ch)-ORD('0')) REM 100;
          IF n<0 THEN n:=n/100*100+i ELSE n:=n/100*100+i END;
        ELSIF n>=0 THEN
          IF  n<1000000 THEN
            n:=n REM 100 + (n/100*10+(ORD(ch)-ORD('0')))*100;
          END;
        ELSE
          IF -n<1000000 THEN
            n:=n REM 100 + (n/100*10-(ORD(ch)-ORD('0')))*100;
          END;
        END;
      END;
    END;
  END edit_num;
  VAR n: INTEGER;
BEGIN
  IF ch=0c THEN RETURN FALSE END;
  IF ch=1c THEN
    vg.mode(w,vg.bic); vg.color(w,15); vg.box(w,x,y,x+219,y+14);
    vg.mode(w,vg.rep); vg.color(w,2); vg.box(w,x,y,x+219,y+14);
    vg.mode(w,vg.or); vg.color(w,1); vg.prop_font(w,TRUE);
    vg.write_string(w,x+76,y+1,'x1.25');
    vg.write_string(w,x+201,y+1,'мм');
    print_num((num*50+12) DIV 24,x,0);
    print_num((num*125+24) DIV 48,x+125,0);
    RETURN TRUE;
  END;
  IF (cy<y) OR (cy>=y+15) THEN RETURN FALSE END;
  IF (cx>=x) & (cx<x+75) THEN
    IF ch=' ' THEN n:=0;
    ELSIF ch=wnd.l_on THEN n:=(num*50+12) DIV 24;
    ELSE RETURN FALSE;
    END;
    IF edit_num(n,x) THEN num:=(n*24+25) DIV 50 END;
    print_num((num*50+12) DIV 24,x,0);
    print_num((num*125+24) DIV 48,x+125,0);
    wnd.ref_box(w,y,15); RETURN TRUE;
  ELSIF (cx>=x+125) & (cx<x+200) THEN
    IF ch=' ' THEN n:=0;
    ELSIF ch=wnd.l_on THEN n:=(num*125+24) DIV 48;
    ELSE RETURN FALSE;
    END;
    IF edit_num(n,x+125) THEN num:=(n*48+62) DIV 125 END;
    print_num((num*50+12) DIV 24,x,0);
    print_num((num*125+24) DIV 48,x+125,0);
    wnd.ref_box(w,y,15); RETURN TRUE;
  ELSE RETURN FALSE;
  END;
END pcb_number_proc;

PROCEDURE do_pcb_num(w: window; i: ICON; x,y: INTEGER; c: CHAR): BOOLEAN;
BEGIN
  RETURN pcb_number_proc(w,i^.x,i^.y,x,y,c,i^.p_val^);
END do_pcb_num;

PROCEDURE pcb_number(w: window; VAR i: ICON; x,y: INTEGER;
                     VAR v: INTEGER);
  VAR ii: ICON;
BEGIN
  NEW(ii); ii^.nxt:=i; i:=ii; i^.ww:=w;
  i^.x:=x; i^.y:=y; i^.sx:=220; i^.sy:=15;
  i^.kind:=ik_pcb_num;
  i^.p_val:=ADR(v);
  IF pcb_number_proc(w,x,y,0,0,1c,v) THEN END;
END pcb_number;

PROCEDURE do_icon(i: ICON; x,y: INTEGER; c: CHAR): BOOLEAN;
BEGIN
  WHILE i#NIL DO
    IF (x>=i^.x) & (x<i^.x+i^.sx) & (y>=i^.y) & (y<i^.y+i^.sy) THEN
      CASE i^.kind OF
        |ik_number : RETURN do_number (i^.ww,i,x,y,c);
        |ik_pcb_num: RETURN do_pcb_num(i^.ww,i,x,y,c);
        |ik_on_off : RETURN do_on_off (i^.ww,i,x,y,c);
      END;
    END;
    i:=i^.nxt;
  END;
  RETURN FALSE;
END do_icon;

PROCEDURE remove(VAR i: ICON);
  VAR ii: ICON;
BEGIN
  WHILE i#NIL DO
    ii:=i; i:=i^.nxt; DISPOSE(ii);
  END;
END remove;

END cdsIcon.
