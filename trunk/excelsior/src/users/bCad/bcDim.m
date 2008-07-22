IMPLEMENTATION MODULE bcDim; (*$X+ brd 31-Jan-91. (c) KRONOS *)

IMPORT  def: bcDef;
IMPORT  obj: bcObj;
IMPORT  bas: bcBase;
IMPORT  dft: bcDraft;
IMPORT  tol: bcTool;
IMPORT  bpm: bcPM;
IMPORT  str: Strings;
IMPORT  wnd: pmWnd;
IMPORT  mth: realMath;
IMPORT  bmt: bcMath;
IMPORT  mem: Heap;
IMPORT  tty: Terminal;
IMPORT  tex: bcText;

WITH STORAGE (NEW    : mem.allocate;
              DISPOSE: mem.deallocate;
              RESIZE : mem.reallocate);

VAR x1,y1,x2,y2: INTEGER;
    x3,y3,x4,y4: INTEGER;
 h_dt,w_dt,l_dt: INTEGER;
       Xc,Yc,Zc: INTEGER;

PROCEDURE Max(i,j: INTEGER): INTEGER;
BEGIN IF i>j THEN RETURN i END; RETURN j END Max;

PROCEDURE set_coord;
BEGIN
  Xc:= bas.cview^.Xc;
  Yc:= bas.cview^.Yc;
  Zc:= bas.cview^.Zc
END set_coord;

PROCEDURE rd_str(prompt: ARRAY OF CHAR; VAR str: ARRAY OF CHAR): BOOLEAN;
  VAR st: STRING;
BEGIN
  bpm.newstr(st,str);
  bpm.diabox(bas.SW DIV 2,20,bas.SW,str,prompt);
  RETURN NOT (str=st)
END rd_str;

PROCEDURE read_deriv(VAR up,dw: ARRAY OF CHAR);
  VAR s1,s2: STRING;
       done: BOOLEAN;
PROCEDURE char(ch: CHAR): BOOLEAN;
BEGIN
  IF (ch >= 101c) & (ch < 132c) THEN RETURN TRUE
  ELSIF (ch >= 141c) & (ch < 172c) THEN RETURN TRUE END;
  RETURN FALSE
END char;

BEGIN
  dw[0]:=0c; up[0]:=0c;
  NEW(s1,HIGH(up)); NEW(s2,HIGH(dw));
  done:= rd_str('нижний  допуск:',dw);
  done:= rd_str('верхний допуск:',up) & done;
  IF done THEN
    str.sub_str(s1,dw,1,HIGH(dw));
    str.sub_str(s2,up,1,HIGH(up));
    IF (s1=s2) & (s1#'') & (up[0]#dw[0]) THEN dw[0]:=032c; up:=dw END;
    IF char(dw[0]) THEN up:=dw END;
    IF char(up[0]) THEN dw:=up END
  END;
  DISPOSE(s1);
  DISPOSE(s2)
END read_deriv;

PROCEDURE print_dim(x: REAL; VAR s: ARRAY OF CHAR);
  VAR t,f: ARRAY [0..127] OF CHAR;
        p: INTEGER;
BEGIN
  t[0]:=0c; f[0]:=0c;
  str.print(s,'%-g%c',x,0c);
  p:=0; str.search(s,p,' '); s[p]:=0c;
  str.print(t,'размер <%s>:',s);
  IF rd_str(t,f) THEN str.print(s,'%s%c',f,0c) END;
  p:=0; str.search(s,p,' '); s[p]:=0c
END print_dim;

PROCEDURE _arrow(beg,end: def.VERTEX; VAR first,second: def.VECTOR);
  CONST  cb=0.961; sb=0.259;
    VAR  ca,sa,l: REAL;
BEGIN
 first [0]:= beg;   first [1,Zc]:= beg[Zc];
 second[0]:= beg;   second[1,Zc]:= beg[Zc];
 IF    beg[Xc] = end[Xc]  THEN
   IF  beg[Yc] = end[Yc]  THEN RETURN END;
   first [1,Xc]:= beg[Xc]+arr_s*0.259;
   second[1,Xc]:= beg[Xc]-arr_s*0.259;
   IF beg[Yc] > end[Yc] THEN
     first [1,Yc]:= beg[Yc]-arr_s;
     second[1,Yc]:= beg[Yc]-arr_s;
   ELSE
     first [1,Yc]:= beg[Yc]+arr_s;
     second[1,Yc]:= beg[Yc]+arr_s;
   END
 ELSIF beg[Yc] = end[Yc]  THEN
   first [1,Yc]:= beg[Yc]+arr_s*0.259;
   second[1,Yc]:= beg[Yc]-arr_s*0.259;
   IF beg[Xc] > end[Xc] THEN
     first [1,Xc]:= beg[Xc]-arr_s;
     second[1,Xc]:= beg[Xc]-arr_s;
   ELSE
     first [1,Xc]:= beg[Xc]+arr_s;
     second[1,Xc]:= beg[Xc]+arr_s;
   END
 ELSE
   l:= bmt.dist1(beg,end);
   ca:=(end[Xc]- beg[Xc])/l;
   sa:=(end[Yc]- beg[Yc])/l;
   first [1,Xc]:= beg[Xc]+(arr_s/cb)*(ca*cb-sa*sb);
   first [1,Yc]:= beg[Yc]+(arr_s/cb)*(sa*cb+ca*sb);
   second[1,Xc]:= beg[Xc]+(arr_s/cb)*(ca*cb+sa*sb);
   second[1,Yc]:= beg[Yc]+(arr_s/cb)*(sa*cb-ca*sb)
 END
END _arrow;

--------------------- HORIZONTAL DIMENSION ---------------------

PROCEDURE cross_hd(X,Y: INTEGER);
BEGIN
  WITH bas.cview^ DO
    wnd.line(wind,tool,x1,Y,x2,Y);
    wnd.line(wind,tool,x1,y1,x1,Y);
    wnd.line(wind,tool,x2,y2,x2,Y)
  END
END cross_hd;

PROCEDURE cross_ht(X,Y: INTEGER);
BEGIN
  WITH bas.cview^ DO
    wnd.line(wind,tool,x1,y3,x2,y3);
    wnd.line(wind,tool,x1,y1,x1,y3);
    wnd.line(wind,tool,x2,y2,x2,y3);
    wnd.line(wind,tool,X,y4,X+l_dt,y4);
    wnd.line(wind,tool,X,y4+h_dt,X+l_dt,y4+h_dt);
    wnd.line(wind,tool,X+l_dt,y4,X+l_dt,y4+h_dt);
    wnd.line(wind,tool,X,y4,X,y4+h_dt)
  END
END cross_ht;

PROCEDURE hdim?(top1,top2: def.VERTEX; VAR txt: ARRAY OF CHAR);
 VAR d: REAL;
BEGIN
  d:=ABS(top1[Xc]-top2[Xc]);
  print_dim(d,txt);
END hdim?;

PROCEDURE horizontal;
  VAR top1,top2,top3,top4: def.VERTEX;
 text_dim,text_up,text_dw: ARRAY [0..15] OF CHAR; --размер и  предельные отклонения

PROCEDURE read_hdim(): BOOLEAN;
BEGIN
  text_dim[0]:=0c;
  text_up[0] :=0c;
  text_dw[0] :=0c;
  h_dt:=mth.round(h_tex * bas.cview^.scale);
  w_dt:=mth.round(w_tex * bas.cview^.scale);
  IF bas.read_point('первая точка:',bas.cross,bas.null_monitor,top1) THEN
    bas.b_s(bas.cview,top1,bas.x_old,bas.y_old);
    IF bas.read_point('вторая точка:',bas.cross_hline,bas.null_monitor,top2) THEN
      bas.b_s(bas.cview,top1,x1,y1);
      bas.b_s(bas.cview,top2,x2,y2);
      IF bas.read_point('координата размерной линии:',cross_hd,bas.null_monitor,top3)
      THEN
        bas.b_s(bas.cview,top3,x3,y3);
        y4:= mth.round(H_tex*bas.cview^.scale)+y3;
        hdim?(top1,top2,text_dim);
        read_deriv(text_up,text_dw);
        l_dt:= w_dt*str.len(text_dim)+
               w_dt*Max(str.len(text_up),str.len(text_dw)) DIV 2;
        IF bas.read_point('положение текста <x,y>:',cross_ht,bas.null_monitor,top4)
        THEN
          top4[Yc]:= top3[Yc]+1.;
          RETURN TRUE
        END
      END
    END
  END;
  RETURN FALSE
END read_hdim;

PROCEDURE new_hdim;
  VAR lines: ARRAY [0..6] OF def.VECTOR;
       text: ARRAY [0..2] OF def.Text;
      th,tw: ARRAY [0..2] OF REAL;
        new: def.Group;
       clip: obj.Clip;
        i,j: INTEGER;
      z,ltx: REAL;
      xl,xr: REAL;
BEGIN
  WITH new DO
    NEW(ellips);
    NEW(arc);
    NEW(pline);
    NEW(circ);
    NEW(surf)
  END;
  xl:=mth.min(top1[Xc],top2[Xc]);
  xr:=mth.max(top1[Xc],top2[Xc]);
  z:=top1[Zc];
  NEW(text[0].stxt,HIGH(text_dim)+1);
  NEW(text[1].stxt,HIGH(text_up)+1);
  NEW(text[2].stxt,HIGH(text_dw)+1);
  text[0].stxt:= text_dim;
  text[1].stxt:= text_up;
  text[2].stxt:= text_dw;
  th[0]:= h_tex;
  tw[0]:= w_tex;
  IF (text[1].stxt=text[2].stxt) & (text[1].stxt#'') THEN
    text[1].stxt:='';
    FOR i:=1 TO 2 DO th[i]:= h_tex; tw[i]:= w_tex END
  ELSE
    FOR i:=1 TO 2 DO th[i]:= h_tex/2.; tw[i]:=w_tex/2. END
  END;
  ltx:=FLOAT(str.len(text[0].stxt)) * w_tex;
  IF (text[1].stxt#'') OR (text[2].stxt#'') THEN
    ltx:=ltx+FLOAT(Max(str.len(text[1].stxt),str.len(text[2].stxt)))*w_tex/2.
  END;
    (* first vertical line 0  second 1*)
  lines[0][0]:=top1;
  lines[1][0]:=top2;
  lines[0][1][Xc]:=top1[Xc];
  lines[1][1][Xc]:=top2[Xc];
  IF (top1[Yc] < top3[Yc]) & (top2[Yc] <top3[Yc]) THEN
    lines[0][1][Yc]:=top3[Yc]+2.5;
    lines[1][1][Yc]:=top3[Yc]+2.5
  ELSIF (top1[Yc] > top3[Yc]) & (top2[Yc] > top3[Yc]) THEN
    lines[0][1][Yc]:=top3[Yc]-2.5;
    lines[1][1][Yc]:=top3[Yc]-2.5
  ELSIF (top1[Yc] > top3[Yc]) & (top2[Yc] < top3[Yc]) THEN
    lines[0][1][Yc]:=top3[Yc]-2.5;
    lines[1][1][Yc]:=top3[Yc]+2.5
  ELSE
    lines[0][1][Yc]:=top3[Yc]+2.5;
    lines[1][1][Yc]:=top3[Yc]-2.5
  END;
    (*  horizontal line  2 *)
  IF (top4[Xc] > xl+arr_s) & (top4[Xc]+ltx < xr-arr_s) THEN
    lines[2][0][Xc]:=xl; lines[2][1][Xc]:=xr
  ELSIF (xr-xl < 3.*arr_s) THEN
    IF (top4[Xc] < xl) THEN
      top4[Xc]:=xl-ltx-arr_s;
      lines[2][0][Xc]:=top4[Xc];
      lines[2][1][Xc]:=xr+arr_s*1.5;
    ELSE
      top4[Xc]:=xr+1.5+arr_s;
      lines[2][0][Xc]:=xl-arr_s*1.5;
      lines[2][1][Xc]:=top4[Xc]+ltx;
    END;
  ELSE
    IF (top4[Xc] < xl+arr_s) THEN
      top4[Xc]:=xl-ltx;
      lines[2][0][Xc]:=top4[Xc];
      lines[2][1][Xc]:=xr;
    ELSIF (top4[Xc]+ltx>xr-arr_s) THEN
      top4[Xc]:=xr+1.5;
      lines[2][0][Xc]:=xl;
      lines[2][1][Xc]:=top4[Xc]+ltx;
    END
  END;
  lines[2][0][Yc]:=top3[Yc];
  lines[2][1][Yc]:=top3[Yc];

    (* arrow left up 3 dw 4  right up 5 dw 6 *)
  IF (xr-xl > 3.*arr_s) THEN
    lines[3][1][Xc]:=xl+arr_s;
    lines[4][1][Xc]:=xl+arr_s;
    lines[5][1][Xc]:=xr-arr_s;
    lines[6][1][Xc]:=xr-arr_s;
  ELSE
    lines[3][1][Xc]:=xl-arr_s;
    lines[4][1][Xc]:=xl-arr_s;
    lines[5][1][Xc]:=xr+arr_s;
    lines[6][1][Xc]:=xr+arr_s;
  END;
  lines[3][0][Xc]:=xl;
  lines[3][0][Yc]:=top3[Yc];
  lines[3][1][Yc]:=top3[Yc]+arr_s*0.259;

  lines[4][0][Xc]:=xl;
  lines[4][0][Yc]:=top3[Yc];
  lines[4][1][Yc]:=top3[Yc]-arr_s*0.259;

  lines[5][0][Xc]:=xr;
  lines[5][0][Yc]:=top3[Yc];
  lines[5][1][Yc]:=top3[Yc]+arr_s*0.259;

  lines[6][0][Xc]:=xr;
  lines[6][0][Yc]:=top3[Yc];
  lines[6][1][Yc]:=top3[Yc]-arr_s*0.259;
  FOR i:=0 TO HIGH(lines) DO
    lines[i][0][Zc]:=z; lines[i][1][Zc]:=z
  END;
  text[0].xyz:=top4;
  text[1].xyz[Xc]:=top4[Xc]+w_tex*FLOAT(str.len(text[0].stxt));
  text[2].xyz[Xc]:=top4[Xc]+w_tex*FLOAT(str.len(text[0].stxt));
  text[1].xyz[Yc]:=top4[Yc]+th[1];     --верхний допуск
  text[2].xyz[Yc]:=top4[Yc];           --нижний  допуск
  FOR i:=0 TO HIGH(text) DO text[i].xyz[Zc]:= bas.cview^.cntx^.c_Z; END;
  WITH new DO
    NEW(line,HIGH(lines)+1);
    FOR i:=0 TO HIGH(line) DO
      WITH line[i] DO
        xyz:= lines[i,0];
        XYZ:= lines[i,1];
        NEW(pict);
        ltyp:=0;
        color:= col_pc;
      END
    END;
    NEW(txt,HIGH(text)+1);
    FOR j:=0 TO HIGH(txt) DO
      WITH txt[j] DO
        xyz:= text[j].xyz;
        NEW(stxt,HIGH(text[j].stxt)+1);
        stxt:= text[j].stxt;
        i:= i_tex;
        a:= 0.;
        w:= tw[j];
        h:= th[j];
        color:= col_ch;
        bas.make_str(dfont,txt[j])
      END
    END
  END;
  obj.new_group(bas.cview^.model^.mbody,new,bas.cview^.cntx^.work);
  bas.extr_grp(bas.cview^.model,new);
  bas.show_group(bas.cview^.model,new)
END new_hdim;

BEGIN
  set_coord;
  IF read_hdim() THEN new_hdim END
END horizontal;

---------------------------- VERICAL ---------------------------

PROCEDURE cross_vd(X,Y: INTEGER);
BEGIN
  WITH bas.cview^ DO
    wnd.line(wind,tool,X,y1,X,y2);
    wnd.line(wind,tool,x1,y1,X,y1);
    wnd.line(wind,tool,x2,y2,X,y2)
  END
END cross_vd;

PROCEDURE cross_vt(X,Y: INTEGER);
BEGIN
  WITH bas.cview^ DO
    wnd.line(wind,tool,x3,y2,x2,y2);
    wnd.line(wind,tool,x1,y1,x3,y1);
    wnd.line(wind,tool,x3,y2,x3,y1);
    wnd.line(wind,tool,x4,Y,x4,Y+l_dt);
    wnd.line(wind,tool,x4-h_dt,Y,x4-h_dt,Y+l_dt);
    wnd.line(wind,tool,x4-h_dt,Y,x4,Y);
    wnd.line(wind,tool,x4-h_dt,Y+l_dt,x4,Y+l_dt)
  END
END cross_vt;

PROCEDURE vdim?(top1,top2: def.VERTEX; VAR txt: ARRAY OF CHAR);
 VAR d: REAL;
BEGIN
  d:=ABS(top1[Yc]-top2[Yc]);
  print_dim(d,txt);
END vdim?;

PROCEDURE vertical;
  VAR top1,top2,top3,top4: def.VERTEX;
 text_dim,text_up,text_dw: ARRAY [0..15] OF CHAR; --размер и предельные отклонения

PROCEDURE read_vdim(): BOOLEAN;
BEGIN
  text_dim:=''; text_up:=''; text_dw:='';
  h_dt:=mth.round(h_tex*bas.cview^.scale);
  w_dt:=mth.round(w_tex*bas.cview^.scale);
  IF bas.read_point('первая точка <x,y>:',bas.cross,bas.null_monitor,top1) THEN
    bas.b_s(bas.cview,top1,bas.x_old,bas.y_old);
    IF bas.read_point('вторая точка<x,y>:',bas.cross_vline,bas.null_monitor,top2)
    THEN
      bas.b_s(bas.cview,top1,x1,y1);
      bas.b_s(bas.cview,top2,x2,y2);
      IF bas.read_point('координата размерной линии:',cross_vd,bas.null_monitor,top3)
      THEN
        bas.b_s(bas.cview,top3,x3,y3);
        x4:= x3-mth.round(H_tex*bas.cview^.scale);
        vdim?(top1,top2,text_dim);
        read_deriv(text_up,text_dw);
        l_dt:= w_dt*str.len(text_dim)+
               w_dt*Max(str.len(text_up),str.len(text_dw)) DIV 2;
        IF bas.read_point('Position dimension text:',cross_vt,bas.null_monitor,top4)
        THEN
          top4[Xc]:= top3[Xc]-H_tex;
          RETURN TRUE
        END
      END
    END
  END;
  RETURN FALSE
END read_vdim;

PROCEDURE new_vdim;
  VAR lines: ARRAY [0..6] OF def.VECTOR;
       text: ARRAY [0..2] OF def.Text;
        new: def.Group;
       clip: obj.Clip;
      th,tw: ARRAY [0..2] OF REAL;
      z,ltx: REAL;
      yu,yd: REAL;
        i,j:INTEGER;
BEGIN
  WITH new DO
    NEW(ellips);
    NEW(arc);
    NEW(pline);
    NEW(circ);
    NEW(surf)
  END;
  z:=top1[Zc];
  yd:=mth.min(top1[Yc],top2[Yc]);
  yu:=mth.max(top1[Yc],top2[Yc]);
  NEW(text[0].stxt,HIGH(text_dim)+1);
  NEW(text[1].stxt,HIGH(text_up)+1);
  NEW(text[2].stxt,HIGH(text_dw)+1);
  text[0].stxt:=text_dim;
  text[1].stxt:=text_up;
  text[2].stxt:=text_dw;
  th[0]:= h_tex; tw[0]:= w_tex;
  IF (text[1].stxt=text[2].stxt) & (text[1].stxt#'') THEN
    text[1].stxt:='';
    th[1]:= h_tex; tw[1]:= w_tex;
    th[2]:= h_tex; tw[2]:= w_tex
  ELSE
    th[1]:= h_tex/2.; tw[1]:= w_tex/2.;
    th[2]:= h_tex/2.; tw[2]:= w_tex/2.
  END;
  ltx:=FLOAT(str.len(text[0].stxt))*w_tex;
  IF (text[1].stxt#'') OR (text[2].stxt#'') THEN
    ltx:=ltx+FLOAT(Max(str.len(text[1].stxt),str.len(text[2].stxt)))*w_tex/2.
  END;
  (* first horizontal line 0 second 1*)
  lines[0][0]:=top1;
  lines[1][0]:=top2;
  lines[0][1][Yc]:=top1[Yc];
  lines[1][1][Yc]:=top2[Yc];
  IF (top3[Xc]<top2[Xc]) & (top3[Xc]<top1[Xc]) THEN
    lines[0][1][Xc]:=top3[Xc]-2.5;
    lines[1][1][Xc]:=top3[Xc]-2.5
  ELSIF (top3[Xc]>top2[Xc]) & (top3[Xc]>top1[Xc]) THEN
    lines[0][1][Xc]:=top3[Xc]+2.5;
    lines[1][1][Xc]:=top3[Xc]+2.5
  ELSIF (top3[Xc]>top2[Xc]) & (top3[Xc]<top1[Xc]) THEN
    lines[0][1][Xc]:=top3[Xc]-2.5;
    lines[1][1][Xc]:=top3[Xc]+2.5
  ELSE
    lines[0][1][Xc]:=top3[Xc]+2.5;
    lines[1][1][Xc]:=top3[Xc]-2.5
  END;
    (*  horizontal line  2 *)
  IF (top4[Yc] > yd+arr_s) & (top4[Yc]+ltx < yu-arr_s) THEN
    lines[2][0][Yc]:=yd; lines[2][1][Yc]:=yu
  ELSIF (yu-yd < 3.*arr_s) THEN
    IF (top4[Yc] < yd) THEN
      top4[Yc]:=yd-ltx-arr_s;
      lines[2][0][Yc]:=top4[Yc];
      lines[2][1][Yc]:=yu+arr_s*1.5;
    ELSE
      top4[Yc]:=yu+arr_s;
      lines[2][0][Yc]:=yd-arr_s*1.5;
      lines[2][1][Yc]:=top4[Yc]+ltx;
    END;
  ELSE
    IF (top4[Yc] < yd+arr_s) THEN
      top4[Yc]:=yd-ltx;
      lines[2][0][Yc]:=top4[Yc];
      lines[2][1][Yc]:=yu;
    ELSIF (top4[Yc]+ltx>yu-arr_s) THEN
      top4[Yc]:=yu;
      lines[2][0][Yc]:=yd;
      lines[2][1][Yc]:=top4[Yc]+ltx;
    END
  END;
  lines[2][0][Xc]:=top3[Xc];
  lines[2][1][Xc]:=top3[Xc];

    (* arrow down 3,4  up 5,6 *)
  IF (yu-yd > 3.*arr_s) THEN
    lines[3][1][Yc]:=yd+arr_s;
    lines[4][1][Yc]:=yd+arr_s;
    lines[5][1][Yc]:=yu-arr_s;
    lines[6][1][Yc]:=yu-arr_s;
  ELSE
    lines[3][1][Yc]:=yd-arr_s;
    lines[4][1][Yc]:=yd-arr_s;
    lines[5][1][Yc]:=yu+arr_s;
    lines[6][1][Yc]:=yu+arr_s;
  END;
  lines[3][0][Yc]:=yd;
  lines[3][0][Xc]:=top3[Xc];
  lines[3][1][Xc]:=top3[Xc]+arr_s*0.259;

  lines[4][0][Yc]:=yd;
  lines[4][0][Xc]:=top3[Xc];
  lines[4][1][Xc]:=top3[Xc]-arr_s*0.259;

  lines[5][0][Yc]:=yu;
  lines[5][0][Xc]:=top3[Xc];
  lines[5][1][Xc]:=top3[Xc]+arr_s*0.259;

  lines[6][0][Yc]:=yu;
  lines[6][0][Xc]:=top3[Xc];
  lines[6][1][Xc]:=top3[Xc]-arr_s*0.259;
  FOR i:=0 TO HIGH(lines) DO
    lines[i][0][Zc]:=z; lines[i][1][Zc]:=z
  END;
  text[0].xyz:=top4;
  text[1].xyz[Yc]:=top4[Yc]+w_tex*FLOAT(str.len(text[0].stxt));
  text[2].xyz[Yc]:=top4[Yc]+w_tex*FLOAT(str.len(text[0].stxt));
  text[1].xyz[Xc]:=top4[Xc]-th[1];     --верхний допуск
  text[2].xyz[Xc]:=top4[Xc];           --нижний  допуск
  FOR i:=0 TO HIGH(text) DO text[i].xyz[Zc]:= bas.cview^.cntx^.c_Z END;
  WITH new DO
    NEW(line,HIGH(lines)+1);
    FOR i:=0 TO HIGH(line) DO
      WITH line[i] DO
        xyz:= lines[i,0];
        XYZ:= lines[i,1];
        NEW(pict);
        ltyp:=0;
        color:= col_pc;
      END
    END;
    NEW(txt,HIGH(text)+1);
    FOR j:=0 TO HIGH(txt) DO
      WITH txt[j] DO
        xyz:= text[j].xyz;
        NEW(stxt,HIGH(text[j].stxt)+1);
        stxt:= text[j].stxt;
        i:= i_tex;
        a:= mth.pi/2.;
        w:= tw[j];
        h:= th[j];
        color:= col_ch;
        bas.make_str(dfont,txt[j])
      END
    END
  END;
  obj.new_group(bas.cview^.model^.mbody,new,bas.cview^.cntx^.work);
  bas.extr_grp(bas.cview^.model,new);
  bas.show_group(bas.cview^.model,new)
END new_vdim;

BEGIN
  set_coord;
  IF read_vdim() THEN new_vdim END;
END vertical;

----------------------------- ANGLE ----------------------------

VAR _xc,_yc: INTEGER;

PROCEDURE cross_arc(X,Y: INTEGER);
  VAR x,y,r: INTEGER;
BEGIN
  bas.cross(X,Y);
  x:=X-_xc;
  y:=Y-_yc;
  r:=bmt.sqrt(x*x+y*y);
  wnd.arc(bas.cview^.wind,bas.cview^.tool,
  _xc,_yc,bas.x_old,bas.y_old,bas.x_old1,bas.y_old1,r);
END cross_arc;

PROCEDURE cross_t(X,Y: INTEGER);
BEGIN wnd.frame(bas.cview^.wind,bas.cview^.tool,X,Y,X+l_dt,Y+h_dt) END cross_t;

PROCEDURE make_arc(R,a1,a2: REAL; top: def.VERTEX; VAR pict: def.VPICTURE);
  VAR a,da: REAL;
       i,n: INTEGER;
         v: def.POLYLINE;
BEGIN
    n:= TRUNC((a2-a1)*R/dft.step_p);
    da:=(a2-a1)/FLOAT(n);
    a:=a1;
    NEW(pict,n);
    NEW(v,n+1);
    FOR i:=0 TO n DO
      v[i,Xc]:= top[Xc]+R*mth.cos(a);
      v[i,Yc]:= top[Yc]+R*mth.sin(a);
      a:=a+da;
    END;
    NEW(pict,HIGH(v));
    FOR i:=0 TO HIGH(pict) DO pict[i,0]:=v[i]; pict[i,1]:=v[i+1] END;
    DISPOSE(v)
END make_arc;

PROCEDURE _top?(t1,t2,t_1,t_2: def.VERTEX; VAR t: def.VERTEX): BOOLEAN;
  VAR a1,b1,c1,a2,b2,c2,d: REAL;
                      x,y: INTEGER;
BEGIN
  a1:=t1[Yc]-t2[Yc];
  b1:=t2[Xc]-t1[Xc];
  c1:=t1[Xc]*t2[Yc]-t1[Yc]*t2[Xc];
  a2:=t_1[Yc]-t_2[Yc];
  b2:=t_2[Xc]-t_1[Xc];
  c2:=t_1[Xc]*t_2[Yc]-t_1[Yc]*t_2[Xc];
  d:=a1*b2-a2*b1;
  IF d=0. THEN RETURN FALSE END;
  t[Zc]:=t1[Zc];
  t[Xc]:=(b1*c2-b2*c1)/d;
  t[Yc]:=(c1*a2-c2*a1)/d;
  RETURN  TRUE
END _top?;

PROCEDURE _arc(centr:def.VERTEX; a1,a2,R: REAL);
  VAR x1,x2,y1,y2,xc,yc,r: INTEGER;
              t1,t2: def.VERTEX;
BEGIN
   t1[Xc]:= centr[Xc]+ R*mth.cos(a1);
   t1[Yc]:= centr[Yc]+ R*mth.sin(a1);
   t2[Xc]:= centr[Xc]+ R*mth.cos(a2);
   t2[Yc]:= centr[Yc]+ R*mth.sin(a2);
   t1[Zc]:=centr[Zc];
   t2[Zc]:=centr[Zc];
   bas.b_s(bas.cview,t1,x1,y1);
   bas.b_s(bas.cview,t2,x2,y2);
   bas.b_s(bas.cview,centr,xc,yc);
   r:= TRUNC(R*bas.cview^.scale);
   wnd.arc(bas.cview^.wind,bas.cview^.tool,_xc,_yc,x1,y1,x2,y2,r)
END _arc;

PROCEDURE angle;
VAR txt_dim,txt_up,txt_dw: ARRAY [0..15] OF CHAR;
                  tc,ttex: def.VERTEX;
                  a1,a2,R: REAL;

PROCEDURE adim?(a: REAL; VAR s: ARRAY OF CHAR);
  VAR A,A1,A2: INTEGER;
          t,f: ARRAY [0..127] OF CHAR;
            p: INTEGER;
BEGIN
  A := TRUNC(a);
  A2:= TRUNC((a-FLOAT(A)) * 3600.);
  A1:= A2 DIV 60;
  A2:= A2- A1*60;
  t[0]:=0c; f[0]:=0c;
  str.print(s,'%d%c'"%d'"'%d" \n',A,07c,A1,A2);
  p:=0; str.search(s,p,' '); s[p]:=0c;
  IF bas.ENG THEN str.print(t,'angle<%s>:',s);
  ELSE            str.print(t,'угол <%s>:',s) END;
  IF rd_str(t,f) THEN str.print(s,'%s',f) END;
  p:=0; str.search(s,p,' '); s[p]:=0c
END adim?;

PROCEDURE read_angle(): BOOLEAN;
  VAR s1,s2,s3,s4,s5,s6: ARRAY [0..31] OF CHAR;
            t1,t2,t3,t4: def.VERTEX;
BEGIN
  IF bas.ENG THEN
    s1:= 'begin first line:';   s2:='end first line:';
    s1:= 'begin second line:';  s2:='end second line:';
    s5:= 'radius arc:';          s6:='text point:'
  ELSE
    s1:= 'начало первого луча:'; s2:= 'конец  первого луча:';
    s3:= 'начало второго луча:'; s4:= 'конец  второго луча:';
    s5:= 'радиус дуги:';         s6:= 'положение текста:'
  END;
  IF NOT bas.read_point(s1,bas.cross,bas.null_monitor,t1)
  THEN RETURN FALSE END;
  bas.b_s(bas.cview,t1,bas.x_old,bas.y_old);
  IF NOT bas.read_point(s2,bas.cross_line,bas.null_monitor,t2)
  THEN RETURN FALSE END;
  bas.xshow_vector(bas.cview,t1,t2,bas.rubb);
  IF NOT bas.read_point(s3,bas.cross,bas.null_monitor,t3) THEN
    bas.xshow_vector(bas.cview,t1,t2,bas.bckg);
    RETURN FALSE
  END;
  bas.b_s(bas.cview,t3,bas.x_old,bas.y_old);
  IF NOT bas.read_point(s4,bas.cross_line,bas.null_monitor,t4) THEN
    bas.xshow_vector(bas.cview,t1,t2,bas.bckg);
    RETURN FALSE
  END;
  bas.xshow_vector(bas.cview,t3,t4,bas.rubb);
  IF NOT _top?(t1,t2,t3,t4,tc) THEN
    bas.xshow_vector(bas.cview,t1,t2,bas.bckg);
    bas.xshow_vector(bas.cview,t3,t4,bas.bckg);
    RETURN FALSE
  END;
  bas.b_s(bas.cview,tc,_xc,_yc);
  bas.b_s(bas.cview,t4,bas.x_old1,bas.y_old1);
  bas.b_s(bas.cview,t2,bas.x_old ,bas.y_old );
  IF NOT bas.read_long(tc,s5,bas.long,cross_arc,bas.null_monitor,R) THEN
    bas.xshow_vector(bas.cview,t1,t2,bas.bckg);
    bas.xshow_vector(bas.cview,t3,t4,bas.bckg);
    RETURN FALSE
  END;
  a1:= bmt.angle_0(tc,t2);
  a2:= bmt.angle_0(tc,t4);
  IF a2<a1 THEN a2:=mth.pi*2.+a2 END;
  _arc(tc,a1,a2,R);
  adim?(mth.rtod(a2-a1),txt_dim);
  read_deriv(txt_up,txt_dw);
  h_dt:=mth.round(h_tex * bas.cview^.scale);
  w_dt:=mth.round(w_tex * bas.cview^.scale);
  l_dt:= w_dt*str.len(txt_dim)+w_dt*Max(str.len(txt_up),str.len(txt_dw)) DIV 2;
  IF NOT bas.read_point(s6,cross_t,bas.null_monitor,ttex)
  THEN RETURN FALSE END;
  _arc(tc,a1,a2,R);
  bas.xshow_vector(bas.cview,t1,t2,bas.bckg);
  bas.xshow_vector(bas.cview,t3,t4,bas.bckg);
  RETURN TRUE
END read_angle;

PROCEDURE new_angle;
  VAR   new: def.Group;
       text: ARRAY [0..2] OF def.Text;
      th,tw: ARRAY [0..2] OF REAL;
      lines: def.VPICTURE;
       clip: obj.Clip;
        i,j: INTEGER;
BEGIN
  WITH new DO
    NEW(ellips);
    NEW(arc);
    NEW(pline);
    NEW(circ);
    NEW(surf)
  END;
  NEW(text[0].stxt,HIGH(txt_dim)+1);
  NEW(text[1].stxt,HIGH(txt_up)+1);
  NEW(text[2].stxt,HIGH(txt_dw)+1);
  text[0].stxt:= txt_dim;
  text[1].stxt:= txt_up;
  text[2].stxt:= txt_dw;
  th[0]:= h_tex;
  tw[0]:= w_tex;

  IF (text[1].stxt=text[2].stxt) & (text[1].stxt#'') THEN
    text[1].stxt:='';
    FOR i:=1 TO 2 DO th[i]:= h_tex; tw[i]:= w_tex END
  ELSE
    FOR i:=1 TO 2 DO th[i]:= h_tex/2.; tw[i]:=w_tex/2. END
  END;
  text[0].xyz:=ttex;
  text[1].xyz[Xc]:=ttex[Xc]+w_tex*FLOAT(str.len(text[0].stxt));
  text[2].xyz[Xc]:=ttex[Xc]+w_tex*FLOAT(str.len(text[0].stxt));
  text[1].xyz[Yc]:=ttex[Yc]+th[1];     --верхний допуск
  text[2].xyz[Yc]:=ttex[Yc];           --нижний  допуск
  FOR i:=0 TO HIGH(text) DO text[i].xyz[Zc]:= bas.cview^.cntx^.c_Z; END;

  make_arc(R,a1,a2,tc,lines);
  j:=HIGH(lines);
  RESIZE(lines,HIGH(lines)+5);
  _arrow(lines[0,0],lines[0,1],lines[j+1],lines[j+2]);
  _arrow(lines[j,1],lines[j,0],lines[j+3],lines[j+4]);
  WITH new DO
    NEW(line,HIGH(lines)+1);
    FOR i:=0 TO HIGH(line) DO
      WITH line[i] DO
        xyz:= lines[i,0];
        XYZ:= lines[i,1];
        NEW(pict);
        ltyp:=0;
        color:= col_pc
      END
    END;
    NEW(txt,HIGH(text)+1);
    FOR j:=0 TO HIGH(txt) DO
      WITH txt[j] DO
        xyz:= text[j].xyz;
        NEW(stxt,HIGH(text[j].stxt)+1);
        stxt:= text[j].stxt;
        i:= i_tex;
        a:= 0.;
        w:= tw[j];
        h:= th[j];
        color:= col_ch;
        bas.make_str(dfont,txt[j])
      END
    END
  END;
  obj.new_group(bas.cview^.model^.mbody,new,bas.cview^.cntx^.work);
  bas.extr_grp(bas.cview^.model,new);
  bas.show_group(bas.cview^.model,new)
END new_angle;

BEGIN
  set_coord;
  IF read_angle() THEN new_angle END
END angle;

--------------------- CIRC & ARC DIMENSION ---------------------

PROCEDURE _select(): BOOLEAN;
 VAR s1,s2: ARRAY [0..63] OF CHAR;
       top: def.VERTEX;
         S: BOOLEAN;

PROCEDURE _search;
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO HIGH(bas.cview^.model^.mbody^) DO
    IF bas.cview^.cntx^.mask[i]#0 THEN
      tol.search_cir(i,top);
      tol.search_arc(i,top)
    END
  END
END _search;

BEGIN
  IF bas.ENG THEN
    s1:='Select arc or circel:';
    s2:='Only one arc or one circel!'
  ELSE
    s1:='Выберите дугу или окружность:';
    s2:='Только одна дуга или одна окружность!'
  END;
  S:=bas.cview^.cntx^.STEP; bas.cview^.cntx^.STEP:=FALSE;
  LOOP
    IF bas.read_point(s1,bas.marker,bas.null_monitor,top) THEN
      _search;
      IF (HIGH(obj.mark_cir)+HIGH(obj.mark_arc))=-1 THEN RETURN TRUE END;
      IF (HIGH(obj.mark_cir)+HIGH(obj.mark_arc))>=0 THEN
        bpm.message(bas.SW DIV 2,bas.SH DIV 2,s2);
        tol.unmark
      END;
    ELSE
      bas.cview^.cntx^.STEP:=S; bas.step; RETURN FALSE
    END
  END
END _select;

PROCEDURE radius;

  PROCEDURE read_rad(): BOOLEAN;
  BEGIN
    RETURN FALSE
  END read_rad;

  PROCEDURE new_rad;
  BEGIN
  END new_rad;

BEGIN
  set_coord;
  IF read_rad() THEN new_rad END
END radius;

PROCEDURE diametr;
BEGIN
  set_coord;
END diametr;

PROCEDURE other;
BEGIN
  set_coord;
END other;

PROCEDURE init;
BEGIN
    tex.load(dfont,'bcMain.plf');
    IF NOT tex.done THEN tty.perror(tex.error,'file bcMain.plf'); HALT END;
    H_tex:= 1.;
    h_tex:= 5.;
    w_tex:= 3.;
    i_tex:= mth.dtor(10.);
    arr_s:= 4.5;
    col_pc:= bas.red;
    col_ch:= bas.white
END init;

BEGIN
  init
END bcDim.
