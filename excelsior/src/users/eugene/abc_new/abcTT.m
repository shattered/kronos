IMPLEMENTATION MODULE abcTT; (* Hady. 19-Feb-89. (c) KRONOS *)

IMPORT  mem: Heap,
        voc: abcVoc,
        tty: Terminal,
        res: Resource,
        img: Image;

------------------------VT 220 SUPPORT------------------------
                        --------------

PROCEDURE set_roll_bounds(t,b: INTEGER);
  VAR s: ARRAY [0..11] OF CHAR;
BEGIN
  img.image0(s,'%d;%dr',t,b);
  tty.WriteString(''033c'[');
  tty.WriteString(s);
END set_roll_bounds;


PROCEDURE set_bold(lin: INTEGER); -- cursor stays at (lin,0) position);
BEGIN tty.SetCrs(lin,0); tty.WriteString(''033c'#6') END set_bold;

PROCEDURE clear_bold(lin: INTEGER);
BEGIN tty.SetCrs(lin,0); tty.WriteString(''033c'#5') END clear_bold;

----------------------INFO LINE SUPPORT-----------------------
                      -----------------

(*
0         1         2         3         4         5         6         7
01234567890123456789012345678901234567890123456789012345678901234567890123456789
    |            |       | |      | |        | |        |                 |
<<< *** SILVER ***  BUFF 000  INS off   BELL off  FILE  abracarba           >>>
    01234567890123                                      0123456789012345678
*)

VAR   INFO: ARRAY [0..80] OF CHAR;
    on_off: ARRAY BOOLEAN OF ARRAY [0..7] OF CHAR;

PROCEDURE set_str(VAL s: ARRAY OF CHAR; from: INTEGER);
  VAR i: INTEGER; bump: ARRAY [0..79] OF CHAR;
BEGIN ASSERT(from>=0); i:=0;
  WHILE (i<=HIGH(s))&(i+from<HIGH(INFO))&(s[i]#0c) DO
    bump[i]:=s[i]; INFO[from+i]:=s[i]; INC(i)
  END;
  bump[i]:=0c;
  IF info? THEN
    tty.SetCrs(info_line,from);
    i:=tty.Reverse(1);
    tty.WriteString(bump);
    i:=tty.Reverse(i);
  END;
END set_str;

PROCEDURE set_on_off(on_off: BOOLEAN; from: INTEGER);
  VAR bump: ARRAY [0..3] OF CHAR;
BEGIN
  IF on_off THEN set_str(' on',from)
  ELSE           set_str('off',from)
  END;
END set_on_off;

PROCEDURE ins(on_off: BOOLEAN);  BEGIN set_on_off(on_off,34) END ins;

CONST  spaces = '                                       '
                '                                      ';

PROCEDURE bell(on_off: BOOLEAN);
BEGIN bell?:=on_off; set_on_off(bell?,45) END bell;

PROCEDURE info(on_off: BOOLEAN);
  VAR i: INTEGER;
BEGIN
  IF vt220 THEN info?:=on_off;
  ELSE
    IF info?#on_off THEN tty.SetCrs(info_line,0);
      IF info? THEN tty.ClearLine;
      ELSE i:=tty.Reverse(1);
        tty.WriteString(INFO);
        i:=tty.Reverse(i);
      END; info?:=on_off;
    END;
  END;
END info;

PROCEDURE buff(i: INTEGER);
  VAR bump: ARRAY [0..3] OF CHAR; j: INTEGER;
BEGIN buff_depth:=i; j:=2;
  WHILE j>=0 DO
    bump[j]:=CHAR(INTEGER('0')+(i MOD 10)); i:=i DIV 10; DEC(j);
  END; bump[3]:=0c; set_str(bump, 25);
END buff;

PROCEDURE state(s: ARRAY OF CHAR);
  VAR i: INTEGER; r: BOOLEAN;
BEGIN
  IF vt220 THEN i:=0; r:=FALSE;
    WHILE (i<HIGH(s))&(s[i]#0c)&(s[i]=' ') DO INC(i) END;
    IF s[i]#0c THEN tty.WriteString(''033c'[5m');
      set_str(s,4); tty.WriteString(''033c'[25m');
    ELSE set_str(s,4);
    END;
  ELSE
    set_str(s,4);
  END;
END state;

PROCEDURE file(s: ARRAY OF CHAR);
  VAR bump: ARRAY [0..15] OF CHAR; i: INTEGER;
BEGIN i:=0;
  WHILE (i<=HIGH(s))&(i<HIGH(bump))&(s[i]#0c) DO
    bump[i]:=s[i]; INC(i)
  END;
  WHILE (i<HIGH(bump)) DO bump[i]:=' '; INC(i) END;
  bump[i]:=0c;
  set_str(bump,56);
END file;

PROCEDURE bell!; BEGIN IF bell? THEN tty.Write(07c) END END bell!;

PROCEDURE ref_info;
  VAR i: INTEGER;
BEGIN
  IF info? THEN
    i:=tty.Reverse(1);
    tty.SetCrs(info_line,0);
    tty.WriteString(INFO);
    i:=tty.Reverse(i);
  END;
END ref_info;

PROCEDURE message(VAL s: ARRAY OF CHAR);
  VAR  i: INTEGER;
    bump: ARRAY [0..79] OF CHAR;
BEGIN i:=0;
  WHILE (i<HIGH(bump)) & (i<HIGH(s)) &
             (s[i]#0c) DO bump[i]:=s[i]; INC(i)
  END;
  WHILE i<HIGH(bump)   DO bump[i]:=' ';  INC(i) END;
  bump[i]:=0c;       tty.SetCrs(info_line,0);
  i:=tty.Reverse(1); tty.WriteString(bump);
  i:=tty.Reverse(i);
  bell!;
END message;

PROCEDURE init_info;
BEGIN
  bell?:=TRUE; info?:=FALSE;
  INFO:='<<<                 BUFF 000  INS  on   BELL'
        '  on  FILE                      >>>';
  info(TRUE);
END init_info;

------------------------SCREEN SUPPORT------------------------
                        --------------

TYPE  str_ptr = POINTER TO ARRAY [0..256] OF CHAR;
     wordBody = RECORD
                      from, to: str_ptr;
                  l_from, l_to: INTEGER;
                           vis: INTEGER;
                END;
         word = POINTER TO wordBody;

CONST marks_col = 0;
      left_marg = 2;

VAR     SCREEN: ARRAY [0..scr_final] OF word;
         marks: ARRAY [0..scr_final] OF CHAR;
          bump: ARRAY [0..78] OF CHAR; -- for random usage
          vt52: BOOLEAN;
    first_line: INTEGER;
      cur_line: INTEGER;

PROCEDURE il; BEGIN IF tty.IL() THEN END END il;

VAR marked_line: INTEGER;

PROCEDURE roll_up;
BEGIN
  IF vt220 THEN
    tty.SetCrs(marked_line,0); tty.Write(' '); -- clear marker
    first_line:=(first_line+scr_final-1) MOD scr_final;
    cur_line:=first_line;
    tty.SetCrs(scr_start,0);
    tty.WriteString(""233c 1c "L");
  ELSE
    IF NOT vt52 THEN
      tty.SetCrs(marked_line,0); tty.Write(' '); -- clear marker
      first_line:=(first_line+scr_final-1) MOD scr_final;
      cur_line:=first_line;
      tty.SetCrs(scr_start,0);
      il; ref_info;
    ELSE
      cur_line:=(cur_line+1) MOD scr_final;
    END;
  END;
  marks[cur_line]:=' ';
END roll_up;

PROCEDURE sync; VAR i: INTEGER;
BEGIN
  tty.SetCrs(marked_line,marks_col);
  tty.Write(' ');
  marked_line:=(cur_line+scr_final-first_line) MOD scr_final + scr_start;
  tty.SetCrs(marked_line,marks_col);
  IF vt52 & (marks[cur_line]=' ') THEN tty.Write(177c)
  ELSIF vt52 THEN tty.Write(marks[cur_line])
  ELSE
    i:=tty.Reverse(1);
    tty.Write(marks[cur_line]);
    i:=tty.Reverse(i);
  END;
  tty.Write(' ');
END sync;

PROCEDURE kill_word(VAR w: word);
BEGIN ASSERT(w#NIL);
  IF w^.from#NIL THEN mem.DEALLOCATE(w^.from,(w^.l_from+4) DIV 4) END;
  IF w^.to#NIL   THEN mem.DEALLOCATE(  w^.to,  (w^.l_to+4) DIV 4) END;
  mem.DEALLOCATE(w, SIZE(wordBody));
  w:=NIL;
END kill_word;

PROCEDURE length(VAL s: ARRAY OF CHAR): INTEGER;
  VAR i: INTEGER;
BEGIN i:=0;
  WHILE (i<HIGH(s))&(s[i]#0c) DO INC(i) END;
  RETURN i;
END length;

PROCEDURE assign(VAR   to: ARRAY OF CHAR ;
                 VAL from: ARRAY OF CHAR);
  VAR i: INTEGER;
BEGIN i:=0;
  WHILE (i<HIGH(from))&(from[i]#0c) DO to[i]:=from[i]; INC(i);  END;
END assign;

PROCEDURE new_word(VAR w: word; VAL W: voc.Word);
BEGIN ASSERT(w=NIL);
  mem.ALLOCATE(w, SIZE(wordBody)); ASSERT(w#NIL);
  w^.l_from:=length(W.org); w^.l_to:=length(W.trn); w^.vis:=0;
  mem.ALLOCATE(w^.from, (w^.l_from+4) DIV 4);
  mem.ALLOCATE(w^.to  , (w^.l_to  +4) DIV 4);
  assign(w^.from^,W.org);
  assign(  w^.to^,W.trn);
END new_word;

PROCEDURE app_str(VAR dest: ARRAY OF CHAR;
                  VAL sour: ARRAY OF CHAR;
                  VAR from: INTEGER ;
                    h_dest: INTEGER ;
                       vis: INTEGER ;
                    h_sour: INTEGER);
BEGIN
  IF vis>=h_sour THEN dest[0]:=0c; RETURN END;
  WHILE (vis<h_sour) & (from<h_dest)
     DO dest[from]:=sour[vis]; INC(from); INC(vis)
  END;
END app_str;

PROCEDURE show_word(w: word);
  VAR i: INTEGER;
BEGIN i:=0;
  app_str(bump, w^.from^, i,         30,     0, w^.l_from);
  app_str(bump,    ' = ', i, HIGH(bump),     0,         3);
  app_str(bump,   w^.to^, i, HIGH(bump),w^.vis,   w^.l_to);
  bump[i]:=0c;
  tty.WriteString(bump); tty.ClearLine;
END show_word;

PROCEDURE write(VAL W: voc.Word);
  VAR i: INTEGER;
BEGIN
  IF SCREEN[cur_line]#NIL THEN kill_word(SCREEN[cur_line]) END;
  new_word(SCREEN[cur_line],W);
  sync; show_word(SCREEN[cur_line]);
END write;

PROCEDURE show(VAL W: voc.Word);
BEGIN roll_up; write(W);
END show;

PROCEDURE get_word(VAR w: voc.Word);

  PROCEDURE ass_str(VAR dest: ARRAY OF CHAR;
                    VAL sour: ARRAY OF CHAR;
                      h_sour: INTEGER);
    VAR i: INTEGER;
  BEGIN i:=0;
    WHILE (i<HIGH(dest))&(i<h_sour)&(sour[i]#0c) DO
      dest[i]:=sour[i]; INC(i);
    END;
    dest[i]:=0c;
  END ass_str;

BEGIN w.org[0]:=0c; w.trn[0]:=0c;
  IF SCREEN[cur_line]=NIL THEN RETURN END;
  WITH SCREEN[cur_line]^ DO
    ass_str(w.org,from^,l_from);
    ass_str(w.trn,to^,l_to);
  END;
END get_word;

CONST step = 08; -- step of left/right moving;

PROCEDURE move_left;
BEGIN
  WITH SCREEN[cur_line]^ DO
    IF vis+step<=l_to THEN
      INC(vis,step); sync; show_word(SCREEN[cur_line])
    END;
  END;
END move_left;

PROCEDURE move_right;
BEGIN
  WITH SCREEN[cur_line]^ DO
    IF vis-step>=0 THEN
      DEC(vis,step); sync; show_word(SCREEN[cur_line])
    END;
  END;
END move_right;

PROCEDURE mark_up;
BEGIN
  IF cur_line=0 THEN cur_line:=scr_final-1;
  ELSE
    cur_line:=(cur_line-1) MOD scr_final;
  END;
  sync;
END mark_up;

PROCEDURE mark_dw;
BEGIN cur_line:=(cur_line+1) MOD scr_final; sync;
END mark_dw;

PROCEDURE mark(c: CHAR);
BEGIN marks[cur_line]:=c; sync;
END mark;

PROCEDURE write_out(put: put_proc);

CONST left = 15; size = 60;

  VAR  bump: ARRAY [0..254] OF CHAR;
          i: INTEGER;
BEGIN
  IF SCREEN[cur_line]#NIL THEN
    WITH SCREEN[cur_line]^ DO
      i:=0;
      app_str(bump,from^,i,HIGH(bump),0,l_from);
      app_str(bump,' = ',i,HIGH(bump),0,3);
      app_str(bump, to^ ,i,HIGH(bump),0,l_to);
      bump[i]:=0c; put(bump);
    END;
  END;
END write_out;

-------------------WHOLE SCREEN OPERATIONS--------------------
                   -----------------------

PROCEDURE refresh;
  VAR i,sav_cur: INTEGER;
BEGIN
  IF vt220 THEN set_roll_bounds(scr_start+1,screen-1) END;
  tty.SetCrs(scr_start,0); tty.Clear;
  i:=first_line; sav_cur:=cur_line; cur_line:=i;
  sync; show_word(SCREEN[cur_line]);
  cur_line:=(cur_line+1) MOD scr_final;
  WHILE cur_line#first_line DO
    sync; show_word(SCREEN[cur_line]);
    cur_line:=(cur_line+1) MOD scr_final;
  END;
  cur_line:=sav_cur; ref_info; sync;
END refresh;

PROCEDURE save(put: put_proc);
  VAR i,j : INTEGER;
      bump: ARRAY [0..255] OF CHAR;
BEGIN
  i:=first_line;
  REPEAT j:=0;
    IF SCREEN[i]=NIL THEN bump[0]:=0c; put(bump); put(bump);
    ELSE
      WITH SCREEN[i]^ DO
        app_str(bump,from^,j,HIGH(bump),0,l_from);
        bump[j]:=0c; put(bump); j:=0;
        app_str(bump,to^,j,HIGH(bump),0,l_to);
        bump[j]:=0c; put(bump);
      END;
    END;
    i:=(i+1) MOD scr_final;
  UNTIL i=first_line;
  i:=first_line; j:=0;
  REPEAT
    bump[j]:=marks[i]; i:=(i+1) MOD scr_final; INC(j)
  UNTIL i=first_line;
  bump[j]:=0c; put(bump);
END save;

PROCEDURE restore(get: get_proc);
  VAR w: voc.Word; i: INTEGER;
BEGIN cur_line:=0; first_line:=0;
  REPEAT
    get(w.org); get(w.trn);
    IF (w.org[0]#0c)&(w.trn[0]#0c) THEN write(w) END;
    cur_line:=(cur_line+1) MOD scr_final;
  UNTIL cur_line=first_line;
  get(bump);
  FOR i:=0 TO HIGH(marks) DO marks[i]:=bump[i] END;
  sync;
END restore;

PROCEDURE final;
BEGIN
  set_roll_bounds(1,24);
  clear_bold(scr_start-1);
  tty.ClearLine;
END final;

PROCEDURE init_scr;
  VAR i: INTEGER;
BEGIN
  tty.SetCrs(scr_start,0); tty.Clear;
  IF vt220 THEN vt52:=FALSE;
    set_roll_bounds(scr_start+1,screen-1);
    res.Final(final);
  ELSE vt52:=(tty.IL());
  END;
  FOR i:=0 TO HIGH(SCREEN) DO SCREEN[i]:=NIL; marks[i]:=' '; END;
  first_line:=0; cur_line:=0; sync; init_info; buff(0); ref_info;
END init_scr;

--------------------------------------------------------------

BEGIN
END abcTT.

break
break =
