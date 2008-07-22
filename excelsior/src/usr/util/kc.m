MODULE kc; (*kl  20-Sep-90. (c) KRONOS *)

IMPORT  tty: Terminal,
        lex: Lexicon,
        srt: Sorts,
        xpr: regExpr,
        tim: Time,
        scn: kcScreens,
        key: Keyboard,
        str: Strings,
        bio: BIO,
        env: tskEnv,
        arg: tskArgs,
        sed: strEditor,
        com: Shell,
        usr: Users,
        mem: Heap;

IMPORT  ASCII;

FROM kcScreens IMPORT SCREEN,TTY;
FROM SYSTEM    IMPORT WORD,ADDRESS,ADR;

WITH STORAGE (NEW    : mem.ALLOCATE;
              DISPOSE: mem.DEALLOCATE;
              RESIZE : mem.REALLOCATE);

TYPE SORT    = (Nam,Ext,Tim,Siz,Uns);
TYPE NAME    = ARRAY [0..31] OF CHAR;
     STR64   = ARRAY [0..63] OF CHAR;
     LONGSTR = ARRAY [0..255] OF CHAR;
     WTYPE   = (BRIEF,FULL,INFO,TREE);
     FDSC = RECORD
        ftype,mode: BITSET;
          eof,time: INTEGER;
              name: NAME
            END;

     FD = DYNARR OF FDSC;
     PFD = POINTER TO FD;


     WINDOW = RECORD
                  type: WTYPE;
            numb,  all: INTEGER;
          marked,bytes: INTEGER;
          first,  oldf: INTEGER;
          pos,  oldpos: INTEGER;
         h,w,fw,fields: INTEGER;
        old_fld,old_fw: INTEGER;
                   scr: SCREEN;
                on,hid: BOOLEAN;
                s_kind: SORT;
                  path: LONGSTR;
                  file: bio.FILE;
                  time: INTEGER;
                  fdsc: FD
              END;


TYPE V_MENU = RECORD
                scr: SCREEN;
               alts: DYNARR OF NAME;
              END;


TYPE OPTSET = RECORD
                header,subject,ok_alt: NAME;
                cur,wid: INTEGER;
                alt: DYNARR OF STRING;
                ed : DYNARR OF BOOLEAN
              END;

TYPE EXTACT  = RECORD ext,act: STRING END;
     EXTMENU = DYNARR OF EXTACT;

TYPE COMP = PROCEDURE(INTEGER,INTEGER,INTEGER): INTEGER;

VAR Bars,Timer,Lmenu: SCREEN;
    filter: ARRAY [0..1] OF OPTSET;
    vm    : ARRAY [0..4] OF V_MENU;

VAR timer_on : BOOLEAN;
VAR save_time: INTEGER;

VAR sel_str,dev_str,dst_str,dir_str: STR64; cmd_str: LONGSTR;

VAR LINES,L,HALF,x,y,w: INTEGER;

VAR black,blue,yellow,orange,green,dark_blue,red,violet,head_mac: INTEGER;

VAR  ALTS: ARRAY [0..13] OF NAME;
     dsc,shdsc: sed.descriptor;
     dst,src,tmp: LONGSTR;

VAR e_menu: EXTMENU;

VAR set_back: PROCEDURE(INTEGER);

CONST
  Digits =
'Digits: 1Help  2Mount  3View  4Ex  5Cp  6Mv  7MkDir  8Rm   9PullDw   0Quit   '
;
  Gold   =
'Gold  : F1Shell F2Change Window Size 1Save setup 2On/Off  7Find file  0Tree  '
;
  Bronze =
'[Save setup] [Find file] [Compare directories] [Panels on/off] [On/Off]      '
;
  Silver =
''
;
  Size   =
'Arrows: left,right,up,down  ins,del -width   "+","-" - columns  "." - 3*12  '
;

CONST min_fw=8; max_fw=32;

VAR wnd: ARRAY [0..2] OF WINDOW;
    num,active: INTEGER;

VAR hb,vb,ul,ur,dl,dr,ud,du,lr,rl,buf_ch: CHAR;
    mac,mic,noc: INTEGER;

CONST CHAR_MAP = 481;

---------------------------- COMMON ----------------------------
                            --------

PROCEDURE new(i: INTEGER;
              get_path: BOOLEAN);FORWARD;
PROCEDURE new_screen(n: INTEGER);FORWARD;
PROCEDURE action   (ch: CHAR);   FORWARD;
PROCEDURE save_setup;           FORWARD;

PROCEDURE min(a,b: INTEGER): INTEGER;
BEGIN
  IF a<b THEN RETURN a ELSE RETURN b END
END min;

PROCEDURE max(a,b: INTEGER): INTEGER;
BEGIN
  IF a>b THEN RETURN a ELSE RETURN b END
END max;

PROCEDURE diap(x,a,b: INTEGER): INTEGER;
BEGIN
  IF x<a THEN RETURN a ELSIF x>b THEN RETURN b ELSE RETURN x END
END diap;

PROCEDURE sm(i: INTEGER): BOOLEAN;
BEGIN
  RETURN (i MOD 10=2)&(i#12)
END sm;

PROCEDURE setcr(c,r: INTEGER);
BEGIN
  scn.set_color(c); scn.set_reverse(r)
END setcr;

PROCEDURE setcbr(c,b,r: INTEGER);
BEGIN
  scn.set_color(c); set_back(b); scn.set_reverse(r)
END setcbr;

PROCEDURE ttycbr(c,b,r: INTEGER);
BEGIN
  tty.set_color(c); tty.set_back(b); tty.set_reverse(r)
END ttycbr;

PROCEDURE cut(VAR s,d: ARRAY OF CHAR);
  VAR i: INTEGER;
BEGIN i:=0;
  WHILE (s[i]#' ')&(s[i]#0c)&(i<HIGH(d)) DO d[i]:=s[i]; INC(i) END; d[i]:=0c
END cut;

PROCEDURE sup(VAR s,d: ARRAY OF CHAR);
  VAR i,j,p: INTEGER;
BEGIN
   i:=str.len(s)-1; WHILE (i>=0)&(s[i]=' ') DO DEC(i) END; s[i+1]:=0c;
   j:=str.len(s); p:=0;
   WHILE (p<j)&(s[p]=' ') DO INC(p) END;
   FOR i:=p TO j DO d[i-p]:=s[i] END;
END sup;

PROCEDURE edit_str(VAR s: ARRAY OF CHAR;y,x0,x1: INTEGER; dsc: sed.descriptor);
  VAR i: INTEGER;
BEGIN
   sed.edit_str('',s,y,x0,x1,dsc,key.up,key.dw,033c);
   tty.set_pos(y,tty.state^.columns-1); ttycbr(noc,mic,0); tty.Write(' ');
   sup(s,s);
END edit_str;

PROCEDURE put_ext(VAL f: ARRAY OF CHAR; VAR e: ARRAY OF CHAR);
  VAR l,i,j: INTEGER;
BEGIN
  l:=str.len(f); i:=l-1;
  WHILE (i>=0)&(f[i]#'.') DO DEC(i) END;
  IF i<=0 THEN e:=''
  ELSE
    INC(i); FOR j:=i TO l DO e[j-i]:=f[j] END
  END;
END put_ext;

PROCEDURE splitnamext(VAR f,e: ARRAY OF CHAR);
  VAR l,i,j: INTEGER;
BEGIN
  l:=str.len(f); i:=l-1;
  WHILE (i>=0)&(f[i]#'.') DO DEC(i) END;
  IF i<=0 THEN e:=''
  ELSE
    f[i]:=0c; INC(i); FOR j:=i TO l DO e[j-i]:=f[j] END
  END;
END splitnamext;

PROCEDURE fill(VAR s: ARRAY OF CHAR);
  VAR j: INTEGER;
BEGIN
  FOR j:=str.len(s) TO HIGH(s)-1 DO s[j]:=' ' END; s[HIGH(s)]:=0c
END fill;

PROCEDURE passive(): INTEGER;
BEGIN
  RETURN (active+1) MOD 2
END passive;

PROCEDURE spy(VAR ch: CHAR); FORWARD;

PROCEDURE wnd_is_dir(n: INTEGER): BOOLEAN;
BEGIN
  RETURN (wnd[n].type=BRIEF)OR(wnd[n].type=FULL)
END wnd_is_dir;

PROCEDURE is_dir(b: BITSET): BOOLEAN;
BEGIN
  RETURN b*bio.e_dir#{}
END is_dir;

PROCEDURE isnt_dir(b: BITSET): BOOLEAN;
BEGIN
  RETURN b*bio.e_dir={}
END isnt_dir;

PROCEDURE empty_dir(d: bio.FILE): BOOLEAN;
   VAR name: NAME; mode: BITSET; ans: BOOLEAN;
BEGIN
  bio.dir_walk(d,bio.s_dirfwd);
  IF NOT bio.done THEN RETURN FALSE END;
  ans:=bio.get_entry(d,name,mode);
  IF ans & (name='..') THEN
    ans:=bio.get_entry(d,name,mode)
  END;
  bio.end_walk(d); RETURN NOT ans
END empty_dir;

PROCEDURE last(VAL s: ARRAY OF CHAR): CHAR;
  VAR i: INTEGER;
BEGIN
  FOR i:=1 TO HIGH(s) DO
    IF s[i]=0c THEN RETURN s[i-1] END
  END;
  RETURN 0c
END last;

PROCEDURE chdir(VAL path: ARRAY OF CHAR);
BEGIN
  IF path='' THEN bio.chdir('/') ELSE bio.chdir(path) END;
END chdir;

PROCEDURE splitpathname(VAR path,name: ARRAY OF CHAR);
  VAR i: INTEGER;
BEGIN
  IF path='' THEN name:='/'0c
  ELSE
    bio.splitpathname(path,name);
    IF (name='')&(last(path)='/') THEN
      path[str.len(path)-1]:=0c
    END;
  END;
END splitpathname;

----------------------------- MENUS ----------------------------
                             -------

PROCEDURE ini_set(VAR set: OPTSET; c,w: INTEGER;
                  VAL hd,sb,ok,alt: ARRAY OF CHAR);
  VAR pos,num,cur,i,k: INTEGER;
BEGIN
  set.header :=hd; set.subject:=sb;
  set.ok_alt :=ok; set.cur:=c;
  set.wid:=w;      num:=0;
  FOR i:=0 TO str.len(alt)-1 DO
    IF alt[i]='|' THEN INC(num) END;
  END;
  NEW(set.alt,num); pos:=0;
  NEW(set.ed, num); cur:=0;
  WHILE cur<num DO
    IF alt[pos]='#' THEN INC(pos);
         set.ed[cur]:=TRUE
    ELSE set.ed[cur]:=FALSE
    END;
    k:=0;
    WHILE alt[pos]#'|' DO tmp[k]:=alt[pos]; INC(k); INC(pos) END; tmp[k]:=0c;
    IF set.ed[cur] THEN k:=255 END;
    NEW(set.alt[cur],k+1); str.copy(set.alt[cur],tmp);
    INC(pos); INC(cur)
  END;
END ini_set;

PROCEDURE ini_menu(VAR m: V_MENU; pos: INTEGER; SEQ s: NAME);
  VAR i,j,a,n: INTEGER; ch: CHAR; scr: SCREEN;
BEGIN
  WITH m DO
    n:=HIGH(s)+1; NEW(alts,n);
    FOR i:=0 TO n-1 DO alts[i]:=s[i] END;
    j:=0;
    FOR i:=0 TO n-1 DO j:=max(j,str.len(s[i])) END;
    setcbr(mac,black,1);
    scn.create(scr,1,pos*10+1,n+2,j+4);
    scn.frame(scr);
    FOR i:=0 TO HIGH(s) DO
     IF s[i]#'' THEN
        scn.set_pos(scr,i+1,2);
        scn.write(scr,s[i],1,str.len(s[i])-1)
      ELSE
        scn.set_pos(scr,i+1,1);
        scn.repeat(scr,hb,j+2)
      END;
    END;
    setcr(noc,0)
  END;
END ini_menu;

VAR warn_s,warn_v,warn_h: SCREEN;

PROCEDURE cre_warning(VAL hd,ob,er: ARRAY OF CHAR; ok: BOOLEAN);
  VAR qo: STR64;
BEGIN
  IF ob#'' THEN str.print(qo,'"%s"',ob) END;
  setcbr(orange,red,0);
  scn.create(warn_s,y,x,5,w); scn.frame(warn_s);
  scn.center(warn_s,hd,1); scn.center(warn_s,qo,2);
  scn.center(warn_s,er,3); scn.set_pos(warn_s,4,w-6);
  IF ok THEN setcbr(orange,black,1); scn.write(warn_s,' OK ',0,4) END;
  scn.on_top(warn_s);         setcbr(orange,black,0);
  scn.create(warn_h,y+5,x+2,1,w-2); scn.on_top(warn_h);
  scn.create(warn_v,y+1,x+w,5,2);   scn.on_top(warn_v);
END cre_warning;

PROCEDURE kill_warning;
BEGIN
  scn.kill(warn_s);
  scn.kill(warn_v);
  scn.kill(warn_h)
END kill_warning;

PROCEDURE warning(VAL h,o,e: ARRAY OF CHAR);
  VAR ch: CHAR;
BEGIN
  cre_warning(h,o,e,TRUE); spy(ch); kill_warning
END warning;

PROCEDURE s_warning(VAL h,o,e: ARRAY OF CHAR);
  VAR ch: CHAR;
BEGIN
  cre_warning(h,o,e,TRUE); key.read(ch); kill_warning
END s_warning;


PROCEDURE select_target(VAR s: ARRAY OF CHAR; where: INTEGER): BOOLEAN; FORWARD;

PROCEDURE say(VAL head: ARRAY OF CHAR;
              VAR item: ARRAY OF CHAR;
                    ed: BOOLEAN;
               SEQ alt: INTEGER): BOOLEAN;

  VAR i,pos: INTEGER; ch: CHAR; scr,hsh,vsh: SCREEN;

  PROCEDURE show_alts;
    VAR i,j: INTEGER;
  BEGIN
    FOR i:=0 TO HIGH(alt) DO
      IF i=pos THEN setcbr(mac,blue,1) ELSE setcbr(noc,blue,0) END;
      j:=str.len(ALTS[alt[i]]);
      scn.set_pos(scr,3,(w DIV (HIGH(alt)+2))*(i+1)-(j DIV 2)-1);
      scn.write(scr,ALTS[alt[i]],0,j)
    END;
    setcr(noc,0)
  END show_alts;

  PROCEDURE hot(ch: CHAR): BOOLEAN;
    VAR i,j: INTEGER; kind: BITSET;
  BEGIN
    ch:=ASCII.CAPITAL(ch); kind:=ASCII.KIND(ch);
    IF (ASCII.latin IN kind) OR (ASCII.cyril IN kind) THEN
      FOR i:=0 TO HIGH(alt) DO
        FOR j:=0 TO str.len(ALTS[alt[i]])-1 DO
          IF ch=ALTS[alt[i],j] THEN pos:=i; show_alts; RETURN TRUE END;
        END;
      END;
    END;
    RETURN FALSE
  END hot;

  PROCEDURE cre;
  BEGIN
    set_back(blue);
    scn.create(scr,y,x,5,w); scn.frame(scr); scn.on_top(scr);
    set_back(black);
    scn.create(hsh,y+5,x+2,1,w-2); scn.on_top(hsh);
    scn.create(vsh,y+1,x+w,5,2);   scn.on_top(vsh);
  END cre;

  PROCEDURE kil;
  BEGIN
    scn.kill(scr);
    scn.kill(vsh);
    scn.kill(hsh)
  END kil;

BEGIN
  setcr(noc,0); cre;
  setcbr(noc,blue,0);
  i:=str.len(head);
  IF ed
  THEN scn.set_pos(scr,1,1);            scn.write(scr,head,0,i);
       scn.set_pos(scr,2,1); set_back(black); scn.repeat(scr,' ',w-2)
  ELSE scn.set_pos(scr,1,(w-i) DIV 2);  scn.write(scr,head,0,i)
  END;
  pos:=-1;
  LOOP
    show_alts;
    IF pos=-1 THEN
        IF ed THEN
          IF mac-mic<7
          THEN ttycbr(blue  ,black,1)
          ELSE ttycbr(orange,black,0)
          END;
          str.copy(tmp,item);
          edit_str(tmp,y+2,x+1,x+w-2,dsc);
          setcbr(noc,blue,0);
          scn.set_cursor(0);
          IF dsc^.last=033c   THEN kil; RETURN FALSE
          ELSE str.copy(item,tmp)
          END;
          IF dsc^.last=key.cr THEN kil; RETURN TRUE
          ELSE pos:=min(0,HIGH(alt))
          END;
        ELSE
          i:=str.len(item);
          scn.set_pos(scr,2,(w-i) DIV 2);  scn.write(scr,item,0,i);
          IF HIGH(alt)<0 THEN kil; RETURN TRUE
          ELSE pos:=0 END;
        END;
    ELSE
      LOOP
        spy(ch);
        CASE ch OF
          key.right: pos:=(pos+1) MOD (HIGH(alt)+1); EXIT
         |key.left : pos:=(pos+HIGH(alt)) MOD (HIGH(alt)+1); EXIT
         |key.up   : IF ed THEN pos:=-1; EXIT END
         |033c     : kil; RETURN FALSE
        ELSE
          IF (ch=key.cr) OR hot(ch) THEN
            IF    pos=0         THEN kil; RETURN TRUE
            ELSIF pos=HIGH(alt) THEN kil; RETURN FALSE
            ELSIF alt[pos]=1    THEN
              IF select_target(item,3) THEN pos:=-1; EXIT END;
            ELSIF alt[pos]=13   THEN save_setup; RETURN TRUE
            END;
          END;
        END;
      END;
    END;
  END;
END say;

PROCEDURE say2(VAL head1: ARRAY OF CHAR;
               VAR item1: ARRAY OF CHAR;
               VAL head2: ARRAY OF CHAR;
               VAR item2: ARRAY OF CHAR;
                 SEQ alt: INTEGER): INTEGER;

  VAR i,pos: INTEGER; ch: CHAR; scr,hsh,vsh: SCREEN;

  PROCEDURE show_alts;
    VAR i,j: INTEGER;
  BEGIN
    FOR i:=0 TO HIGH(alt) DO
      IF i=pos THEN setcbr(mac,blue,1) ELSE setcbr(noc,blue,0) END;
      j:=str.len(ALTS[alt[i]]);
      scn.set_pos(scr,5,(w DIV (HIGH(alt)+2))*(i+1)-(j DIV 2)-1);
      scn.write(scr,ALTS[alt[i]],0,j)
    END;
    setcr(noc,0)
  END show_alts;

  PROCEDURE hot(ch: CHAR): BOOLEAN;
    VAR i,j: INTEGER; kind: BITSET;
  BEGIN
    ch:=ASCII.CAPITAL(ch); kind:=ASCII.KIND(ch);
    IF (ASCII.latin IN kind) OR (ASCII.cyril IN kind) THEN
      FOR i:=0 TO HIGH(alt) DO
        FOR j:=0 TO str.len(ALTS[alt[i]])-1 DO
          IF ch=ALTS[alt[i],j] THEN pos:=i; show_alts; RETURN TRUE END;
        END;
      END;
    END;
    RETURN FALSE
  END hot;

  PROCEDURE cre;
  BEGIN
    set_back(blue);
    scn.create(scr,y,x,7,w); scn.frame(scr); scn.on_top(scr);
    set_back(black);
    scn.create(hsh,y+7,x+2,1,w-2); scn.on_top(hsh);
    scn.create(vsh,y+1,x+w,7,2);   scn.on_top(vsh);
    set_back(blue);
  END cre;

  PROCEDURE kil; BEGIN scn.kill(scr); scn.kill(vsh); scn.kill(hsh) END kil;

BEGIN
  setcr(noc,0); cre;
  scn.set_pos(scr,1,2);  scn.WriteString(scr,head1);
  scn.set_pos(scr,3,2);  scn.WriteString(scr,head2);
  IF mac-mic<7
  THEN setcbr(blue  ,black,1)
  ELSE setcbr(orange,black,0)
  END;
  scn.set_pos(scr,2,1);  i:=str.len(item1);
  scn.write(scr,item1,0,i); scn.repeat(scr,' ',w-i-2);
  scn.set_pos(scr,4,1);  i:=str.len(item2);
  scn.write(scr,item2,0,i); scn.repeat(scr,' ',w-i-2);
  pos:=0; show_alts;
  LOOP
    IF pos<0 THEN
      IF mac-mic<7
      THEN ttycbr(blue  ,black,1)
      ELSE ttycbr(orange,black,0)
      END;
      IF pos=-2 THEN str.copy(tmp,item1) ELSE str.copy(tmp,item2) END;
      edit_str(tmp,y+4+2*(pos+1),x+1,x+w-2,dsc);
      IF dsc^.last#033c THEN
        IF pos=-2 THEN str.copy(item1,tmp) ELSE str.copy(item2,tmp) END;
      END;
      setcbr(noc,blue,0); scn.set_cursor(0);
      CASE dsc^.last OF
         033c     : kil; RETURN 0
        |key.dw   : IF pos=-1 THEN pos:=0 ELSE pos:=-1 END
        |key.up   : IF pos=-2 THEN pos:=0 ELSE pos:=-2 END
      ELSE
      END;
    ELSE
      LOOP
        show_alts; spy(ch);
        CASE ch OF
          key.right: pos:=(pos+1) MOD (HIGH(alt)+1)
         |key.left : pos:=(pos+HIGH(alt)) MOD (HIGH(alt)+1)
         |key.up   : pos:=-1; show_alts; EXIT
         |key.dw   : pos:=-2; show_alts; EXIT
         |033c     : kil; RETURN 0
        ELSE
          IF (ch=key.cr) OR hot(ch) THEN
            IF pos<HIGH(alt)-1  THEN kil; RETURN pos+1
            ELSIF pos=HIGH(alt) THEN kil; RETURN 0
            ELSE (*tree*)
              IF select_target(item2,3) THEN pos:=-1; EXIT END;
            END;
          END;
        END;
      END;
    END;
  END;
END say2;

PROCEDURE ask(n: INTEGER; VAR set: OPTSET): BOOLEAN;
  VAR i,d,x,y,u,pos: INTEGER; ch: CHAR; scr: SCREEN;

  PROCEDURE show_alts;
    VAR i,j: INTEGER;
  BEGIN
    IF pos=-1 THEN setcr(mac,1) ELSE setcr(noc,0) END;
    j:=str.len(set.ok_alt);
    scn.set_pos(scr,u,(set.wid DIV 3)-(j DIV 2));
    scn.write(scr,set.ok_alt,0,j);
    IF pos=-2 THEN
      scn.set_reverse(1); scn.set_color(mac)
    ELSE
      scn.set_reverse(0); scn.set_color(noc)
    END;
    scn.set_pos(scr,u,(set.wid DIV 3)*2-5);
    scn.write(scr,'[ Cancel ]',0,10);
    scn.set_reverse(0); scn.set_color(noc)
  END show_alts;

  PROCEDURE kil; BEGIN scn.kill(scr) END kil;

BEGIN
  u:=HIGH(set.alt)+1; d:=1;
  IF set.subject#'' THEN INC(u); d:=2 END;
  IF set.ok_alt #'' THEN INC(u,2)     END;
  IF n<0 THEN x:=HALF-(set.wid  DIV 2)
  ELSE
    x:=((HALF-set.wid)  DIV 2)+HALF*n
  END;
  y:=max(2,(LINES-u) DIV 2);
  setcbr(noc,red,0);
  scn.create(scr,y,x,u+2,set.wid);
  scn.frame(scr);  scn.title(scr,set.header);
  IF set.subject#'' THEN
    scn.center(scr,set.subject,1)
  END;
  IF set.ok_alt #'' THEN
    scn.set_pos(scr,u-1,0);
    scn.Write(scr,lr); scn.repeat(scr,hb,set.wid-2); scn.Write(scr,rl)
  END;
  scn.on_top(scr); pos:=0;
  LOOP
    show_alts;
    FOR i:=0 TO HIGH(set.alt) DO
      IF set.ed[i] THEN
           scn.set_pos(scr,i+d,2)
      ELSE scn.set_pos(scr,i+d,2);
       IF i=set.cur THEN
         scn.WriteString(scr,'[+] ') ELSE
         scn.WriteString(scr,'[ ] ')
       END;
       IF i=pos THEN
         scn.set_reverse(1)
       END
      END;
      scn.WriteString(scr,set.alt[i]);
      scn.set_reverse(0)
    END;
    IF pos>=0 THEN
      IF set.ed[pos] THEN
        str.copy(tmp,set.alt[pos]);
        edit_str(tmp,y+pos+d,x+2,x+set.wid-3,dsc);
        scn.set_cursor(0);
        IF    dsc^.last=033c   THEN kil; RETURN FALSE
        ELSE str.copy(set.alt[pos],tmp)
        END;
        IF    dsc^.last=key.dw THEN INC(pos)
        ELSIF dsc^.last=key.up THEN DEC(pos)
        END;
        pos:=max(0,pos);
        IF pos>HIGH(set.alt) THEN pos:=-1 END
      ELSE
        LOOP
          spy(ch);
          CASE ch OF
            key.up: DEC(pos); EXIT
           |key.dw: INC(pos); EXIT
           |key.cr: set.cur:=pos; kil; RETURN TRUE
           |033c  : kil; RETURN FALSE
          ELSE
          END;
        END;
        pos:=max(0,pos);
        IF pos>HIGH(set.alt) THEN pos:=-1 END
      END;
    ELSE
      LOOP
        spy(ch);
        CASE ch OF
          key.left,
          key.right: IF pos=-1 THEN pos:=-2 ELSE pos:=-1 END; EXIT
         |key.up   : pos:=HIGH(set.alt); EXIT
         |033c     : kil; RETURN FALSE
         |key.cr   :
           IF pos=-1 THEN kil; RETURN TRUE
           ELSIF pos=-2 THEN kil; RETURN FALSE
           END;
        ELSE
        END;
      END;
    END;
  END;
END ask;

PROCEDURE header(n: INTEGER);
  VAR name: NAME; i: INTEGER;
BEGIN
  WITH wnd[n] DO
    CASE type OF
      BRIEF,
      FULL: str.copy(name,path) |
      INFO: name:='INFO'|
      TREE: name:='TREE'
    ELSE
      ASSERT(FALSE)
    END;
    IF mac-mic<7 THEN
      IF n=active
      THEN setcbr(head_mac,yellow,1)
      ELSE setcbr(blue  ,black, 1)
      END;
    ELSE
      IF n=active
      THEN setcbr(violet,yellow,1)
      ELSE setcbr(violet,black, 1)
      END;
    END;
    i:=str.len(name);
    scn.set_pos(scr,0,1+(w-i-4)*n);
    scn.Write(scr,' '); scn.write(scr,name,0,i); scn.Write(scr,' ');
    scn.set_pos(scr,0,1+(i+2)*(1-n)); scn.repeat(scr,' ',w-i-4);
    set_back(dark_blue);
  END;
END header;

PROCEDURE d_frame(i: INTEGER);
  VAR j,k: INTEGER;
BEGIN
  setcbr(violet,black,0);
  WITH wnd[i] DO
    FOR j:=1 TO fields-1 DO
      scn.set_pos(scr,h+1,fw*j); scn.Write(scr,du)
    END;
    setcbr(violet,dark_blue,0);
    FOR k:=1 TO h DO
      FOR j:=0 TO fields-2 DO
        scn.set_pos(scr,k,1+fw*j);
        scn.repeat(scr,' ',fw-1); scn.Write(scr,vb)
      END;
      scn.set_pos(scr,k,1+fw*(fields-1)); scn.repeat(scr,' ',fw-1)
    END;
  END;
END d_frame;

PROCEDURE bars_menu(VAL s: ARRAY OF CHAR);
  VAR j: INTEGER;

  PROCEDURE sh;
    VAR i: INTEGER;
  BEGIN
    FOR i:=j TO str.len(s)-1 DO
      IF (s[i]>='*') & (s[i]<='9')
      THEN setcr(mac,0)
      ELSE setcr(noc,0)
      END;
      scn.Write(Bars,s[i])
    END;
  END sh;

BEGIN
  scn.off(Bars);
  j:=0; WHILE s[j]#':' DO INC(j) END;
  setcbr(mac,red,0); scn.set_pos(Bars,0,0);
  scn.write(Bars,s,0,j); sh;
  scn.on(Bars); scn.refresh(Bars)
END bars_menu;

PROCEDURE frame;
  VAR i: INTEGER;
BEGIN
  header(0); header(1); bars_menu(Digits);
END frame;

----------------------------- SORT -----------------------------
                             ------

PROCEDURE scomp(VAL s1,s2: ARRAY OF CHAR): INTEGER;
  VAR e1,e2: NAME; i1,i2,l1,l2,j: INTEGER;
BEGIN
  l1:=0; WHILE s1[l1]#' ' DO INC(l1) END;
  i1:=l1-1;
  WHILE (i1>=0)&(s1[i1]#'.') DO DEC(i1) END;
  IF i1<0 THEN e1:=''
  ELSE
    INC(i1); FOR j:=i1 TO l1-1 DO e1[j-i1]:=s1[j] END;
    e1[l1-i1]:=0c
  END;
  l2:=0; WHILE s2[l2]#' ' DO INC(l2) END;
  i2:=l2-1;
  WHILE (i2>=0)&(s2[i2]#'.') DO DEC(i2) END;
  IF i2<0 THEN e2:=''
  ELSE
    INC(i2); FOR j:=i2 TO l2-1 DO e2[j-i2]:=s2[j] END;
    e2[l2-i2]:=0c
  END;
  j:=srt.str_comp(e1,e2);
  IF j#0 THEN RETURN j ELSE RETURN srt.str_comp(s1,s2) END
END scomp;


PROCEDURE ext_comp(n,a,b: INTEGER): INTEGER;
  VAR na,ea,nb,eb: NAME; i: INTEGER;
BEGIN
  WITH wnd[n] DO
    cut(fdsc[a].name,na); splitnamext(na,ea);
    cut(fdsc[b].name,nb); splitnamext(nb,eb);
    i:=srt.str_comp(ea,eb);
    IF i=0 THEN RETURN srt.str_comp(na,nb) ELSE RETURN i END;
  END;
END ext_comp;

PROCEDURE dir_comp(n,a,b: INTEGER): INTEGER;
  PROCEDURE file_comp(): INTEGER;
    VAR i: INTEGER;
  BEGIN
    WITH wnd[n] DO
      CASE s_kind OF
        Nam: RETURN srt.str_comp(fdsc[a].name,fdsc[b].name)
       |Ext: RETURN ext_comp(n,a,b)
       |Tim: IF fdsc[a].name#fdsc[b].name THEN
               RETURN fdsc[a].time-fdsc[b].time
             ELSE RETURN 0
             END;
       |Siz: IF fdsc[a].name#fdsc[b].name THEN
               RETURN fdsc[a].eof -fdsc[b].eof
             ELSE RETURN 0
             END;
       |Uns: IF fdsc[a].name#fdsc[b].name THEN
               RETURN -1
             ELSE RETURN 0
             END;
      END;
    END;
  END file_comp;
BEGIN
  WITH wnd[n] DO
    IF is_dir(fdsc[a].ftype) THEN
      IF is_dir(fdsc[b].ftype) THEN
        RETURN srt.str_comp(fdsc[a].name,fdsc[b].name);
      ELSE RETURN -1
      END;
    ELSIF is_dir(fdsc[b].ftype) THEN RETURN 1
    ELSE
      RETURN file_comp()
    END;
  END;
END dir_comp;

PROCEDURE tree_comp(n,a,b: INTEGER): INTEGER;
BEGIN
  WITH wnd[n] DO
    RETURN srt.str_comp(fdsc[a].name,fdsc[b].name);
  END;
END tree_comp;

PROCEDURE swap(n,a,b: INTEGER);
  VAR t: FDSC;
BEGIN
  WITH wnd[n] DO
    t:=fdsc[a]; fdsc[a]:=fdsc[b]; fdsc[b]:=t;
    IF pos=a THEN pos:=b ELSIF pos=b THEN pos:=a END;
  END;
END swap;

PROCEDURE quick(n,a,b: INTEGER; comp: COMP);

  PROCEDURE do(l,r: INTEGER);
    VAR i,j,ix: INTEGER;
  BEGIN
    i:=l; j:=r;
    ix:=(l+r) DIV 2;
    REPEAT
      WHILE comp(n,i,ix)<0 DO INC(i) END;
      WHILE comp(n,ix,j)<0 DO DEC(j) END;
      IF i<=j THEN
        IF i#j THEN
          IF    ix=i THEN ix:=j
          ELSIF ix=j THEN ix:=i
          END;
          swap(n,i,j);
        END;
        INC(i); DEC(j);
      END;
    UNTIL i>j;
    IF l<j THEN do(l,j) END;
    IF i<r THEN do(i,r) END;
  END do;

BEGIN
  do(a,b);
END quick;



----------------------------- TREE -----------------------------
                             ------

PROCEDURE create_short_tree(n: INTEGER; p: BOOLEAN);
  VAR name,patt: NAME;
           mode: BITSET;
              i: INTEGER;
BEGIN
  WITH wnd[n] DO
    pos:=0; numb:=1; marked:=1;
    NEW(fdsc,32); tmp:=path;
    IF path='/'0c THEN p:=FALSE END;
    splitpathname(tmp,fdsc[0].name);
    IF p THEN
      chdir(tmp); patt:=fdsc[0].name;
      splitpathname(tmp,fdsc[0].name)
    ELSE
      chdir(path)
    END;
    fdsc[0].time:=0; fdsc[0].ftype:={0};
    bio.dup(file,bio.cd);
    bio.get_attr(file,bio.a_wtime,time);
    bio.dir_walk(bio.cd,bio.s_dirfwd);
    WHILE bio.get_entry(bio.cd,name,mode) DO
      IF is_dir(mode) & (name#'..') THEN
        IF HIGH(fdsc)<numb THEN
          RESIZE(fdsc,numb+8)
        END;
        fdsc[numb].time:=1;
        fdsc[numb].ftype:={1};
        fdsc[numb].name:=name;
        INC(numb)
      END;
    END;
    bio.end_walk(bio.cd);
    quick(n,1,numb-1,tree_comp);
    fdsc[numb-1].ftype:={}; first:=0;
    IF p THEN i:=1;
      WHILE (i<numb)&(patt#fdsc[i].name) DO INC(i) END;
      IF i#numb THEN pos:=i END;
    END;
    IF p&(pos=0) THEN splitpathname(path,tmp) END;
    IF p THEN chdir(path) END;
  END;
END create_short_tree;

PROCEDURE recreate_short(n: INTEGER; p: BOOLEAN);
  VAR cd: STRING;
BEGIN
  bio.close(wnd[n].file); DISPOSE(wnd[n].fdsc); create_short_tree(n,p)
END recreate_short;

---------------------------- FILTER ----------------------------
                            --------

PROCEDURE reorder_all(n: INTEGER);
  VAR i,j,k: INTEGER; d: FDSC;
BEGIN
  WITH wnd[n] DO
    FOR i:=numb TO all-1 DO
      d:=fdsc[i]; k:=INTEGER(d.mode) DIV 2; d.mode:=d.mode*{0};
      FOR j:=i-1 TO k BY -1 DO fdsc[j+1]:=fdsc[j] END;
      fdsc[k]:=d; IF pos>=k THEN INC(pos) END;
    END;
    numb:=all
  END;
END reorder_all;

PROCEDURE filter_match(e: xpr.EXPR; n,i: INTEGER): BOOLEAN;
BEGIN
  IF is_dir(wnd[n].fdsc[i].ftype) THEN RETURN TRUE
  ELSE  cut(wnd[n].fdsc[i].name,tmp);  RETURN xpr.match(e,tmp,0)
  END;
END filter_match;

PROCEDURE try_filter(n: INTEGER; e: xpr.EXPR);
  VAR i,j: INTEGER; d: FDSC;
BEGIN
  WITH wnd[n] DO
    FOR i:=all-1 TO 0 BY -1 DO
      IF NOT filter_match(e,n,i) THEN
        d:=fdsc[i]; d.mode:=d.mode+BITSET(i*2);
        FOR  j:=i TO numb-2 DO fdsc[j]:=fdsc[j+1] END;
        fdsc[numb-1]:=d; DEC(numb);
        IF pos>i THEN DEC(pos) END;
      END;
    END;
  END;
END try_filter;

PROCEDURE check_marked(n: INTEGER);
  VAR i: INTEGER; f: bio.FILE;
BEGIN
  WITH wnd[n] DO
   pos:=min(pos,numb-1); bytes:=0; marked:=1;
   FOR i:=0 TO numb-1 DO
     WITH fdsc[i] DO
       IF mode#{} THEN INC(marked); INC(bytes,eof) END;
     END;
   END;
  END;
END check_marked;

PROCEDURE refilter(n: INTEGER);
  VAR e: xpr.EXPR;
BEGIN
  scn.delete(Timer);
  CASE filter[n].cur OF
    0: tmp:='*'
   |1: tmp:='*.cod|*.@'
   |2: tmp:=filter[n].alt[3]
  ELSE ASSERT(FALSE)
  END;
  IF tmp#'*'0c THEN
    xpr.compile(tmp,e);
    IF NOT xpr.done THEN
      warning('Wrong pattern:',tmp,"Can't refilter."); RETURN
    ELSE
      reorder_all(n); try_filter(n,e); xpr.dispose(e)
    END;
  ELSE
    reorder_all(n)
  END;
  check_marked(n)
END refilter;

--------------------------- READ DIR ---------------------------
                           ----------

PROCEDURE iter_dir(n: INTEGER);
  VAR f: bio.FILE; d: FDSC;

  PROCEDURE find(VAR i: INTEGER; from,to: INTEGER): BOOLEAN;
    VAR how: INTEGER; mid: INTEGER;
  BEGIN
    IF to<from THEN i:=from; RETURN FALSE END;
    mid:=(from+to+1) DIV 2;
    how:=dir_comp(n,mid,wnd[n].numb);
    IF    how=0 THEN i:=mid; RETURN TRUE
    ELSIF how<0 THEN
      IF from=to THEN i:=to+1; RETURN FALSE
      ELSE RETURN find(i,mid+1,to)
      END;
    ELSE
      IF from=to THEN i:=from; RETURN FALSE
      ELSE RETURN find(i,from,mid-1)
      END;
    END;
  END find;

  PROCEDURE push(from: INTEGER);
    VAR i: INTEGER;
  BEGIN
    WITH wnd[n] DO
      FOR i:=numb-1 TO from BY -1 DO fdsc[i+1]:=fdsc[i] END;
      fdsc[from]:=d; INC(numb);
      IF pos>=from THEN INC(pos) END;
    END;
  END push;

  PROCEDURE insert;
    VAR i,j: INTEGER;
  BEGIN
    WITH wnd[n] DO
      fdsc[numb]:=d;
      IF NOT find(i,0,numb-1) THEN
        IF numb>=HIGH(fdsc) THEN RESIZE(fdsc,numb+16) END; push(i)
      ELSE
        WITH fdsc[i] DO
           INCL(mode,1);
           IF (type=FULL)OR(s_kind=Tim)OR(s_kind=Siz) THEN
             time:=d.time; eof:=d.eof;
           END;
           ftype:=d.ftype
        END;
      END;
    END;
  END insert;

  PROCEDURE delete;
    VAR i,j: INTEGER;
  BEGIN
    WITH wnd[n] DO
      i:=0; j:=0;
      FOR i:=0 TO numb-1 DO
        IF fdsc[i].mode*{1}={} THEN
          INC(j); DEC(numb);
          IF i<pos THEN DEC(pos) END;
        ELSE
          fdsc[i].mode:=fdsc[i].mode/{1};
          IF j#0 THEN fdsc[i-j]:=fdsc[i] END;
        END;
      END;
      all:=numb;
    END;
  END delete;

BEGIN
  scn.delete(Timer);
  WITH wnd[n] DO
    IF numb=0 THEN
      marked:=0; bytes:=0; all:=0;
      oldpos:=0; first:=0; pos:=-1;
    END;
    bio.open(file,path,'r');
    IF NOT bio.done THEN
      lex.perror(tmp,bio.error,'%%s "%s"',bio.ename);
      warning("Can't read the directory:",path,tmp);
      WITH fdsc[0] DO
        name:='..'; fill(name);
        mode:={}; ftype:=bio.e_dir;
        numb:=1; all:=1;
      END;
    ELSE
      reorder_all(n);
      bio.get_attr(file,bio.a_wtime,time); chdir(path);
      bio.dir_walk(bio.cd,bio.s_none);
      LOOP
        IF bio.get_entry(bio.cd,d.name,d.ftype) THEN
          IF hid OR is_dir(d.ftype) OR (d.ftype*bio.e_hidden={}) THEN
            IF (type=FULL)OR(s_kind=Tim)OR(s_kind=Siz) THEN
              bio.open(f,d.name,'');
              IF bio.done THEN
                d.eof:=bio.eof(f);
                IF NOT bio.done THEN d.eof:=-1 END;
                bio.get_attr(f,bio.a_wtime,d.time);
                IF NOT bio.done THEN d.time:=0 END;
                bio.close(f);
              ELSE d.eof:=-1; d.time:=0;
              END;
            END;
            fill(d.name); d.mode:={1};
            insert
          END;
        ELSE EXIT
        END;
      END;
      bio.end_walk(bio.cd); delete;
      IF HIGH(fdsc)>numb THEN RESIZE(fdsc,numb+1) END;
    END;
    refilter(n)
  END;
END iter_dir;

----------------------------- SHOW -----------------------------
                             ------

PROCEDURE stl(n,i: INTEGER; full: BOOLEAN); (* show tree line *)
  VAR j,spa,len,lno: INTEGER;
BEGIN
  setcbr(yellow,dark_blue,0);
  WITH wnd[n] DO
    IF (i-first>h+1) THEN RETURN END;
    lno:=2+i-first;
    IF i>=numb THEN
      scn.set_pos(scr,lno,1);
      scn.repeat(scr,' ',w-2); RETURN
    END;
    spa:=max(0,(w-12) DIV 2);
    IF full THEN
      scn.set_pos(scr,lno,1);
      scn.repeat(scr,' ',spa)
    ELSE
      scn.set_pos(scr,lno,spa+1)
    END;
    WITH fdsc[i] DO
      IF i#0 THEN
        FOR j:=1 TO time-1 DO
          scn.Write(scr,' ');
          IF j IN ftype THEN scn.Write(scr,vb) ELSE scn.Write(scr,' ')
          END;
          scn.Write(scr,' ')
        END;
        scn.Write(scr,' ');
        IF time IN ftype
        THEN scn.Write(scr,lr)
        ELSE scn.Write(scr,dl)
        END;
        scn.repeat(scr,hb,3)
      END;
      IF (i=pos)&(n=active) THEN setcr(red,1) ELSE setcr(yellow,0) END;
      len:=str.len(name); spa:=w-spa-time*5-2;
      scn.write(scr,name,0,min(len,spa));
      spa:=spa-len;
      IF NOT full THEN spa:=min(spa,32-len) END;
      setcr(yellow,0); scn.repeat(scr,' ',spa)
    END
  END
END stl;

PROCEDURE sdl(n,i: INTEGER);

  PROCEDURE write_brief;
  BEGIN
    WITH wnd[n] DO
      WITH fdsc[i] DO
        IF is_dir(ftype) THEN
          scn.write(scr,name,0,min(30,fw-2));
          scn.Write(scr,'/')
        ELSE
          scn.write(scr,name,0,min(31,fw-1))
        END;
      END;
    END;
  END write_brief;

  PROCEDURE write_full;
     VAR y,m,d,hh,mm,ss: INTEGER;
  BEGIN
    WITH wnd[n].fdsc[i] DO
      tmp:=name;
      IF is_dir(ftype) THEN tmp[12]:='/' END;
      tmp[13]:=0c;
      tim.unpack(time,y,m,d,hh,mm,ss);
      str.print(dst,'%c%2d.%$2d%c%2d-%$2d-%$2d%c%6d',vb,d,m,vb,hh,mm,ss,vb,eof);
      str.app(tmp,dst);
      scn.write(wnd[n].scr,tmp,0,min(wnd[n].w-2,str.len(tmp)))
    END;
  END write_full;

  PROCEDURE write;
  BEGIN
    IF wnd[n].type=BRIEF THEN write_brief ELSE write_full END
  END write;

  PROCEDURE empty;
  BEGIN
    IF wnd[n].type=BRIEF THEN scn.repeat(wnd[n].scr,' ',wnd[n].fw-1)
    ELSE
      str.print(tmp,'%.13c%c%.5c%c%.8c%c%.6c',' ',vb,' ',vb,' ',vb,' ');
      scn.write(wnd[n].scr,tmp,0,min(wnd[n].w-2,str.len(tmp)))
    END;
  END empty;

BEGIN
  setcbr(violet,dark_blue,0);
  WITH wnd[n] DO
    scn.set_pos(scr,(i-first) MOD h+1,1+fw*((i-first) DIV h));
    IF i<numb THEN
      WITH fdsc[i] DO
        IF (i=pos)&(n=active) THEN
          IF mac-mic<7 THEN
            scn.set_reverse(1);
            IF mode={} THEN scn.set_color(red) END;
          ELSE
            set_back(red)
          END;
          IF mode#{} THEN
            scn.set_color(yellow);
            IF mac-mic=1 THEN
              scn.set_back(mac); write; scn.set_back(mic)
            ELSE  write
            END;
            scn.set_color(violet)
          ELSE write
          END;
          scn.set_reverse(0); set_back(dark_blue);
        ELSIF mode#{} THEN
          scn.set_color(yellow);
          IF mac-mic=1 THEN
            scn.set_back(mac); write; scn.set_back(mic)
          ELSE  write
          END;
          scn.set_color(violet)
        ELSE write
        END;
      END;
    ELSE
      empty
    END;
  END;
END sdl;

PROCEDURE show_line(i,p: INTEGER);
BEGIN
  CASE wnd[i].type OF
    BRIEF: sdl(i,p)|
    FULL : sdl(i,p)|
    TREE : stl(i,p,FALSE)
  ELSE
  END;
END show_line;

PROCEDURE show_tree(n: INTEGER; r: BOOLEAN);
  VAR i,j,M,D: INTEGER;
BEGIN
  WITH wnd[n] DO
    IF NOT r & (pos=oldpos) THEN RETURN END;
    i:=first; M:=h DIV 2; D:=h DIV 4;
    IF pos<first THEN first:=max(0,pos-M)
    ELSIF pos>=first+h-1 THEN first:=min(pos-M,numb-h+1)
    ELSIF pos<first+1 THEN first:=max(0,pos-D)
    ELSIF pos>first+h-3 THEN first:=min(first+D,numb-h+1)
    END;
    IF NOT r & (i=first) THEN stl(n,pos,r); stl(n,oldpos,r)
    ELSE
      tmp:=path;    splitpathname(tmp,src);
      IF pos#0 THEN splitpathname(tmp,src) END;
      setcr(violet,0); set_back(dark_blue);
      scn.set_pos(scr,1,1); scn.WriteString(scr,tmp);
      scn.repeat(scr,' ',w-str.len(tmp)-2);
      j:=first+h-2;
      IF NOT r THEN j:=min(numb-1,j) END;
      FOR i:=first TO j DO stl(n,i,r) END
    END
  END;
END show_tree;

PROCEDURE show_marked(n: INTEGER; r: BOOLEAN);
  VAR i: INTEGER;
BEGIN
  WITH wnd[n] DO
    scn.set_pos(scr,h+1,1); setcbr(blue,yellow,1);
    i:=ABS(marked);
    IF i#1 THEN
      IF (marked<0) OR r THEN
        str.print(tmp,'%7d bytes in %3d file',bytes,i-1);
        IF sm(i)
        THEN str.app(tmp,'.   ')
        ELSE str.app(tmp,'s.  ')
        END;
        scn.write(scr,tmp,0,min(w-2,29))
      END;
    ELSE
      tmp:=fdsc[pos].name; scn.write(scr,tmp,0,min(w-2,29))
    END;
    marked:=i;
  END;
END show_marked;

PROCEDURE show_dir(n: INTEGER; r: BOOLEAN);
  VAR i,j,f: INTEGER;
BEGIN
  WITH wnd[n] DO
    pos:=max(0,min(pos,numb-1));
    IF type=FULL THEN f:=1 ELSE f:=fields END;
    IF r THEN header(n)
    ELSIF (pos=oldpos) & (marked>0) THEN RETURN
    END;
    show_marked(n,r);
    first:=min(first,( ((numb-1) DIV h) +1-f)*h);
    first:=max(0,first);
    IF first#oldf THEN r:=TRUE END;
    IF    pos<first THEN first:=max(0,pos-(h DIV 2))
    ELSIF pos>=first+f*h THEN
      first:=min(numb-f*h,pos-f*h+1+(h DIV 2))
    ELSE
      IF NOT r THEN
        sdl(n,pos); sdl(n,oldpos); oldpos:=pos; RETURN
      END;
    END;
    oldpos:=pos; j:=first+f*h-1;
    IF NOT r THEN j:=min(numb-1,j) END;
    FOR i:=first TO j DO sdl(n,i)  END
  END
END show_dir;

PROCEDURE correct_path;
BEGIN
  WITH wnd[active] DO
    IF pos=0 THEN splitpathname(path,tmp)
    ELSE
      src:=path;
      IF oldpos#0 THEN
        splitpathname(src,tmp)
      END;
      IF src#'/'0c THEN
        str.print(path,'%s/%s',src,fdsc[pos].name)
      ELSE
        str.print(path,'/%s',fdsc[pos].name)
      END;
    END;
  END;
END correct_path;

PROCEDURE tree_cd;
BEGIN
  WITH wnd[active] DO
    chdir(path); env.put_str('CD',path,FALSE)
  END;
END tree_cd;

PROCEDURE refresh_dir;
  VAR n: INTEGER;
BEGIN
  n:=(active+1) MOD 2;
  WITH wnd[n] DO
    IF wnd_is_dir(n) & (path#wnd[active].path) THEN
      path:=wnd[active].path;
      bio.close(file);
      numb:=0; iter_dir(n); header(n); show_dir(n,TRUE)
    END;
  END;
END refresh_dir;

PROCEDURE show_info(n: INTEGER; r: BOOLEAN);
  VAR i,j,free,used: INTEGER;
BEGIN
  WITH wnd[n] DO
    setcbr(violet,black,0);
    FOR j:=1 TO fields-1 DO
      scn.set_pos(scr,h+1,fw*j);
      scn.Write(scr,hb)
    END;
    set_back(dark_blue);
    IF r THEN
      FOR i:=1 TO h DO
        scn.set_pos(scr,i,1); scn.repeat(scr,' ',w-2)
      END;
    END;
    bio.du(bio.cd,free,used);
    free:=free DIV 1024;
    used:=used DIV 1024;
    tmp:='The Kronos  Commander (V 1.0)';
    j:=str.len(tmp);
    scn.set_pos(scr,h DIV 2,1+((w-j-2) DIV 2)); scn.write(scr,tmp,0,j);
    str.print(tmp,'At "." %d,%$3d kB free',free DIV 1000, free MOD 1000);
    j:=str.len(tmp);
    scn.set_pos(scr,h DIV 2+1,1+((w-j-2) DIV 2)); scn.write(scr,tmp,0,j);
    str.print(tmp,'Total: %d,%$3d kB',(free+used) DIV 1000,
                                      (free+used) MOD 1000);
    j:=str.len(tmp);
    scn.set_pos(scr,h DIV 2+2,1+((w-j-2) DIV 2)); scn.write(scr,tmp,0,j);
  END;
END show_info;

PROCEDURE show(i: INTEGER; r: BOOLEAN);
BEGIN
  CASE wnd[i].type OF
    BRIEF,
    FULL: show_dir(i,r)|
    INFO: show_info(i,r)|
    TREE: show_tree(i,r)
  ELSE
    ASSERT(FALSE)
  END;
END show;

PROCEDURE ref_screen(all: BOOLEAN);
  VAR s: STRING;
BEGIN
  env.get_str('CD',s); str.copy(tmp,s);
  WITH wnd[active] DO
    IF tmp#path THEN
      scn.off(scr); str.copy(path,tmp);
      IF (type=BRIEF) OR (type=FULL) THEN
        bio.close(file); numb:=0; iter_dir(active)
      ELSIF type=TREE THEN recreate_short(active,TRUE)
      END;
      show(active,TRUE); scn.on(scr)
    END;
  END;
--  scn.refresh_all
  ttycbr(noc,black,0);
  IF all THEN tty.erase(2)
  ELSE
    tty.set_pos(2+min(wnd[0].h,wnd[1].h),tty.state^.columns-1); tty.erase(1);
  END;
  bars_menu(Digits);
  scn.refresh(wnd[0].scr);
  scn.refresh(wnd[1].scr);
  scn.refresh(Timer);
END ref_screen;

PROCEDURE print_prompt(VAR l: INTEGER);
BEGIN
  com.get_prompt(tmp,shdsc^.ins,shdsc^.bel);
  tty.bottom; ttycbr(violet,black,1);
  tty.write('KC',0,2);
  tty.set_reverse(0); tty.print(' %s',tmp);
  l:=3+str.len(tmp)
END print_prompt;

PROCEDURE rd_buf(VAR ch: CHAR);
BEGIN
  ch:=buf_ch; shdsc^.read:=key.read
END rd_buf;

PROCEDURE shell(ch: CHAR);
  VAR echo: ARRAY [0..7] OF CHAR;
       len: INTEGER;
BEGIN
  scn.delete(Timer);
  IF cmd_str#'' THEN shdsc^.how:=sed.show END;
  shdsc^.read:=rd_buf; buf_ch:=ch;
  LOOP
    print_prompt(len);
    sed.edit_str('',cmd_str,tty.state^.lines-1,len,L,shdsc,033c);
    shdsc^.how:=sed.confirm;
    IF  shdsc^.last=033c THEN
      scn.set_cursor(0); scn.set_pos(TTY,0,0);
      ref_screen(FALSE); cmd_str:=''; RETURN
    ELSE
      tty.WriteLn; key.set_break(1);
      com.get_echo(echo); com.system(cmd_str,echo);
--      com.system(cmd,TRUE);
      tty.WriteLn; key.set_break(0)
    END;
    cmd_str:=''
  END;
END shell;

PROCEDURE show_cmd;
  VAR i: INTEGER;
BEGIN
  print_prompt(i);
  tty.write(cmd_str,0,min(str.len(cmd_str),tty.state^.columns-i-1))
END show_cmd;

PROCEDURE exec(s: ARRAY OF CHAR);
  VAR echo: ARRAY [0..7] OF CHAR;
       len: INTEGER;
BEGIN
  scn.delete(Timer);
  key.set_break(1); print_prompt(len); tty.print(s); tty.WriteLn;
  com.get_echo(echo); com.system(s,echo);
--  com.system(s,TRUE)
  key.set_break(0); ref_screen(FALSE)
END exec;

---------------------------- ACTION ----------------------------
                            --------

PROCEDURE get_next(VAR i: INTEGER; VAR s: ARRAY OF CHAR): BOOLEAN;
BEGIN
  WITH wnd[active] DO
    INC(i);
    IF i>=numb THEN RETURN FALSE
    ELSIF marked=1 THEN
      IF i>pos THEN RETURN FALSE
      ELSE i:=pos
      END;
    ELSE
      WHILE (i<numb)&(fdsc[i].mode={}) DO INC(i) END;
      IF i>=numb THEN RETURN FALSE END;
    END;
    cut(fdsc[i].name,s)
  END;
  RETURN TRUE
END get_next;

CONST patt = ' PU RWX RWX RWX ';

PROCEDURE vis_cmask(m: BITSET; VAR s: ARRAY OF CHAR);
  VAR i,j,k: INTEGER;
BEGIN
  s:=patt;
  IF NOT(14 IN m) THEN s[1]:=hb END;
  IF NOT(15 IN m) THEN s[2]:=hb END;
  FOR i:=0 TO 8 DO
    k:=(i DIV 3)*4+(i MOD 3)+4;
    j:=(i DIV 3)*4+(2-(i MOD 3));
    IF j IN m THEN s[k]:=hb END
  END;
  s[16]:=0c
END vis_cmask;

PROCEDURE get_user(VAR U: usr.USER);
BEGIN
  usr.unpack(usr.user(),U.usr,U.gro,U.priv); usr.get_user(U)
END get_user;

PROCEDURE find_user(VAR U: usr.USER): BOOLEAN;
BEGIN
  usr.find(U);
  IF NOT U.done THEN
    warning("Wrong user name:",U.name,'');
    RETURN FALSE
  ELSE
    usr.get_user(U); RETURN TRUE
  END;
END find_user;

PROCEDURE mk_cmask(VAL s: ARRAY OF CHAR; VAR m: BITSET);
  VAR i,j,k: INTEGER;
BEGIN
  m:={};
  IF s[1]#hb THEN INCL(m,14) END;
  IF s[2]#hb THEN INCL(m,15) END;
  FOR i:=0 TO 8 DO
    k:=(i DIV 3)*4+(i MOD 3)+4;
    j:=(i DIV 3)*4+(2-(i MOD 3));
    IF s[k]=hb THEN INCL(m,j) END
  END;
END mk_cmask;

PROCEDURE attrs;
  VAR ch: CHAR; a_scr: SCREEN; m: BITSET;
    x,y,w,i,pos: INTEGER; U: usr.USER;

  PROCEDURE ed_cmask(VAR m: BITSET);
    VAR i,j,k: INTEGER;

    PROCEDURE sh(i: INTEGER);
    BEGIN
      vis_cmask(m,tmp);
      scn.set_pos(a_scr,6,12+i);
      scn.Write(a_scr,tmp[i])
    END sh;

    PROCEDURE pos_to_bit(i: INTEGER; VAR j: INTEGER);
    BEGIN
      IF i=1 THEN j:=14; RETURN END;
      IF i=2 THEN j:=15; RETURN END;
      j:=((i-4) DIV 4)*4+2-(i MOD 4)
    END pos_to_bit;

    PROCEDURE inc(VAR i: INTEGER);
    BEGIN
      i:=(i+1) MOD HIGH(patt);
      IF patt[i]=' ' THEN inc(i) END;
    END inc;

    PROCEDURE dec(VAR i: INTEGER);
    BEGIN
      i:=(i-1+HIGH(patt)) MOD HIGH(patt);
      IF patt[i]=' ' THEN dec(i) END;
    END dec;

  BEGIN
    i:=1;
    LOOP
      setcbr(mac,black,1); sh(i);
      spy(ch); k:=i;
      IF ch=' ' THEN
        pos_to_bit(i,j);
        IF j IN m THEN EXCL(m,j) ELSE INCL(m,j) END;
        inc(i);
      ELSIF ch=key.right THEN inc(i)
      ELSIF ch=key.left  THEN dec(i)
      END;
      setcbr(red,noc,1); sh(k);
      IF (ch=033c) OR (ch=key.cr)
      OR(ch=key.up)OR (ch=key.dw) THEN RETURN
      END;
    END;
  END ed_cmask;

VAR f: bio.FILE;

BEGIN
  IF wnd[active].type=INFO THEN RETURN END;
  IF mac-mic<7 THEN setcbr(noc,      blue,0)
  ELSE              setcbr(dark_blue,blue,0)
  END;
  w:=max(str.len(wnd[active].path)+7, 32);
  x:=(L-w) DIV 2;
  y:=(LINES-1) DIV 2 -6;
  scn.create(a_scr,y,x,10,w);
  scn.frame(a_scr);
  scn.center(a_scr,'Change attributes of',1);
  WITH wnd[active] DO
    IF marked<=2 THEN
      i:=-1; IF get_next(i,src) THEN END;
      IF (type=TREE) OR is_dir(fdsc[i].ftype) THEN
        str.print(tmp,'the directory "%s"',src)
      ELSE
        str.print(tmp,'"%s"',src)
      END;
    ELSE
      str.print(tmp,'%d files',marked-1);
      IF sm(marked) THEN tmp[str.len(tmp)-1]:=0c END;
    END;
    scn.center(a_scr,tmp,2);
    str.copy(src,path);
    IF type=TREE THEN splitpathname(src,tmp) END;
    str.print(tmp,'at "%s"',src);
    scn.center(a_scr,tmp,3);
  END;
  scn.set_pos(a_scr,5,2); scn.print(a_scr,"OWNER : ");
  scn.set_pos(a_scr,6,2); scn.print(a_scr,"ACCESS: ");
  m:=bio.cmask; setcbr(red,noc,1); vis_cmask(m,tmp);
  scn.set_pos(a_scr,6,12); scn.print(a_scr,tmp);
  scn.on_top(a_scr); get_user(U); pos:=0;
  LOOP
    IF pos=0 THEN
      ttycbr(mac,black,1);
      edit_str(U.name,y+5,x+12,x+w-5,dsc);
      scn.set_cursor(0);
      IF dsc^.last=033c THEN scn.kill(a_scr); RETURN
      ELSIF find_user(U) THEN
        IF dsc^.last=key.cr THEN EXIT
        ELSE pos:=1
        END;
      END;
    ELSE ed_cmask(m);
      IF ch=033c THEN scn.kill(a_scr); RETURN
      ELSIF ch=key.cr THEN EXIT
      ELSE pos:=0
      END;
    END;
  END;
  i:=-1;
  WHILE get_next(i,src) DO
    IF wnd[active].type=TREE THEN  bio.open(f,wnd[active].path,'');
    ELSE  bio.open(f,src,'')
    END;
    IF NOT bio.done THEN
      warning("Can't open",src,bio.ename)
    ELSE
      bio.chaccess(f,m);
      IF NOT bio.done THEN
        vis_cmask(m,dst);
        lex.perror(tmp,bio.error,'%%s "%s"',bio.ename);
        warning("Can't change cmask to",dst,tmp)
      END;
      bio.chowner(f,U.usr,U.gro);
      IF NOT bio.done THEN
        lex.perror(tmp,bio.error,'%%s "%s"',bio.ename);
        warning("Can't change owner to",U.name,tmp)
      END;
    END;
    bio.close(f)
  END;
  scn.kill(a_scr)
END attrs;

PROCEDURE swap_wnd;
  VAR tmp: WINDOW;
        i: INTEGER;
BEGIN
  FOR i:=0 TO 1 DO scn.move(wnd[i].scr,1,(1-i)*(L-wnd[i].w-2)) END;
  tmp:=wnd[0]; wnd[0]:=wnd[1]; wnd[1]:=tmp; active:=passive()
END swap_wnd;

PROCEDURE get_attrs(n: INTEGER);
  VAR d,f: bio.FILE; i: INTEGER;
BEGIN
  scn.delete(Timer);
  WITH wnd[n] DO
    bio.open(d,path,'r');
    IF NOT bio.done THEN
      FOR i:=0 TO all-1 DO fdsc[i].time:=0; fdsc[i].eof:=-1 END;
    ELSE
      FOR i:=0 TO all-1 DO
        WITH fdsc[i] DO
          cut(name,tmp); bio.fopen(d,f,tmp,'');
          IF NOT bio.done THEN time:=0; eof:=-1
          ELSE
            bio.get_attr(f,bio.a_wtime,time); eof:=bio.eof(f); bio.close(f)
          END;
        END;
      END;
      bio.close(d);
    END;
  END;
END get_attrs;

PROCEDURE resort(n: INTEGER; t: SORT);
  VAR m: NAME; i: INTEGER;
BEGIN
  IF t=wnd[n].s_kind THEN RETURN END;
  CASE t OF
     Nam,Ext:
    |Tim,Siz: IF (wnd[n].type=BRIEF)&(wnd[n].s_kind<Tim) THEN get_attrs(n) END;
    |Uns    : RETURN
             (*
              wnd[n].s_kind:=Uns; wnd[n].numb:=0; m:=wnd[n].fdsc[pos].name;
              bio.close(wnd[n].file); iter_dir(n); refilter(n);
              FOR i:=0 TO numb-1 DO
                IF wnd[n].fdsc[i].name=m THEN wnd[n].pos:=i RETURN END;
              END;
              RETURN
              *)
  END;
  wnd[n].s_kind:=t; wnd[n].numb:=wnd[n].all;
  quick(n,0,wnd[n].all-1,dir_comp); refilter(n);
END resort;

PROCEDURE cd_active;
BEGIN
  IF select_target(tmp,active) & (tmp#wnd[active].path) THEN
    env.put_str('CD',tmp,FALSE);
    IF wnd[active].type#INFO THEN bio.close(wnd[active].file) END;
    new(active,TRUE); show(active,TRUE)
  END;
END cd_active;

PROCEDURE ext_edit;
  VAR p,a: INTEGER;
      i  : INTEGER;
      d,h: INTEGER;
      s  : SCREEN;
      ch : CHAR;
     LAST: INTEGER;

  PROCEDURE ref_line(pos,alt: INTEGER);
    VAR i,j: INTEGER;
  BEGIN
    setcbr(red,mic,1);
    scn.set_pos(s,pos+3,2+alt*14); i:=12+9*alt;
    IF pos>HIGH(e_menu) THEN j:=0
    ELSIF alt=0 THEN
      j:=min(12,str.len(e_menu[pos].ext)); scn.write(s,e_menu[pos].ext,0,j)
    ELSE
      j:=min(21,str.len(e_menu[pos].act)); scn.write(s,e_menu[pos].act,0,j)
    END;
    scn.repeat(s,' ',i-j)
  END ref_line;

  PROCEDURE ref_set(a,b: INTEGER);
    VAR i: INTEGER;
  BEGIN
    FOR i:=a TO b DO ref_line(i,0); ref_line(i,1) END;
  END ref_set;

  PROCEDURE del(i: INTEGER);
    VAR j: INTEGER;
  BEGIN
    IF p>i THEN DEC(p) END;
    DISPOSE(e_menu[i].ext);
    DISPOSE(e_menu[i].act);
    FOR j:=i TO h-1 DO
      e_menu[j]:=e_menu[j+1]
    END;
    RESIZE(e_menu,h);
    ref_set(i,h); DEC(h)
  END del;

BEGIN
  LAST:=LINES-6; setcbr(red,mic,1);
  scn.create(s,0,0,LINES,40); scn.frame(s);
  scn.center(s,'EXTENTIONS:      FUNCTIONS:          ',1);
  scn.center(s,'( F1 - Delete Line )',LINES-2);
  ref_set(0,HIGH(e_menu));
  scn.on_top(s);
  p:=0; a:=0;
  scn.set_cursor(1);
  LOOP
    IF p>HIGH(e_menu) THEN tmp:=''
    ELSIF a=0 THEN str.copy(tmp,e_menu[p].ext)
    ELSE           str.copy(tmp,e_menu[p].act)
    END;
    ttycbr(noc,mic,0);
    sed.edit_str('',tmp,p+3,2+14*a,13+23*a,dsc
                   ,key.up  ,key.dw  ,key.tab ,033c
                   ,key.pgup,key.pgdw,key.home,key.end
                   ,key.f1);
    IF dsc^.last#033c THEN
      sup(tmp,dst); i:=str.len(dst);
      IF (p>HIGH(e_menu))&(i>0) THEN
        RESIZE(e_menu,p+1);
        NEW(e_menu[p].ext,08); str.copy(e_menu[p].ext,'');
        NEW(e_menu[p].act,12); str.copy(e_menu[p].act,'');
      END;
      IF p<=HIGH(e_menu) THEN
        IF a=0 THEN
          IF i>HIGH(e_menu[p].ext) THEN
            DISPOSE(e_menu[p].ext); NEW(e_menu[p].ext,i+1)
          END;
          str.copy(e_menu[p].ext,dst)
        ELSE
          IF i>HIGH(e_menu[p].act) THEN
            DISPOSE(e_menu[p].act); NEW(e_menu[p].act,i+1)
          END;
          str.copy(e_menu[p].act,dst)
        END;
      END;
    ELSE EXIT
    END;
    ref_line(p,a);
    i:=p; h:=HIGH(e_menu); IF h<0 THEN d:=0 ELSE d:=h END;
    CASE dsc^.last OF
       key.up  : p:=(p+d) MOD (d+1)
      |key.dw  : p:=(p+1) MOD (h+2)
      |key.home: p:=0
      |key.end : p:=h
      |key.pgup: p:=(p-(d DIV 3)+h) MOD (d+1)
      |key.pgdw: p:=(p+(d DIV 3)+1) MOD (d+1)
      |key.tab : a:=(a+1) MOD 2;
      |key.f1  : IF p<=h THEN del(p) END;
      |key.cr  : EXIT
    ELSE
    END;
    p:=p MOD (LAST+1);
    IF i<=h THEN
      WITH e_menu[i] DO
        IF (ext='')&(act='') THEN del(i) END;
      END;
    END;
  END;
  scn.set_cursor(0);
  scn.kill(s)
END ext_edit;

PROCEDURE m_line(ch: CHAR; VAR pos: INTEGER; s: ARRAY OF CHAR): BOOLEAN;
  VAR i,p,j,n: INTEGER;

  PROCEDURE right_delim(i: INTEGER): BOOLEAN;
  BEGIN
    RETURN (s[i]='[') OR (s[i]='<')
  END right_delim;

  PROCEDURE left_delim(i: INTEGER): BOOLEAN;
  BEGIN
    RETURN (s[i]=']') OR (s[i]='>')
  END left_delim;

  PROCEDURE next(VAR i: INTEGER): BOOLEAN;
  BEGIN
    WHILE i<=HIGH(s) DO
      IF right_delim(i) THEN RETURN TRUE ELSE INC(i) END;
    END;
    RETURN FALSE
  END next;

  PROCEDURE hot(ch: CHAR; i: INTEGER): BOOLEAN;
  BEGIN
    REPEAT INC(i) UNTIL s[i]#' ';
    WHILE NOT left_delim(i) DO
      IF s[i]=ch THEN RETURN TRUE ELSE INC(i) END;
    END;
    RETURN FALSE
  END hot;

BEGIN
  n:=0; ch:=ASCII.CAPITAL(ch);
  IF ch>' ' THEN i:=0; p:=0;
    LOOP
      IF next(i) THEN
        IF hot(ch,i) THEN pos:=p; EXIT ELSE INC(p); INC(i) END;
      ELSE RETURN FALSE
      END;
    END;
  END;
  setcbr(red,black,0);
  i:=0; p:=0;
  WHILE next(i) DO
    FOR j:=0 TO HIGH(s) DO
      IF right_delim(j) THEN INC(n) END;
    END;
    pos:=(pos+n) MOD n;
    IF s[i]='<' THEN INC(i) END;
    j:=i; REPEAT INC(j) UNTIL left_delim(j);
    IF s[j]='>' THEN DEC(j) END;
    scn.set_pos(Lmenu,0,1+i); scn.set_reverse(INTEGER(pos=p));
    scn.write  (Lmenu,s,i,j-i+1); INC(p)
  END;
  RETURN TRUE
END m_line;

PROCEDURE wnd_menu;
  VAR ch    : CHAR;
      pos, w, old,
      alt, n: INTEGER;
      inside: BOOLEAN;

  PROCEDURE menu_line(ch: CHAR): BOOLEAN;
  BEGIN
    RETURN m_line(ch,pos,
    ' < Left >   < Files >  <Commands>  < Options >   < Right >')
  END menu_line;

  PROCEDURE v_menu(m: V_MENU): BOOLEAN;
    VAR i,j,n,old: INTEGER; ch: CHAR;

    PROCEDURE sh_alt(i: INTEGER);
    BEGIN
      scn.set_color(mac); scn.set_pos(m.scr,i+1,2);
      IF i#alt THEN scn.set_reverse(1) ELSE scn.set_reverse(0) END;
      scn.write(m.scr,m.alts[i],1,str.len(m.alts[i])-1)
    END sh_alt;

    PROCEDURE hot(ch: CHAR): BOOLEAN;
      VAR i,j,l: INTEGER; kind: BITSET;
    BEGIN
      ch:=ASCII.CAPITAL(ch); kind:=ASCII.KIND(ch);
      IF (ASCII.latin IN kind) OR (ASCII.cyril IN kind) THEN
        FOR i:=0 TO HIGH(m.alts) DO
          j:=1; l:=str.len(m.alts[i])-1;
          REPEAT INC(j);
            IF ch=m.alts[i,j] THEN alt:=i; RETURN TRUE END;
          UNTIL (j>=l) OR ((m.alts[i,j]=' ')&(m.alts[i,j+1]=' '));
        END;
      END;
      RETURN FALSE
    END hot;

    PROCEDURE ref;
     VAR i: INTEGER;
    BEGIN
      i:=alt; alt:=-1; sh_alt(i); alt:=i; scn.set_color(noc)
    END ref;

    PROCEDURE change;
      VAR i: INTEGER; c0,c1: CHAR;

      PROCEDURE var(i: INTEGER): BOOLEAN;
      BEGIN
       RETURN (m.alts[i,0]=' ')
      END var;

      PROCEDURE off(i: INTEGER);
      BEGIN
        IF m.alts[i,1]='+' THEN
          m.alts[i,1]:=' '; scn.set_pos(m.scr,i+1,2); scn.Write(m.scr,' ')
        END
      END off;

      PROCEDURE onoff(i: INTEGER);
        VAR ch: CHAR;
      BEGIN
        IF m.alts[i,1]='+' THEN ch:=' ' ELSE ch:='+' END;
        m.alts[i,1]:=ch; scn.set_pos(m.scr,i+1,2); scn.Write(m.scr,ch)
      END onoff;

    BEGIN
      c0:=m.alts[alt,0]; c1:=m.alts[alt,1];
      IF    c0='.' THEN RETURN
      ELSIF c0='*' THEN onoff(alt); RETURN
      END;
      IF c1#'+' THEN onoff(alt);
        i:=alt-1; WHILE (i>=0)           & var(i) DO off(i); DEC(i) END;
        i:=alt+1; WHILE (i<=HIGH(m.alts))& var(i) DO off(i); INC(i) END
      END;
    END change;

  BEGIN
    alt:=0; old:=0; ch:=' ';
    n:=HIGH(m.alts)+1;
    LOOP
      alt:=(alt+n) MOD n;
      IF m.alts[alt]='' THEN
        IF ch=key.up THEN DEC(alt) ELSE INC(alt) END;
      END;
      sh_alt(alt); sh_alt(old);
      old:=alt;
      LOOP
        spy(ch);
        CASE ch OF
             033c   :         ref; RETURN FALSE
          |key.right: inside:=TRUE; INC(pos); ref; RETURN FALSE
          |key.left : inside:=TRUE; DEC(pos); ref; RETURN FALSE
          |key.dw   : INC(alt); EXIT
          |key.up   : DEC(alt); EXIT
        ELSE
          IF ch=key.cr  THEN change; ref;         RETURN TRUE
          ELSIF hot(ch) THEN change; sh_alt(old); RETURN TRUE
          END;
        END;
      END;
    END;
  END v_menu;

  PROCEDURE on_top;
  BEGIN
    scn.on_top(vm[pos].scr)
  END on_top;

  PROCEDURE del;
  BEGIN
    scn.delete(vm[old].scr)
  END del;

  PROCEDURE kil;
  BEGIN
    scn.delete(Lmenu); setcr(noc,0)
  END kil;

BEGIN
  w:=12; inside:=FALSE;
  pos:=-1;       IF menu_line(0c) THEN END;
  pos:=active*4; IF menu_line(0c) THEN END;
  scn.on_top(Lmenu);
  LOOP
    IF inside THEN
      inside:=FALSE; old:=pos; on_top;
      CASE pos OF
         0,4:
            IF v_menu(vm[pos]) THEN
              del; kil;
              IF pos=0 THEN n:=0 ELSE n:=1 END;
              IF alt<4 THEN
                IF alt#ORD(wnd[n].type) THEN
                   WITH wnd[n] DO
                     IF wnd_is_dir(n) THEN
                       IF alt<2 THEN
                         IF type=FULL THEN type:=BRIEF;
                           IF old_fw*old_fld#fw*fields THEN
                             fw:=old_fw; fields:=old_fld;
                             scn.kill(scr); new_screen(n);
                             scn.on_top(scr); scn.on_top(wnd[active].scr);
                           ELSE
                             fw:=old_fw; fields:=old_fld;
                           END;
                           d_frame(n)
                         ELSE type:=FULL; old_fw:=fw; old_fld:=fields;
                           IF 3*12#fw*fields THEN
                             scn.kill(scr); new_screen(n);
                             scn.on_top(scr); scn.on_top(wnd[active].scr);
                           END;
                           IF s_kind<Tim THEN get_attrs(n) END;
                         END;
                         show(n,TRUE); RETURN
                       ELSE DISPOSE(fdsc); bio.close(file)
                       END;
                     ELSIF type=TREE THEN DISPOSE(fdsc); bio.close(file)
                     END;
                     IF type=FULL THEN
                       type:=WTYPE(alt); fw:=old_fw; fields:=old_fld;
                       IF fields*fw#3*12 THEN
                         scn.kill(scr); new_screen(n);
                         scn.on_top(scr); scn.on_top(wnd[active].scr)
                       END;
                     ELSE
                       type:=WTYPE(alt);
                       IF type=FULL THEN
                         IF fields*fw#3*12 THEN
                           scn.kill(scr); new_screen(n);
                           scn.on_top(scr); scn.on_top(wnd[active].scr)
                         ELSE
                           old_fld:=fields; old_fw:=fw;
                         END;
                       ELSIF type=BRIEF THEN d_frame(n)
                       END;
                     END;
                     new(n,FALSE); show(n,TRUE);
                   END;
                END;
              ELSE
                CASE alt OF
                   4: (*On/Off*)
                    wnd[n].on:=NOT wnd[n].on;
                    IF wnd[n].on THEN scn.on_top(wnd[n].scr)
                    ELSE scn.delete(wnd[n].scr)
                    END;
                 | 6: (*Hidden*)
                    wnd[n].hid:=NOT wnd[n].hid;
                    IF wnd_is_dir(n) THEN
                      bio.close(wnd[n].file); iter_dir(n); show(n,TRUE)
                    END;
                 |8,9,10,11,12:
                    IF wnd_is_dir(n) THEN resort(n,SORT(alt-8)); show(n,TRUE)
                    ELSE wnd[n].s_kind:=SORT(alt-8)
                    END;
                 |14: IF wnd_is_dir(n) THEN get_attrs(n); show(n,TRUE) END;
                 |15: (*Filter*)
                    IF ask(n,filter[n])&wnd_is_dir(n) THEN
                      refilter(n); show(n,TRUE)
                    END;
                END;
              END;
              RETURN
            ELSE
              del
            END;

        |1: (* files *)
            IF v_menu(vm[pos]) THEN
              IF alt=9 THEN attrs
              ELSIF alt<9 THEN action(CHAR(ORD('1')+alt))
              ELSIF alt=15 THEN action('0')
              ELSIF alt=11 THEN action('+')
              ELSIF alt=12 THEN action('-')
              ELSIF alt=13 THEN action('*')
              END;
              del; kil; RETURN
            END; del;

        |2: (* commands *)
            IF v_menu(vm[pos]) THEN
              del; kil;
              CASE alt OF
                0: cd_active
               |1:
               |3: swap_wnd
               |4:
               |5:
               |7:
               |8: ext_edit
              ELSE
              END;
              RETURN
            END; del;

        |3: (* options *)
            IF v_menu(vm[pos]) THEN
              CASE alt OF
                0: save_time:=60
               |1: save_time:=60*5
               |2: save_time:=60*10
               |3: save_time:=60*15
               |4: save_time:=60*30
               |6: IF timer_on THEN scn.delete(Timer) END;
                   timer_on:=NOT timer_on;
               |8: save_setup
              ELSE
              END;
              del; kil; RETURN
            END; del;

      ELSE
        ASSERT(FALSE);
      END;
      IF menu_line(0c) THEN END;
    ELSE
      LOOP
        spy(ch);
        IF ch=key.cr THEN inside:=TRUE; EXIT
        ELSIF ch=key.right THEN INC(pos); IF menu_line(0c) THEN END; EXIT
        ELSIF ch=key.left  THEN DEC(pos); IF menu_line(0c) THEN END; EXIT
        ELSIF ch=033c THEN
          kil; RETURN
        ELSIF menu_line(ch) THEN inside:=TRUE; EXIT
        END;
      END;
    END;
  END;
END wnd_menu;

TYPE LC = RECORD l,c: INTEGER END;

CONST
  _max=32767;
  _a  =30000;
  _b  =17000;

PROCEDURE snake;
  VAR s: ARRAY [0..7] OF LC;
      first,last,h: INTEGER;
                ch: CHAR;

  VAR a,b,c: INTEGER;

  PROCEDURE random(): INTEGER;
  BEGIN
    c:=a+b;
    IF c>_max THEN DEC(c,_max+1) END;
    c:=(c*2) MOD _max;
    a:=b;
    b:=c;
    RETURN c
  END random;

  PROCEDURE pos(i: INTEGER);
  BEGIN
    tty.set_pos(s[i].l,s[i].c)
  END pos;

  PROCEDURE ints(cur: INTEGER): BOOLEAN;
    VAR i,j: INTEGER;
  BEGIN
    FOR i:=cur+1 TO cur+h-1 DO j:=i MOD h;
      IF (s[j].l=s[cur].l)&(s[j].c=s[cur].c) THEN RETURN TRUE END
    END;
    RETURN FALSE
  END ints;

  PROCEDURE show;
  BEGIN
    last:=(first+1) MOD h;
    ttycbr(blue,black,0); pos(first); tty.write('()',0,2);
    IF NOT ints(last) THEN
      pos( last); tty.write('  ',0,2)
    END
  END show;

  PROCEDURE rnd3(): INTEGER;
  BEGIN
    RETURN random() DIV (_max DIV 3 +1) -1
  END rnd3;

  PROCEDURE rnd2(): INTEGER;
  BEGIN
    RETURN random() DIV (_max DIV 2 +1)
  END rnd2;

  PROCEDURE move;
    VAR t,i,d,prev: INTEGER;
  BEGIN t:=0;
    prev:=(first-1+h) MOD h;
    WITH s[last] DO
      REPEAT
        d:=diap(s[first].l-s[prev].l,-1,1);
        IF d=0 THEN i:=rnd3() ELSE i:=rnd2()*d END;
        l:=diap(s[first].l+i  ,0,tty.state^.lines-1);
        d:=diap(s[first].c-s[prev].c,-1,1);
        IF d=0 THEN i:=rnd3() ELSE i:=rnd2()*d END;
        c:=diap(s[first].c+i*2,0,tty.state^.columns-4);
        INC(t);
      UNTIL (t>10) OR (NOT ints(last))
    END;
    first:=last
  END move;

VAR i,t: INTEGER;

BEGIN
  a:=_a; b:=_b;
  ttycbr(blue,black,0); tty.erase(2); h:=HIGH(s); first:=h;
  FOR i:=0 TO h DO s[i].l:=0; s[i].c:=i*2 END;
  INC(h);
  LOOP
    scn.set_cursor(0); show; move; key.wait(100);
    IF key.ready()>0 THEN key.read(ch); scn.refresh_all; RETURN
    ELSE
      FOR i:=0 TO 1 DO
        bio.get_attr(wnd[i].file,bio.a_wtime,t);
        IF t#wnd[i].time THEN scn.refresh_all; RETURN END;
      END;
    END;
  END;
END snake;

VAR gh,gm,gs: INTEGER;

PROCEDURE spy(VAR ch: CHAR);
  CONST cc='.:';
  VAR t,i,j,p,f: INTEGER;
       hh,mm,ss: INTEGER;
BEGIN
  t:=tim.sys_time(tim.sec);
  WHILE key.ready()<1 DO scn.set_cursor(0);
    FOR j:=0 TO 1 DO
      WITH wnd[j] DO
        IF timer_on THEN
          scn.on_top(Timer);
          setcr(mac,1);
          tim.unpack(tim.time(),i,p,f,hh,mm,ss);
          IF hh#gh THEN
            scn.set_pos(Timer,0,0); scn.print(Timer,'%2d',hh); gh:=hh
          END;
          IF mm#gm THEN
            scn.set_pos(Timer,0,3); scn.print(Timer,'%$2d',mm); gm:=mm
          END;
          IF ((ss-gs) MOD 2)#0 THEN
            scn.set_pos(Timer,0,2); scn.Write(Timer,cc[ss MOD 2]); gs:=ss
          END;
        END;
        IF (type=BRIEF) OR (type=FULL) THEN
          bio.get_attr(file,bio.a_wtime,i);
          IF bio.done & (i#time) THEN
--            cre_warning('Some changes in',path,'Re-reading...',FALSE);
            bio.close(file); iter_dir(j); show_dir(j,TRUE);
            IF j#active THEN chdir(wnd[active].path) END;
--            kill_warning;
          END;
        ELSIF type=TREE THEN
          bio.get_attr(file,bio.a_wtime,i);
          IF bio.done & (i#time) THEN
            str.print(tmp,'"%s"',path);
--            cre_warning('Some changes in',path,'RE-READING!',FALSE);
            recreate_short(j,pos#0); show_tree(j,TRUE);
            IF j#active THEN chdir(wnd[active].path) END;
--            kill_warning;
          END;
        END;
      END;
    END;
    IF tim.sys_time(tim.sec)-t>save_time THEN
      snake; t:=tim.sys_time(tim.sec)
    ELSE key.wait(200)
    END
  END;
  scn.set_cursor(0); key.read(ch)
END spy;

PROCEDURE wnd_size(n: INTEGER);
  VAR s: ARRAY [0..27] OF SCREEN; _fn,_fw,_ww,_wh: INTEGER;

  PROCEDURE cl(): INTEGER;
  BEGIN
    RETURN n*(L-_ww-2)
  END cl;

  PROCEDURE cr(): INTEGER;
  BEGIN
    RETURN (L-2)*n+(_ww-1)*(1-n)
  END cr;

  PROCEDURE ci(i: INTEGER): INTEGER;
  BEGIN
    RETURN cl()+_fw*(i DIV 2)
  END ci;

  PROCEDURE li(i: INTEGER): INTEGER;
  BEGIN
    RETURN 1+(i MOD 2)*(_wh+1)
  END li;

VAR i,j: INTEGER; ok,done: BOOLEAN; ch: CHAR;

BEGIN
  WITH wnd[n] DO
    _fn:=fields; _fw:=fw; _ww:=w; _wh:=h; done:=FALSE;
    REPEAT
      setcbr(mac,mic,1); j:=_fn;
      FOR i:=0 TO 2*j+1 DO
        scn.create(s[i],li(i),ci(i),1,1);
        scn.Write(s[i],'+'); scn.on_top(s[i])
      END;
      REPEAT
        spy(ch); ok:=TRUE;
        CASE ch OF
           key.ins: INC(_fw); _fw:=min(_fw,max_fw);
                    _fw:=min(_fw,(L-3) DIV _fn)
          |key.del: DEC(_fw); _fw:=max(_fw,min_fw)
          |'+'    : INC(_fn);
                    _fn:=min(_fn,(_ww-1) DIV min_fw);
                    _fw:=(_ww+_fn-2) DIV _fn;
                    WHILE _fw*_fn>L-3 DO DEC(_fw) END;
          |'-'    : DEC(_fn); _fn:=max(1,_fn);
                    _fn:=max(_fn,(_ww-2) DIV max_fw +1);
                    _fw:=(_ww+_fn-2) DIV _fn;
                    WHILE _fw*_fn>L-3 DO DEC(_fw) END;
          |key.right: IF n=0 THEN INC(_fn) ELSE DEC(_fn) END;
            _fn:=diap(_fn,1,(L-3) DIV _fw)
          |key.left : IF n=1 THEN INC(_fn) ELSE DEC(_fn) END;
            _fn:=diap(_fn,1,(L-3) DIV _fw)
          |key.up   : DEC(_wh); _wh:=diap(_wh,3,LINES-4)
          |key.dw   : INC(_wh); _wh:=diap(_wh,3,LINES-4)
          |'.':  _fw:=12; _fn:=3
          |key.cr,033c: done:=TRUE
        ELSE
          ok:=FALSE
        END;
        _ww:=_fn*_fw+1
      UNTIL ok;
      FOR i:=0 TO 2*j+1 DO scn.kill(s[i]) END;
    UNTIL done;
    IF ch=033c THEN RETURN END;
    scn.kill(scr); h:=_wh;
    IF type#FULL THEN fields:=_fn; fw:=_fw; w:=_ww END;
    new_screen(n); show(n,TRUE); scn.on_top(scr)
  END;
END wnd_size;

PROCEDURE view;
BEGIN
  WITH wnd[active] DO
    cut(fdsc[pos].name,src);
    IF is_dir(fdsc[pos].ftype) THEN
      str.print(tmp,'diff %s',src); exec(tmp)
    ELSE
--      put_ext(src,dst);
      str.print(tmp,'ex %s -w',src); exec(tmp)
    END;
  END;
END view;

PROCEDURE action(ch: CHAR);
  VAR f1,f2: bio.FILE;
        ans: BOOLEAN;
      s1: ARRAY [0..63] OF CHAR;
      scr1: SCREEN; e,p,m: INTEGER;

  PROCEDURE start;
  BEGIN
    m:=wnd[active].marked-1; e:=m; p:=0;
    IF e=0 THEN e:=1 END
  END start;

  PROCEDURE get_next(VAR s: ARRAY OF CHAR): BOOLEAN;

    PROCEDURE unmark;
    BEGIN
      WITH wnd[active] DO
        fdsc[p-1].mode:={};
        DEC(bytes,fdsc[p-1].eof); DEC(marked);
      END;
      sdl(active,p-1); show_marked(active,TRUE)
    END unmark;

  BEGIN
    WITH wnd[active] DO
      IF e=0 THEN unmark; RETURN FALSE
      ELSIF m=0 THEN p:=pos
      ELSE
        IF p>0 THEN unmark END;
        WHILE fdsc[p].mode={} DO INC(p) END;
      END;
      cut(fdsc[p].name,s)
    END;
    INC(p); DEC(e); RETURN TRUE
  END get_next;

  PROCEDURE sel_group(mark: BOOLEAN);
    VAR i,num: INTEGER;
          reg: xpr.EXPR;
          f  : bio.FILE;
  BEGIN
    scn.delete(Timer);
    xpr.compile(sel_str,reg);
    IF NOT xpr.done THEN
      warning('Wrong pattern:',sel_str,"Can't do it."); RETURN
    ELSE
      WITH wnd[active] DO
        IF type#BRIEF THEN num:=h ELSE num:=fields*h END;
        FOR i:=0 TO numb-1 DO
          IF isnt_dir(fdsc[i].ftype) THEN
            cut(fdsc[i].name,dst);
            IF xpr.match(reg,dst,0) THEN
              IF mark THEN
                IF fdsc[i].mode={} THEN
                  INC(marked);  fdsc[i].mode:={0};
                  IF (type=BRIEF)&(s_kind<Tim) THEN
                    bio.open(f,dst,''); fdsc[i].eof:=bio.eof(f); bio.close(f)
                  END;
                  INC(bytes,fdsc[i].eof);
                  IF (i>=first)&(i-first<num) THEN show_line(active,i) END;
                END;
              ELSE
                IF fdsc[i].mode#{} THEN
                  DEC(marked);  fdsc[i].mode:={};
                  DEC(bytes,fdsc[i].eof);
                  IF (i>=first)&(i-first<num) THEN show_line(active,i) END;
                END;
              END;
            END;
          END;
        END;
        IF marked=1 THEN bytes:=0 END; show_marked(active,TRUE);
      END;
      xpr.dispose(reg)
    END;
  END sel_group;

  PROCEDURE mv;
    VAR eof: INTEGER; f1,f2,f3: bio.FILE;
        from_name,to_name,to_path: ARRAY [0..63] OF CHAR;
        all_in_one: BOOLEAN; tab: SCREEN;

  PROCEDURE cre;
  BEGIN
    setcbr(blue,orange,1);
    scn.create(tab,7,20,7,36); scn.frame(tab);
    scn.center(tab,'Moving',1); scn.center(tab,'to',3);
    scn.on_top(tab)
  END cre;

  PROCEDURE show_names;
    PROCEDURE center(i: INTEGER);
    BEGIN
      scn.set_pos(tab,i,1); scn.repeat(tab,' ',34); scn.center(tab,tmp,i)
    END center;
  BEGIN
    str.print(tmp,'"%s"',from_name);          center(2);
    str.print(tmp,'"%s/%s"',to_path,to_name); center(4)
  END show_names;

  PROCEDURE kil;
  BEGIN
    scn.kill(tab)
  END kil;

  BEGIN
    WITH wnd[active] DO
      IF wnd_is_dir(active)&((marked#1)
      OR isnt_dir(fdsc[pos].ftype)) THEN
        IF marked#1 THEN
          IF sm(marked) THEN src:='' ELSE src:='s'0c END;
          str.print(tmp,'Rename or move %d file%s to:',marked-1,src)
        ELSE
          cut(fdsc[pos].name,src);
          str.print(tmp,'Rename or move "%s" to:',src)
        END;
        str.copy(dst,dst_str);
        IF NOT say(tmp,dst,TRUE,6,1,0) THEN RETURN END;
        cut(dst,dst_str); str.copy(to_path,dst_str); to_name:='';
        IF to_path='' THEN RETURN END;
        IF last(to_path)#'/' THEN bio.open(f3,to_path,'');
          IF bio.done THEN
            IF bio.kind(f3)*bio.is_dir={} THEN
              splitpathname(to_path,to_name)
            END;
            bio.close(f3)
          ELSE splitpathname(to_path,to_name)
          END;
        ELSE
          to_path[str.len(to_path)-1]:=0c
        END;
        IF to_path='' THEN to_path:='.' END;
        all_in_one:=(to_name#'');
        bio.open(f2,to_path,'w');
        IF NOT bio.done THEN
          lex.perror(src,bio.error,'%%s "%s"',bio.ename);
          warning("Invalid path name:",to_path,src);
          RETURN
        END;
        start; cre;
        WHILE get_next(from_name) DO
          show_names;
          bio.open(f1,from_name,'r');
          IF NOT bio.done THEN
            lex.perror(src,bio.error,'%%s');
            warning("Can't open:",from_name,src)
          ELSE
            IF NOT all_in_one THEN str.copy(to_name,from_name) END;
            IF bio.is_hidd(f1) THEN tmp:='h' ELSE tmp:='' END;
            bio.flink(f2,f1,to_name,tmp);
            IF NOT bio.done THEN
              lex.perror(src,bio.error,'%%s "%s"',bio.ename);
              warning("Can't link:",to_name,src); bio.close(f1)
            ELSE
              bio.close(f1); bio.unlink(from_name);
              IF NOT bio.done THEN
              lex.perror(src,bio.error,'%%s "%s"',bio.ename);
                warning("Can't unlink:",from_name,src);
              END;
            END;
          END;
        END;
        kil; bio.close(f2)
      END;
    END;
  END mv;

  PROCEDURE cp;
    VAR cps: SCREEN; all_in_one: BOOLEAN;
        eof: INTEGER; from_name: STR64;
   f1,f2,f3: bio.FILE;  to_name: STR64;
        buf: STRING;    to_path: STR64;

  PROCEDURE cre;
  BEGIN
    setcbr(blue,orange,1);
    scn.create(cps,7,20,7,36); scn.frame(cps);
    scn.center(cps,'Copying',1);
    scn.center(cps,'to',3);
    NEW(buf,4096)
  END cre;

  PROCEDURE kil;
  BEGIN
    DISPOSE(buf); scn.kill(cps)
  END kil;

  PROCEDURE copy(from,to: bio.FILE);
    VAR rest,pos,old: INTEGER; time: WORD;
  BEGIN
    setcbr(blue,orange,1);
    scn.set_pos(cps,2,1); scn.repeat(cps,' ',34);
    scn.set_pos(cps,4,1); scn.repeat(cps,' ',34);
    str.print(tmp,'"%s"',from_name); scn.center(cps,tmp,2);
    str.print(tmp,'"%s/%s"',to_path,to_name); scn.center(cps,tmp,4);
    setcbr(yellow,black,0); scn.set_pos(cps,5,8); scn.repeat(cps,' ',20);
    setcbr(yellow,black,1); scn.set_pos(cps,5,8); old:=0;
    rest:=eof;
    scn.on_top(cps);
    WHILE rest>0 DO
      (*$U+*)
      bio.read(from,buf^.ADR,min(4096,rest));
      bio.write(to ,buf^.ADR,min(4096,rest));
      (*$U-*)
      DEC(rest,min(rest,4096)); pos:=20*(eof-rest) DIV eof;
      IF pos>old THEN
        scn.repeat(cps,' ',pos-old); old:=pos
      END;
    END;
    bio.get_attr(from,bio.a_ctime,time);
    bio.set_attr(to,  bio.a_ctime,time);
    bio.get_attr(from,bio.a_wtime,time);
    bio.set_attr(to,  bio.a_wtime,time);
    bio.close(from);  bio.close(to);
    IF NOT bio.done THEN
      lex.perror(tmp,bio.error,'%%s "%s"',bio.ename);
      warning("Can't link the file:",to_name,tmp)
    END;
  END copy;

  BEGIN
    WITH wnd[active] DO
      IF wnd_is_dir(active)&((marked#1)
      OR isnt_dir(fdsc[pos].ftype)) THEN
        IF marked#1 THEN
          IF sm(marked) THEN src:='' ELSE src:='s'0c END;
          str.print(tmp,'Copy %d file%s to:',marked-1,src)
        ELSE
          cut(fdsc[pos].name,src);
          str.print(tmp,'Copy "%s" to:',src)
        END;
        str.copy(dst,dst_str);
        IF NOT say(tmp,dst,TRUE,5,1,0) THEN RETURN END;
        cut(dst,dst_str); str.copy(to_path,dst_str); to_name:='';
        IF to_path='' THEN RETURN END;
        IF last(to_path)#'/' THEN bio.open(f3,to_path,'');
          IF bio.done THEN
            IF bio.kind(f3)*bio.is_dir={} THEN
              splitpathname(to_path,to_name)
            END;
            bio.close(f3)
          ELSE splitpathname(to_path,to_name)
          END;
        ELSE
          to_path[str.len(to_path)-1]:=0c
        END;
        IF to_path='' THEN to_path:='.' END;
        all_in_one:=(to_name#'');
        bio.open(f2,to_path,'w');
        IF NOT bio.done THEN
          lex.perror(src,bio.error,'%%s "%s"',bio.ename);
          warning("Inavalid path name:",to_path,src);
          RETURN
        END;
        start; cre;
        IF NOT mem.done THEN
          warning("No memory for buffer.","","");
          bio.close(f2); scn.kill(cps); RETURN
        END;
        WHILE get_next(from_name) DO
          bio.open(f1,from_name,'r');
          IF NOT bio.done THEN
            lex.perror(tmp,bio.error,'%%s');
            warning("Can't open:",from_name,tmp);
            bio.close(f2); kil; RETURN
          END;
          eof:=bio.eof(f1);
          IF NOT all_in_one THEN str.copy(to_name,from_name) END;
          IF bio.is_hidd(f1) THEN tmp:='hw' ELSE tmp:='w' END;
          bio.fcreate(f2,f3,to_name,tmp,eof);
          IF NOT bio.done THEN
            lex.perror(tmp,bio.error,'%%s "%s"',bio.ename);
            warning("Can't create:",to_name,tmp);
            bio.close(f1)
          ELSE
            copy(f1,f3); (* closes f1 f3 *);
            IF NOT bio.done & all_in_one THEN bio.close(f2); kil; RETURN END;
          END;
        END;
        bio.close(f2); kil
      END;
    END;
  END cp;

  PROCEDURE rm;
  BEGIN
    WITH wnd[active] DO
      IF type=INFO THEN RETURN
      ELSIF type=TREE THEN
        src:=fdsc[pos].name; marked:=1;
        IF NOT empty_dir(bio.cd) THEN
          warning("The directory",src,"is not empty!");
          RETURN
        END
      ELSIF marked=1 THEN
        cut(fdsc[pos].name,src);
        IF is_dir(fdsc[pos].ftype) THEN
          bio.open(f1,src,'x');
          IF NOT bio.done THEN
            lex.perror(tmp,bio.error,'%%s');
            warning("Can't delete",src,tmp);
            RETURN
          END;
          ans:=empty_dir(f1); bio.close(f1);
          IF NOT ans THEN
            warning("The directory",src,"is not empty!");
            RETURN
          END
        END;
      END;
      IF marked#1 THEN
        IF sm(marked) THEN src:='' ELSE src:='s'0c END;
        str.print(tmp,'%d file%s?',marked-1,src)
      ELSE
        str.print(tmp,'"%s" ?',src)
      END;
      IF say('Do you wish to delete',tmp,FALSE,8,0) THEN
        IF type=TREE THEN
          bio.unlink(path);
          IF NOT bio.done THEN
            lex.perror(tmp,bio.error,'%%s "%s"',bio.ename);
            warning("Can't delete:",src,tmp)
          END;
        ELSE
          start;
          WHILE get_next(src) DO
            bio.unlink(src);
            IF NOT bio.done THEN
              lex.perror(tmp,bio.error,'%%s "%s"',bio.ename);
              warning("Can't delete:",src,tmp)
            END;
          END;
        END;
      END;
    END;
  END rm;


BEGIN
  s1:=''; src:=''; dst:='';
  CASE ch OF
    '1':
      s1:='"GOLD" bar also available (press & look).';
      IF say('Everything is on the screen!',s1,FALSE,9) THEN END;
   |'2':
      p:=say2('Do you wish to (un)mount',dev_str,'at',dir_str,3,2,1,0);
      IF p=1 THEN
        bio.mount(dir_str,dev_str,'',tmp,FALSE);
        IF NOT bio.done THEN
          lex.perror(tmp,bio.error,'%%s "%s"',bio.ename);
          warning("Can't mount",dev_str,tmp)
        END;
      ELSIF p=2 THEN
        bio.unmount(dir_str,2);
        IF NOT bio.done THEN
          lex.perror(tmp,bio.error,'%%s "%s"',bio.ename);
          warning("Can't unmount",dir_str,tmp)
        END;
      END;
   |'3': view; (* ref_screen(TRUE) *)
   |'4': (* ex *)
            WITH wnd[active] DO
              IF wnd_is_dir(active) THEN
                IF isnt_dir(fdsc[pos].ftype) THEN
                  str.print(s1,'ex %s',fdsc[pos].name);
                ELSIF pos=0 THEN s1:='ex'
                ELSE RETURN
                END;
                com.system(s1,"");
                scn.set_cursor(0); ref_screen(TRUE)
              END;
            END;
            RETURN
   |'5': cp
   |'6': mv
   |'7': IF say('Create the directory',s1,TRUE)&(s1#'') THEN
           bio.mkdir(s1,FALSE);
           IF NOT bio.done THEN
             lex.perror(tmp,bio.error,'%%s "%s"',bio.ename);
             warning("Can't create the directory:",s1,tmp)
           END
         END
   |'8': rm
   |'9': wnd_menu;
   |'0': IF say('Do you wish to quit the commander?',dst,FALSE,11,13,12)
         THEN HALT END;
   |'+',key.newln:
         IF wnd_is_dir(active)&say('Select files:',sel_str,TRUE)
            &(sel_str#'') THEN sel_group(TRUE)
         END
   |'-': IF wnd_is_dir(active)&say('Unselect files:',sel_str,TRUE)
            &(sel_str#'') THEN sel_group(FALSE)
         END
   |'*': IF ask(active,filter[active])&wnd_is_dir(active) THEN
           refilter(active); show(active,TRUE)
         END

   |key.nak: swap_wnd
   |key.f1: bars_menu(Gold); spy(ch); setcr(violet,0); set_back(dark_blue);
     CASE ch OF
      key.f1: shell(0c)
     |key.f2: bars_menu(Size); wnd_size(active)
     |'1': save_setup
     |'0': cd_active
     ELSE
     END;
     bars_menu(Digits)
  ELSE
   IF (ch>' ') & (ch<200c) THEN shell(ch) END;
  END;
  scn.set_cursor(0)
END action;

----------------------------- MOVE -----------------------------
                             ------

PROCEDURE short_tree_move;
  VAR i: INTEGER;
     ch: CHAR;
BEGIN
  LOOP
    spy(ch);
    WITH wnd[active] DO
      oldpos:=pos;
      CASE ch OF
         key.up: DEC(pos)
       | key.dw: INC(pos)
       | key.right:
         IF pos#0 THEN
           IF NOT empty_dir(bio.cd) THEN
             recreate_short(active,FALSE);
             show_tree(active,TRUE)
           END;
           RETURN
         ELSE pos:=1
         END;
       | key.left:
         IF (pos=0)&(path#'/'0c) THEN
           recreate_short(active,TRUE);
           show_tree(active,TRUE); RETURN
         ELSE pos:=0
         END;
       | key.pgup: DEC(pos,h DIV 3)
       | key.pgdw: INC(pos,h DIV 3)
       | key.home: pos:=0;
       | key.end : pos:=numb-1;
       | key.cr:   refresh_dir; RETURN
       | key.tab, 033c(*esc*): active:=(active+1) MOD 2; RETURN
      ELSE
        tree_cd; action(ch); RETURN
      END;
      IF NOT on THEN on:=TRUE; scn.on_top(scr) END;
      pos:=max(0,min(numb-1,pos));
      IF pos#oldpos THEN correct_path END;
      show_tree(active,FALSE)
   END;
  END;
END short_tree_move;

PROCEDURE help_tree_move(): BOOLEAN;
  VAR r: BOOLEAN;
      i: INTEGER;
     ch: CHAR;
BEGIN
  LOOP
    r:=FALSE; spy(ch);
    WITH wnd[2] DO
      oldpos:=pos;
      CASE ch OF
         key.up: DEC(pos)
       | key.dw: INC(pos)
       | key.right:
         IF pos#0 THEN
           IF NOT empty_dir(bio.cd) THEN
             recreate_short(2,FALSE);
             oldpos:=pos; r:=TRUE
           END;
         ELSE pos:=1
         END;
       | key.left:
         IF (pos=0)&(path#'/'0c) THEN
           recreate_short(2,TRUE);
           oldpos:=pos; r:=TRUE
         ELSE pos:=0
         END;
       | key.pgup: DEC(pos,h DIV 3)
       | key.pgdw: INC(pos,h DIV 3)
       | key.home: pos:=0;
       | key.end : pos:=numb-1;
       | key.cr  : RETURN TRUE
       | 033c    : RETURN FALSE
      ELSE
      END;
      pos:=max(0,min(numb-1,pos));
      IF pos#oldpos THEN correct_path END;
      show_tree(2,r);
(*
      setcbr(noc,mic,0);
      scn.set_pos(TTY,LINES-1,0);
      scn.print(TTY,'PATH="%s"%.20c',path,' ');
*)
   END;
  END;
END help_tree_move;


PROCEDURE ch_dir;
  VAR i: INTEGER; dir: NAME;
BEGIN
  dir:='';
  WITH wnd[active] DO
    i:=0;
    WITH fdsc[pos] DO
      WHILE name[i]#' ' DO tmp[i]:=name[i]; INC(i) END;
      tmp[i]:=0c;
      IF tmp='..' THEN
        IF path='/'0c THEN RETURN
        ELSE splitpathname(path,dir); fill(dir)
        END;
      ELSE
        IF path#'/'0c THEN str.app(path,'/') END;
        str.app(path,tmp)
      END;
    END;
    env.put_str('CD',path,FALSE);
    bio.close(file); numb:=0; iter_dir(active);
    IF dir#'' THEN pos:=0;
      WHILE (pos<numb)&(fdsc[pos].name#dir) DO INC(pos) END;
    END;
    show_dir(active,TRUE)
  END;
END ch_dir;

PROCEDURE dir_move;
  VAR ch: CHAR;
      f : bio.FILE;
      n : NAME;
     fld: INTEGER;

  PROCEDURE try_act;
    VAR i: INTEGER;
  BEGIN
    WITH wnd[active] DO
      cut(fdsc[pos].name,src); put_ext(src,dst);
      FOR i:=0 TO HIGH(e_menu) DO
        IF dst=e_menu[i].ext THEN
          str.print(tmp,'%s %s',e_menu[i].act,src);
          exec(tmp); RETURN
        END;
      END;
    END;
  END try_act;

BEGIN
  WITH wnd[active] DO
    LOOP
      spy(ch);
      IF type=BRIEF THEN fld:=fields ELSE fld:=1 END;
      oldpos:=pos;
      oldf:=first;
      CASE ch OF
         key.up:   DEC(pos)
       | key.dw:   INC(pos)
       | key.pgup: DEC(pos,h DIV 3)
       | key.pgdw: INC(pos,h DIV 3)
       | key.left: DEC(pos,h);
         IF (pos>=0)&(pos<first) THEN DEC(first,h)
         END
       | key.right:INC(pos,h);
         IF (pos<numb)&(pos>=first+h*fld) THEN INC(first,h)
         END
       | key.home: pos:=0      | key.end : pos:=numb-1
       | (* key.newln, *) key.lf, key.ins:
                 WITH fdsc[pos] DO
                   IF isnt_dir(ftype) THEN
                     mode:=mode/{0};
                     IF mode#{} THEN
                       INC(marked); marked:=-marked;
                       IF (type=BRIEF)&(s_kind<Tim) THEN
                         cut(name,n); bio.open(f,n,'');
                         eof:=bio.eof(f); bio.close(f)
                       END;
                       INC(bytes,eof)
                     ELSE
                       DEC(marked); DEC(bytes,eof);
                       IF marked=1 THEN bytes:=0 END;
                       marked:=-marked;
                     END;
                   END;
                   INC(pos)
                 END;
       | key.tab, 033c(*esc*): active:=(active+1) MOD 2; RETURN
       | ' '   :  cut(fdsc[pos].name,tmp);
                  str.app(cmd_str,' '0c); INC(pos);
                  str.app(cmd_str,tmp); show_cmd
       | key.cr:
          IF is_dir(fdsc[pos].ftype) THEN ch_dir
          ELSE try_act
          END;
      ELSE
         action(ch); RETURN
      END;
      IF NOT on THEN on:=TRUE; scn.on_top(scr) END;
      show_dir(active,FALSE)
    END;
  END;
END dir_move;

----------------------------- INIT -----------------------------
                             ------
PROCEDURE new_screen(n: INTEGER);
  VAR x,y: INTEGER;
BEGIN
  WITH wnd[n] DO
    IF type=FULL THEN
      old_fw:=fw; old_fld:=fields;
      IF fw*fields#12*3 THEN fw:=12; fields:=3 END;
    END;
    w:=fw*fields+1;
    IF n=2 THEN x:=(L-w) DIV 2; y:=3; h:=19
    ELSE
      y:=1; x:=n*(L-w-2)
    END;
    setcbr(violet,dark_blue,0);
    scn.create(scr,y,x,h+2,w);
    set_back(black); scn.frame(scr);
    set_back(dark_blue);
    IF type=BRIEF THEN d_frame(n) END;
  END;
END new_screen;


PROCEDURE new(i: INTEGER; get_path: BOOLEAN);
  VAR j: INTEGER;
      s: STRING;
BEGIN
  WITH wnd[i] DO
    setcr(violet,0);
    set_back(black);
    scn.frame(scr);
    set_back(dark_blue);
    IF get_path OR (path='.'0c) THEN
      env.get_str('CD',s); str.copy(path,s)
    END;
    CASE type OF
      BRIEF,
        FULL: NEW(fdsc,32); numb:=0; iter_dir(i)
       |TREE: create_short_tree(i,TRUE)
       |INFO:
    ELSE
      ASSERT(FALSE)
    END;
  END;
END new;

PROCEDURE select_target(VAR s: ARRAY OF CHAR; where: INTEGER): BOOLEAN;
  VAR ans: BOOLEAN; p: ARRAY [0..63] OF CHAR;
      string: STRING; a,y: INTEGER;
BEGIN
  env.get_str("CD",string); str.copy(p,string);
  a:=active; active:=2;
  WITH wnd[2] DO
    fields:=1; fw:=30; w:=32; h:=17;
    setcbr(violet,dark_blue,0);
    CASE where OF
     0,1: w:=30; h:=19; y:=2; where:=(L-w) DIV 2 -1
    ELSE  w:=30; h:=17; y:=3; where:=L-w-1
    END;
    scn.create(scr,y,where,h+2,w);
    set_back(black); scn.frame(scr);
    scn.center(scr," Please, select a directory ",0);
    set_back(dark_blue);
    type:=TREE; str.copy(path,p);
    create_short_tree(2,TRUE); show(2,TRUE);
    scn.on_top(scr); ans:=help_tree_move();
    IF ans THEN str.copy(s,path) END;
    chdir(p); env.put_str("CD",p,FALSE);
    DISPOSE(fdsc); scn.kill(scr);
    bio.close(file); active:=a; RETURN ans
  END;
END select_target;

PROCEDURE no_back(i: INTEGER); BEGIN END no_back;

PROCEDURE ini_colors;
BEGIN
  set_back:=no_back;
  head_mac:=mac;
  CASE mac-mic+1 OF
    8: IF tty.state^.type=CHAR_MAP  THEN DEC(mac); head_mac:=mac;
         black:=mic  ; blue     :=mic+3; yellow:=mic+6; orange:=mic+5;
         green:=mic+4; dark_blue:=mic+1; red   :=mic+3; violet:=noc
       ELSE
         set_back:=scn.set_back;
         black:=mic;   blue     :=mic+1; yellow:=mic+2; orange:=mic+3;
         green:=mic+4; dark_blue:=mic+5; red   :=mic+6; violet:=mac
       END|
    4: head_mac:=noc;
       black:=mic;   blue     :=mic+1; yellow:=mac  ; orange:=mic+3;
       green:=mic+2; dark_blue:=mic  ; red   :=mic+1; violet:=noc  |
    3: black:=mic;   blue     :=mic+1; yellow:=mic+2; orange:=mic+2;
       green:=mic+1; dark_blue:=mic  ; red   :=mic+1; violet:=noc  |
    2: black:=mic;   blue     :=mic+1; yellow:=mic+1; orange:=mic+1;
       green:=mic+1; dark_blue:=mic+0; red   :=mic+1; violet:=mac
  ELSE
    ASSERT(FALSE)
  END;
END ini_colors;

PROCEDURE ini_windows;
BEGIN
  ALTS[0]:='[Cancel]';  ALTS[1]:='[ Tree ]';
  ALTS[2]:='[Unmount]'; ALTS[3]:='[Mount]';
  ALTS[4]:='[ Edit ]';  ALTS[5]:='[ Copy ]';
  ALTS[6]:='[ Move ]';  ALTS[7]:='[ Make ]';
  ALTS[8]:='[Delete]';  ALTS[9]:='[ Ok ]';
  ALTS[10]:='[ Quit ]'; ALTS[11]:='[ Yes ]';
  ALTS[12]:='[ No ]';   ALTS[13]:='[Save&Exit]';

  ini_set(filter[0],0,25,' Filter ','Select files to display','[ Ok ]'
                           ,'All files|Executable files|Custom:|#*.*|');

  ini_set(filter[1],0,25,' Filter ','Select files to display','[ Ok ]'
                           ,'All files|Executable files|Custom:|#*.*|');
  ini_menu(vm[0],0,
           '  Brief            ', '  Full             ',
           '  Info             ', '  Tree             ',
           '. On/Off    Gold-2 ',
           '',
           '*+Hidden files     ',
           '',
           '  Name             ', '  eXtention        ',
           '  tiMe             ', '  Size             ',
           '  Unsorted         ',
           '',
           '. Re-read          ', '. fiLter...        ');

  ini_menu(vm[4],4,
           '  Brief            ', '  Full             ',
           '  Info             ', '  Tree             ',
           '. On/Off    Gold-2 ',
           '',
           '*+Hidden files     ',
           '',
           '  Name             ', '  eXtention        ',
           '  tiMe             ', '  Size             ',
           '  Unsorted         ',
           '',
           '. Re-read          ', '. fiLter...        ');

  ini_menu(vm[1],1,
      '. Help             1   ',  '. Mount            2   ',
      '. View             3   ',  '. Edit             4   ',
      '. Copy             5   ',  '. Rename or move   6   ',
      '. maKe directory   7   ',  '. Delete           8   ',
      '',
      '. file Attributes      ',
      '',
      '. select Group     Gray +','. uNselect group   Gray -',
      '. fiLter           Gray *',
      '',
      '. Quit             0   ');

  ini_menu(vm[2],2,
      '. KCD tree             Gold-0 ', '. Find file            Gold-7 ',
      '',
      '. Swap panels          Ctrl-U ', '. Panels on/off        Ctrl-O ',
      '. Compare directories         ',
      '',
      '. Menu file edit              ', '. eXtention file edit         ');

  ini_menu(vm[3],3,
      '  Clear screen in 1 min. ',
      '  [F]             5 min. ',
      '  [T]            10 min. ',
      '  [E]            15 min. ',
      '  [R]            30 min. ',
      '',
      '*+cLock                   ',
      '',
      '. Save setup       Gold-1 ');
END ini_windows;

PROCEDURE read_setup;
  VAR home,kcs,cod: bio.FILE;
               bin: bio.PATHs;
                ss: STRING;
              done: BOOLEAN;
          cod_time,
          set_time,
                 i: INTEGER;

  PROCEDURE nextw(VAR i: WORD);
    VAR ch: CHAR;
  BEGIN
    bio.getch(kcs,ch); i:=ORD(ch)-ORD('a');
  END nextw;

  PROCEDURE nexts(VAR s: ARRAY OF CHAR);
  BEGIN
    bio.getstr(kcs,s,0)
  END nexts;

  PROCEDURE read_ext;
    VAR i,j: INTEGER;
  BEGIN
    nextw(j);
    NEW(e_menu,j+1);
    FOR i:=0 TO j DO
      nexts(tmp); NEW(e_menu[i].ext,str.len(tmp)+1);
      str.copy(e_menu[i].ext,tmp);
      nexts(tmp); NEW(e_menu[i].act,str.len(tmp)+1);
      str.copy(e_menu[i].act,tmp);
    END;
  END read_ext;

  PROCEDURE default;
    VAR i: INTEGER;

    PROCEDURE put_ext(i: INTEGER; VAL e,a: ARRAY OF CHAR);
    BEGIN
      NEW(e_menu[i].ext,str.len(e)+1); str.copy(e_menu[i].ext,e);
      NEW(e_menu[i].act,str.len(a)+1); str.copy(e_menu[i].act,a);
    END put_ext;

  BEGIN
    LINES:=scn.state^.lines;
    FOR i:=0 TO 1 DO
      WITH wnd[i] DO
        fields:=3; fw:=12; h:=16; type:=BRIEF; path:='.'; on:=TRUE;
        numb:=0; pos:=-1; hid:=TRUE; s_kind:=Nam
      END;
    END;
    wnd[0].type:=INFO;
    wnd[0].path:='/';
    timer_on:=TRUE; save_time:=5;
    active:=1; NEW(e_menu,6);
    put_ext(0,'d'0c,'ex');
    put_ext(1,'m'0c,'ex');
    put_ext(2,'txt','ex');
    put_ext(3,'cod',  '');
    put_ext(4,'@'0c,  '');
    put_ext(5,'sh' ,  '');
  END default;


BEGIN
  IF arg.flag('-','p') THEN default; RETURN END;
  env.get_str('HOME',ss);
  bio.open(home,ss,'r');
  IF bio.done THEN
    bio.fopen(home,kcs,'KC.SETUP','r');
    done:=bio.done; bio.close(home);
    IF done THEN
      bio.get_paths(bin,'BIN');
      bio.lookup(bin,cod,'kc.cod','');
      bio.get_attr(cod,bio.a_wtime,cod_time);
      IF arg.flag('-','t') THEN bio.set_attr(kcs,bio.a_wtime,tim.time()) END;
      bio.get_attr(kcs,bio.a_wtime,set_time);
      bio.close(cod); bio.close_paths(bin);
      IF cod_time<set_time THEN
--      IF TRUE THEN
        bio.buffers(kcs,1,1024);
        FOR i:=0 TO 1 DO
          WITH wnd[i] DO
            nextw(type); nextw(fields); nextw(fw); nextw(h); nexts(path);
            nextw(pos);  nextw(hid);    nextw(s_kind);
            nextw(filter[i].cur); nexts(filter[i].alt[3]);
            on:=TRUE
          END;
        END;
        nextw(LINES); nextw(active); nextw(timer_on); nextw(save_time);
        read_ext;
        bio.close(kcs);
        RETURN
      ELSE
        s_warning('Old version of','KC.SETUP',"Can't read it.")
      END;
    END;
  END;
  default;
END read_setup;

PROCEDURE save_setup;
  VAR home,kcs: bio.FILE; ss: STRING; i: INTEGER;

  PROCEDURE nextw(i: WORD);
  BEGIN
    bio.putch(kcs,CHAR(INTEGER(i)+ORD('a')))
  END nextw;

  PROCEDURE nexts(VAL s: ARRAY OF CHAR);
    VAR ch: CHAR;
  BEGIN
    bio.print(kcs,'%s%c',s,key.nl)
  END nexts;

  PROCEDURE save_ext;
    VAR i: INTEGER;
  BEGIN
    nextw(HIGH(e_menu));
    FOR i:=0 TO HIGH(e_menu) DO
      nexts(e_menu[i].ext);
      nexts(e_menu[i].act);
    END;
  END save_ext;

BEGIN
  env.get_str('HOME',ss);
  bio.open(home,ss,'w');
  IF bio.done THEN bio.fcreate(home,kcs,'KC.SETUP','w',64)
  ELSE
    warning('Can not open "home" directory:',ss,"Can't save SETUP !");
    RETURN
  END;
  IF bio.done THEN
    FOR i:=0 TO 1 DO
      WITH wnd[i] DO
        nextw(type); nextw(fields); nextw(fw); nextw(h); nexts(path);
        nextw(pos);  nextw(hid);    nextw(s_kind);
        nextw(filter[i].cur); nexts(filter[i].alt[3]);
      END;
    END;
    nextw(LINES); nextw(active); nextw(timer_on); nextw(save_time DIV 60);
    save_ext;
    bio.close(kcs);
    bio.close(home)
  ELSE
    warning('Can not create "KC.SETUP" at',ss,"Can't save SETUP !");
  END;
END save_setup;

PROCEDURE switch(i,j: INTEGER);
BEGIN
  vm[i].alts[j,1]:='+';
  setcbr(mac,black,1);
  scn.set_pos(vm[i].scr,j+1,2);
  scn.Write  (vm[i].scr,'+');
END switch;

PROCEDURE init;
  VAR i,p: INTEGER;
BEGIN
  key.set_break(0);
  com.hold_break(TRUE);
  WITH scn.state^ DO
   mac:=max_color; hb:=hbar;
   mic:=min_color; vb:=vbar;
    ul:=bars[0,0]; ud:=bars[0,1]; ur:=bars[0,2];
    lr:=bars[1,0];                rl:=bars[1,2];
    dl:=bars[2,0]; du:=bars[2,1]; dr:=bars[2,2];
   noc:=color
  END;
  ini_colors;
  sed.new(dsc,0);      dsc^.how:=sed.confirm;
  sed.new(shdsc,16); shdsc^.how:=sed.confirm;

  LINES:=scn.state^.lines; L:=78; HALF:=L DIV 2;

  w:=2*L DIV 3;
  x:=(L-w) DIV 2;
  y:=(LINES-1) DIV 2 -3;

  gh:=-1; gm:=-1; gs:=-1;
  setcbr(mac,mic,1); scn.create(Timer,LINES-2,L-8,1,5);
  setcbr(noc,mic,0); scn.create(Bars,0,1,1,L-2);
                     scn.create(Lmenu,0,1,1,76);

  ini_windows;

  read_setup;

  FOR i:=0 TO 1 DO
    switch(4*i,ORD(wnd[i].type));
    switch(4*i,ORD(wnd[i].s_kind)+8);
    IF wnd[i].hid THEN switch(4*i,6) END;
    p:=wnd[i].pos; new_screen(i); new(i,FALSE); wnd[i].pos:=p
  END;


  IF timer_on THEN switch(3,6) END;
  switch(3,(save_time DIV 5) MOD 5);

  save_time:=save_time*60;

  dev_str:='/dev/fd0';
  dir_str:='/mnt';
  sel_str:='*.*';
  cmd_str:='';
  str.copy(dst_str,wnd[passive()].path);

END init;

PROCEDURE swap_cursor;
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO 1 DO show_line(i,wnd[i].pos) END
END swap_cursor;

PROCEDURE manage;
  VAR i: INTEGER; ch: CHAR;
BEGIN
  init; frame; scn.on_top(Bars);
  FOR i:=0 TO 1 DO
    show(i,TRUE); scn.on_top(wnd[i].scr);
  END;
  LOOP
    i:=active;
    WITH wnd[active] DO
      env.put_str('CD',wnd[active].path,FALSE);
      chdir(wnd[active].path);
      CASE wnd[active].type OF
        BRIEF,FULL:    dir_move|
        TREE : short_tree_move |
        INFO : spy(ch);
              IF (ch#key.tab)&(ch#033c)
              THEN action(ch) ELSE active:=passive() END;
      ELSE
      END;
      header(0); header(1);
      IF active#i THEN
        scn.on_top(wnd[active].scr); on:=TRUE; swap_cursor;
        str.copy(dst_str,wnd[i].path);
      END
    END;
  END;
END manage;

----------------------------- FINAL ----------------------------
                             -------

PROCEDURE quit;
BEGIN
  ttycbr(noc,black,0); tty.erase(2)
END quit;

CONST PRO_NAME  = 'profile.@';

PROCEDURE up_tty;
  VAR    s: STRING;
      path: ARRAY [0..79] OF CHAR;
      name: ARRAY [0..31] OF CHAR;
BEGIN
  env.get_str(env.tty,s);
  IF env.done THEN
    str.copy(path,s);
    bio.splitpathname(path,name);
    IF bio.done THEN
      str.print(path,"%s_up.@",name);
      com.submit(path,TRUE);
    END
  END
END up_tty;

PROCEDURE home(VAL dir: ARRAY OF CHAR);
  VAR s: STRING;
BEGIN
  NEW(s,str.len(dir)+4);
  IF NOT mem.done THEN HALT(mem.error) END;
  str.print(s,"CD=%s",dir);
  com.system(s,"");  DISPOSE(s);
  IF com.result#0 THEN RETURN END;
  env.put_str(env.home,dir,FALSE);
  IF arg.string('profile',s)
  THEN com.system(s,       "")
  ELSE com.system(PRO_NAME,"")
  END
END home;

PROCEDURE put_user;
  VAR u: usr.USER;
      i: INTEGER;
BEGIN
  usr.unpack(usr.user(),u.usr,u.gro,u.priv);
  usr.get_user(u);
  env.put_str("USER"  ,u.name,FALSE);
  env.put_str(env.info,u.name,FALSE)
END put_user;

PROCEDURE try_go_home;
  VAR args: STRING;
BEGIN
  IF arg.flag('-','p') THEN RETURN END;
  env.get_str('ARGS',args);
  IF str.len(args)>5 THEN
    tty.print('KC: args="%s"\n',args);
    IF arg.string('home',args) THEN
      tty.print('KC: home="%s"\n',args);
      up_tty; put_user; home(args);
      ttycbr(tty.state^.color,tty.state^.min_color,0);
      tty.set_cursor(0); tty.erase(2);
    END;
  END;
END try_go_home;


BEGIN
  tty.nop; key.nop; env.final(quit); try_go_home; manage

------------------------- THAT's ALL!!! ------------------------
                         ---------------
END kc.
