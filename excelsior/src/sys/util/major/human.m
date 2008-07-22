MODULE human; (* Leo  28-Feb-90. (c) KRONOS *)
              (* John 05-May-90. (c) KRONOS *)

IMPORT  SYSTEM;
IMPORT  usr: Users;
IMPORT  env: tskEnv;
IMPORT  bio: BIO;
IMPORT  arg: tskArgs;
IMPORT  str: Strings;
IMPORT  std: StdIO;
IMPORT  tty: Terminal;
IMPORT  kbd: Keyboard;
IMPORT  edt: strEditor;
IMPORT  mem: Heap;
IMPORT       ASCII;


WITH STORAGE: mem;

CONST PASSWD = "PASS.WD";

PROCEDURE wait; VAR ch: CHAR; BEGIN kbd.read(ch) END wait;

PROCEDURE help;
BEGIN
  std.print('\n  "human" user manager utility program    (c) KRONOS\n'
            'usage:\n'
            '     human [-i] [-e]\n\n'
            ' -h: help\n');
  std.print(' -i: load   password file\n'
            ' +c: create password file\n\n'
            '                                   John, May 05 90\n');
  HALT;
END help;

PROCEDURE message(VAL s: ARRAY OF CHAR);
BEGIN
  tty.set_pos(23,0);
  tty.erase_line(2);
  tty.WriteString(s);
  kbd.wait(-1);
  tty.erase_line(2);
END message;

PROCEDURE query(VAL f: ARRAY OF CHAR; SEQ arg: SYSTEM.WORD): BOOLEAN;
  VAR c: CHAR;
BEGIN
  tty.set_pos(23,0);
  tty.erase_line(2);
  tty.print(f,arg);
  kbd.read(c);
  tty.erase_line(2);
  RETURN (c='y') OR (c='Y');
END query;

VAR txt: STRING;
   done: BOOLEAN;
   users : ARRAY [0..127] OF usr.USER;
   groups: ARRAY [0..127] OF usr.USER;
   shells: ARRAY [0..127] OF STRING;

PROCEDURE parser;

  VAR U: usr.USER;
    i,n: INTEGER;
    l,c: INTEGER;

  PROCEDURE syntax(VAL msg: ARRAY OF CHAR);
  BEGIN
    done:=FALSE;
    tty.print('error in "%s" line=%d column=%d\n*** %s ***\n',PASSWD,l,c,msg);
    wait
  END syntax;

  PROCEDURE blanks;
  BEGIN
    WHILE (i<=HIGH(txt)) & (txt[i]=' ') DO c:=c+1; i:=i+1 END
  END blanks;

  PROCEDURE number(VAR n: INTEGER);
  BEGIN
    blanks;
    n:=0;
    IF (txt[i]<'0') OR (txt[i]>'9') THEN syntax('number expected'); RETURN END;
    WHILE ('0'<=txt[i]) & (txt[i]<='9') & (n<=127) DO
      n:=n*10+ORD(txt[i])-ORD('0');
      INC(i); INC(c);
    END;
    IF n>127 THEN syntax('number out of range') END;
  END number;

  PROCEDURE header(stop: CHAR);
    VAR beg,len: INTEGER;
  BEGIN
    c:=c+1; i:=i+1;
    blanks;
    U.priv:=(txt[i]='+');
    IF U.priv THEN INC(i); INC(c) END;
    number(U.usr);
    IF NOT done THEN RETURN END;
    blanks;
    U.gro:=U.usr;
    beg:=i;
    len:=0;
    WHILE (i<=HIGH(txt)) & (txt[i]#stop) DO
      c:=c+1; i:=i+1; len:=len+1
    END;
    IF len>7 THEN syntax('too long name'); RETURN END;
    str.sub_str(U.name,txt,beg,len)
  END header;

  PROCEDURE user;
    VAR ch: CHAR; beg,len: INTEGER; shell: STRING;
  BEGIN
    header(' ');
    INC(i); INC(c);
    number(U.gro);
    IF NOT done THEN RETURN END;
    blanks;
    IF (i>HIGH(txt)) OR (txt[i]#'"') THEN
      syntax('"password" expected'); RETURN
    END;
    c:=c+1; i:=i+1;
    beg:=i;
    len:=0;
    WHILE (i<=HIGH(txt)) & (txt[i]#'"') & (txt[i]#' ') & (txt[i]#ASCII.NL) DO
      c:=c+1; i:=i+1; len:=len+1
    END;
    IF (i>HIGH(txt)) OR (txt[i]#'"') THEN
      syntax('"password" expected'); RETURN
    END;
    IF len>7 THEN syntax('too long "password"'); RETURN END;
    str.sub_str(U.pass,txt,beg,len);
    c:=c+1; i:=i+1; blanks;
    beg:=i;
    len:=0;
    WHILE (i<HIGH(txt)) & (txt[i]#ASCII.NL) DO
      c:=c+1; i:=i+1; len:=len+1
    END;
    IF (len=0) OR (i>HIGH(txt)) THEN
      syntax('shell name expected or illegal'); RETURN
    END;
    NEW(shells[U.usr],len+1);
    str.sub_str(shells[U.usr],txt,beg,len);
    shells[U.usr][len]:=0c;
    users[U.usr]:=U;
    WHILE (i<HIGH(txt)) & (txt[i]#ASCII.NL) & (txt[i]#0c) DO
      c:=c+1; i:=i+1
    END
  END user;

  PROCEDURE group;
  BEGIN
    U.pass:="";
    header(ASCII.NL);
    U.usr:=0;
    IF done THEN groups[U.gro]:=U; END
  END group;

BEGIN
  i:=0; l:=0; c:=0;
  WHILE i<HIGH(txt) DO
    CASE txt[i] OF
      |'G': group
      |'U': user
    ELSE
      syntax("'U' or 'G' expected");
    END;
    blanks;
    IF NOT done THEN RETURN END;
    IF (i<=HIGH(txt)) & (txt[i]#ASCII.NL) & (txt[i]#0c) THEN
      syntax("illegal syntax"); RETURN
    ELSE
      INC(i); INC(l); c:=0
    END
  END
END parser;

PROCEDURE io_error(VAL op: ARRAY OF CHAR);
BEGIN
  done:=FALSE;
  tty.set_pos(23,0);
  tty.perror(bio.error,"%s(%s): %%s\n",op,PASSWD);
END io_error;

PROCEDURE read_passwd;
  VAR f: bio.FILE; ETC: bio.PATHs; g,u: INTEGER;
BEGIN
  done:=TRUE;
  NEW(txt,0);
  IF NOT done THEN RETURN END;
  bio.get_paths(ETC,env.etc);
  IF NOT bio.done THEN io_error('get_paths'); ETC:=bio.here END;
  bio.lookup(ETC,f,PASSWD,'r');
  IF NOT bio.done THEN io_error('lookup'); RETURN END;
  bio.get_attr(f,bio.a_gid,g);
  IF NOT bio.done THEN io_error('lookup'); RETURN END;
  bio.get_attr(f,bio.a_uid,u);
  IF NOT bio.done THEN io_error('lookup'); RETURN END;
  IF (u MOD 128 # 0) OR (g MOD 128 # 0) THEN
    tty.print('"%s" do not belong to SU!\n',PASSWD); RETURN
  END;
  NEW(txt,bio.eof(f)+1);
  bio.get(f,txt,bio.eof(f));
  IF NOT bio.done THEN io_error('read'); RETURN END;
  txt[HIGH(txt)]:=0c;
  bio.close(f);
  IF NOT bio.done THEN io_error('close') END;
  bio.close_paths(ETC);
  IF NOT bio.done THEN io_error('close_paths') END;
  parser
END read_passwd;

PROCEDURE set_passwd;
  VAR i: INTEGER;
BEGIN
  read_passwd;
  FOR i:=0 TO HIGH(groups) DO
    IF (groups[i].gro=i) & (groups[i].name#"") THEN
      usr.set_group(groups[i])
    END
  END;
  FOR i:=0 TO HIGH(groups) DO
    IF (users[i].gro>=0) & (users[i].usr=i) & (users[i].name#"") THEN
      usr.set_user(users[i],shells[i])
    END
  END
END set_passwd;

PROCEDURE write_passwd;
  VAR f: bio.FILE; ETC: bio.PATHs;
      i: INTEGER;
    out,p: ARRAY [0..255] OF CHAR;
BEGIN
  done:=TRUE;
  bio.get_paths(ETC,env.etc);
  IF NOT bio.done THEN io_error('get_paths'); ETC:=bio.here END;
  bio.lookup(ETC,f,PASSWD,'w');
  IF NOT bio.done THEN io_error('lookup'); RETURN END;
  FOR i:=0 TO HIGH(groups) DO
    WITH groups[i] DO
      IF (gro=i) & (name#"") THEN
        str.print(out,'G    %03d %s%c',gro,name,ASCII.NL);
        IF priv THEN out[2]:='+' END;
        bio.put(f,out,str.len(out));
        IF NOT bio.done THEN io_error('put'); bio.purge(f); RETURN END;
      END
    END
  END;
  FOR i:=0 TO HIGH(users) DO
    WITH users[i] DO
      IF (usr=i) & (name#"") & (gro>=0) THEN
        str.print(p,'"%s"',pass);
        str.print(out,'U    %03d %-8s %3d %-9s %s%c',
                            usr ,name,gro,p    ,shells[i],ASCII.NL);
        IF priv THEN out[2]:='+' END;
        bio.put(f,out,str.len(out));
        IF NOT bio.done THEN io_error('put'); bio.purge(f); RETURN END;
      END
    END
  END;
  IF bio.pos(f)<bio.eof(f) THEN bio.cut(f,bio.pos(f)) END;
  IF NOT bio.done THEN io_error('cut') END;
  bio.close(f);
  IF NOT bio.done THEN io_error('close') END;
  bio.close_paths(ETC);
  IF NOT bio.done THEN io_error('close_paths') END;
END write_passwd;

PROCEDURE FINAL;
BEGIN
  tty.set_pos(0,0); tty.erase(0);
  tty.set_pos(tty.state^.lines-3,0);
  tty.print('execute "human -i" to update changes into working system\n');
END FINAL;

PROCEDURE editor;

  TYPE LINE = ARRAY [0..19] OF CHAR;

  CONST hL=10; hC=20;

  PROCEDURE readstr(VAR s: ARRAY OF CHAR; echo: BOOLEAN);
    VAR i: INTEGER;
       ch: CHAR;
  BEGIN
    i:=0;
    REPEAT
      kbd.read(ch);
      IF (ch>40c) & (ch<177c) THEN
        s[i]:=ch; INC(i);
        IF echo THEN tty.print("%c",ch) END;
      END;
    UNTIL (ch=ASCII.CR) OR (i=HIGH(s));
    s[i]:=0c;
  END readstr;

  PROCEDURE frame(l,c,h,w: INTEGER);
    VAR o: ARRAY [0..83] OF CHAR;
        i: INTEGER;
  BEGIN
    o[0]:=tty.state^.bars[0,0];
    FOR i:=0 TO w-2 DO o[i+1]:=tty.state^.hbar END;
    o[w]:=tty.state^.bars[0,2];
    o[w+1]:=0c;
    tty.set_pos(l,c); tty.WriteString(o);
    o[0]:=tty.state^.vbar;
    FOR i:=0 TO w-2 DO o[i+1]:=' ' END;
    o[w]:=tty.state^.vbar;
    o[w+1]:=0c;
    FOR i:=0 TO h-2 DO
      tty.set_pos(l+i+1,c); tty.WriteString(o)
    END;
    o[0]:=tty.state^.bars[2,0];
    FOR i:=0 TO w-2 DO o[i+1]:=tty.state^.hbar END;
    o[w]:=tty.state^.bars[2,2];
    o[w+1]:=0c;
    tty.set_pos(l+h,c); tty.WriteString(o);
  END frame;

  PROCEDURE edr_help();
  BEGIN
    frame(hL,hC,6,34);
    tty.set_pos(hL  ,hC+7); tty.print(' QUICK human HELP ');
    tty.set_pos(hL+1,hC+1); tty.print(' CR,LF,NL,SPACE - to select item ');
    tty.set_pos(hL+2,hC+1); tty.print(' ESCAPE         - exit           ');
    tty.set_pos(hL+3,hC+1); tty.print('                                 ');
    tty.set_pos(hL+4,hC+1); tty.print(' To delete item - empty name     ');
    tty.set_pos(hL+5,hC+1); tty.print(' To create item - enter new name ');
    wait;
  END edr_help;

  PROCEDURE enter(VAL f: ARRAY OF LINE; L,C: INTEGER);
    VAR i: INTEGER;
  BEGIN
    frame(L,C,HIGH(f)+2,HIGH(f[0]));
    FOR i:=0 TO HIGH(f) DO
      tty.set_pos(L+1+i,C+1);
      tty.WriteString(f[i])
    END
  END enter;

  PROCEDURE select(VAL f: ARRAY OF LINE;
                   L,C: INTEGER; VAR ln: INTEGER): BOOLEAN;
    VAR ch : CHAR; hlp: BOOLEAN;
  BEGIN
    tty.set_cursor(0);
    hlp:=FALSE;
    LOOP
      tty.set_pos(L+1+ln,C+1);
      tty.set_reverse(1);
      tty.WriteString(f[ln]);
      tty.set_reverse(0);
      kbd.read(ch);
      CASE ch OF
      |kbd.f1   : hlp:=TRUE; EXIT
      |ASCII.ESC: ln:=-1; EXIT
      |kbd.up,kbd.dw:
        tty.set_pos(L+1+ln,C+1);
        tty.WriteString(f[ln]);
        IF ch=kbd.up THEN ln:=(ln+HIGH(f)+1-1) MOD (HIGH(f)+1);
        ELSE              ln:=(ln+HIGH(f)+1+1) MOD (HIGH(f)+1);
        END
      |ASCII.CR,ASCII.LF,ASCII.NL,' ': EXIT
      ELSE
      END
    END;
    tty.set_cursor(1);
    RETURN hlp
  END select;

  PROCEDURE edit_line(VAR s: ARRAY OF CHAR; l,c,w: INTEGER);
    VAR ou: ARRAY [0..79] OF CHAR; pos: INTEGER;
        ch: CHAR; done,edi: BOOLEAN;
  BEGIN
    FOR pos:=0 TO w-1 DO ou[pos]:=' ' END;
    ou[w]:=0c;
    s[w]:=0c;
    tty.set_pos(l,c); tty.WriteString(ou);
    tty.set_pos(l,c); tty.WriteString(s);
    edi:=FALSE;
    pos:=0;
    LOOP
      kbd.read(ch);
      CASE ch OF
      |ASCII.ESC: RETURN
      |ASCII.LF,ASCII.CR,ASCII.NL : EXIT
      |kbd.back :
        IF edi THEN
          IF pos>0 THEN DEC(pos);
            tty.set_pos(l,c+pos); tty.Write(' '); tty.set_pos(l,c+pos)
          END
        ELSE
          tty.set_pos(l,c); tty.WriteString(ou); tty.set_pos(l,c);
          edi:=TRUE
        END
      ELSE
        IF (ORD(ch)>40b) & (ORD(ch)<177b) THEN
          IF edi THEN
            IF pos<w THEN tty.Write(ch); ou[pos]:=ch; INC(pos)
            ELSE tty.Write(7c);
            END
          ELSE edi:=TRUE;
            ou[0]:=ch; pos:=1;
            tty.set_pos(l,c); tty.WriteString(ou); tty.set_pos(l,c+1);
          END
        END
      END
    END;
    IF edi THEN done:=FALSE;
      FOR l:=0 TO pos-1 DO IF ou[l]#' ' THEN done:=TRUE END END;
      IF done THEN
        FOR l:=0 TO pos-1 DO s[l]:=ou[l] END;
        s[pos]:=0c
      ELSE s:=""
      END
    END
  END edit_line;

  PROCEDURE edit_shell(VAR s: STRING);
    VAR dsk: edt.descriptor; out: ARRAY [0..1023] OF CHAR;
        i,l: INTEGER;
  BEGIN
    tty.set_cursor(1);
    edt.new(dsk,1);
    l:=str.len(s);
    FOR i:=0 TO l-1 DO out[i]:=s[i] END;
    out[l]:=0c;
    dsk^.how:=edt.confirm;
    edt.edit_str('SHELL: ',out,23,0,78,dsk,33c);
    IF dsk^.last#33c THEN
      l:=str.len(out);
      IF l>HIGH(out) THEN l:=HIGH(out) END;
      RESIZE(s,l+1);
      FOR i:=0 TO l-1 DO s[i]:=out[i] END;
      s[l]:=0c;
    END;
    edt.dispose(dsk);
    tty.set_pos(23,0);
    tty.erase_line(2);
    tty.set_cursor(0);
  END edit_shell;

  VAR U: usr.USER;

  PROCEDURE us;
    CONST L=10; C=25;

    VAR f: ARRAY [0..3] OF LINE;

    PROCEDURE e(l: INTEGER);
    BEGIN
      tty.set_cursor(1);
      tty.set_pos(L+1+l,C+1);
      tty.WriteString(f[l]);
      IF l=0 THEN
        edit_line(U.name,L+1,C+12,7);
        str.print(f[0],'name       %7s',U.name);
      ELSE
        edit_line(U.pass,L+2,C+12,7);
        str.print(f[1],'password   %7s',U.pass);
      END;
      tty.set_cursor(0);
    END e;

    PROCEDURE make;
    BEGIN
      str.print(f[0],'name       %7s',U.name);
      str.print(f[1],'password   %7s',U.pass);
      str.print(f[2],'shell             ');
      str.print(f[3],'UPDATE & EXIT     ');
    END make;

    VAR shell: ARRAY [0..255] OF CHAR;
        ch   : CHAR;
        line : INTEGER;
  BEGIN
    U.pass:="";
    make;
    enter(f,L,C);
    read_passwd;
    U:=users[U.usr];
    make;
    enter(f,L,C);
    line:=0;
    LOOP
      IF select(f,L,C,line) THEN
        edr_help;
        tty.erase(2);
        enter(f,L,C);
      ELSIF line=-1 THEN EXIT
      ELSE
        CASE line OF
        |0,1: e(line);
          IF U.name="" THEN
         message('Empty name deletes user. To delete yourself call superuser.');
            U.name:=users[U.usr].name;
          END;
          IF U.pass="" THEN U.priv:=FALSE END;
        |2  : edit_shell(shells[U.usr]);
        |3  : users[U.usr]:=U; write_passwd; EXIT
        END;
        make
      END
    END
  END us;

  PROCEDURE su;
    CONST BASE_LINE=3; BASE_COL = 0;

    PROCEDURE sh_node(VAL u: ARRAY OF usr.USER; no: INTEGER);
      VAR ou: ARRAY [0..15] OF CHAR;
    BEGIN
      str.print(ou,'%-8s',u[no].name);
      tty.set_pos(BASE_LINE+1+no MOD 16,BASE_COL + 1 + (no DIV 16)*9);
      tty.WriteString(ou);
    END sh_node;

    PROCEDURE sh_f(L,C,H,W: INTEGER);
      VAR l,c: INTEGER; ch: CHAR;
    BEGIN
      FOR l:=L TO L+H-1 DO
        tty.set_pos(l,C);
        FOR c:=C TO C+W-1 DO
          IF c MOD 9=0 THEN ch:=tty.state^.vbar ELSE ch:=' ' END;
          tty.Write(ch)
        END
      END;
    END sh_f;

    PROCEDURE restore(VAL u: ARRAY OF usr.USER; L,C,H,W: INTEGER);
      VAR l,c,i: INTEGER;
    BEGIN
      sh_f(L,C,H,W);
      FOR i:=0 TO HIGH(u) DO
        l:=BASE_LINE + 1 + (i MOD 16);
        c:=BASE_COL  + 1 + (i DIV 16)*9;
        IF (l>=L) & (l<L+H) & (c<C+W) & (c+8>=C) THEN sh_node(u,i) END
      END
    END restore;

    PROCEDURE edr_group(no,L,C: INTEGER);
      VAR f: ARRAY [0..2] OF LINE;
          G: usr.USER;

      PROCEDURE make;
      BEGIN
        str.print(f[0],'name       %7s',G.name);
        str.print(f[1],'privileges     ');
        IF G.priv THEN str.append(f[1],' ON')
        ELSE           str.append(f[1],'OFF')
        END;
        str.print(f[2],'  UPDATE & EXIT   ');
      END make;

      VAR ln: INTEGER; ch: CHAR;
    BEGIN
      G:=groups[no];
      make;
      enter(f,L,C);
      ln:=0;
      LOOP
        IF select(f,L,C,ln) THEN
          edr_help;
          restore(groups,hL,hC,7,35);
          enter(f,L,C);
        ELSIF ln=-1 THEN EXIT
        ELSE
          CASE ln OF
          |0:
            tty.set_pos(L+1,C+1);
            tty.WriteString(f[0]);
            edit_line(G.name,L+1,C+12,7);
            IF (G.gro=0) & (G.name="") THEN
              message('First group MUST be not empty. Try enother one.');
              G.name:=groups[0].name;
            END;
          |1: G.priv:=NOT G.priv
          |2: groups[no]:=G; EXIT
          END;
          make
        END
      END;
    END edr_group;

    PROCEDURE edr_user(no,L,C: INTEGER);

      PROCEDURE free(): INTEGER;
        VAR i: INTEGER;
      BEGIN
        FOR i:=0 TO HIGH(groups) DO
          IF groups[i].name="" THEN RETURN i END;
        END;
        RETURN -1
      END free;

      PROCEDURE find(VAL s: ARRAY OF CHAR; VAR g: INTEGER);
        VAR i: INTEGER;
      BEGIN
        g:=-1;
        FOR i:=0 TO HIGH(groups) DO
          IF groups[i].name=s THEN g:=i; RETURN END
        END
      END find;

      VAR f: ARRAY [0..5] OF LINE;
          U: usr.USER;

      PROCEDURE make;
      BEGIN
        str.print(f[0],'name       %7s',U.name);
        str.print(f[1],'password   %7s',U.pass);
        str.print(f[2],'group      %7s',groups[U.gro].name);
        str.print(f[3],'privileges     ');
        IF U.priv THEN str.append(f[3],' ON')
        ELSE           str.append(f[3],'OFF')
        END;
        str.print(f[4],'shell             ');
        str.print(f[5],'  UPDATE & EXIT   ');
      END make;

      VAR gro,ln: INTEGER; ch: CHAR; group: ARRAY [0..7] OF CHAR;
    BEGIN
      U:=users[no];
      make;
      enter(f,L,C);
      ln:=0;
      LOOP
        IF select(f,L,C,ln) THEN
          edr_help;
          restore(users,hL,hC,7,35);
          enter(f,L,C);
        ELSIF ln<0 THEN EXIT
        ELSE
          CASE ln OF
          |0,1:
            tty.set_pos(L+1+ln,C+1);
            tty.WriteString(f[ln]);
            IF ln=0 THEN edit_line(U.name,L+1+ln,C+12,7);
              IF (U.usr=0) & (U.name="") THEN
                message('First user MUST be not empty. Try enother one.');
                U.name:=users[0].name;
              END;
            ELSE         edit_line(U.pass,L+1+ln,C+12,7);
              IF U.pass="" THEN
                U.priv:=FALSE;
                make;
                tty.set_pos(L+1+3,C+1);
                tty.WriteString(f[3]);
              END
            END
          |2:
            tty.set_pos(L+3,C+1);
            tty.WriteString(f[2]);
            str.print(group,'%s',groups[U.gro].name);
            edit_line(group,L+3,C+12,7);
            IF group="" THEN
              message('Please, enter full group name.')
            ELSE
              find(group,gro);
              IF gro<0 THEN
                IF query('Group "%s" is absend. Create?',group) THEN
                  gro:=free();
                  IF gro<0 THEN
              message('There are no free group here. Clean one to create enather.');
                  ELSE
                    groups[gro].name:=group;
                    groups[gro].priv:=FALSE;
                    groups[gro].gro :=gro;
                    U.gro:=gro;
                  END
                END
              ELSE U.gro:=gro
              END
            END
          |3: IF NOT U.priv & (U.pass="") THEN
                message('Make password BEFORE setting privileges !!!')
              ELSE
                U.priv:=NOT U.priv
              END;
          |4: edit_shell(shells[U.usr]);
          |5: users[U.usr]:=U; EXIT
          END;
          make
        END
      END;
      IF U.name='' THEN RESIZE(shells[U.usr],0) END
    END edr_user;

    PROCEDURE edit(VAR u: ARRAY OF usr.USER; user: BOOLEAN);
      CONST L=10; C=25;
      VAR ln: INTEGER; ch: CHAR;
    BEGIN
      tty.set_reverse(0);
      FOR ln:=0 TO HIGH(u) DO sh_node(u,ln) END;
      ln:=0;
      LOOP
        tty.set_reverse(1);
        sh_node(u,ln);
        tty.set_reverse(0);
        kbd.read(ch);
        CASE ch OF
        |kbd.f1   : edr_help; restore(u,hL,hC,7,35);
        |ASCII.ESC: sh_node(u,ln); EXIT
        |ASCII.CR,ASCII.LF,ASCII.NL,' ':
          IF user THEN
            edr_group(ln,L,C);
            restore(u,L,C,5,21)
          ELSE
            edr_user(ln,L,C);
            restore(u,L,C,8,21)
          END;
          tty.set_cursor(0);
        |kbd.up   : sh_node(u,ln); ln:=(ln+128 - 1  ) MOD 128;
        |kbd.dw   : sh_node(u,ln); ln:=(ln+128 + 1  ) MOD 128;
        |kbd.right: sh_node(u,ln); ln:=(ln+128 + 16 ) MOD 128;
        |kbd.left : sh_node(u,ln); ln:=(ln+128 - 16 ) MOD 128;
        ELSE
        END;
      END;
    END edit;

    CONST L=0; C=11;

    VAR main: ARRAY [0..2] OF ARRAY [0..15] OF CHAR;

    PROCEDURE frame;
      VAR i: INTEGER;
         ou: ARRAY [0..83] OF CHAR;

    BEGIN
      FOR i:=0 TO 49 DO ou[i]:=tty.state^.hbar END;
      ou[16]:=tty.state^.bars[0,1]; ou[32]:=tty.state^.bars[0,1];
      ou[ 0]:=tty.state^.bars[0,0]; ou[48]:=tty.state^.bars[0,2];
      ou[49]:=0c;
      tty.set_pos(L  ,C);   tty.WriteString(ou);
      ou[16]:=tty.state^.bars[2,1]; ou[32]:=tty.state^.bars[2,1];
      ou[ 0]:=tty.state^.bars[2,0]; ou[48]:=tty.state^.bars[2,2];
      ou[49]:=0c;
      tty.set_pos(L+2,C);    tty.WriteString(ou);
      tty.set_pos(L+1,C   ); tty.Write(tty.state^.vbar);
      tty.set_pos(L+1,C+16); tty.Write(tty.state^.vbar);
      tty.set_pos(L+1,C+32); tty.Write(tty.state^.vbar);
      tty.set_pos(L+1,C+48); tty.Write(tty.state^.vbar);
      FOR i:=0 TO HIGH(main) DO
        tty.set_pos(L+1,C+1+i*16);  tty.WriteString(main[i])
      END;

      FOR i:=0 TO HIGH(ou) DO ou[i]:=' ' END;
      FOR i:=0 TO 8 DO ou[i*9]:=tty.state^.vbar END;
      ou[80]:=0c;
      FOR i:=0 TO 15 DO
        tty.set_pos(BASE_LINE+1+i,BASE_COL);
        tty.WriteString(ou)
      END;

      FOR i:=0 TO 71 DO ou[i]:=tty.state^.hbar END;
      FOR i:=0 TO 6 DO ou[(i+1)*9]:=tty.state^.bars[0,1] END;
      ou[ 0]:=tty.state^.bars[0,0]; ou[72]:=tty.state^.bars[0,2]; ou[80]:=0c;
      tty.set_pos(BASE_LINE,BASE_COL); tty.WriteString(ou);
      FOR i:=0 TO 6 DO ou[(i+1)*9]:=tty.state^.bars[2,1] END;
      ou[ 0]:=tty.state^.bars[2,0]; ou[72]:=tty.state^.bars[2,2]; ou[80]:=0c;
      tty.set_pos(BASE_LINE+17,BASE_COL); tty.WriteString(ou);
    END frame;

    VAR ln: INTEGER; ch: CHAR;
  BEGIN
    main[0]:=' users   table ';
    main[1]:=' groups  table ';
    main[2]:=' UPDATE & EXIT ';
    frame;
    ln:=0;
    read_passwd;
    tty.set_cursor(0);
    LOOP
      tty.set_reverse(1);
      tty.set_pos(L+1,C+1+ln*16);
      tty.WriteString(main[ln]);
      tty.set_reverse(0);
      kbd.read(ch);
      CASE ch OF
      |kbd.f1   : edr_help; sh_f(hL,hC,7,35);
      |kbd.right: tty.set_pos(L+1,C+1+ln*16); tty.WriteString(main[ln]);
                  ln:=(ln+1) MOD 3;
      |kbd.left : tty.set_pos(L+1,C+1+ln*16); tty.WriteString(main[ln]);
                  ln:=(ln+2) MOD 3;
      |ASCII.CR,ASCII.LF,ASCII.NL,' ':
        IF    ln=2 THEN write_passwd; EXIT
        ELSIF ln=0 THEN edit(users,FALSE)
        ELSIF ln=1 THEN edit(groups,TRUE)
        ELSE ASSERT(FALSE)
        END
      |ASCII.ESC: EXIT
      ELSE
      END
    END;
    tty.set_cursor(1);
  END su;

  VAR i: INTEGER;
    bad: BOOLEAN;
   pass: ARRAY [0..7] OF CHAR;

BEGIN
  tty.print('\nusername: '); readstr(U.name,TRUE);
  bad:=FALSE;
  usr.find(U);
  IF NOT U.done THEN bad:=TRUE END;
  IF U.done THEN
    usr.get_user(U);
    IF NOT U.done THEN bad:=TRUE END
  END;
  i:=0;
  REPEAT
    tty.print('\npassword: '); readstr(pass,FALSE);
    INC(i);
  UNTIL NOT bad & (pass=U.pass) OR (i=3);
  IF bad OR (pass#U.pass) THEN RETURN END;
  env.final(FINAL);
  tty.set_pos(0,0); tty.erase(0);
  IF (U.gro=0) & (U.usr=0) THEN su ELSE us END;
END editor;

PROCEDURE create;
  VAR f: bio.FILE;
BEGIN
  bio.create(f,PASSWD,'w',0);
  IF NOT bio.done THEN io_error('create'); RETURN END;
  bio.close(f);
  IF NOT bio.done THEN io_error('close') END;
  groups[0].name:='root';
  WITH users[0] DO
    name:='su';
    pass:='X';
    priv:=TRUE;
  END;
  write_passwd;
END create;

PROCEDURE ini;
  VAR i: INTEGER; U: usr.USER;
BEGIN
  WITH U DO
    gro:=0;   usr:=0;
    name:=''; pass:='';
    priv:=FALSE;
  END;
  FOR i:=0 TO HIGH(users) DO
    users [i]:=U; users [i].usr:=i;
    groups[i]:=U; groups[i].gro:=i;
    NEW(shells[i],0)
  END;
END ini;

BEGIN
  ini;
  IF arg.flag('-','h')  THEN help;       HALT END;
  IF arg.flag('-','i')  THEN set_passwd; HALT END;
  IF arg.flag('+','c')  THEN create;     HALT END;
  editor
END human.
