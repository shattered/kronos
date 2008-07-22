IMPLEMENTATION MODULE pedEdit; (* Leo 11-Mar-87. (c) KRONOS *)

FROM ASCII     IMPORT   DC4;
IMPORT  m  : pedMouse;
IMPORT  tty: Terminal;
IMPORT  kbd: Keyboard;
IMPORT  str: Strings;

PROCEDURE control(ch: CHAR): BOOLEAN;
BEGIN RETURN ORD(ch) MOD 128 < 40b END control;

VAR readkey: readproc;

PROCEDURE pos(from, to: INTEGER);
BEGIN
  IF from=to THEN RETURN END;
  IF from>to THEN tty.left(from-to) ELSE tty.right(to-from) END;
END pos;

PROCEDURE ReadValid(VAR s: ARRAY OF CHAR; vp: ValidProc; how: How);
  VAR a: Action;      ins,first: BOOLEAN;
    key: CHAR;
    i,l: INTEGER;
  sp,cp: INTEGER;  (* sp -- string pos   cp -- cursor pos *)

  PROCEDURE moveleft;  FORWARD;
  PROCEDURE moveright; FORWARD;

  PROCEDURE _?(): BOOLEAN;
  BEGIN RETURN control(s[sp]) END _?;

  PROCEDURE w(ch: CHAR);
  BEGIN
    IF control(ch) THEN
      tty.Write("^"); tty.Write(CHAR(ORD(ch)+100b)); INC(cp,2)
    ELSE
      tty.Write(ch); INC(cp);
    END;
  END w;

  PROCEDURE refresh;
    VAR i,p: INTEGER;
  BEGIN
    pos(cp,0);  p:=cp;  cp:=0;
    FOR i:=0 TO l-1 DO w(s[i]) END; tty.erase_line(0);
    pos(cp,p); cp:=p;
  END refresh;

  PROCEDURE insert(ch: CHAR);
    VAR p: INTEGER;
  BEGIN
    IF sp=l THEN
      str.append(s,'%c',ch); l:=str.len(s);
    ELSE
      str.insert(s,sp,1); s[sp]:=ch; l:=str.len(s);
      IF _?() THEN tty.ins_char(2) ELSE tty.ins_char(1) END;
    END;
    w(ch);
    IF _?() THEN p:=cp-2 ELSE p:=cp-1 END;
    pos(cp,p); cp:=p;
  END insert;

  PROCEDURE delete;
    VAR p: INTEGER; ?: BOOLEAN;
  BEGIN
    IF sp=l THEN RETURN END;
    ?:=_?();
    IF sp=l-1 THEN w(' ');
      IF ? THEN w(' '); p:=cp-2 ELSE p:=cp-1 END;
      pos(cp,p); cp:=p; DEC(l); s[l]:=0c; RETURN
    END;
    str.delete(s,sp,1); l:=str.len(s);
    IF ? THEN tty.del_char(2) ELSE tty.del_char(1) END;
  END delete;

  PROCEDURE putch(ch: CHAR);
  BEGIN
    IF sp>=HIGH(s)-2 THEN RETURN END;
    IF ins THEN insert(ch); moveright; RETURN END;
    IF (sp<l) & (_?()#control(ch)) THEN
      IF _?() THEN delete; insert(' ');
      ELSE delete; insert(ch); moveright; RETURN
      END;
    END;
    w(ch);
    IF sp=l THEN str.append(s,'%c',ch); l:=str.len(s); sp:=l;
    ELSE s[sp]:=ch; INC(sp)
    END;
  END putch;

  PROCEDURE moveleft;
    VAR p: INTEGER;
  BEGIN
    IF sp=0 THEN RETURN END;
    DEC(sp);
    IF _?() THEN p:=cp-2 ELSE p:=cp-1 END;
    pos(cp,p); cp:=p
  END moveleft;

  PROCEDURE moveright;
    VAR p: INTEGER;
  BEGIN
    IF sp>=HIGH(s)-2 THEN RETURN END;
    IF sp=l THEN
      str.append(s,' ');
      IF l<str.len(s) THEN w(' '); INC(l); INC(sp) END;
      RETURN
    END;
    IF sp<l THEN w(s[sp]);
    ELSE w(' ')
    END;
    INC(sp);
  END moveright;

  PROCEDURE delch;
  BEGIN
    IF sp=0 THEN RETURN  END;
    moveleft;
    IF NOT ins THEN
      putch(' '); moveleft
    ELSE  delete
    END;
  END delch;

  PROCEDURE tab(left: BOOLEAN);
  BEGIN
    IF left THEN moveleft ELSE moveright END;
    WHILE (sp MOD 8 # 0) & (sp<HIGH(s)-2) DO
      IF left THEN moveleft ELSE moveright END;
    END;
  END tab;

  PROCEDURE end;
    VAR i: INTEGER;
  BEGIN
    FOR i:=sp TO l-1 DO w(s[i]) END; sp:=l;
  END end;

  PROCEDURE Lpull;
  BEGIN
    IF sp=l THEN RETURN END;
    WHILE (sp<l) & (s[sp]=' ') DO
      str.delete(s,sp,1);
    END;
    l:=str.len(s); refresh;
  END Lpull;

  PROCEDURE Rpull;
    VAR i: INTEGER;
  BEGIN i:=0;
    WHILE (i<sp) & (s[i]=' ') DO INC(i) END;
    IF i>=sp-1 THEN RETURN END;
    str.insert(s,i,sp-i); l:=str.len(s);
    refresh
  END Rpull;

  PROCEDURE interpret;
    PROCEDURE r; BEGIN IF first THEN refresh; first:=FALSE END END r;
  BEGIN
    CASE key OF
--    |kbd.eraln  : l:=sp; Truncate(s,l); ClearLine;
--    |kbd.ltab   : tab(TRUE)
--    |kbd.rtab   : tab(FALSE)
--    |kbd.lpull  : Lpull
--    |kbd.rpull  : Rpull
    |kbd.del    : delch;
    |kbd.left   : moveleft
    |kbd.right  : moveright
--    |kbd.insc   : insert(' '); r;
--    |kbd.delc   : delete; r;
--    |kbd.uppg   : pos(cp,0); cp:=0; sp:=0; r;
--    |kbd.dwpg   : end
--    |kbd.ins_rep: ins:=NOT ins;
    ELSE IF NOT control(key) THEN putch(key) END;
    END;
  END interpret;

(*
PROCEDURE ReadValid(VAR s: ARRAY OF CHAR; vp: ValidProc; how: How);
  VAR a: Action;      ins,first: BOOLEAN;
    key: CHAR;
    i,l: INTEGER;
  sp,cp: INTEGER;  (* sp -- string pos   cp -- cursor pos *)

*)

BEGIN
  ins:=FALSE; first:=(how=old);
  IF how=dummy THEN s:='' END;
  l:=str.len(s); cp:=0; sp:=0;
  IF (how#dummy) & (how#old) THEN
    FOR i:=0 TO l-1 DO w(s[i]) END; sp:=l;
  END;
  IF how=show  THEN pos(cp,0); cp:=0; sp:=0 END;
  LOOP
    key:=readkey();
    a:=vp(key,sp,s);
    CASE a OF
      |term    : EXIT
      |validkey: interpret
      |validch : putch(key)
      |invalid :
      |bell    : tty.Write(7c)
    ELSE
    END;
  END;
END ReadValid;

PROCEDURE StandardValid(VAR k: CHAR; p: INTEGER; VAR s: ARRAY OF CHAR): Action;
  VAR i: INTEGER;
BEGIN
  IF (k=kbd.cr) OR (k=kbd.lf) OR (k=kbd.newln) THEN
    IF p<str.len(s) THEN s[p]:=0c;
      FOR i:=0 TO str.len(s)-p-1 DO tty.Write(' ') END;
      FOR i:=0 TO str.len(s)-p-1 DO tty.left(1) END;
    END;
    RETURN term
  ELSE RETURN validkey
  END;
END StandardValid;

PROCEDURE Confirm(VAL Ask: ARRAY OF CHAR; VAR s: ARRAY OF CHAR);
BEGIN
  tty.WriteString(Ask);
  ReadValid(s,StandardValid,confirm);
END Confirm;

PROCEDURE ReadString(VAL Ask: ARRAY OF CHAR; VAR s: ARRAY OF CHAR);
BEGIN
  tty.WriteString(Ask);
  ReadValid(s,StandardValid,old);
END ReadString;

PROCEDURE SetRead(read: readproc);
BEGIN readkey:=read END SetRead;

BEGIN readkey:=m.Read; (* pedMouse.Read *)
END pedEdit.
