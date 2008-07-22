MODULE diff;  (* Leo 06-Feb-86. (c) KRONOS *)
              (* Ned 19-Dec-89. (c) KRONOS (Excelsior iV) *)
              (* Leo 17-Mar-90. (c) KRONOS *)

IMPORT       SYSTEM;
IMPORT  str: Strings;
IMPORT  bio: BIO;
IMPORT  std: StdIO;
IMPORT  tty: Terminal;
IMPORT  key: Keyboard;
IMPORT args: tskArgs;

TYPE ADDRESS = SYSTEM.ADDRESS;

VAR
  wait: BOOLEAN;
  log : BOOLEAN;

PROCEDURE io_error(VAL s: ARRAY OF CHAR);
BEGIN
  tty.perror(bio.error,'"%s" %%s "%s"\n',s,bio.ename);
END io_error;

PROCEDURE comp(pos,len: INTEGER; VAL b1,b2: ARRAY OF CHAR; VAR difs: INTEGER);

  VAR s: ARRAY [0..79] OF CHAR; p,i,l: INTEGER;

  PROCEDURE vis(VAL b,x: ARRAY OF CHAR);
    VAR n,sp: INTEGER;
  BEGIN sp:=i+7-l;
    FOR n:=i TO l DO
      str.image(s,p,'%02h',b[n]);
      IF b[n]#x[n] THEN s[p]:='.' ELSE s[p]:=' ' END;
      INC(p);
    END;
    str.image(s,p,'%.*c|',sp*3,' ');
    FOR n:=i TO l DO
      IF ORD(b[n]) MOD 128 >=32 THEN s[p]:=b[n] ELSE s[p]:='_' END;
      INC(p);
    END;
    str.image(s,p,'%.*c',sp,' ');
  END vis;

  VAR equ: BOOLEAN;  n: INTEGER;
       p1: ADDRESS; p2: ADDRESS;  end: ADDRESS;

BEGIN
  IF NOT log THEN
    end:=SYSTEM.ADR(b1) + len DIV 4 -1;
    p1:=SYSTEM.ADR(b1)-1;
    p2:=SYSTEM.ADR(b2)-1;
    REPEAT INC(p1); INC(p2) UNTIL BITSET(p1^#p2^)+BITSET(p1=end)#{};
    i:=(p1-SYSTEM.ADR(b1)) DIV 8 * 8;
  ELSE i:=0
  END;
  FOR i:=i TO len-1 BY 8 DO
    equ:=TRUE;
    IF i+7<len THEN l:=i+7 ELSE l:=len-1 END;
    FOR n:=i TO l DO
      IF b1[n]#b2[n] THEN INC(difs); equ:=FALSE END;
    END;
    IF log OR NOT equ THEN
      p:=0;
      str.image(s,p,'%05h: ',pos+i);
      vis(b1,b2);
      IF equ THEN str.image(s,p,' | ') ELSE str.image(s,p,' # ') END;
      vis(b2,b1);
      std.print('%s\n',s);
    END
  END
END comp;

CONST size=16*1024;

VAR b1,b2: ARRAY [0..size-1] OF CHAR;

PROCEDURE _compare(VAL s1,s2: ARRAY OF CHAR; VAR f1,f2: bio.FILE);
  VAR eof,len,pos,difs: INTEGER; dir: bio.FILE;
    path,name: ARRAY [0..255] OF CHAR;
BEGIN
  bio.open(f1,s1,'r');
  IF NOT bio.done THEN io_error(s1); RETURN END;
  std.print('[%6d] %s\n',bio.eof(f1),s1);
  bio.open(f2,s2,'r');
  IF NOT bio.done THEN io_error(s2); RETURN END;
  IF bio.kind(f2)*bio.is_dir#{} THEN
    str.copy(path,s1);
    bio.splitpathname(path,name); ASSERT(bio.done);
    str.print(path,'%s/%s',s2,name);
    dir:=f2;
    bio.fopen((dir),f2,name,'r');
    IF NOT bio.done THEN io_error(path); bio.close(dir); RETURN END;
    bio.close(dir);
  ELSE str.copy(path,s2);
  END;
  std.print('[%6d] %s\n',bio.eof(f2),path);
  eof:=bio.eof(f1);
  IF eof>bio.eof(f2) THEN eof:=bio.eof(f2) END;
  pos:=0; difs:=0;
  WHILE eof>0 DO
    IF eof>size THEN len:=size ELSE len:=eof END;
    bio.get(f1,b1,len);
    IF NOT bio.done THEN io_error(s1); RETURN END;
    bio.get(f2,b2,len);
    IF NOT bio.done THEN io_error(path); RETURN END;
    comp(pos,len,b1,b2,difs);
    DEC(eof,len);
    INC(pos,len);
  END;
  IF difs>0 THEN
    std.print('%d differences\n',difs);
  END;
END _compare;

PROCEDURE compare(VAL s1,s2: ARRAY OF CHAR);

  PROCEDURE close(f: bio.FILE; VAL s: ARRAY OF CHAR);
  BEGIN
    IF f#bio.null THEN
      bio.close(f);
      IF NOT bio.done THEN io_error(s) END;
    END;
  END close;

  VAR f1,f2: bio.FILE;
BEGIN
  f1:=bio.null;
  f2:=bio.null;
  _compare(s1,s2,f1,f2);
  close(f1,s1); close(f2,s2);
END compare;

---------------------------------------------------------------

PROCEDURE _dump(pos,len: INTEGER; VAL b: ARRAY OF CHAR): BOOLEAN;
  VAR s: ARRAY [0..79] OF CHAR;
      x: CHAR;
      p,i,n,l,sp,line: INTEGER;
BEGIN
  line:=0;
  FOR i:=0 TO len-1 BY 16 DO
    p:=0;
    str.image(s,p,'%05h: ',pos+i);
    IF i+15<len THEN l:=i+15 ELSE l:=len-1 END;
    sp:=(i+15-l);
    FOR n:=i TO l DO str.image(s,p,'%02h ',b[n]) END;
    str.image(s,p,'%.*c|',sp*3,' ');
    FOR n:=i TO l DO
      IF ORD(b[n]) MOD 128 >=32 THEN s[p]:=b[n] ELSE s[p]:='_' END;
      INC(p);
    END;
    str.image(s,p,'%.*c|',sp,' ');
    std.print('%s\n',s);
    INC(line);
    IF wait & (line=16) THEN
      line:=0;
      tty.print('... PRESS A KEY');
      key.read(x);
      tty.print('\r'); tty.erase_line(0);
      tty.print('\n');
      IF x=33c THEN RETURN TRUE END;
    END;
  END;
  RETURN FALSE
END _dump;

VAR buf: ARRAY [0..size-1] OF CHAR;

PROCEDURE dump(pos: INTEGER; VAL s: ARRAY OF CHAR);
  VAR f: bio.FILE; eof,len: INTEGER;
BEGIN
  bio.open(f,s,'r');
  IF NOT bio.done THEN io_error(s); RETURN END;
  eof:=bio.eof(f);
  std.print('[%d] %s\n',eof,s);
  IF pos#0 THEN
    bio.seek(f,pos,0);
    IF NOT bio.done THEN io_error(s); RETURN END;
  END;
  DEC(eof,pos);
  WHILE eof>0 DO
    IF eof>size THEN len:=size ELSE len:=eof END;
    bio.get(f,buf,len);
    IF NOT bio.done THEN bio.close(f); io_error(s); RETURN END;
    IF _dump(pos,len,buf) THEN eof:=0
    ELSE
      INC(pos,len);
      DEC(eof,len);
    END;
  END;
  bio.close(f);
  IF NOT bio.done THEN io_error(s); RETURN END;
END dump;

PROCEDURE pusage;
BEGIN
  std.print(
      '   "diff"   dump & compare files utility program   (c) KRONOS\n'
      'dump:\n'
      '    diff  file_name [ofs=number] [-hw]\n'
      'compare:\n'
      '    diff  file_name1 file_name2 [-hl]\n'
      '                                     Ned, 19-Dec-89\n'
           )
END pusage;

VAR b: INTEGER;

BEGIN
  IF (HIGH(args.words)<0) OR args.flag('-','h') THEN pusage; HALT END;
  wait:=args.flag('-','w');
  log :=args.flag('-','l');
  tty.set_cursor(0);
  IF HIGH(args.words)>=1 THEN
    compare(args.words[0],args.words[1])
  ELSIF HIGH(args.words)=0 THEN
    IF NOT args.number('ofs',b) THEN b:=0 END;
    dump(b,args.words[0])
  END
END diff.
