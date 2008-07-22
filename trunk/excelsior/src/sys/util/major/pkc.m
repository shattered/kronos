MODULE pkc; (*$U+ Ned 09-Feb-90. (c) KRONOS *)

IMPORT  def: defCode;
IMPORT  err: defErrors;
IMPORT  str: Strings;
IMPORT  bio: BIO;
IMPORT  key: Keyboard;
IMPORT  std: StdIO;
IMPORT  vis: visCode;
IMPORT  reg: regExpr;
IMPORT args: tskArgs;
IMPORT       Heap;

WITH STORAGE: Heap;

TYPE
  node_ptr = POINTER TO node_rec;
  node_rec = RECORD
               name: ARRAY [0..31] OF CHAR;
               next: node_ptr;
             END;

VAR ou: bio.FILE;
  expr: reg.EXPR;
  path: bio.PATHs;
  node: node_ptr;
 match: BOOLEAN;
 wname: ARRAY [0..255] OF CHAR;
  show: BOOLEAN;
   qry: BOOLEAN;

PROCEDURE query(VAL s: ARRAY OF CHAR): BOOLEAN;
  VAR c,ch: CHAR;
BEGIN
  std.print('%-12s?',s);
  LOOP
    key.read(ch);
    c:=CAP(ch);
    IF (c='Y') OR (c='N') OR (c='A') THEN EXIT END;
    key.bell(1)
  END;
  std.print('%c\n',ch);
  qry:=(c#'A');
  RETURN (c='Y') OR (c='A');
END query;

PROCEDURE read(VAR buf: STRING; VAL name: ARRAY OF CHAR): BOOLEAN;
  VAR f: bio.FILE;
     fn: ARRAY [0..255] OF CHAR;
BEGIN
  str.print(fn,'%s.cod',name);
  bio.lookup(path,f,fn,'r');
  IF NOT bio.done THEN
    IF bio.error#err.no_entry THEN std.perror(bio.error,'"%s" %%s\n',fn) END;
    RETURN FALSE
  END;
  NEW(buf,bio.eof(f));
  bio.get(f,buf,bio.eof(f));
  IF NOT bio.done THEN bio.close(f); RETURN FALSE END;
  bio.close(f);
  RETURN TRUE
END read;

PROCEDURE pack(VAL name: ARRAY OF CHAR; VAR buf: STRING);
  VAR cod: def.code_ptr;
     c,ch: CHAR;
BEGIN
  cod:=buf^.ADR;
  IF cod^.size=0 THEN cod^.size:=SIZE(buf); RETURN END;
  std.print('"%s" is already packed codfile; Quit/Truncate/Pack?',name);
  REPEAT key.read(ch); c:=CAP(ch) UNTIL (c='Q') OR (c='T') OR (c='P');
  std.print("%c\n",ch);
  IF c='Q' THEN HALT END;
  IF c='T' THEN buf^.HIGH:=cod^.size*4-1 END;
  cod^.size:=SIZE(buf)
END pack;

PROCEDURE append(VAL name: ARRAY OF CHAR);
  VAR i: INTEGER;     buf: STRING;
      l: node_ptr;   code: vis.code_ptr;
BEGIN
  l:=node;
  WHILE l#NIL DO
    IF l^.name=name THEN RETURN END;
    l:=l^.next;
  END;
  IF NOT read(buf,name) THEN RETURN END;
  NEW(l);
  str.copy(l^.name,name);
  l^.next:=node; node:=l;
  IF match & NOT reg.match(expr,name,0) THEN RETURN END;
  IF qry   & NOT query(name)            THEN RETURN END;
  IF NOT qry & show THEN std.print('%s\n',name) END;

  pack(name,buf);
  bio.put(ou,buf,BYTES(buf));
  IF NOT bio.done THEN
    std.perror(bio.error,'"%s" %%s\n',wname); bio.purge(ou); HALT(1)
  END;

  vis.connect(code,buf);
  FOR i:=1 TO HIGH(code^.exts) DO append(code^.exts[i]^.name) END;
  vis.disconnect(code)
END append;

PROCEDURE pusage;
BEGIN
  std.print('usage:\n');
  std.print('  pkc [-qv] module_name [pattern] [path] [out]\n');
  std.print('    path   :  BIN=path_specification\n');
  std.print('    out    :  out=out_file_name\n');
  std.print('  -q   no query\n');
  std.print('  -v   no verbose\n');
END pusage;

VAR x: STRING;

BEGIN
  IF args.flag('-','h') OR (HIGH(args.words)<0) THEN
    pusage; HALT
  END;
  qry :=NOT args.flag('-','q');
  show:=NOT args.flag('-','v');
  match:=(HIGH(args.words)>=1);
  IF match THEN
    reg.compile(args.words[1],expr);
    IF NOT reg.done THEN
      std.perror(reg.error,'"%s" %%s\n',args.words[1]); HALT
    END;
  END;
  IF args.string('out',x) THEN str.copy(wname,x);
  ELSE str.print(wname,'%s.cod',args.words[0]);
  END;
  bio.create(ou,wname,'wh',0);
  IF NOT bio.done THEN std.perror(bio.error,'"%s" %%s\n',wname); HALT END;
  IF args.string('BIN',x) THEN
    bio.open_paths(path,x);
    IF NOT bio.done THEN std.perror(bio.error,'"%s" %%s\n',x); HALT END;
  ELSE
    bio.get_paths(path,'BIN');
    IF NOT bio.done THEN path:=bio.here END
  END;
  node:=NIL;
  append(args.words[0]);
  IF bio.eof(ou)=0 THEN bio.purge(ou)
  ELSE
    IF show THEN std.print('"%s" %d bytes\n',wname,bio.eof(ou)) END;
    bio.close(ou)
  END;
  IF NOT bio.done THEN
    std.perror(bio.error,'"%s" %%s\n',wname); HALT
  END
END pkc.
