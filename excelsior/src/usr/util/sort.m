MODULE sort; (*  11-Nov-90. (c) KRONOS *)

IMPORT   str: Strings;
IMPORT   bio: BIO;
IMPORT   std: StdIO;
IMPORT  sort: Sorts;
IMPORT   mem: Heap;
IMPORT  args: tskArgs;

WITH STORAGE: mem;

VAR text: DYNARR OF STRING;

VAR _comp: PROCEDURE (ARRAY OF CHAR,ARRAY OF CHAR): INTEGER;

PROCEDURE comp(x: INTEGER; a,b: INTEGER): INTEGER;
BEGIN
  RETURN _comp(text[a],text[b])
END comp;

PROCEDURE swap(x: INTEGER; a,b: INTEGER);
  VAR s: STRING;
BEGIN
(*$<U+*)
  s^:=text[a]^;
  text[a]^:=text[b]^;
  text[b]^:=s^;
(*$>*)
END swap;

PROCEDURE read(VAL name: ARRAY OF CHAR);
  VAR
    pos: INTEGER;
    s  : ARRAY [0..255] OF CHAR;
    f  : bio.FILE;
    p  : INTEGER;
BEGIN
  bio.open(f,name,'r');  ASSERT(bio.done,bio.error);
  bio.buffers(f,1,4096); ASSERT(bio.done,bio.error);
  NEW(text); pos:=0;
  LOOP
    bio.getstr(f,s,0);
    IF bio.iolen=0 THEN EXIT END;
    p:=0;
    str.skip(s,p,' ');
    IF pos>HIGH(text) THEN RESIZE(text,LEN(text)+100) END;
    NEW(text[pos],str.len(s)+1-p);
    str.print(text[pos],'%..*s',p,s);
    INC(pos);
  END;
  bio.close(f);
  RESIZE(text,pos);
END read;

PROCEDURE pusage;
BEGIN
  std.print(
      '  "sort"  file sorting utility program (c) KRONOS\n'
      'usage:\n'
      '   sort [-haA] [+h] file_name\n');
  std.print(
      'writes sorted file to standard output\n'
      '    -a   lexical order sorting\n'
      '    -A   lexical order sorting;\n'
      '         small and capital letters are equal\n'
      '    +h   heap  sorting\n'
      '   else  quick sorting\n'
      '                                   Ned, Dec 13 90\n')
END pusage;

VAR i: INTEGER;

BEGIN
  IF args.flag('-','h') OR (LEN(args.words)=0) THEN pusage; HALT END;
  IF    args.flag('-','A') THEN _comp:=sort.ABC_comp
  ELSIF args.flag('-','a') THEN _comp:=sort.abc_comp
  ELSE                          _comp:=sort.str_comp
  END;
  bio.check_io(TRUE);
  read(args.words[0]);
  IF args.flag('+','h') THEN
    sort.heap (0,LEN(text),comp,swap);
  ELSE
    sort.quick(0,LEN(text),comp,swap);
  END;
  FOR i:=0 TO HIGH(text) DO std.print('%s\n',text[i]) END;
END sort.
