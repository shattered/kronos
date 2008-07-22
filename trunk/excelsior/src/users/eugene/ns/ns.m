MODULE ns; (* Ned 13-Jan-89. (c) KRONOS *)

IMPORT sys : SYSTEM;
IMPORT bcb : ns_bcb;
IMPORT arg : tskArgs;
IMPORT tty : Terminal;
IMPORT str : Strings;
IMPORT bio : BIO;

PROCEDURE io_chk;
BEGIN
  ASSERT(bio.done);
END io_chk;

PROCEDURE load(VAL w: ARRAY OF CHAR; addr: INTEGER);
  VAR sou: bio.FILE; eof: INTEGER;
BEGIN
  bio.open(sou,w,'r'); io_chk;
  eof:=bio.eof(sou);
  ASSERT(eof<HIGH(bcb.buffer));
  bio.read(sou,bcb.buffer.ADR,eof); io_chk;
  bio.close(sou); io_chk;
  tty.print('move(%h,%h,%h)\n',addr,bcb.buf_ma,eof);
  bcb.move(addr,bcb.buf_ma,eof);
END load;

PROCEDURE exec(pc: INTEGER);
  VAR res,time: INTEGER;
BEGIN
  tty.print('call %$8h\n',pc);
  bcb.call(pc,res,time);
  tty.print('res  %$8h\n',res);
  tty.print('execution time: %d miliseconds\n',time);
END exec;

PROCEDURE write(VAL w: ARRAY OF CHAR; from,len: INTEGER);
  VAR out: bio.FILE;
BEGIN
  ASSERT(len<HIGH(bcb.buffer));
  tty.print('move(%h,%h,%h)\n',bcb.buf_ma,from,len);
  bcb.move(bcb.buf_ma,from,len);
  bio.create(out,w,'w',0); io_chk;
  bio.write(out,bcb.buffer.ADR,len); io_chk;
  bio.close(out); io_chk;
END write;

PROCEDURE prt_word(n: INTEGER);
  VAR mbus: POINTER TO ARRAY [0..0FFFFFh] OF CHAR; i: INTEGER;
BEGIN
  IF (n<0) OR (n>0FFFFFh) THEN RETURN END;
  mbus:=sys.ADDRESS(0F00000h);
  tty.print('mbus %$8hh = ',n);
  FOR i:=3 TO 0 BY -1 DO
    tty.print('%$2h',mbus^[n+i]);
  END;
  tty.print('\n');
END prt_word;

CONST
  start = 1000h;

VAR
  f,l,p: INTEGER;
  nm   : ARRAY [0..255] OF CHAR;

BEGIN
  IF HIGH(arg.words)>=0 THEN
    str.print(nm,'%s.exe',arg.words[0]);
  ELSE
    nm:=''
  END;
  IF arg.flag('-','f') THEN bcb.bcb^.free:=TRUE END;
  IF arg.flag('-','v') THEN bcb.vis_bcb END;
  IF arg.flag('-','h') THEN
    tty.print('ns -Vis -Load -eXec -Write word f=<from> l=<len>\n');
    HALT;
  END;
  f:=start; l:=1;
--  IF arg.NumFlag?('f',f) THEN f:=start    END;
  IF NOT arg.number('l',l) THEN l:=1        END;
  IF arg.flag('-','l')     THEN load(nm,f)    END;
  IF arg.flag('-','x')     THEN exec(f)       END;
  IF arg.flag('-','w')     THEN write(nm,f,l) END;
  IF arg.number('p',p)     THEN prt_word(p) END;
END ns.
