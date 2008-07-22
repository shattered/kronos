IMPLEMENTATION MODULE Lexicon; (* Leo 18-Jan-90. (c) KRONOS *)

IMPORT       SYSTEM;
IMPORT  err: defErrors;
IMPORT  req: defRequest;
IMPORT  str: Strings;
IMPORT  env: tskEnv;
IMPORT  bio: BIO;
IMPORT  mem: Heap;

(*$U+*)

WITH STORAGE (NEW    : mem.allocate;
              DISPOSE: mem.deallocate;
              RESIZE : mem.reallocate);

TYPE LEX = bio.FILE;

CONST MAGIC = 004C4558h; (* "LEX"0c *)

CONST
  CHECK = req.CONTROL+0*100h;
  GET   = req.CONTROL+1*100h;

PROCEDURE open(VAR lex: LEX; name: ARRAY OF CHAR);
  VAR r: req.REQUEST;
BEGIN
  bio.open(lex,name,'X');
  done:=bio.done;
  IF NOT done THEN error:=bio.error; RETURN END;
  IF bio.is_spec*bio.kind(lex)={} THEN
    bio.close(lex); done:=FALSE; error:=err.ill_desc; RETURN
  END;
  r.op :=CHECK;
  r.ofs:=0;                r.pos:=0;
  r.buf:=NIL;              r.len:=0;
  bio.doio(lex,r);
  done:=bio.done;
  IF NOT done THEN error:=bio.error; RETURN END;
  IF r.len#MAGIC THEN
    bio.close(lex); done:=FALSE; error:=err.ill_desc
  END
END open;

PROCEDURE close(VAR lex: LEX);
BEGIN
  bio.close(lex);
  done:=bio.done;
  IF NOT done THEN error:=bio.error END
END close;

VAR no_memory: ARRAY [0..15] OF CHAR;

PROCEDURE no_mem(VAR data: STRING);
BEGIN
  data^.ADR :=SYSTEM.ADR(no_memory); data^.HIGH:=HIGH(no_memory);
  done:=FALSE; error:=err.no_memory
END no_mem;

PROCEDURE bad(VAR data: STRING; erc,code: INTEGER);
BEGIN
  done:=FALSE; error:=erc;
  NEW(data,8);
  IF data^.ADR=NIL THEN no_mem(data); RETURN END;
  str.print(data,"%04hh",code)
END bad;

PROCEDURE get(lex: LEX; code: INTEGER; VAR data: STRING);
  VAR r: req.REQUEST;
     db: STRING;
    i,j: INTEGER;
    p,l: INTEGER;
BEGIN
  done:=TRUE;
  data^.ADR:=NIL; data^.HIGH:=0;
  r.op :=GET;
  r.ofs:=code;             r.pos:=0;
  r.buf:=NIL;              r.len:=0;
  bio.doio(lex,r);
  l:=r.len; p:=r.pos;
  IF NOT bio.done THEN bad(data,bio.error,code);   RETURN END;
  IF l<0          THEN bad(data,err.no_data,code); RETURN END;
  NEW(data,l+1);
  IF data^.ADR=NIL THEN no_mem(data); RETURN END;
  db^.ADR:=r.buf; db^.HIGH:=p+l;
  j:=0;
  FOR i:=p TO p+l-1 DO data[j]:=db[i]; INC(j) END;
  data[j]:=0c;
END get;

PROCEDURE dispose(VAR data: STRING);
BEGIN
  done:=TRUE;
  IF (data^.ADR=NIL) OR (data^.ADR=SYSTEM.ADR(no_memory)) THEN
    data^.ADR:=NIL; data^.HIGH:=-1; RETURN
  END;                            ------
  DISPOSE(data)
END dispose;

PROCEDURE sprint(VAR res: ARRAY OF CHAR;
                     lex: LEX;
                    code: INTEGER;
                  format: ARRAY OF CHAR;
                SEQ args: SYSTEM.WORD);
  VAR bump: STRING;
      data: STRING;
BEGIN
  done:=TRUE;
  NEW(bump,HIGH(res)+1);
  IF bump^.ADR=NIL THEN
    str.print(res,"%s %s.%04hh",no_memory,lex,code); RETURN
  END;
  get(lex,code,data);
  str.print(bump,format,args);
  str.print(res,bump,data);
  DISPOSE(bump);
  dispose(data)
END sprint;

---------------------------------------------------------------


PROCEDURE perror(VAR res: ARRAY OF CHAR;
                    code: INTEGER;
                  format: ARRAY OF CHAR;
                SEQ args: SYSTEM.WORD);

  PROCEDURE set_error;
    VAR i: BITSET;
        s: ARRAY [0..127] OF CHAR;
        x: ARRAY [0.. 63] OF CHAR;
      m,w: BITSET;
  BEGIN
    s:="";
    i:={8};
    m:=BITSET(code)-{8..30};
    w:=BITSET(code)-m;
    WHILE w#{} DO
      IF i*w#{} THEN
        sprint(x,sysmsg,INTEGER(i+m),"%%s");
        IF s#"" THEN str.append(s,", %s",x)
        ELSE         str.append(s,"%s"  ,x)
        END;
        w:=w-i
      END;
      i:=i<<1
    END;
    str.print(x,format,args); str.print(res,x,s)
  END set_error;

BEGIN
  IF sysmsg=null THEN
    str.print(res," NO ERRORS HANDLER, error: %04#h\n",code);
  ELSIF (BITSET(code)*{31}#{}) & (BITSET(code)*{8..30}#{}) THEN
    set_error
  ELSE
    sprint(res,sysmsg,code,format,args)
  END
END perror;

PROCEDURE change_sysmsg(lex: LEX);
BEGIN
  close(sysmsg); done:=TRUE; sysmsg:=lex
END change_sysmsg;

VAR c: INTEGER;
 name: STRING;

BEGIN
  null:=bio.null; done:=TRUE; error:=err.ok;
  no_memory:=" NO MEMORY ";
  sysmsg:=null;
  c:=mem.credit;
  mem.set_credit(2);
  env.get_str(env.msg,name);
  IF env.done THEN
    open(sysmsg,name);
    IF NOT done THEN sysmsg:=null END
  END;
  mem.set_credit(c)
END Lexicon.
