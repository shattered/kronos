IMPLEMENTATION MODULE StdIO; (* Andy 15-Jan-90. (c) KRONOS *)
                             (* Leo  27-Jun-90. (c) KRONOS *)


IMPORT       SYSTEM;            IMPORT       ASCII;
IMPORT  err: defErrors;         IMPORT  tty: Terminal;
IMPORT  str: Strings;           IMPORT  lex: Lexicon;
IMPORT  bio: BIO;               IMPORT  tsk: tskEnv;
IMPORT  mem: Heap;              IMPORT args: tskArgs;

WITH STORAGE (NEW    : mem.allocate;
              DISPOSE: mem.deallocate;
              RESIZE : mem.reallocate);

VAR is_tty_in: BOOLEAN;

PROCEDURE is_tty(f: bio.FILE): BOOLEAN;
  VAR r: BOOLEAN;
BEGIN
  r:=(bio.is_tty*bio.kind(f)#{});
  IF NOT bio.done THEN done:=bio.done; error:=bio.error; RETURN FALSE END;
  RETURN r
END is_tty;

PROCEDURE write(ch: CHAR);
BEGIN
  bio.putch(out,ch);    iolen:=bio.iolen;
  done:=bio.done;
  IF NOT done THEN error:=bio.error END
END write;

PROCEDURE read(VAR ch: CHAR);
BEGIN
  IF NOT is_tty_in & (bio.pos(in)>=bio.eof(in)) THEN
    done:=FALSE; error:=err.no_data; ch:=EOF; iolen:=0; RETURN
  END;
  bio.getch(in,ch);     iolen:=bio.iolen;
  done:=bio.done;
  IF NOT done THEN error:=bio.error END
END read;

PROCEDURE writestr(VAL S: ARRAY OF CHAR);
BEGIN
  bio.putstr(out,S,0);  iolen:=bio.iolen;
  done:=bio.done;
  IF NOT done    THEN error:=bio.error END
END writestr;

PROCEDURE readstr(VAR S: ARRAY OF CHAR);
BEGIN
  bio.getstr(in,S,0);  iolen:=bio.iolen;
  done:=bio.done;
  IF NOT done THEN error:=bio.error END;
  IF (bio.iolen=0) & (HIGH(S)>=0) THEN S[0]:=EOF END;
END readstr;

PROCEDURE writeln;
BEGIN
  bio.putch(out,ASCII.NL);
  done:=bio.done;
  IF NOT done THEN error:=bio.error END
END writeln;

PROCEDURE print(VAL format: ARRAY OF CHAR; SEQ args: SYSTEM.WORD);
BEGIN
  bio.print(out,format,args);  iolen:=bio.iolen;
  done:=bio.done;
  IF NOT done THEN error:=bio.error END
END print;

PROCEDURE perror(code: INTEGER; VAL f: ARRAY OF CHAR; SEQ args: SYSTEM.WORD);
  VAR s: ARRAY [0..127] OF CHAR;
BEGIN
  s:="";
  lex.perror(s,code,f,args);
  WriteString(s);
  IF NOT done THEN RETURN END;
  done:=lex.done;
  IF NOT done THEN error:=lex.error END
END perror;

PROCEDURE Write(ch: CHAR); BEGIN write(ch) END Write;
PROCEDURE WriteLn;         BEGIN writeln   END WriteLn;

PROCEDURE WriteString(VAL S: ARRAY OF CHAR); BEGIN writestr(S) END WriteString;
PROCEDURE ReadString (VAR S: ARRAY OF CHAR); BEGIN readstr(S)  END ReadString;
PROCEDURE Read       (VAR ch: CHAR);         BEGIN read (ch)   END Read;



VAR _err: BOOLEAN;

PROCEDURE _error(in: BOOLEAN; VAL name: ARRAY OF CHAR; errcode: INTEGER);
  VAR dir: ARRAY [0..15] OF CHAR;
BEGIN
  IF _err THEN RETURN END;
  done:=FALSE; error:=errcode;
  IF in THEN dir:='input' ELSE dir:='output' END;
  tty.perror(errcode,'\nstandard %s: %s - %%s\n',dir,name);
  _err:=TRUE;
END _error;

PROCEDURE make_inp(VAL name: ARRAY OF CHAR);
  VAR _name: STRING;
    in_name: ARRAY [0..63] OF CHAR;
BEGIN
  _err:=FALSE;
  IF name='' THEN
     tsk.get_str(tsk.stdin,_name);
     IF NOT tsk.done & (tsk.error=err.no_entry) THEN
       tsk.get_str(tsk.key,_name)
     END;
     IF tsk.done THEN
       str.print(in_name,'%s',_name)
     ELSE
       _error(TRUE,'default stream',tsk.error); RETURN
     END
  ELSE
    str.copy(in_name,name)
  END;
  bio.open(in,in_name,'r');
  IF bio.done THEN
    bio.buffers(in,1,4096)
  ELSE
    _error(TRUE,in_name,bio.error); in:=bio.null
  END
END make_inp;

PROCEDURE make_out(VAL name: ARRAY OF CHAR; app?: BOOLEAN);

  VAR    e: INTEGER;
     _name: STRING;
    create: BOOLEAN;
  out_name: ARRAY [0..63] OF CHAR;

  PROCEDURE fault(errcode: INTEGER);
  BEGIN
    _error(FALSE,out_name,errcode);  out:=bio.null
  END fault;

BEGIN
  _err:=FALSE;  create:=TRUE;
  IF name='' THEN
     tsk.get_str(tsk.stdout,_name);
     IF NOT tsk.done & (tsk.error=err.no_entry) THEN
       tsk.get_str(tsk.tty,_name);
       IF tsk.done THEN create:=FALSE END
     END;
     IF tsk.done THEN
       app?:=(LEN(_name)>0) & (_name[0]='>');
       str.print(out_name,'%..*s',ORD(app?),_name)
     ELSE
       _error(FALSE,'default stream',tsk.error); RETURN
     END
  ELSE
    str.copy(out_name,name)
  END;
  bio.open(out,out_name,'w');
  IF bio.done THEN
    IF app? THEN bio.seek(out,0,2) ELSE bio.cut(out,0) END;
    IF NOT bio.done THEN e:=bio.error; bio.close(out); fault(e); RETURN END
  ELSIF create & (bio.error=err.no_entry) THEN
    bio.create(out,"",'w',1);
    IF NOT bio.done THEN fault(bio.error); RETURN END;
    bio.link(out,out_name,'');
    IF NOT bio.done THEN fault(bio.error); RETURN END
  ELSE
    fault(bio.error); RETURN
  END;
  bio.buffers(out,1,4096);
END make_out;

PROCEDURE redirect;
  VAR   s: STRING;          app: BOOLEAN;
     name: STRING;      has_inp: BOOLEAN;
    len,i: INTEGER;     has_out: BOOLEAN;
BEGIN
  has_inp:=FALSE;
  has_out:=FALSE;
  i:=HIGH(args.words);
  WHILE (i>=0) AND NOT (has_inp AND has_out) DO
(*$<U+*)
    s^:=args.words[i]^;
(*$>*)
    len:=str.len(s);
    IF len>=2 THEN
      IF (s[0]='<') AND NOT has_inp THEN
        NEW(name,len+1);
        IF mem.done THEN
          str.move(name,0,s,1,len-1); name[len-1]:=0c;
          make_inp(name);
          has_inp:=done;
          DISPOSE(name)
        ELSE
          _error(FALSE,name,mem.error)
        END;
        args.del_word(i)
      ELSIF (s[0]='>') AND NOT has_out THEN
        app:=(s[1]='>');
        NEW(name,len+1);
        IF mem.done THEN
          str.move(name,0,s,1+ORD(app),len-1-ORD(app));
          name[len-1-ORD(app)]:=0c;
          make_out(name,app);
          has_out:=done;
          DISPOSE(name)
        ELSE
          _error(FALSE,name,mem.error)
        END;
        args.del_word(i)
      END
    END;
    DEC(i)
  END;
  IF NOT has_inp  THEN make_inp('')       END;
  IF NOT has_out  THEN make_out('',FALSE) END
END redirect;

PROCEDURE final;
BEGIN
  bio.close(in);  bio.close(out)
END final;

VAR i: INTEGER;

BEGIN
  EOF:=ASCII.EOF;  iolen:=0;
  in :=bio.null;   done :=TRUE;
  out:=bio.null;   error:=err.ok;
  tsk.final(final);
  i:=mem.credit;
  mem.set_credit(2);  redirect;  mem.set_credit(i);
  is_tty_in:=is_tty(in)
END StdIO.
