IMPLEMENTATION MODULE tskEnv; (* Ned 28-Sep-89. (c) KRONOS *)

IMPORT  sys: SYSTEM;
IMPORT  err: defErrors;
IMPORT  def: defTasks;
IMPORT  low: lowLevel;
IMPORT  os : osKernel;

CONST ok=err.ok;

VAR tsk: os.task_ptr;

PROCEDURE get_str(VAL name: ARRAY OF CHAR; VAR s: STRING);
  VAR res: INTEGER; priv: BOOLEAN;
BEGIN
  res:=os.get_env(tsk,-1,name,s,priv);
  done:=(res=ok);
  IF NOT done THEN error:=res; NEW(s) END;
END get_str;

PROCEDURE get_env(VAL nm: ARRAY OF CHAR;
                      ms: INTEGER;
                   VAR s: STRING;
                   VAR p: BOOLEAN);
  VAR res: INTEGER;
BEGIN
  res:=os.get_env(tsk,(ms+os.tick-1) DIV os.tick,nm,s,p);
  done:=(res=ok);
  IF NOT done THEN
    error:=res; NEW(s);
  END
END get_env;

PROCEDURE env_entry(en: INTEGER;
                    ms: INTEGER;
              VAR name: STRING;
              VAR data: STRING;
              VAR priv: BOOLEAN);
  VAR res: INTEGER;
BEGIN
  res:=os.show_env(tsk,en,(ms+os.tick-1) DIV os.tick,name,data,priv);
  done:=(res=ok);
(*$<U+*)
  IF NOT done THEN
    error:=res;
    data^.ADR:=NIL; data^.HIGH:=-1;
    name^.ADR:=NIL; name^.HIGH:=-1
  END
(*$>*)
END env_entry;

PROCEDURE put_str(VAL name,str: ARRAY OF CHAR; priv: BOOLEAN);
  VAR res: INTEGER;
BEGIN
  res:=os.put_str(tsk,name,str,priv);
  done:=(res=ok);
  IF NOT done THEN error:=res END
END put_str;

PROCEDURE put_env(VAL name: ARRAY OF CHAR;
                  VAL data: ARRAY OF sys.WORD;
                      priv: BOOLEAN);
  VAR res: INTEGER;
BEGIN
  res:=os.put_env(tsk,name,data,priv);
  done:=(res=ok);
  IF res#ok THEN error:=res END
END put_env;

PROCEDURE final(p: PROC);
  VAR res: INTEGER;
BEGIN
  res:=os.final(tsk,p);
  done:=(res=ok);
  IF NOT done THEN error:=res END
END final;

PROCEDURE ipr(): BOOLEAN;
BEGIN
  done:=TRUE; RETURN tsk^.ipr
END ipr;

PROCEDURE become_ipr;
BEGIN
  tsk^.ipr:=TRUE;
  os.send(tsk^.out[def.ipr]^);
  done:=TRUE
END become_ipr;

PROCEDURE exit(res: INTEGER);
BEGIN
  tsk^.res:=res;
  os.send(tsk^.inp[def.kill]^);
  done:=TRUE;
END exit;

---------------------------------------------------------------

PROCEDURE allocate(VAR a: sys.ADDRESS; size: INTEGER);
BEGIN
  os.ALLOCATE(tsk^.area,a,size);
  done:=(a#NIL)
END allocate;

PROCEDURE deallocate(VAR a: sys.ADDRESS; size: INTEGER);
BEGIN
  done:=TRUE;
  os.DEALLOCATE(tsk^.area,a,size)
END deallocate;

PROCEDURE reallocate(VAR a: sys.ADDRESS; VAR high: INTEGER;
                                        len,bytes: INTEGER);
  VAR new: sys.ADDRESS; curs,news: INTEGER;
BEGIN
  IF len<=0 THEN len:=0 END;
  IF bytes=1 THEN
    curs:=(high+4) DIV 4;
    news:=(len +3) DIV 4
  ELSE ASSERT(bytes MOD 4=0);
    curs:=(high+1)*(bytes DIV 4);
    news:=     len*(bytes DIV 4)
  END;
  IF len>0 THEN
    os.ALLOCATE(tsk^.area,new,news);
    IF new=NIL THEN done:=FALSE; error:=err.no_memory; RETURN END
  END;
  IF a#NIL THEN
    os.DEALLOCATE(tsk^.area,a,curs);
    IF curs<news THEN news:=curs END;
    low.move(new,a,news)
  END;
  a:=new; high:=len-1;
  done:=TRUE
END reallocate;

BEGIN
  error:=ok; done:=TRUE;
  tsk:=os.self(); id:=tsk^.id
END tskEnv.
