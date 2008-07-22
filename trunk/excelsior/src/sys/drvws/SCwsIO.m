MODULE SCwsIO;

IMPORT  err: defErrors;         IMPORT  fs : osFiles;
IMPORT  sio: SIOws;             IMPORT  env: tskEnv;
IMPORT  req: defRequest;        IMPORT  str: Strings;

PROCEDURE init(chan,tno: INTEGER);
  VAR            r: INTEGER;
                 i: ARRAY [0..31] OF CHAR;
   tt_name,kb_name: ARRAY [0..7] OF  CHAR;
BEGIN
  r:=sio.init(chan, sio.x16 + sio.stop1 + sio.bits8 ); ASSERT(r=err.ok,r);
  tt_name:="sco0"; tt_name[3]:=CHAR(ORD("0")+tno);
  kb_name:="sci0"; kb_name[3]:=CHAR(ORD("0")+tno);
  i:=''; str.app(i,tt_name); str.app(i,' '); str.app(i,kb_name);
  env.put_str(env.info,i,TRUE);
  r:=fs.define_driver(tt_name,'',0,fs.tty,sio.doio); ASSERT(r=err.ok,r);
  r:=fs.define_driver(kb_name,'',0,fs.tty,sio.doio); ASSERT(r=err.ok,r);
END init;

PROCEDURE parms(VAR chan, tno: INTEGER);

  PROCEDURE default; BEGIN tno:=1; chan:=1 END default;

  VAR pos: INTEGER;
       ps: STRING;
     done: BOOLEAN;

BEGIN
  env.get_str(env.args,ps); IF NOT env.done THEN default; RETURN END;
  pos:=0; done:=TRUE; str.iscan(tno,ps,pos,done);
  IF NOT done THEN default; RETURN END;
  str.iscan(chan,ps,pos,done);
  IF NOT done THEN default; RETURN END;
END parms;

PROCEDURE mon; BEGIN LOOP sio.put(sio.get()) END END mon;

VAR chan,tno: INTEGER;

BEGIN
  parms(chan,tno); init(chan,tno);
  env.become_ipr;  sio.monitor(mon)
END SCwsIO.
