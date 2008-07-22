MODULE SC5IO; (* Leo 22-Oct-89. (c) KRONOS *)

(* Instalation  SC5IO dev_name port_no [-xn] *)
(* -x   xon/xoff on                          *)
(* -n   NL -> CR+LF                          *)
(* port_no  assigment:                       *)
(*          channel    backpanel             *)
(*      0        9     VDU console           *)
(*      1       14     Serial 1              *)
(*      2       12     Serial 2              *)
(*      3       13     Serial 3              *)
(*      4        2     Serial 4              *)
(*      8       11     Parallel Printer & IO *)
(*      9       10     Serial   Printer      *)

IMPORT       SYSTEM;
IMPORT  env: tskEnv;
IMPORT  sio: SIOmBUS;
IMPORT  fs : osFiles;
IMPORT  err: defErrors;

VAR name: ARRAY [0..7] OF CHAR;

PROCEDURE halt;
BEGIN
  sio.stop;
  IF fs.remove_driver(name)#0 THEN END
END halt;

PROCEDURE install;

  VAR p: STRING;
     nl: BOOLEAN;
    xon: BOOLEAN;
  r,i,j: INTEGER;

  PROCEDURE skip;
  BEGIN
    WHILE (i<=HIGH(p)) & (p[i]#0c) & (p[i]=' ') DO INC(i) END
  END skip;

BEGIN
  env.get_str(env.args,p);
  IF NOT env.done THEN HALT(env.error) END;
  i:=0; j:=0; skip;
  WHILE (i<=HIGH(p)) & (p[i]#0c) & (p[i]#' ') & (j<HIGH(name)) DO
    name[j]:=p[i]; INC(i); INC(j)
  END;
  name[j]:=0c;
  IF j=0 THEN HALT(err.bad_name) END;
  skip;
  IF (i<=HIGH(p)) & (p[i]>='0') & (p[i]<='9') THEN
    j:= ORD(p[i])-ORD("0")
  ELSE
    HALT(err.bad_parm)
  END;
  skip;
  IF (i<=HIGH(p)) & (p[i]>='-') THEN INC(i);
    xon:=(i<=HIGH(p)) & (p[i]>='x');
    nl :=(i<=HIGH(p)) & (p[i]>='n');
    IF xon OR nl THEN INC(i) END;
    xon:=(i<=HIGH(p)) & (p[i]>='x') OR xon;
    nl :=(i<=HIGH(p)) & (p[i]>='n') OR nl;
  END;

  r:=sio.init(j);
  IF r#err.ok THEN HALT(r) END;
  r:=fs.define_driver(name,"",0,fs.tty,sio.doio);
  IF r#err.ok THEN HALT(r) END;

  sio.raw_out:=NOT nl;
  IF xon THEN sio.xon:=21c; sio.xoff:=23c
  ELSE        sio.xon:=00c; sio.xoff:=00c
  END
END install;

VAR ch: CHAR;

BEGIN
  name:="";
  env.final(halt);
  install;
  env.become_ipr;
  LOOP
    sio.get(ch); sio.put(ch)
  END
END SC5IO.
