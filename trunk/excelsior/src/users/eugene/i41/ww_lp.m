MODULE ww_lp; (* 26-Apr-88. (c) KRONOS *)

IMPORT  sys: SYSTEM;
IMPORT  sch: Scheduler;
IMPORT  fs : FsDrv;
IMPORT  fp : FsPublic;
IMPORT  mcd: mCodeMnem;
IMPORT  tty: Terminal;
IMPORT  res: Resource;
IMPORT  Image;

TYPE
  BUFF=POINTER TO ARRAY [0..4095] OF CHAR;

VAR
  Door            : sch.Gate;
  Lock            : sch.Gate;
  MyName          : ARRAY [0..3] OF CHAR;

CONST
  csr = 16h<<30;
  dtA = 10h<<30;
  dtB = 12h<<30;
  dtC = 14h<<30;

PROCEDURE out(a: INTEGER; v: sys.WORD); CODE 93h END out;
PROCEDURE inp(a: INTEGER): BITSET; CODE 92h END inp;

PROCEDURE write(c: CHAR);
BEGIN
  IF 2 IN inp(dtC) THEN
    tty.print('LP is in error state.\n');
    REPEAT UNTIL NOT (2 IN inp(dtC));
  END;
  out(dtA,c);
--tty.print('data out\n'); IF tty.Read()=0c THEN END;
  REPEAT UNTIL 1 IN inp(dtC);
--tty.print('printer ready\n'); IF tty.Read()=0c THEN END;
  out(csr,{3,2,1,0});
--tty.print('strob activ\n'); IF tty.Read()=0c THEN END;
  REPEAT UNTIL NOT (1 IN inp(dtC));
--tty.print('printer busy\n'); IF tty.Read()=0c THEN END;
  out(csr,{3,2,1});
--tty.print('strob done\n'); IF tty.Read()=0c THEN END;
END write;

PROCEDURE Init_LP;
BEGIN
  out(csr,{7,1,0});
  out(csr,{3,2,1});
  out(csr,{3,2,0});
  out(csr,{3,0});
  out(csr,{3,1,0});
END Init_LP;

PROCEDURE DrvRd(dev,block: INTEGER; s: BUFF; l: INTEGER): BOOLEAN;
BEGIN
  RETURN FALSE
END DrvRd;

PROCEDURE DrvWr(dev,block: INTEGER; s: BUFF; l: INTEGER): BOOLEAN;
  VAR i: INTEGER;
BEGIN
  sch.EnterGate(Door);
  FOR i:=0 TO l-1 DO write(s^[i]) END;
  sch.ExitGate(Door);
  RETURN FALSE
END DrvWr;

PROCEDURE DrvCFE(dev,op: INTEGER; info: sys.ADDRESS): BOOLEAN;
  VAR s: POINTER TO ARRAY [0..10] OF CHAR;
    d,i: INTEGER;
BEGIN
  CASE op OF
      0: info^:=0  --  0 - Обьем в блоках
    | 1: IF info#NIL THEN s:=info; s^:=MyName END;  -- Имя драйвера
    | 4: Init_LP;
    |10:
    |21: sch.EnterGate(Lock);
    |22: sch.ExitGate(Lock);
   ELSE
     RETURN fp.NoSuchCFE;
   END;
   RETURN FALSE
END DrvCFE;

PROCEDURE parms;
  VAR
    p  : sch.ProcessId;
    i,j: INTEGER;
    ps : POINTER TO ARRAY [0..255] OF CHAR;
BEGIN
  p :=sch.MyTask();
  i:=0; j:=0; ps:=p^.ParmLink;
  WHILE (j<HIGH(ps^)) & (ps^[j]=' ') DO INC(j) END;
  WHILE (i<2) & (j<=HIGH(ps^)) & (ps^[j]#0c) & (ps^[j]#' ') DO
    MyName[i]:=ps^[j]; INC(j); INC(i);
  END;
  IF MyName='' THEN MyName:='lp' ELSE MyName[2]:=0c END;
END parms;

PROCEDURE Finish;
BEGIN
  sch.EnterGate(Door);
  IF fs.RemDriver(MyName) THEN
    tty.print("LPI6EPSON: Can't remove myself!\n")
  END;
  out(csr,{3,2});
  sch.ExitGate(Door);
END Finish;

VAR
  never: sch.Signal;

BEGIN
  sch.InitGate(Lock);
  sch.InitGate(Door);
  parms;
  IF fs.IamDriver(MyName,{0},fs.serial,DrvRd,DrvWr,DrvCFE) THEN
    tty.print('Can not instalate driver.\n'); HALT(1)
  END;
  res.Final(Finish);
  sch.InitSignal(never);
  sch.Wait(never);
END ww_lp.
