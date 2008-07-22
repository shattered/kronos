IMPLEMENTATION MODULE Time; (* Leo  02-Nov-86. (c) KRONOS *)
                            (* Hady 24-Jul-89. (c) KRONOS *)

IMPORT os: osKernel;

PROCEDURE set_time(time: INTEGER);
BEGIN os.time:=time+os.zone END set_time;

PROCEDURE time(): INTEGER;
BEGIN RETURN os.time-os.zone END time;

PROCEDURE set_zone(zone: INTEGER);
BEGIN
  ASSERT(zone+11 IN {0..23},4Fh);
  os.zone:=zone*3600
END set_zone;

PROCEDURE zone(): INTEGER;
BEGIN RETURN os.zone DIV 3600 END zone;

PROCEDURE mili_secs(): INTEGER;
BEGIN
  RETURN os.timer MOD 3FFFFFFh * os.tick
END mili_secs;

PROCEDURE eval(from: UNIT; val: INTEGER; to: UNIT): INTEGER;
  VAR mul: INTEGER;
BEGIN
  IF  val<0   THEN RETURN val END;
  IF from=to  THEN RETURN val END;
  mul:=1;
  CASE from OF
    |tick    :
    |microsec: val:=(val+1000*os.tick-1) DIV (1000*os.tick)
    |milisec : val:=(val+os.tick-1) DIV os.tick
    |sec     : val:=val*(1000 DIV os.tick)
    |minute  : val:=val*(1000 DIV os.tick);  mul:=60
    |hour    : val:=val*(1000 DIV os.tick);  mul:=3600
  ELSE
    ASSERT(FALSE,4Fh); RETURN -1
  END;
  CASE to OF
    |tick    :
    |microsec: val:= val*os.tick*1000
    |milisec : val:= val*os.tick
    |sec     : val:=(val*os.tick+     1000-1) DIV      1000
    |minute  : val:=(val*os.tick+  60*1000-1) DIV   60*1000
    |hour    : val:=(val*os.tick+3600*1000-1) DIV 3600*1000
  ELSE
    ASSERT(FALSE,4Fh); RETURN -1
  END;
  RETURN val*mul
END eval;

PROCEDURE sys_time(unit: UNIT): INTEGER;
BEGIN RETURN eval(tick,os.timer,unit) END sys_time;

PROCEDURE delay(value: INTEGER; unit: UNIT);
BEGIN
  IF unit#tick THEN value:=eval(unit,value,tick) END;
  os.delay(value)
END delay;

----------------------------------------------------------------

CONST FIRST_YEAR = 1986;  LAST_YEAR = FIRST_YEAR+31;
      -- NOTE: при смене константы FIRST_YEAR должны быть пересчитаны
      --       jf0, vis_add, VIS, shift !!!

CONST
  add     = ARRAY OF INTEGER {0,1,-1,0,0,1,1,2,3,3,4,4};
  vis_add = ARRAY OF INTEGER -- сколько прошло ПОЛНЫХ високосных
                                 -- лет с FIRST_YEAR года

          (********  0  1  2  3  4  5  6  7  8  9    ********)
          (* 1980 *)                 { 0, 0, 0, 1,  (* 1989 *)
          (* 1990 *) 1, 1, 1, 2, 2, 2, 2, 3, 3, 3,  (* 1999 *)
          (* 2000 *) 3, 4, 4, 4, 4, 5, 5, 5, 5, 6,  (* 2009 *)
          (* 2010 *) 6, 6, 6, 7, 7, 7, 7, 8, 8, 8}; (* 2019 *)
          (********  0  1  2  3  4  5  6  7  8  9    ********)

  VIS = BITSET(44444444h); -- для FIRST_YEAR=1986,1990,1994 и т.д.
     -- BITSET(11111111h); -- для FIRST_YEAR=1988 и всех високосного
     -- BITSET(22222222h); -- для FIRST_YEAR, следующего за високосным
     -- BITSET(88888888h); -- для FIRST_YEAR=1991,1995,1999 и т.д.

  jf0 = FIRST_YEAR * 365 + 1; -- jf(FIRST_YEAR,1,1);

  shift = 03; -- номер дня недели 1-Jan-FIRST_YEAR минус 1;

PROCEDURE long?(y: INTEGER): BOOLEAN;
BEGIN
  ASSERT((y-FIRST_YEAR) IN {0..31});
  RETURN y-FIRST_YEAR IN VIS
END long?;

PROCEDURE jf(y,m,d: INTEGER): INTEGER;
  VAR tog,tog1: INTEGER;
BEGIN
  IF y<FIRST_YEAR THEN tog:=0 ELSE tog:=vis_add[y-FIRST_YEAR] END;
  tog1:=ORD(y-FIRST_YEAR IN VIS)*ORD(m>2);
  RETURN  y*365 + tog + (m-1)*30 + add[m-1] + tog1 + d;
END jf;

PROCEDURE mf(j: INTEGER; VAR y,m,d: INTEGER);
BEGIN
  ASSERT(j>=0);
  y:=j DIV 366;
  IF y>LAST_YEAR THEN y:=FIRST_YEAR; m:=1; d:=1; RETURN END;
  WHILE (y<=LAST_YEAR) & (jf(y,1,1)<=j) DO INC(y) END; DEC(y);
  m:=(j-jf(y,1,1)) DIV 28 + 1;
  IF m>12 THEN m:=12 END;
  WHILE (m<=12) & (jf(y,m,1)<=j) DO INC(m) END;
  IF m>1 THEN DEC(m) END;
  d:=j-jf(y,m,1)+1;
  ASSERT(jf(y,m,d)=j)
END mf;

----------------------------------------------------------------

PROCEDURE pack(y,m,d,h,mn,sc: INTEGER): INTEGER;
BEGIN
  IF (y<FIRST_YEAR) OR (y>LAST_YEAR)      THEN RETURN -1
  ELSIF (m IN {1,3,5,7,8,10,12}) & (d>31) THEN RETURN -1
  ELSIF (m IN {  4, 6, 9, 11  }) & (d>30) THEN RETURN -1
  ELSIF (m=2) & ( d>(28+ORD(long?(y))) )  THEN RETURN -1
  ELSIF NOT (m IN {1..12}) THEN RETURN -1
  END;
  IF (d<0) OR (h<0) OR (h>23) OR (mn<0) OR (mn>59) OR (sc<0) OR (sc>59) THEN
    RETURN -1
  END;
  RETURN ( ( (jf(y,m,d)-jf0)*24+h )*60 + mn )*60 + sc
END pack;

PROCEDURE unpack(t: INTEGER; VAR Y,M,D,H,MN,SC: INTEGER);
  VAR y,m,d,h,mn,sc: INTEGER; (* because of call: unpack(t,y,m,d,i,i,i) *)
BEGIN
  IF t<0 THEN y:=0; m:=1; d:=1; h:=0; mn:=0; sc:=0; RETURN END;
  sc:=t MOD 60; t:=t DIV 60;
  mn:=t MOD 60; t:=t DIV 60;
  h :=t MOD 24; t:=t DIV 24;
  mf(t+jf0,y,m,d);
  IF y>LAST_YEAR THEN Y:=0; M:=1; D:=1; H:=0; MN:=0; SC:=0; RETURN END;
  Y:=y; M:=m; D:=d; H:=h; MN:=mn; SC:=sc
END unpack;

PROCEDURE day(t: INTEGER): INTEGER;
BEGIN RETURN (t DIV (60*60*24) + jf0 + shift) MOD 7 + 1 END day;

PROCEDURE scan_date(VAR t: INTEGER; VAL str: ARRAY OF CHAR; night: BOOLEAN);

  VAR sep: ARRAY [0..7] OF CHAR;
       nm: ARRAY [0..7] OF INTEGER;
      err: BOOLEAN;
      i,n: INTEGER;
    h,m,s: INTEGER;
   y,mn,d: INTEGER;

  PROCEDURE get(): BOOLEAN;
    VAR val: INTEGER;
  BEGIN
    val:=0;
    WHILE (i<=HIGH(str)) & (str[i]=' ') DO INC(i) END;
    IF (str[i]<"0") OR (str[i]>"9") THEN err:=TRUE; RETURN FALSE END;
    WHILE (i<HIGH(str)) & ("0"<=str[i]) & (str[i]<="9") DO
      val:=val*10+ORD(str[i])-ORD("0"); INC(i);
      IF val>9999 THEN err:=TRUE; RETURN FALSE END;
    END;
    nm[n]:=val;
    IF i>HIGH(str) THEN sep[n]:=0c ELSE sep[n]:=str[i]; INC(i) END;
    RETURN sep[n]#0c
  END get;

BEGIN
  IF (HIGH(str)<0) OR (str="") THEN t:=-1; RETURN END;
  i:=0;
  n:=0;
  err:=FALSE;
  WHILE (n<6) & get() DO INC(n) END; INC(n);
  IF err OR (n<2) THEN t:=-1; RETURN END;
  unpack(time(),y,mn,d,h,m,s);
  i:=0;
  IF    (sep[0]='#') & (sep[1]='#') & (n>=3) THEN
    y:=nm[0]; d:=nm[1]; mn:=nm[2]; i:=3
  ELSIF (sep[0]='/') & (sep[1]='/') & (n>=3) THEN
    d:=nm[0]; mn:=nm[1]; y:=nm[2]; i:=3
  END;
  h:=0; m:=0; s:=0;
  IF i>=n THEN
    IF night THEN h:=23; m:=59; s:=59 END;
  ELSE
    IF (n<i+2) OR (sep[i]#':') THEN t:=-1; RETURN END;
    h:=nm[i]; m:=nm[i+1];
    IF n>i+2 THEN
      IF sep[i+1]#'.' THEN t:=-1; RETURN END; s:=nm[i+2]
    ELSIF night THEN
      s:=59
    END
  END;
  IF y<1900 THEN y:=y+1900 END;
  t:=pack(y,mn,d,h,m,s)
END scan_date;

END Time.
