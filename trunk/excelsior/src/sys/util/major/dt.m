MODULE dt; (* Leo   30-Nov-86. (c) KRONOS *)
           (* Igo   15-Feb-89. (c) KRONOS *)
           (* Hady. 18-Jul-88. (c) KRONOS *)
           (* Hady. 10-Oct-89. (c) KRONOS *)
           (* Ned   30-Nov-89. (c) KRONOS *)
           (* Hady. 05-Apr-90. (c) KRONOS *)
           (* Hady. 23-Nov-90. (c) KRONOS *)
           (* Leo.  25-Apr-91. (c) KRONOS *)

IMPORT  tim: Time;
IMPORT  arg: tskArgs;
IMPORT  tty: Terminal;
IMPORT  str: Strings;
IMPORT  std: StdIO;
IMPORT  key: Keyboard;


CONST (* ACHTUNG ! This format must not be changed !!! *)
   time_format = " %3.3s  %3.3s %02d  %02d:%02d.%02d  %04d. [%d:%02d]";

(*              0123456789012345678901234567890
                 Thu  Apr 05  16:58.33  1990.            *)
  Margin = 20;

VAR
  Line: INTEGER;

--------------------------- TIMES ------------------------------
                           -------

VAR time: INTEGER;

CONST p_mon   = 00;  p_day   = 01;
      p_hour  = 02;  p_min   = 03;
      p_sec   = 04;  p_year  = 05;

poss = ARRAY OF INTEGER { Margin+06, Margin+10, Margin+14,
                          Margin+17, Margin+20, Margin+24};

TYPE str3= ARRAY [0..3] OF CHAR;
VAR monthes: ARRAY [1..12] OF str3;
       days: ARRAY [1..7]  OF str3;

VAR Mon, Year, Hour, Min, Sec, Day: INTEGER;

PROCEDURE AppTime(VAR s: ARRAY OF CHAR; time: INTEGER);
  VAR st: INTEGER;
BEGIN
  tim.unpack(time, Year, Mon, Day, Hour, Min, Sec);
  st:=tim.sys_time(tim.sec) DIV 60;
  str.print(s, time_format, days[tim.day(time)]
                          , monthes[Mon], Day, Hour, Min, Sec, Year
                          , st DIV 60, st MOD 60 );
END AppTime;

PROCEDURE wr_time(tt: BOOLEAN);
  VAR bump: ARRAY [0..80] OF CHAR;
BEGIN
  AppTime(bump,time);
  IF tt OR std.is_tty(std.out) THEN
    tty.set_pos(Line,Margin); tty.set_reverse(1);
    tty.WriteString(bump);
    tty.set_reverse(0); tty.erase_line(0)
  ELSE
    std.print("%.*c%s\n",Margin," ",bump)
  END
END wr_time;

PROCEDURE write_time(tim: INTEGER);
BEGIN time:=tim; wr_time(FALSE) END write_time;

PROCEDURE edit_time(VAR tm: INTEGER): BOOLEAN;

  VAR pos: INTEGER;

  PROCEDURE max_day(Mon, Year: INTEGER): INTEGER;
  BEGIN
    ASSERT(Mon IN {1..12});
    IF (Mon=2) & (Year MOD 4 = 0) &
       ((Year MOD 100 # 0) OR (Year MOD 400 = 0)) THEN RETURN 29
    ELSIF Mon=2 THEN RETURN 28;
    ELSIF Mon IN {1,3,5,7,8,10,12} THEN RETURN 31
    ELSE  RETURN 30
    END;
  END max_day;

  PROCEDURE sync_time;
  BEGIN
    IF Day>max_day(Mon,Year) THEN Day:=max_day(Mon, Year) END;
    time:=tim.pack(Year,Mon,Day,Hour,Min,Sec);
    wr_time(TRUE)
  END sync_time;

  PROCEDURE sync;
  BEGIN
    tty.set_pos(Line,poss[pos])
  END sync;

  CONST digits = ARRAY OF CHAR {"0","1","2","3","4",
                                "5","6","7","8","9"};

  VAR ch: CHAR;

  PROCEDURE edit2(VAR val: INTEGER; l,h: INTEGER);
    VAR d: ARRAY [0..3] OF INTEGER;
        p: INTEGER;

    PROCEDURE check(): BOOLEAN;
      VAR i: INTEGER;
    BEGIN
      i:=d[0]*10+d[1];
      RETURN (i>=l) & (i<=h)
    END check;

  BEGIN p:=0;
    val:=val MOD 100;
    d[0]:=val DIV 10;
    d[1]:=val MOD 10;
    tty.set_reverse(1);
    LOOP key.read(ch);
      CASE ch OF
        |key.right : IF p>0 THEN EXIT END;
                     tty.right(1); p:=1
        |key.left  : IF p=0 THEN EXIT END;
                     tty.left(1) ; p:=0
        |key.up,"+":
           REPEAT d[p]:=(d[p]+1) MOD 10 UNTIL check();
           tty.Write(digits[ORD(d[p])]);  tty.left(1)
        |key.dw,"-":
           REPEAT d[p]:=(d[p]+9) MOD 10 UNTIL check();
           tty.Write(digits[ORD(d[p])]);  tty.left(1)
        |key.cr, 033c: EXIT
      ELSE
      END;
    END;
    tty.set_reverse(0);
    val:=d[0]*10+d[1]
  END edit2;

  PROCEDURE edit_dig(VAR val: INTEGER; l,h: INTEGER);
    VAR old,D: INTEGER;
  BEGIN
    D:=h-l;
    LOOP
      key.read(ch);
      IF    (ch="+") OR (ch=key.up) THEN
        val:=l+((val-l+1) MOD (D+1)); sync_time; sync
      ELSIF (ch="-") OR (ch=key.dw) THEN
        val:=l+((val-l+D) MOD (D+1)); sync_time; sync
      ELSIF (ch=33c)      OR (ch=key.cr)    OR
            (ch=key.left) OR (ch=key.right) THEN EXIT
      END;
    END;
  END edit_dig;

BEGIN
  time:=tm; wr_time(TRUE);
  pos:=p_mon; sync;
  LOOP
    CASE pos OF
      |p_mon : edit_dig(Mon,1,12)
      |p_day : edit_dig(Day,1,max_day(Mon,Year))
      |p_hour: edit2(Hour,0,23)
      |p_min : edit2(Min ,0,59)
      |p_sec : edit2(Sec ,0,59)
      |p_year: edit_dig(Year,1986,2017)
    END;
    IF    ch=key.left  THEN pos:=(pos+5) MOD 6; sync
    ELSIF ch=key.right THEN pos:=(pos+1) MOD 6; sync
    ELSIF (ch=key.cr) OR (ch=33c) THEN  EXIT
    END
  END;
  sync_time; tm:=time; RETURN ch#033c
END edit_time;


CONST SYNTAX =
    "syntax of string TIME:\n"
    "   TIME ::=   [DD/MN/[19]YY,][HH:MM[.SS]]\n"
    "            | [[19]YY#DD#MM,][HH:MM[.SS]]\n"
    "   if date is ommited default is TODAY\n"
    "   if HH:MM.SS is ommited default is 00:00.00\n"
    "\n";

PROCEDURE usage;
BEGIN
  std.print(
    '  "dt"  dATE & tIME service utility program v1.4.1. (c) KRONOS\n');
  std.print(
    "usage:\n"
    "   dt      -- show current time\n"
    "   dt -h   -- help\n"
    "   dt -#   -- version\n"
    "   dt -c   -- timer (breaks by any key hit)\n"
    "   dt -e   -- edit system time\n"
    "   dt TIME -- set time specified by string TIME\n"
    "\n");
  std.print(SYNTAX);
  std.print(
    "-e keys:\n"
    '   (up,"+") / (down,"-") -- increment/decrement\n'
    "   left, right -- movies\n"
    "   cr          -- exit and set time\n"
    "   escape      -- cancel\n");
  std.print("                                   Hady, Nov 23 90.\n")
END usage;

VAR stime: INTEGER;  x: CHAR;

BEGIN
  tty.nop;
  Line:=tty.state^.lines-1;

  monthes [1] := "Jan";   monthes [2] := "Feb";
  monthes [3] := "Mar";   monthes [4] := "Apr";
  monthes [5] := "May";   monthes [6] := "Jun";
  monthes [7] := "Jul";   monthes [8] := "Aug";
  monthes [9] := "Sep";   monthes[10] := "Oct";
  monthes[11] := "Nov";   monthes[12] := "Dec";

  days[1] := "Mon";  days[2] := "Tue";
  days[3] := "Wed";  days[4] := "Thu";
  days[5] := "Fri";  days[6] := "Sat";
  days[7] := "Sun";
  IF arg.flag('-','h') THEN usage; HALT END;
  IF arg.flag('-','#') THEN
    std.print('  dt v1.6.0 /25-Apr-91/ (c) KRONOS\n');
    HALT
  END;
  stime:=tim.time();
  IF arg.flag('-','e') THEN
    tty.reset;
    IF edit_time(stime) THEN tim.set_time(stime) END
  ELSIF arg.flag('-','c') THEN
    tty.set_cursor(0);
    REPEAT
      tim.delay(1000,tim.milisec);
      stime:=tim.time();
      write_time( stime )
    UNTIL key.ready()>0;
    key.read(x);
    tty.set_cursor(1)
  ELSIF HIGH(arg.words)>=0 THEN
    IF HIGH(arg.words)>=1 THEN arg.pack_words(0,1) END;
    tim.scan_date(stime,arg.words[0],FALSE);
    IF stime<0 THEN
      std.print('*** illegal time/date specification'
                ' "%.32s" ***\n\n',arg.words[0]);
      std.print(SYNTAX); HALT
    END;
    tim.set_time(stime);
    write_time(stime)
  ELSE write_time(stime)
  END;
  tty.WriteLn
END dt.
