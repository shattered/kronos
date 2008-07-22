MODULE labtam; (*$N+ Leo 18-Nov-88. (c) KRONOS *)

IMPORT       SYSTEM;
IMPORT  cod: defCodes;
IMPORT  cdf: defCode;
IMPORT  err: defErrors;
IMPORT  req: defRequest;
IMPORT  tim: Time;
IMPORT  arg: tskArgs;
IMPORT  dia: strEditor;
IMPORT  bio: BIO;
IMPORT  tty: Terminal;
IMPORT  key: Keyboard;
IMPORT  env: tskEnv;
IMPORT  str: Strings;
IMPORT  low: lowLevel;

CONST ok=err.ok;

TYPE ADDRESS = SYSTEM.ADDRESS;

VAR argc: INTEGER;

PROCEDURE take_word(VAR s: ARRAY OF CHAR);
BEGIN
  IF argc<=HIGH(arg.words) THEN
    str.copy(s,arg.words[argc]); INC(argc);
  ELSE
    s:=""
  END;
END take_word;

-------------------------  T I M E R  --------------------------
                         -------------

PROCEDURE timer(action: CHAR);

  CONST B_CLOCK = 4;  (* real time clock  *)

  CONST BASE=80000h;
        HALF=10000h;

  VAR     BCB: POINTER TO ARRAY [0..0FFFFh] OF CHAR;
         wBCB: POINTER TO ARRAY [0..0FFFFh] OF INTEGER;

  PROCEDURE wait(flag: INTEGER);
  CODE cod.copt cod.trb cod.jbsc 04 cod.drop END wait;

  PROCEDURE di; CODE cod.getm cod.li3 cod.bic cod.setm END di;

  PROCEDURE initBCB(channel: INTEGER);
    VAR adr: ADDRESS;
  BEGIN
    adr:=BASE+(0F8010h+channel*4) DIV 4;
    adr:=INTEGER(adr^) MOD HALF;
    ASSERT(adr MOD 4 = 0);
    BCB:=ADDRESS( BASE + 0F0000h DIV 4 ) + adr DIV 4; wBCB:=ADDRESS(BCB);
  END initBCB;

  PROCEDURE get_time;

    PROCEDURE from_bcd(VAR int: INTEGER; byte: CHAR);
    BEGIN int:=(ORD(byte) DIV 16) * 10 + (ORD(byte) MOD 16) END from_bcd;

    VAR sec: INTEGER;    day: INTEGER;
        min: INTEGER;  month: INTEGER;
       hour: INTEGER;   year: INTEGER;
       time: INTEGER;
          w:            ARRAY [0..2] OF INTEGER;
          b: POINTER TO ARRAY [0..9] OF CHAR;

  BEGIN
    w[0]:=wBCB^[0]; w[1]:=wBCB^[1]; w[2]:=wBCB^[2];
    b:=SYSTEM.ADR(w);

    from_bcd( sec,b^[3]);         from_bcd(  day,b^[7]);
    from_bcd( min,b^[4]);         from_bcd(month,b^[8]);
    from_bcd(hour,b^[5]);         from_bcd( year,b^[9]);    INC(year,1978);
    time:=tim.pack(year,month,day,hour,min,sec);
    tim.set_time(time);
  END get_time;
  
  PROCEDURE put_time;
  
    VAR sec: INTEGER;    day: INTEGER;
        min: INTEGER;  month: INTEGER;
       hour: INTEGER;   year: INTEGER;
       time: INTEGER;
          w:            ARRAY [0..2] OF INTEGER;
          b: POINTER TO ARRAY [0..9] OF CHAR;
  
    PROCEDURE to_bcd(int: INTEGER; offs: INTEGER);
    BEGIN b^[offs]:=CHAR( (int DIV 10) * 16 + (int MOD 10) ) END to_bcd;

  BEGIN
    w[0]:=0; w[1]:=0; w[2]:=0;
    b:=SYSTEM.ADR(w);
    time:=tim.time();
    tim.unpack(time,year,month,day,hour,min,sec);

    to_bcd( sec,3);         to_bcd(  day,7);
    to_bcd( min,4);         to_bcd(month,8);      DEC(year,1978);
    to_bcd(hour,5);         to_bcd( year,9);      b^[1]:=CHAR({7});

    di;  wait(ADDRESS(BCB)*4);
    wBCB^[0]:=w[0]; wBCB^[1]:=w[1]; wBCB^[2]:=w[2];
     BCB^[0]:=1c;
  END put_time;

BEGIN
  initBCB(B_CLOCK);
  IF    action="G" THEN get_time
  ELSIF action="P" THEN put_time
  ELSE ASSERT(FALSE)
  END;
END timer;

---------------------------  T T Y  ----------------------------
                           ---------

         (* LABTAM 3000 Terminal initialization *)

(*
PROCEDURE set_keyboard;

  PROCEDURE trans(old, new: INTEGER);
  BEGIN tty.print(""33c"?5;%d;0T\\%d"33c "\\",old,new) END trans;

BEGIN
  IF key.state^.type=2 THEN
    trans(325b,  3b);    (* Break -> alt shift U    *)
  ELSE
    trans(8Ah,86h  );      (* erase eof *)
    trans(8Bh,9Dh  );      (* erase eol *)
    trans(0E5h,321b);      (* tab set   *)
    trans(08Ch,8Fh );      (* line ins  *)
    trans(0E7h,9Ch );      (* clr       *)
    trans(08Dh,99h );      (* line del  *)
    trans(080h,323b);      (* char ins  *)
    trans(090h,7Fh );      (* char del  *)
    trans(0EDh,320b);      (* xmit      *)
    trans(0EEh,322b);      (* print     *)
    trans(0E4h,12b );      (* tab clr   *)
    trans(089h,11b );      (* tab       *)
    trans(0F2h,211b);      (* back tab  *)
    trans(0E1h,17b );      (* F12       *)  (* SI  *)
    trans(0E2h,16b );      (* F13       *)  (* SO  *)
  END
END set_keyboard;
*)

VAR  font:
  RECORD
    w,h  : INTEGER; (* char matrix size                         *)
    base : ADDRESS; (* base address of char matrix array        *)
    body : ARRAY [0..256*16-1] OF BITSET;
  END;

CONST
  lim=4*1024-1-256;

PROCEDURE appint3(VAR s: ARRAY OF CHAR; VAR co: INTEGER; d: INTEGER);
  VAR i,j: INTEGER;
BEGIN
  d:=INTEGER(BITSET(d)*{0..7});
  IF d<10 THEN
    s[co]:=CHAR(d+ORD("0")); INC(co); RETURN
  END;
  IF d<100 THEN
    s[co]:=CHAR(d DIV 10+ORD("0")); INC(co);
    s[co]:=CHAR(d MOD 10+ORD("0")); INC(co); RETURN
  END;
  s[co]:=CHAR( d DIV 100        + ORD("0")); INC(co);
  s[co]:=CHAR((d DIV 10) MOD 10 + ORD("0")); INC(co);
  s[co]:=CHAR( d         MOD 10 + ORD("0")); INC(co);
END appint3;

VAR fnt_buf: ARRAY [0..lim+256] OF CHAR;

PROCEDURE error(VAL s: ARRAY OF CHAR);
BEGIN tty.print("FONT %s\n",s) END error;

PROCEDURE load_font(VAL arg: ARRAY OF CHAR; latin: BOOLEAN);

  VAR  co,j: INTEGER;   asc: INTEGER;
       file: bio.FILE;  W,H: INTEGER;
        e,i: INTEGER;

  PROCEDURE error(VAL s: ARRAY OF CHAR): BOOLEAN;
  BEGIN
    IF bio.done THEN RETURN FALSE END;
    tty.perror(bio.error,'%s file "%s" %%s\n',s,arg); RETURN TRUE
  END error;

  PROCEDURE close;
  BEGIN
    bio.close(file);
    IF error("can't close") THEN END;
  END close;

  VAR ptr: ADDRESS; etc: bio.PATHs;

BEGIN
  bio.get_paths(etc,env.etc);
  IF error("can't get etc paths") THEN RETURN END;
  bio.lookup(etc,file,arg,"r");
  IF error("can't lookup") THEN RETURN END;
  bio.close_paths(etc);
  IF error("can't close paths") THEN RETURN END;
  bio.read(file,SYSTEM.ADR(font),3*4);
  IF error("can't read") THEN close; RETURN END;
  IF NOT (font.w IN {1..10,12,16}) OR NOT (font.h IN {1..16}) THEN
    tty.print('illegal font size [%dx%d] "%s"\n',font.w,font.h,arg);
    close; RETURN
  END;
  W:=font.w; H:=font.h;
  bio.read(file,SYSTEM.ADR(font)+3,H*256*4);
  IF error("can't read") THEN close; RETURN END;
  close;

  tty.WriteString("" 33c "[ D" 33c "[9;9 D" 33c "[8;8 D" 33c ")8");
  IF latin THEN tty.WriteString(""33c"(9")
  ELSE          tty.WriteString(""33c"(0")
  END;

  tty.print("" 33c "?20;8;%d;%d;%d;%d;32;127;1T"
                33c "?20T",font.h,font.w,font.h,font.w);
  fnt_buf:=">,32"; co:=4;
  FOR asc:=200b TO 237b DO ptr:=SYSTEM.ADR(font.body[asc*H]);
    FOR i:=0 TO H-1 DO
      fnt_buf[co]:=";"; co:=co+1; appint3(fnt_buf,co,ptr^); ptr:=ptr+1
    END;
    fnt_buf[co]:=":"; INC(co);
    IF co>lim THEN fnt_buf[co]:=0c; tty.WriteString(fnt_buf); co:=0 END;
  END;
  FOR asc:=300b TO 377b DO ptr:=SYSTEM.ADR(font.body[asc*H]);
    FOR i:=0 TO H-1 DO
      fnt_buf[co]:=";"; co:=co+1; appint3(fnt_buf,co,ptr^); ptr:=ptr+1
    END;
    fnt_buf[co]:=":"; INC(co);
    IF co>lim THEN fnt_buf[co]:=0c; tty.WriteString(fnt_buf); co:=0 END;
  END;
  IF co>0 THEN fnt_buf[co]:=0c; tty.WriteString(fnt_buf); co:=0 END;
  tty.WriteString("" 33c "\");

  IF NOT latin THEN RETURN END;

  tty.print("" 33c "?20;9;%d;%d;%d;%d;32;127;1T"
                33c "?20T",font.h,font.w,font.h,font.w);
  fnt_buf:=">,32"; co:=4;
  FOR asc:=40b TO 177b DO ptr:=SYSTEM.ADR(font.body[asc*H]);
    FOR i:=0 TO H-1 DO
      fnt_buf[co]:=";"; co:=co+1; appint3(fnt_buf,co,ptr^); ptr:=ptr+1
    END;
    fnt_buf[co]:=":"; INC(co);
    IF co>lim THEN fnt_buf[co]:=0c; tty.WriteString(fnt_buf); co:=0 END;
  END;
  IF co>0 THEN fnt_buf[co]:=0c; tty.WriteString(fnt_buf); co:=0 END;
  tty.WriteString("" 33c "\");
END load_font;

PROCEDURE tty_setup;
BEGIN
  tty.WriteString("" 33c "?44;1T" 33c "?41;723T" 33c "?42;1T");
  tty.WriteString("" 33c "?8T" 33c "(0" 33c ")0" 33c " D" );
  (* Remove all translations                           *)
  (* Announcer 3 8-bit output invironment SI/SO enable *)
  (* and both goes to font 0                           *)
--set_keyboard;
  load_font("labtam.fnt",FALSE)
END tty_setup;

---------------------------- BOOTER ----------------------------
                            --------

PROCEDURE booter5(VAL device,bootname: ARRAY OF CHAR);

  CONST z80boot = ARRAY OF INTEGER {
    0000004C3h, 00E040031h, 0509F1106h, 0CD500021h, 0C67C022Bh, 00D1C6708h,
    00011F520h, 080002100h, 0CD022BCDh, 05B1B01F2h, 020313B31h, 0415B1B43h,
    033335B1Bh, 06F724B6Dh, 032736F6Eh, 03820352Eh, 04B5B1B38h, 00011FF0Dh,
    004002168h, 07E180001h, 00B132312h, 0F720B079h, 00B000001h, 0FB20B079h,
    01B01F2CDh, 0313B315Bh, 04B284320h, 01B294752h, 06D32335Bh, 05DCDFF0Dh,
    057F42101h, 036230036h, 00B362344h, 023003623h, 000360C0Eh, 0FA200D23h,
    0210185CDh, 00C4E5800h, 00D15280Dh, 0F0210006h, 000360980h, 07E80F821h,
    03E580432h, 058003200h, 021015DCDh, 0234E57F8h, 0235E2346h, 00185CD56h,
    0B2B3B079h, 0C5D5CD28h, 00D01F2CDh, 06E6F724Bh, 0502D736Fh, 020352E32h, 
    074697571h, 00D2E6465h, 0636F7250h, 020737365h, 0CDE1C1FFh, 0606901BAh,
    0CD01BACDh, 0202C01F2h, 073756163h, 02AFF2065h, 0BACD57FCh, 001F2CD01h,
    07974203Ah, 047206570h, 0206F7420h, 0746E6F63h, 065756E69h, 020726F20h,
    07420435Eh, 06572206Fh, 0746F6F62h, 0CDFF0D2Eh, 003FE0209h, 0E60004CAh,
    02047FEDFh, 0015DCDF2h, 02257F82Ah, 0FA2A57F4h, 057F62257h, 00E57F821h,
    023003608h, 0CDFA200Dh, 093C30185h, 0E5F5F300h, 0FF11C5D5h, 028B27B7Fh,
    0F0211B25h, 0E678DB57h, 0D3E7CB07h, 000364E28h, 028D3A7CBh, 0E5280D0Ch,
    0F1E1D1C1h, 0F021E5C9h, 0E1013657h, 0CDFBC9FBh, 04E0D01F2h, 0724B206Fh,
    0736F6E6Fh, 02E32502Dh, 06F632035h, 06F72746Eh, 06C62206Ch, 00D6B636Fh,
    05F3EF3FFh, 00F3E59D3h, 0F5E576FBh, 0AF0406C5h, 08F29040Eh, 0FEFB200Dh,
    0C602380Ah, 04F30C607h, 00501F7CDh, 0F1C1E920h, 07EF5C9E1h, 028FFFE23h,
    00DFE4F0Fh, 0F7CD0520h, 0CD0A0E01h, 0EB1801F7h, 0CDE1C9F1h, 0D5E901DAh,
    01E0006C5h, 05F3EF3FFh, 0003E59D3h, 0D1C176FBh, 006C5D5C9h, 0F3FF1E02h,
    059D35F3Eh, 076FB003Eh, 0D1C17FE6h, 04F47AFC9h, 0033852EDh, 019F91803h,
    0F5C95950h, 0ED780EC5h, 04707E678h, 0B0F8E67Ch, 0ED100E47h, 0ED180E59h,
    0C9F1C151h };

  CONST base=0B4400h;

  VAR   boot: POINTER TO ARRAY [0..2*1024-1] OF INTEGER;
      buffer: ARRAY [0..8*1024-1] OF CHAR;

  PROCEDURE link(VAL bootname: ARRAY OF CHAR): INTEGER;

    TYPE process = POINTER TO RECORD G,L,PC,M,S,H,T: INTEGER END;

    VAR    p: process;
         G,F: ADDRESS;
        mg,A: ADDRESS;
        code: ADDRESS;
        info: cdf.code_ptr;
       L,eof: INTEGER;
       pc,no: INTEGER;
    size,ofs: INTEGER;

    PROCEDURE read_code(): BOOLEAN; (* FALSE when failure *)

      PROCEDURE error(VAL s: ARRAY OF CHAR): BOOLEAN;
      BEGIN
        IF bio.done THEN RETURN FALSE END;
        tty.perror(bio.error,'%s file "%s" %%s\n',s,bootname); HALT(1)
      END error;

      VAR i: INTEGER;
          f: bio.FILE;
        bin: bio.PATHs;

    BEGIN
      bio.get_paths(bin,env.bin);
      IF error('open_path') THEN bin:=bio.here END;
      bio.lookup(bin,f,bootname,"r");
      IF error("can't lookup") THEN RETURN FALSE END;
      bio.close_paths(bin);
      IF error("can't close paths") THEN RETURN FALSE END;
      eof:=bio.eof(f);
      IF error("can't get eof ") THEN RETURN FALSE END;
      IF (eof+3) DIV 4 > SIZE(boot^)-256-16 THEN
        tty.print('"%s" longer then %dKB',bootname,BYTES(buffer) DIV 1024);
        RETURN FALSE
      END;
      code:=SYSTEM.ADR(boot^[256+16]);

      bio.read(f,code,eof);
      IF error("can't read ") THEN RETURN FALSE END;
      bio.close(f);
      IF error("can't close ") THEN END;
      eof:=(eof+3) DIV 4;
      info:=code;
      RETURN TRUE
    END read_code;

  BEGIN
    info:=NIL;
    IF NOT read_code() THEN RETURN 0 END;
    F :=code+16+info^.str_size;
    pc:=F^;
    F :=F-code+16;
    G :=   eof+16+2;
    boot^[G+255]:=G+base-2;              (* 2 word local dft *)
    boot^[G+254]:=G+base;
    boot^[G+256]:=F +base;
    boot^[G+257]:=16+base+16;
    L :=G + info^.glo_size + 4;
    p:=SYSTEM.ADR(boot^[256]);
    p^.G:=G+base;       p^.M:=0;
    p^.L:=L+base;       p^.S:=L+6+base;
    p^.PC:=pc;          p^.H:=12*256+base-1;
    p^.T:=0;
    no:=info^.no_mg;
    mg:=code+16+info^.code_size+info^.str_size;
    WHILE no>0 DO
      ofs :=mg^; INC(mg);
      size:=mg^; INC(mg);   DEC(p^.H,size);  no:=no-1
    END;
    IF p^.H < p^.S+4*256 THEN
      tty.print("bootstrap globals+stack too long"); RETURN 0
    END;
    no:=info^.no_mg;
    A :=p^.H+1;
    mg:=code+16+info^.code_size+info^.str_size;
    WHILE no>0 DO
      ofs :=mg^; INC(mg);
      size:=mg^; INC(mg);  boot^[G+256+ofs]:=A; INC(A,size); no:=no-1
    END;
    boot^[L+256+5]:=0; (* ESD *)
    RETURN 256+L+5
  END link;

  VAR F: bio.FILE;
    spc: req.REQUEST;

  PROCEDURE bio_error(op: ARRAY OF CHAR);
  BEGIN
    tty.perror(bio.error,'%s("%s") %%s\n',op,device)
  END bio_error;

  PROCEDURE get_spec;
  BEGIN
    low.zero(spc);
    spc.op:=req.GET_SPEC;
    bio.doio(F,spc);
    IF NOT bio.done THEN bio_error('get_spec'); HALT(bio.error) END
  END get_spec;

  PROCEDURE set_spec;
  BEGIN
    spc.op :=req.SET_SPEC;
    bio.doio(F,spc);
    IF NOT bio.done THEN bio_error('set_spec'); HALT(bio.error) END
  END set_spec;

  VAR i: INTEGER;
      a: ADDRESS;
     rs: INTEGER;
    lab: ARRAY [0..12] OF CHAR;
    LAB: POINTER TO ARRAY [0..511] OF CHAR;
   size: INTEGER;

BEGIN
  bio.open(F,device,'cX');
  IF NOT bio.done THEN bio_error('open'); HALT(bio.error) END;
  low.zero(buffer);
  boot:=SYSTEM.ADR(buffer);
  a:=boot;
  FOR i:=0 TO HIGH(z80boot) DO a^:=z80boot[i]; INC(a) END;
  get_spec;
  WITH spc DO
    str.print(lab,"xd%03h%02h%1h%1h%02h "
                 ,cyls,maxsec-minsec+1,heads,ssc,ressec);
    LAB:=SYSTEM.ADR(buffer);
    FOR i:=0 TO 11 DO LAB^[35h+i]:=lab[i] END;

    size:=link(bootname)*4;
    IF size=0 THEN RETURN END;

    i:=ressec*secsize;
    IF size>i THEN
      tty.print("bootstrap (%dKB+%d) longer then reserved area (%dKB+%d)"
                ,size DIV 1024, size MOD 1024, i DIV 1024, i MOD 1024);
      RETURN
    END;
    rs:=ressec;    ressec:=0;   set_spec;
  END;                          ---------
  bio.close(F);
  bio.open(F,device,'cX');
  IF NOT bio.done THEN bio_error('open'); HALT(bio.error) END;
  bio.put(F,buffer,size);
  IF  bio.done THEN
    tty.print("bootstrap for Z80 writen\n")
  ELSE
    bio_error('write boot')
  END;
  spc.ressec:=rs;       set_spec;
  bio.close(F);         ---------
  IF NOT bio.done THEN bio_error('close') END
END booter5;

--------------------------  B O D Y  ---------------------------
                          -----------

PROCEDURE help;
BEGIN
  tty.print("\n\n\n");
  tty.print(" labtam get time  -- get time from LABTAM 30XX clock\n");
  tty.print(" labtam put time  -- put time to   LABTAM 30XX clock\n");
  tty.print(" labtam tty       -- program terminal\n");
  tty.print(" labtam z80 /dev/dk sys25boo\n");
  tty.print("\n\n\n"); HALT
END help;

VAR op: ARRAY [0..63] OF CHAR;
  name: ARRAY [0..63] OF CHAR;
   dev: ARRAY [0..63] OF CHAR;

BEGIN
  argc:=0;
  IF (arg.flag('-','h')) OR (HIGH(arg.words)<0) THEN help END;
  LOOP
    take_word(op);
    op[4]:=0c;
    IF    op="put"  THEN take_word(op);
      IF op="time"  THEN timer('P') END
    ELSIF op="get"  THEN take_word(op);
      IF op="time"  THEN timer('G') END
    ELSIF op="tty"  THEN tty_setup
    ELSIF op="z80"  THEN take_word(dev);
                         take_word(op);
                         IF op="" THEN op:="sys25boo" END;
                         str.print(name,"%s.cod",op);
                         booter5(dev,name)
    ELSIF op="" THEN EXIT
    ELSE help; HALT
    END
  END
END labtam.
