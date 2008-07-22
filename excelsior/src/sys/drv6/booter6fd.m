MODULE booter; (*$T-$N-$I- Leo 20-Dec-89. (c) KRONOS *)
                       (* Hady 02-Mar-90. (c) KRONOS *)

(* booter from FD4110 for 2.6 in Elorg *)

IMPORT  cod: defCodes;
IMPORT  sys: SYSTEM;

TYPE ADDRESS = sys.ADDRESS;
        WORD = sys.WORD;

PROCEDURE move(to,from: ADDRESS; words: INTEGER); CODE cod.move END move;
PROCEDURE quit;                           CODE cod.quit  END quit;
PROCEDURE transfer(VAR from,to: ADDRESS); CODE cod.tra   END transfer;
PROCEDURE S_reg(): ADDRESS;               CODE cod.li0   cod.alloc END S_reg;
PROCEDURE inp(reg: INTEGER): BITSET;      CODE cod.inp   END inp;
PROCEDURE out(reg: INTEGER; w: sys.WORD); CODE cod.out   END out;
PROCEDURE reset;                          CODE cod.reset END reset;
PROCEDURE getm(): BITSET ; CODE cod.getm END getm;
PROCEDURE setm(m: BITSET); CODE cod.setm END setm;

----------------------------  TTY   ----------------------------

CONST  ttCSR=177564b DIV 2;  ttDTR=ttCSR+1;

PROCEDURE Write(ch: CHAR);
BEGIN
  REPEAT UNTIL 7 IN inp(ttCSR);
  out(ttDTR,ORD(ch) MOD 128);
END Write;

PROCEDURE write_str(VAL s: ARRAY OF CHAR);
  VAR i: INTEGER;
BEGIN i:=0;
  WHILE (i<=HIGH(s)) & (s[i]#0c) DO Write(s[i]); INC(i) END;
END write_str;

PROCEDURE write_int(i: INTEGER; n: INTEGER);

  VAR str: ARRAY [0..15] OF CHAR; s: INTEGER;

  PROCEDURE putDig(i: INTEGER; n: INTEGER);
    VAR d: INTEGER;
  BEGIN
    d:=i MOD 10; i:=i DIV 10; DEC(n);
    IF i=0 THEN
      WHILE n>0 DO str[s]:=' '; INC(s); DEC(n) END
    ELSE
      putDig(i,n)
    END;
    str[s]:=CHAR(ORD('0')+d); INC(s);
  END putDig;

BEGIN s:=0;
  IF n<=0 THEN n:=1 END;
  IF i<0 THEN str[s]:='-'; INC(s); DEC(n); i:=ABS(i) END;
  putDig(i,n);
  str[s]:=0c; write_str(str);
END write_int;

PROCEDURE write_hex(w: sys.WORD; n: INTEGER);
  CONST hex="0123456789ABCDEF";
  VAR str: ARRAY [0..15] OF CHAR; s: INTEGER;
      i,d: INTEGER;
BEGIN s:=0;
  IF n<=0 THEN n:=8 END;
  WHILE n>8 DO str[s]:=' '; INC(s); DEC(n) END;
  i:=8-n;
  WHILE i>0 DO w:=(BITSET(w)<<4); DEC(i) END;
  i:=1;
  WHILE i<=n DO
    w:=(BITSET(w)<<4); str[s]:=hex[INTEGER(w) MOD 16]; INC(s); INC(i);
  END;
  str[s]:=0c; write_str(str);
END write_hex;

----------------------------  DISK  ----------------------------

VAR boot_device: ARRAY [0..3] OF CHAR;

CONST (* REGISTERs & CONSTANTs *)

   fdCSR=177170b DIV 2; fdDTR=fdCSR+1;

   double=400b; (* 256 *)
   single=000b; (* 128 *)

   density = double;
   bps     = density DIV 2 + 128;  -- bytes per sector (128 or 256)

   sectors =26;
   tracks  =77;

CONST (*  COMMANDs  *)  readbuf  = 003b;  read    = 007b;
                        readstat = 013b;  readerr = 017b;

CONST (* BOOT DRIVE *)  drive = 0;

PROCEDURE wait(bit: BITSET);
BEGIN REPEAT UNTIL (bit*inp(fdCSR)#{}) END wait;

PROCEDURE read_sector(trk,sec: INTEGER; VAR err: INTEGER);
BEGIN err:=0;
  sec:=sec+1;
  wait({5}); out(fdCSR,read+drive*20b+density);
  wait({7}); out(fdDTR,sec);
  wait({7}); out(fdDTR,trk);
  wait({5});
  IF {15}*inp(fdCSR)#{} THEN err:=INTEGER(inp(fdCSR)); RETURN END;
  out(fdCSR,readbuf+density+drive*20b);
  wait({7}); out(fdDTR,bps DIV 2);
  wait({7}); out(fdDTR,0);
  wait({5});
  IF {15}*inp(fdCSR)#{} THEN err:=INTEGER(inp(fdCSR)) END;
END read_sector;

PROCEDURE sector_io(track,sector: INTEGER; buf: sys.ADDRESS; VAR err: INTEGER);
  VAR try: INTEGER;
BEGIN try:=4;
  REPEAT
    read_sector(track,sector,err);
    IF err#0 THEN
      out(fdCSR,40000b+drive*20b);
      try:=try-1
    END
  UNTIL (err=0) OR (try<=0);
  move(buf,400000h,bps DIV 4)
END sector_io;

PROCEDURE read_block(bno: INTEGER; buf: sys.ADDRESS; VAR err: INTEGER);

VAR     f_sec: INTEGER;
     f_ph_sec: INTEGER;
    f_log_sec: INTEGER;
      log_sec: INTEGER;
      max_sec: INTEGER;
        round: INTEGER;
         size: INTEGER;
       sector: INTEGER;
        track: INTEGER;
BEGIN
  write_int(bno,0); Write(15c);

  size:=4096 DIV bps; -- block size in sectors
  f_sec:=bno*size MOD sectors;
  track:=bno*size DIV sectors;
  f_ph_sec :=f_sec;
  f_log_sec:=0;

  max_sec  :=size+f_sec;
  IF max_sec>sectors THEN max_sec:=sectors END;

  WHILE size>0 DO
    log_sec:=f_log_sec+1;
    sector :=f_ph_sec +1;
    round  :=0;
    WHILE round<2 DO
      WHILE sector<max_sec DO
        sector_io(track,sector,buf+log_sec*(bps DIV 4),err);
        IF err#0 THEN RETURN END;
        INC(log_sec,2); INC(sector,2); DEC(size);
      END;
      round:=round+1; log_sec:=f_log_sec; sector:=f_ph_sec;
    END;
    INC(track);
    f_log_sec:=sectors-f_sec;
    f_ph_sec :=0;
    max_sec  :=(4096 DIV bps)+f_sec-sectors;
  END;
END read_block;

PROCEDURE init_disks; BEGIN out(fdCSR,40000b+drive*20b) END init_disks;

---------------------------- BOOTER ----------------------------
                            --------

CONST KB = 256; (* words in 1KB *)
      system_base = 64*KB;

VAR top : ADDRESS;  (* :=MemoryTop() *)
    size: INTEGER;

PROCEDURE memory_top(): ADDRESS;
  CONST bank = 256*KB;  pattern=12345678h;
  VAR a: ADDRESS;
BEGIN
  a:=S_reg(); move(a,0,bank-a); -- filling first bank
  a:=bank;
  LOOP
   a^:=pattern;
   IF INTEGER(a^)#pattern THEN EXIT END;
   move(a,0,bank); a:=a+bank;
   IF a>=100000h          THEN EXIT END;
  END;
  RETURN a-1;
END memory_top;

PROCEDURE read_system;
  CONST long={2};
  VAR adr: ADDRESS;     buf: ADDRESS;   err: INTEGER;
      lim: ADDRESS;     bno: INTEGER;
      ino: ADDRESS;     ref: ADDRESS;
     mode: POINTER TO BITSET;
     link: POINTER TO INTEGER;
      eof: INTEGER;

  PROCEDURE bad;
  BEGIN write_str("illegal SYSTEM.BOOT file" 12c 15c); quit END bad;

BEGIN
  ino:=system_base-1024*2;
  ref:=system_base-1024*1;
  REPEAT read_block(2,ino,err) UNTIL err=0;
  ino :=ino+16;
  adr :=ino+10;
  eof :=adr^ ; eof:=(eof+4095) DIV 4096;
  link:=ino+9;
  mode:=ino+8;
  IF link^<=0 THEN bad END;
  IF long*mode^={} THEN
    IF eof>8 THEN bad END;
    move(ref,ino,eof)
  ELSE
    IF INTEGER(ino^)<=0 THEN bad END;
    REPEAT read_block(ino^,ref,err) UNTIL err=0
  END;
  buf:=system_base;
  lim:=top-256;
  bno:=ref^;
  size:=0;
  WHILE eof>0 DO
    IF buf+1024>=lim THEN
      write_str("#booter: too small memory" 12c 15c); quit
    END;
    IF bno<=0 THEN bad  END;
    read_block(bno,buf,err);
    IF err=0 THEN
      INC(buf,1024); INC(ref); bno:=ref^; INC(size,1024); DEC(eof)
    ELSE
      write_str(""15c"block  ");    write_int(bno,4);
      write_str(" read error ");    write_hex(err,8);
      write_str("" 12c 15c    )
    END
  END
END read_system;

TYPE process = POINTER TO
     RECORD
       G: ADDRESS;   L: ADDRESS;
      PC: INTEGER;   M: BITSET;
       S: ADDRESS;   H: ADDRESS;
       T: INTEGER;
     END;

PROCEDURE start_system;
  TYPE c = BITSET;
  VAR adr: ADDRESS;
      dev: POINTER TO ARRAY [0..3] OF CHAR;
        p: process;
BEGIN

  adr:=system_base+88h; adr^:=top;
  adr:=system_base+89h; dev :=adr;
  dev^:=boot_device;

  adr:=top-256;
  p:=adr;
  p^.G:=adr+08; p^.L :=adr+10;
  p^.M:={};     p^.PC:=0;
  p^.H:=top;    p^.S :=adr+20;
  p^.T:=0;      p^.G^:=adr+09;

  (* stack *)
  adr  :=p^.S;
  DEC(adr);     adr^:=5;
  DEC(adr);     adr^:=0;
  DEC(adr);     adr^:=1;
  DEC(adr);     adr^:=0;
  DEC(adr);     adr^:=system_base;
  DEC(adr);     adr^:=size;
  (* code *)
  adr:=p^.G^;
  adr^:=c(cod.move)+c(cod.tra<<8)+c(cod.quit<<16);
  transfer(p,p);
END start_system;

VAR i: INTEGER;

BEGIN
  --quit;
  reset;
  FOR i:=0 TO 20000 DO END;
  init_disks;
  top:=memory_top();
  boot_device:="fd";
  write_str("Available memory ");
  write_int((top+1) DIV 256,4); write_str("KB   boot ");
  write_str(boot_device);       write_str("" 15c 12c);
  read_system; start_system;
END booter.
