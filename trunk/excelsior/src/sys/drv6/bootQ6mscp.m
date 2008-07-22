MODULE bootQ6mscp; (*$T-$I- Leo 22-Mar-90. (c) KRONOS *)

IMPORT       SYSTEM;
IMPORT  cod: defCodes;

TYPE WORD = SYSTEM.WORD;
  ADDRESS = SYSTEM.ADDRESS;

CONST
  KB = 256; (* words in 1KB *)

(*
  on screen errors:
      RGB
      030h (green)      -- SYSTEM.BOOT file absent on volume
      003h (blue)       -- SYSTEM.BOOT bad file structure
      300h (red)        -- SYSTEM.BOOT read error
      330h (yellow)     -- SYSTEM.BOOT read failure (RETRIES exhaused)
      033h (light blue) -- no memory for SYSTEM.BOOT
*)

PROCEDURE move(t,f: ADDRESS; s: INTEGER); CODE cod.move END move;
PROCEDURE QUIT; CODE cod.quit END QUIT;
PROCEDURE inp(reg: INTEGER): BITSET;      CODE cod.inp END inp;
PROCEDURE out(reg: INTEGER; w: WORD); CODE cod.out END out;
PROCEDURE zero(a: ADDRESS; size: INTEGER);
BEGIN a^:=0; move(a+1,a,size-1) END zero;


---------------------------- SCREEN ----------------------------
                            --------
(*
PROCEDURE color(c: INTEGER);
  CONST p_trans = ARRAY OF INTEGER
        {0Fh,07,0Bh,03,0Dh,05,09h,01,0Eh,06,0Ah,02,0Ch,04,08h,00};
  CONST WHITE = p_trans[2*4]+p_trans[2*4]*16+p_trans[2*4]*256;
  VAR i,r,g,b: INTEGER; a,l: ADDRESS;
BEGIN
  b:=c MOD 16; c:=c DIV 16;
  g:=c MOD 16; c:=c DIV 16;
  r:=c MOD 16; a:=1F0010h;
  c:=p_trans[r*4]+p_trans[g*4]*16+p_trans[b*4]*256;
  i:=4000;     l:=1F0020h;
  REPEAT DEC(i) UNTIL (i=0) OR (BITSET(l^)*{0}#{});
  l:=1F0000h;  l^:=0;
  FOR i:=0 TO 15 DO
    IF ODD(i) THEN a^:=WHITE ELSE a^:=c END; INC(a);
  END
END color;

PROCEDURE erase;
  VAR a: ADDRESS;
BEGIN
  color(000h);
  a:=1F8000h; a^:=0; move(a+1,a,128*KB-1)
END erase;

PROCEDURE dot(i: INTEGER);
  VAR a: POINTER TO ARRAY [0..3] OF CHAR;
BEGIN
  a:=ADDRESS(1F8000h+350*16+i DIV 4); a^[i MOD 4]:=30c;
END dot;

PROCEDURE delay;
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO 25000 DO END
END delay;
*)
------------------------------------------------------------------

CONST
  CSR=177564b DIV 2;
  DTR=CSR+1;

PROCEDURE Write(ch: CHAR);
BEGIN
  REPEAT UNTIL 7 IN inp(CSR);
  out(DTR,ORD(ch) MOD 128);
END Write;

PROCEDURE write_str(VAL s: ARRAY OF CHAR);
  VAR i: INTEGER;
BEGIN i:=0;
  WHILE (i<=HIGH(s)) & (s[i]#0c) DO Write(s[i]); INC(i) END;
END write_str;

PROCEDURE error(r: INTEGER);
BEGIN
 CASE r OF
   0: write_str("OK   "); RETURN;
  |1: write_str("file absent on volume");
  |2: write_str("bad file structure");
  |3: write_str("read error");
  |4: write_str("read failure");
  |5: write_str("no memory for SYSTEM.BOOT");
 END;
 Write(15c); Write(12c); IF (r#3) THEN QUIT END;
END error;

PROCEDURE dot(i: INTEGER);

  PROCEDURE write_int(i: INTEGER;);
    VAR d: INTEGER;
  BEGIN
   REPEAT
    d:=i MOD 10; i:=i DIV 10;
    Write(CHAR(ORD('0')+d));
   UNTIL i=0;
  END write_int;

BEGIN
 Write(' '); Write(' '); write_int(i); Write(15c);
END dot;

PROCEDURE erase;
BEGIN
 Write(33c); Write('['); Write('H');
 Write(33c); Write('['); Write('J');
END erase;


---------------------------- SERIALS ---------------------------
                            ---------

PROCEDURE reset_serials;
  VAR i: INTEGER;
    csr: ADDRESS;
BEGIN
  FOR i:=0 TO 3 DO
    csr:=ADDRESS(8200F1h+i*2);
    csr^:={};  csr^:={};  csr^:={};  csr^:={};  csr^:={6};
  END;
  csr :=ADDRESS(17E003h);
  csr^:=0;
END reset_serials;

----------------------------- DISK -----------------------------
                             ------

------------------  REGISTERS  & CONSTANTS  --------------------
                  --------------------------
CONST

  IP=172150b DIV 2; SA=IP+1;

  QBUS = 400000h;

-------------------------  COMMANDS  ---------------------------
                         ------------

  read_op   = 41c;        read_answ   = 241c;
  online_op = 11c;        online_answ = 211c;


------------------------  BOOT DRIVE  --------------------------
                        --------------
   drive    = 0;

VAR
  link_area: POINTER TO
  RECORD
    rfe00 : INTEGER;
    iptcod: INTEGER;
    rsp   : BITSET;
    cmd   : BITSET;
  END;

TYPE packet = POINTER TO
  RECORD
    len: INTEGER;
    CASE :INTEGER OF
    |0: b: ARRAY [0..63] OF CHAR;
    |1: w: ARRAY [0..15] OF INTEGER;
    END;
  END;

VAR free: ADDRESS;
  c_pack: packet;
  r_pack: packet;
  buffer: POINTER TO ARRAY [0..4095] OF CHAR;
 counter: INTEGER;

PROCEDURE wait(bit: BITSET); BEGIN REPEAT UNTIL bit*inp(SA)#{} END wait;

PROCEDURE c_wait(bit: BITSET): BOOLEAN;
BEGIN wait(bit); RETURN {15}*inp(SA)#{} END c_wait;

PROCEDURE new(size: INTEGER): ADDRESS;
BEGIN free:=free+size; RETURN free-size END new;

PROCEDURE tie_pack(VAR link: BITSET; VAR pack: packet);
BEGIN
  link:=BITSET( (SYSTEM.ADR(pack^.b)-QBUS)*4 ) + {31};
END tie_pack;

PROCEDURE wait_response(answer: CHAR): INTEGER;
BEGIN
  REPEAT
    zero(r_pack,SIZE(r_pack^));
    r_pack^.len:=64;
    tie_pack(link_area^.rsp,r_pack);
    REPEAT UNTIL ({31}*link_area^.rsp={}) OR ({15}*inp(SA)#{});
    IF {15}*inp(SA)#{} THEN RETURN -1 END;
  UNTIL (r_pack^.w[0]=counter) & (r_pack^.b[8]=answer);
  RETURN ORD(r_pack^.b[10]) MOD 32;
END wait_response;

PROCEDURE on_line(): BOOLEAN;
BEGIN
  counter:=counter+1;
  zero(c_pack,SIZE(c_pack^));
  c_pack^.len :=48;
  c_pack^.w[0]:=counter;
  c_pack^.w[1]:=drive;
  c_pack^.b[8]:=online_op;

  tie_pack(link_area^.cmd,c_pack);
  IF inp(IP)#{} THEN END;
  RETURN wait_response(online_answ)=0;
END on_line;

PROCEDURE init(): BOOLEAN;
BEGIN
  free:=QBUS;
  counter:=334;
  link_area:=new(SIZE(link_area^));
  link_area^.rfe00:=0;
  zero(link_area,SIZE(link_area^));
  c_pack:=new(SIZE(c_pack^));
  r_pack:=new(SIZE(r_pack^));
  buffer:=new(SIZE(buffer^));

  out(IP,4000b);
  IF c_wait({11}) THEN RETURN FALSE END;
  out(SA,{15});
  IF c_wait({12}) THEN RETURN FALSE END;
  out(SA,(SYSTEM.ADR(link_area^.rsp)-QBUS)*4);
  IF c_wait({13}) THEN RETURN FALSE END;
  out(SA,0);    -- high 6 bits of address
  IF c_wait({14}) THEN RETURN FALSE END;
  out(SA,{0});
  RETURN on_line();
END init;

PROCEDURE get_block(bno: INTEGER; buf: ADDRESS; VAR err: INTEGER);
BEGIN
  counter:=counter+1;
  zero(c_pack,SIZE(c_pack^));
  c_pack^.len :=48;
  c_pack^.w[0]:=counter;
  c_pack^.w[1]:=drive;
  c_pack^.w[3]:=4096;
  c_pack^.w[4]:=(ADDRESS(buffer)-QBUS)*4;
  c_pack^.b[8]:=read_op;
  c_pack^.w[7]:=bno*8;

  tie_pack(link_area^.cmd,c_pack);

  IF inp(IP)#{} THEN END;

  err:=wait_response(read_answ);
  move(buf,buffer,SIZE(buffer^));
END get_block;

VAR firstly: BOOLEAN;

PROCEDURE read_block(bno: INTEGER; buf: ADDRESS; VAR err: INTEGER);
BEGIN
  LOOP
    IF NOT firstly THEN
      get_block(bno,buf,err);
      IF err#-1 THEN RETURN END;
    END;
    REPEAT UNTIL init(); firstly:=FALSE;
  END;
END read_block;

---------------------------- BOOTER ----------------------------
                            --------

CONST system_base = 64*256;

VAR top : ADDRESS;  (* :=MemoryTop() *)
    size: INTEGER;

PROCEDURE memory_top(): ADDRESS;
  CONST bank = 256*KB;  pattern=12345678h;
  PROCEDURE S_reg(): ADDRESS; CODE 0 cod.alloc END S_reg;
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
  RETURN a-1
END memory_top;

PROCEDURE read_system;

  CONST long={2};

  VAR i: INTEGER;
    adr: ADDRESS;     buf: ADDRESS;
    lim: ADDRESS;     bno: INTEGER;
    ino: ADDRESS;     ref: ADDRESS;
    eof: INTEGER;     err: INTEGER;
   mode: POINTER TO BITSET;
   link: POINTER TO INTEGER;

BEGIN
  ref:=system_base-4*KB;
  ino:=system_base-4*KB*2;
  read_block(2,ino,err);
  IF err#0 THEN error(4) END;
  ino :=ino+16;
  adr :=ino+10;
  eof :=adr^;   eof:=(eof+4095) DIV 4096;
  link:=ino+9;
  mode:=ino+8;
  IF link^<=0 THEN error(1) END;
  IF long*mode^={} THEN
    IF eof>8 THEN error(2) END;
    move(ref,ino,eof)
  ELSE
    IF INTEGER(ino^)<=0 THEN error(2) END;
    read_block(ino^,ref,err);
    IF err#0 THEN error(4) END
  END;
  buf:=system_base;
  lim:=top-256;
  bno:=ref^;
  size:=0;
  i:=0;
  WHILE eof>0 DO
    IF buf+1024>=lim THEN error(5) END;
    IF bno<=0        THEN error(2) END;
    read_block(bno,buf,err);
    IF err#0 THEN error(4);
    ELSE
      INC(buf ,1024);  INC(ref); bno:=ref^;
      INC(size,1024);  DEC(eof); dot(i);    INC(i)
    END
  END;
  error(0);
END read_system;

TYPE process =
     POINTER TO
     RECORD
       G: ADDRESS;   L: ADDRESS;
      PC: INTEGER;   M: BITSET;
       S: ADDRESS;   H: ADDRESS;
       T: INTEGER;
     END;

PROCEDURE transfer(VAR f,t: process); CODE cod.tra END transfer;

PROCEDURE start_system;
  VAR p: process;
    adr: ADDRESS;
    dev: POINTER TO ARRAY [0..3] OF CHAR;
BEGIN
  top:=memory_top();
  read_system;

  adr:=system_base+88h; adr^:=top;
  adr:=system_base+89h; dev :=adr;  dev^:="dk0"; dev^[2]:=CHAR(ORD("0"));

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
  adr :=p^.G^;
  adr^:=cod.move+cod.tra*100h+cod.quit*10000h;
  transfer(p,p);
END start_system;

BEGIN
  firstly:=TRUE;
  erase;
  reset_serials;
  start_system
END bootQ6mscp.
