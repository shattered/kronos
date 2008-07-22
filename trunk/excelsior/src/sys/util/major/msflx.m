MODULE msflx; (* Leo 16-Apr-89. (c) KRONOS *)

(*$N+$U+*)

IMPORT       SYSTEM;
IMPORT  req: defRequest;
IMPORT  str: Strings;
IMPORT  tty: Terminal;
IMPORT  key: Keyboard;
IMPORT  bio: BIO;
IMPORT   fs: fsWalk;
IMPORT  low: lowLevel;
IMPORT  tim: Time;
IMPORT  mem: Heap;
IMPORT args: tskArgs;

WITH STORAGE: mem;

CONST DEBUG = FALSE;

----------------------  T E R M I N A L  -----------------------
                      -------------------

PROCEDURE rev(on: INTEGER);
BEGIN
  tty.set_reverse(on);
END rev;

PROCEDURE loi(on: INTEGER);
BEGIN
  IF tty.state^.min_color<-1 THEN tty.set_color(-on) END
END loi;

PROCEDURE hii(on: INTEGER);
BEGIN
  IF tty.state^.max_color>1 THEN tty.set_color(on) END
END hii;

PROCEDURE unl(on: INTEGER);
BEGIN
  tty.set_underline(on);
END unl;

----------------------  D I S K   I / O  -----------------------
                      -------------------


TYPE ADDRESS = SYSTEM.ADDRESS;

  VAR
    bytes_per_sec   (* длина сектора в байтах    *)
   ,sec_per_clu     (* число секторов в кластере *)
   ,res_sec         (* число зарезервированных секторов *)
   ,no_FATs         (* число FAT-ов  *)
   ,root_dir_ent    (* макс.число файлов в корневой директории *)
   ,total_sec       (* общее число секторов на диске *)
   ,media_desc      (* тип носителя *)
   ,sec_per_FAT     (* число секторов в FAT-е *)
   ,sec_per_trk     (* число секторов в треке *)
   ,heads           (* количество головок и, соответственно, сторон *)
   ,hidden_sec      (* число "невидимых" секторов *)

--------------------  производные значения  --------------------
                    ------------------------

   ,clu_size            (* размер кластера в байтах *)
   ,offset              (* число секторов, занятых служебной информацией, *)
                        (* то есть номер первого информационного сектора  *)
   ,max_clu             (* номер последнего кластера *)  : INTEGER;


--------------------  B U F F E R    I / O  --------------------
                    ------------------------

CONST blk_size = 4096 ; (* block size in bytes *)

VAR drv: bio.FILE;
 buffer: ADDRESS;  (* pointer to ARRAY [0..blk_size-1] OF CHAR *)
  c_blk: INTEGER;  (* current block no *)
min_sec: INTEGER;  (* first sector in buffer *)
max_sec: INTEGER;  (* last  sector in buffer *)
sec_per_blk: INTEGER;  (* number of sectors in block *)

CONST SEEK = 'seek';

PROCEDURE error(VAL format: ARRAY OF CHAR; SEQ args: SYSTEM.WORD);
  VAR x: CHAR;
BEGIN
  tty.print(format,args);
  tty.perror(bio.error," %%s (PRESS ANY KEY)");
  key.read(x);
  tty.print("\n");
  HALT(1);
END error;

PROCEDURE _read_block(n: INTEGER);
BEGIN
  bio.seek(drv,n*4096,0);
  IF NOT bio.done THEN error(SEEK); RETURN END;
  bio.read(drv,buffer,blk_size);
  IF NOT bio.done THEN error('read block %d ',n) END;
END _read_block;

PROCEDURE read_block(n: INTEGER);
BEGIN
  _read_block(n);
  c_blk:=n;
  min_sec:=n * sec_per_blk;
  max_sec:=min_sec + sec_per_blk - 1;
END read_block;

PROCEDURE write_block;
BEGIN
  bio.seek(drv,c_blk*4096,0);
  IF NOT bio.done THEN error(SEEK); RETURN END;
  bio.write(drv,buffer,blk_size);
  IF NOT bio.done THEN error('write block %d ',c_blk) END;
END write_block;

PROCEDURE read_sectors(first,no: INTEGER; to: ADDRESS);
(* начиная с номера -first-,                    *)
(* считывает с диска подряд -no- секторов       *)
(* и размещает информацию по адресу -to-        *)
  VAR i,len: INTEGER;  adr: ADDRESS;
BEGIN
  -- print('\n --sectorRead(%d, %d, %#h)\n',first,no,to);
  ASSERT( (0<=first) & (first+no<=total_sec));
  WHILE no>0 DO
    IF (first<min_sec) OR (first>max_sec)THEN
      read_block(first DIV sec_per_blk);
    END;
    IF first+no-1<=max_sec THEN
      i:=no; (* all sectors in this block *)
    ELSE
      i:=max_sec-first+1;
    END;
    adr:=buffer + (first-min_sec)*bytes_per_sec DIV 4;
    len:=i*bytes_per_sec DIV 4;
    low.move(to,adr,len);  to:=to+len;     no:=no-i;
    first:=max_sec+1;
  END;
END read_sectors;

PROCEDURE write_sectors(first,no: INTEGER; from: ADDRESS);
(* начиная с номера -first-,                    *)
(* записывает на диск подряд -no- секторов      *)
(* информацию берет по адресу -from-            *)
  VAR i,len: INTEGER; adr: ADDRESS;
BEGIN  --print('\n --sectorWrite(%d, %d, %#h)\n',first,no,from);
  ASSERT((0<=first) & (first+no<=total_sec));
  WHILE no>0 DO
    IF (first<min_sec) OR (first>max_sec) THEN
      read_block(first DIV sec_per_blk);
    END;
    IF first+no-1<=max_sec THEN i:=no ELSE i:=max_sec-first+1 END;
    len:=i * bytes_per_sec DIV 4;
    adr:=buffer + (first-min_sec)*bytes_per_sec DIV 4;
    low.move(adr,from,len);  write_block;
    first:=max_sec+1;    from:=from+len;        no:=no-i;
  END;
END write_sectors;

PROCEDURE read_cluster(cl: INTEGER; to: ADDRESS);
(* считывает с диска кластер -cl-           *)
(* и размещает информацию по адресу -to-    *)
BEGIN
  ASSERT((2<=cl) & (cl<=max_clu));
  read_sectors(offset+(cl-2)*sec_per_clu, sec_per_clu, to);
END read_cluster;

PROCEDURE write_cluster(cl: INTEGER; from: ADDRESS);
(* записывает на диск кластер -cl-          *)
(* информацию берет по адресу -from-        *)
BEGIN
  ASSERT((2<=cl) & (cl<=max_clu));
  write_sectors(offset+(cl-2)*sec_per_clu, sec_per_clu, from);
END write_cluster;

---------------------  R E A D    B O O T  ---------------------
                     ----------------------

PROCEDURE format;

CONST
  F000 = ARRAY OF INTEGER {
    050903EEBh,06F542043h,000736C6Fh,000010202h,0D0007002h,00002FD02h,000020009h,
    000000000h,000000000h,00000000Fh,000000100h,000000000h,000060000h,0000C0401h,
    000000000h,000000000h,0C033FCFAh,000BCD08Eh,036C5367Ch,0561E0078h,03E8DC08Eh,
    00BB97C20h,08026AC00h,00374003Dh,0AA058A26h,0C033F3E2h,07AA3D88Eh,07806C700h,
    0FB7C2000h,0037313CDh,08A00B4E9h,08A7C302Eh,08A7C320Eh,0B27C3136h,00500BB00h,
    0CD0201B8h,08DE57213h,08B7D6236h,0000BB9FBh,00D75A6F3h,0B90520BFh,0A6F3000Bh,
    089E91A74h,078368D00h,0B9FB8B7Dh,0A6F3000Bh,020BF7C75h,0000BB905h,07275A6F3h,
    01CA1D233h,00B36F705h,0A2C0FE7Ch,000BB7C38h,0332E8A07h,0350E8A7Ch,034368A7Ch,
    0A100B27Ch,0C12A7C18h,0B450C0FEh,05813CD02h,006283D72h,01A767C38h,0F75200B4h,
    05A7C0B26h,001B1D803h,0363AC6FEh,0D7727C1Ah,000B6C5FEh,011CDD1EBh,0C0D0C0D0h,
    075000325h,08B404001h,00000B8C8h,01E8B00B2h,000EA7C36h,08D007000h,0EB7D8E36h,
    0368D9005h,00AAC7DA2h,0BB0974C0h,00EB40007h,0F2EB10CDh,07DC21E8Dh,00477F33Bh,
    0E6EBF38Bh,016CDE432h,00078068Fh,0007A068Fh,0424919CDh,04F49424Dh,04F432020h,
    04D42494Dh,020534F44h,04D4F4320h,020204F49h,020202020h,04D535953h,0534F4453h,
    053202020h,00D0A5359h,06B736944h,06F6F4220h,061462074h,072756C69h,00D0A0065h,
    02D6E6F4Eh,074737953h,064206D65h,0206B7369h,06420726Fh,0206B7369h,06F727265h,
    00D0A0072h,06C706552h,020656361h,020646E61h,073657270h,06E612073h,0656B2079h,
    068772079h,072206E65h,079646165h,000000D0Ah,000000000h,000000000h,000000000h,
    000000000h,0AA550000h,003FFFFFDh,060050040h,000800700h,00B00A009h,0E00D00C0h,
    001000F00h,013012011h,060150140h,001801701h,01B01A019h,0E01D01C0h,002001F01h,
    023022021h,060250240h,0028FFF02h,02B02A029h,0E02D02C0h,003002F02h,033032031h,
    060350340h,003803703h,03B03A039h,0E03D03C0h,004003F03h,043042041h,060450440h,
    004804704h,04B04A049h,0FFFFFFF0h,007B04FFFh,06F052051h,060550790h,0058FFF05h,
    05B05A059h,0E05D05C0h,0060FFF05h,063062061h,060650640h,006806706h,06B06A069h,
    0F06D06E0h,0070FFFFFh,0FFFFF071h,0F075FFFFh,0078077FFh,0FF07AFFFh,0F07D07CFh,
    0000000FFh };

  F384 = ARRAY OF INTEGER {
    003FFFFFDh,060050040h,000800700h,00B00A009h,0E00D00C0h,001000F00h,013012011h,
    060150140h,001801701h,01B01A019h,0E01D01C0h,002001F01h,023022021h,060250240h,
    0028FFF02h,02B02A029h,0E02D02C0h,003002F02h,033032031h,060350340h,003803703h,
    03B03A039h,0E03D03C0h,004003F03h,043042041h,060450440h,004804704h,04B04A049h,
    0FFFFFFF0h,007B04FFFh,06F052051h,060550790h,0058FFF05h,05B05A059h,0E05D05C0h,
    0060FFF05h,063062061h,060650640h,006806706h,06B06A069h,0F06D06E0h,0070FFFFFh,
    0FFFFF071h,0F075FFFFh,0078077FFh,0FF07AFFFh,0F07D07CFh,0000000FFh,000000000h,
    000000000h,000000000h,000000000h,000000000h,000000000h,000000000h,000000000h,
    000000000h,000000000h,000000000h,000000000h,000000000h,000000000h,000000000h,
    000000000h };

VAR i: INTEGER;
    r: req.REQUEST;
   ch: CHAR;
  buf: ARRAY [0..4095] OF CHAR;

  PROCEDURE adr(VAL s: ARRAY OF INTEGER): SYSTEM.ADDRESS;
  BEGIN RETURN SYSTEM.ADR(s) END adr;

BEGIN
  tty.print("INITIALIZE VOLUME?");
  REPEAT key.read(ch) UNTIL (CAP(ch)='Y') OR (CAP(ch)='N');
  tty.print("%c\n",ch);
  IF CAP(ch)#'Y' THEN HALT END;
  tty.print('360 or 720 ("3" or "7")?');
  REPEAT key.read(ch) UNTIL (ch='3') OR (ch='7');
  tty.print("%c\n",ch);
  IF ch='7' THEN
    r.op:=req.GET_SPEC;
    bio.doio(drv,r);
    IF NOT bio.done THEN error("GET SPEC\n"); HALT END;
    r.op:=req.SET_SPEC;
    WITH r DO
      dsecs  :=720*2;
      ssc    :=9;
      secsize:=512;
      cyls   :=80;
      heads  :=2;
      minsec :=1;
      maxsec :=9;
      ressec :=0;
    END;
    bio.doio(drv,r);
    IF NOT bio.done THEN error("SET SPEC\n"); HALT END;
  END;
  low.zero(buf);
  low.move(SYSTEM.ADR(buf)    ,adr(F000),SIZE(F000));
  low.move(SYSTEM.ADR(buf)+384,adr(F384),SIZE(F384));
  FOR i:=2560 TO HIGH(buf)       DO buf[i]:=CHAR(0F6h) END;
  FOR i:=2560 TO HIGH(buf) BY 32 DO buf[i]:=0c         END;
  bio.seek(drv,0,0);
  IF NOT bio.done THEN error("SEEK TRACK 0\n"); HALT END;
  bio.put(drv,buf,4096);
  IF NOT bio.done THEN error("INITIALIZE VOLUME\n"); HALT END;
  bio.close(drv);
  IF NOT bio.done THEN error("INITIALIZE VOLUME\n"); HALT END;
  tty.print('VOLUME INITIALIZED. RUN msflx AGAIN!\n'); HALT
END format;

PROCEDURE read_boot;

  VAR boot: ARRAY [0..31] OF CHAR;

  PROCEDURE byte(i: INTEGER): INTEGER;
  BEGIN RETURN INTEGER(boot[i]) END byte;

  PROCEDURE word(i: INTEGER): INTEGER;
  BEGIN  RETURN INTEGER(boot[i])+INTEGER(boot[i+1])*256 END word;

BEGIN
  _read_block(0);
  low.move(SYSTEM.ADR(boot),buffer,SIZE(boot));
  IF ( ORD(boot[0]) # 0E9h ) & ( ORD(boot[0]) # 0EBh ) THEN
    tty.print('Illegal MSDOS header %03hh instead of 0EBh or 0E9h\n',boot[0]);
    format; HALT
  END;
  bytes_per_sec := word(0Bh);            (*  512  *)
  sec_per_clu   := byte(0Dh);            (*    2  *)
  res_sec       := word(0Eh);            (*    1  *)
  no_FATs       := byte(10h);            (*    1  *)
  root_dir_ent  := word(11h);            (*  112  *)
  total_sec     := word(13h);            (*  720  *)
  media_desc    := byte(15h);            (*  0FDh *)
  sec_per_FAT   := word(16h);            (*    2  *)
  sec_per_trk   := word(18h);            (*    9  *)
  heads         := word(1Ah);            (*    2  *)
  hidden_sec    := word(1Ch);            (*    0  *)
  ---- производные значения ----
  clu_size:=bytes_per_sec * sec_per_clu;
  offset  :=res_sec + sec_per_FAT  * no_FATs
                    + root_dir_ent * 32 DIV bytes_per_sec;
  max_clu :=(total_sec - offset) DIV sec_per_clu + 1;

  min_sec:=0; max_sec:= -1;
  sec_per_blk:=blk_size DIV bytes_per_sec;
  read_block(0);
END read_boot;

---------------------------  F A T  ----------------------------
                           ---------

VAR FATs: ARRAY [0..16*1024] OF CHAR;
(* we assumed 12-bits FATs *)

PROCEDURE FAT(i: INTEGER): INTEGER;
  VAR k: INTEGER;
BEGIN
  ASSERT( (2<=i) & (i<=max_clu) );
  k:=i + i DIV 2;
  k:=ORD(FATs[k])+ORD(FATs[k+1])*100h;
  IF ODD(i) THEN RETURN k DIV 10h ELSE RETURN k MOD 1000h END;
END FAT;

PROCEDURE putFAT(i,x:INTEGER);
  VAR k: INTEGER;
BEGIN
  ASSERT( (2<=i) & (i<=max_clu) );
  ASSERT( (0<=x) & (x<=0FFFh) );
  k:=i + i DIV 2;
  IF ODD(i)THEN
    FATs[k]  :=CHAR(ORD(FATs[k]) MOD 10h + (x MOD 10h)*10h);
    FATs[k+1]:=CHAR(x DIV 16);
  ELSE
    FATs[k]  :=CHAR(x MOD 100h);
    FATs[k+1]:=CHAR(BITSET(FATs[k+1])*{4..7} + BITSET(x DIV 256))
  END;
END putFAT;

PROCEDURE remove_cluster(clu: INTEGER);
  VAR next: INTEGER;
BEGIN
  WHILE (clu#0) & (clu#0FFFh) DO
    next:=FAT(clu); putFAT(clu,0); clu:=next;
  END;
END remove_cluster;

PROCEDURE alloc_cluster(no: INTEGER; VAR first: INTEGER): BOOLEAN;
  VAR i,cl: INTEGER;
BEGIN
  ASSERT(no>0);
  i:= 2; first:= 0;
  REPEAT
    IF FAT(i)=0 THEN
      IF first = 0 THEN first:=i ELSE putFAT(cl,i) END;
      cl:= i; DEC(no);
    END;
    INC(i);
  UNTIL (no=0) OR (i>max_clu);
  IF no=0 THEN
    putFAT(cl,0FFFh); RETURN TRUE
  ELSE
    remove_cluster(first); RETURN FALSE
  END;
END alloc_cluster;

PROCEDURE saveFATs;
BEGIN
  write_sectors(1, sec_per_FAT,SYSTEM.ADR(FATs));
  write_sectors(sec_per_FAT+1, sec_per_FAT, SYSTEM.ADR(FATs));
END saveFATs;

PROCEDURE loadFATs;
  VAR i,s: INTEGER; ch: CHAR;
BEGIN
  i:=0;
  REPEAT
    s:=sec_per_FAT*i+1;
    read_sectors( s, sec_per_FAT, SYSTEM.ADR(FATs) );
    IF (ORD(FATs[0]) # media_desc) THEN
      tty.print('Illegal FATs! (Abort/Ignore/Retry)? ');
      key.read(ch); ch:=CAP(ch);
      IF (ch='A') OR (ch='I') OR (ch='R') THEN
        tty.print('%c\n',ch);
      ELSE
        tty.print('%c\n',7c);
      END;
      IF (ch='A') THEN HALT END;
    ELSE
      RETURN
    END;
    IF ch='R' THEN i:=1-i END;
  UNTIL ch='I';
END loadFATs;

-------------------  D I R E C T O R I E S  --------------------
                   -------------------------


CONST
  -- ATTRIB --           -- STATUS --
  read_only = 000;       deleted=0E5h;
  hidden    = 001;       empty  =000h;
  system    = 002;       used   =0FFh;
  label     = 003;
  sub_dir   = 004;
  non_arch  = 005;

  MARK      = 029;
  EMPTY     = 030;
  ROOT      = 031;   (* ROOT not supported by MSDOS and invoked for *)
                     (* this program only                           *)

TYPE ENTRY =
     RECORD
       name: ARRAY [0..7] OF CHAR;
       ext : ARRAY [0..2] OF CHAR;
       attr: BITSET;
       time: INTEGER;
       clu : INTEGER;
       eof : INTEGER;
       sta : INTEGER;
     END;

     ENTRYptr = POINTER TO ENTRY;

     packedEntry = ARRAY [0..31] OF CHAR;

     Directory   = RECORD
                     entry: ENTRY;
                     body : DYNARR OF packedEntry;
                   END;

PROCEDURE get_entry(dir: Directory; entry: INTEGER; VAR e: ENTRY);
  VAR i: INTEGER;                   s,m,h: INTEGER;
      p: packedEntry;               d,n,y: INTEGER;
BEGIN
  p:=dir.body[entry];
  e.name:=""; e.ext:="";
  e.attr:={}; e.eof:=00;
  e.time:=00; e.clu:=00;
  IF ORD(p[0])=empty   THEN e.sta:=empty;   RETURN END;
  IF ORD(p[0])=deleted THEN e.sta:=deleted; RETURN END;
  e.sta:=used;
  FOR i:=0 TO 7 DO e.name[i]:=p[i]   END;
  FOR i:=0 TO 2 DO e.ext [i]:=p[8+i] END;
  e.attr:=BITSET(p[11]);
  i:=ORD(p[22]) + ORD(p[23])*256;
  s:=i MOD 32 * 2;  i:=i DIV 32;
  m:=i MOD 64;      i:=i DIV 64;
  h:=i MOD 32;
  i:=ORD(p[24]) + ORD(p[25])*256;
  d:=i MOD 32;      i:=i DIV 32;
  n:=i MOD 16;      i:=i DIV 16;
  y:=i MOD 128 + 1980;
  IF y<1986 THEN d:= 1; n:=1;  y:=1986 END;
  IF y>2017 THEN d:=31; n:=12; y:=2017 END;
  e.time:=tim.pack(y,n,d,h,m,s);
  e.clu:=ORD(p[26]) + ORD(p[27])*256;
  e.eof:=ORD(p[28]) + ORD(p[29])*256 + ORD(p[30])*(256*256);
  i:=7;
  WHILE (i>=0) & (e.name[i]=' ') DO e.name[i]:=0c; DEC(i) END;
  i:=2;
  WHILE (i>=0) & (e.ext [i]=' ') DO e.ext [i]:=0c; DEC(i) END;
END get_entry;

PROCEDURE put_entry(dir: Directory; entry: INTEGER; VAL e: ENTRY);
  VAR i: INTEGER;                  s,m,h: INTEGER;
      p: packedEntry;              d,n,y: INTEGER;
BEGIN
  FOR i:=0 TO 10 DO p[i]:=' ' END;
  i:=0;
  WHILE (i<=HIGH(e.name)) & (e.name[i]#0c) DO p[i+0]:=e.name[i]; i:=i+1 END;
  i:=0;
  WHILE (i<=HIGH(e.ext )) & (e.ext [i]#0c) DO p[i+8]:=e.ext [i]; i:=i+1 END;
  p[11]:=CHAR(e.attr);
  tim.unpack(e.time,y,n,d,h,m,s); y:=y-1980;
  s:=s+(m+h*64)*32;
  p[22]:=CHAR(s);       p[23]:=CHAR(s DIV 256);
  d:=d+(n+y*128)*16;
  p[24]:=CHAR(d);       p[25]:=CHAR(d DIV 256);
  p[26]:=CHAR(e.clu);   p[27]:=CHAR(e.clu DIV 256);
  p[28]:=CHAR(e.eof);   p[29]:=CHAR(e.eof DIV 256);
  p[30]:=CHAR(e.eof DIV 10000h);  p[31]:=0c;
  dir.body[entry]:=p;
END put_entry;

PROCEDURE lookup(dir: Directory; VAL pattern: ARRAY OF CHAR;
                 entry: INTEGER): INTEGER;
  VAR i,j: INTEGER;
(*
                                        match: BOOLEAN;    ch: CHAR;
     npat: ARRAY [0..8] OF CHAR;          nam: ARRAY [0..8] OF CHAR;
     epat: ARRAY [0..3] OF CHAR;          ext: ARRAY [0..3] OF CHAR;
*)
      ptr: POINTER TO packedEntry;
BEGIN
(*
  npat:="       "; nam:=npat;
  epat:="   ";     ext:=epat;
  i:=0; match:=FALSE;
  WHILE (i<=HIGH(pattern)) & (pattern[i]#0c) & (pattern[i]#'.') DO
    ch:=pattern[i];
    IF i<HIGH(npat) THEN npat[i]:=ch END;
    match:=match OR (ch='*') OR (ch='%');
    INC(i);
  END;
  npat[i]:=0c;
  j:=0;
  IF (i<HIGH(pattern)) & (pattern[i]='.') THEN INC(i);
    WHILE (i<=HIGH(pattern)) & (pattern[i]#0c) & (j<HIGH(epat)) DO
      ch:=pattern[i]; epat[j]:=ch;
      match:=match OR (ch='*') OR (ch='%');
      INC(i); INC(j)
    END;
  END;
  epat[j]:=0c;
*)
  FOR i:=entry TO HIGH(dir.body) DO
    ptr:=SYSTEM.ADR(dir.body[i]);
    IF ORD(ptr^[0])=empty   THEN RETURN -1 END;
    IF ORD(ptr^[0])#deleted THEN
      RETURN i
(*
      j:=0;
      WHILE (j<=7) & (ptr^[j+0]#' ') DO nam[j]:=ptr^[j+0]; INC(j) END;
      nam[j]:=0c;
      j:=0;
      WHILE (j<=2) & (ptr^[j+8]#' ') DO ext[j]:=ptr^[j+8]; INC(j) END;
      ext[j]:=0c;
      IF match THEN
        IF pat.Match(npat,nam) & pat.Match(epat,ext) THEN RETURN i END;
      ELSE
        IF (npat=nam) & (epat=ext) THEN RETURN i END;
      END;
*)
    END;
  END;
  RETURN -1
END lookup;

PROCEDURE open_dir(VAR dir: Directory; VAL e: ENTRY);
  VAR n: INTEGER;      sec: INTEGER;
    clu: INTEGER;      adr: ADDRESS;
BEGIN
  ASSERT({sub_dir,ROOT}*e.attr#{});
  dir.entry:=e;
  IF ROOT IN e.attr THEN
    n:=root_dir_ent
  ELSE
    n:=0; clu:=dir.entry.clu;
    WHILE (1<clu) & (clu<0FFFh) DO INC(n); clu:=FAT(clu) END;
    n:=n * sec_per_clu * bytes_per_sec DIV 32;
  END;
  NEW(dir.body,n);
  IF dir.body^.ADR=NIL THEN
    tty.print("no memory to open directory\n"); HALT(1)
  END;
  adr:=dir.body^.ADR;
  IF ROOT IN e.attr THEN
    n:=root_dir_ent * 32 DIV bytes_per_sec;
    sec:=no_FATs * sec_per_FAT + 1;
    read_sectors(sec,n,adr);
  ELSE
    clu:=dir.entry.clu;
    WHILE (1<clu) & (clu<0FFFh) DO
      read_cluster(clu,adr); INC(adr,clu_size DIV 4); clu:=FAT(clu)
    END;
  END;
END open_dir;

PROCEDURE close_dir(VAR dir: Directory);
BEGIN
  DISPOSE(dir.body);
END close_dir;

PROCEDURE save_dir(VAL dir: Directory);
  VAR n: INTEGER;      sec: INTEGER;
    adr: ADDRESS;      clu: INTEGER;
BEGIN
  ASSERT(dir.body^.ADR#NIL);
  adr:=dir.body^.ADR;
  IF ROOT IN dir.entry.attr THEN
    n:=root_dir_ent * 32 DIV bytes_per_sec;
    sec:=no_FATs * sec_per_FAT + 1;
    write_sectors(sec,n,adr);
  ELSE
    clu:=dir.entry.clu;
    WHILE (1<clu) & (clu<0FFFh) DO
      write_cluster(clu,adr); INC(adr,clu_size DIV 4); clu:=FAT(clu);
    END;
  END;
END save_dir;


-------------------------  D E B U G  --------------------------
                         -------------

PROCEDURE print_spec;
BEGIN
  IF NOT DEBUG THEN RETURN END;
  tty.print("bytes_per_sec  %d\n",bytes_per_sec);
  tty.print("sec_per_clu    %d\n",sec_per_clu  );
  tty.print("res_sec        %d\n",res_sec      );
  tty.print("no_FATs        %d\n",no_FATs      );
  tty.print("root_dir_ent   %d\n",root_dir_ent );
  tty.print("total_sec      %d\n",total_sec    );
  tty.print("media_desc     %h\n",media_desc   );
  tty.print("sec_per_FAT    %d\n",sec_per_FAT  );
  tty.print("sec_per_trk    %d\n",sec_per_trk  );
  tty.print("heads          %d\n",heads        );
  tty.print("hidden_sec     %d\n",hidden_sec   );
  tty.print("\n");
  tty.print("clu_size       %d\n",clu_size     );
  tty.print("offset         %d\n",offset       );
  tty.print("max_clu        %d\n",max_clu      );
END print_spec;

--------------------------  M A I N  ---------------------------
                          -----------

VAR time_tog: BOOLEAN;

CONST N = 10;

TYPE NAME = ARRAY [0..31] OF CHAR;

VAR exc: DYNARR OF NAME;
    xmk: DYNARR OF BOOLEAN;

PROCEDURE monitor(VAR dir: Directory);

  VAR
    scr: DYNARR OF ENTRY;
    ref: DYNARR OF INTEGER;    pos: INTEGER;
    top: INTEGER;             last: INTEGER;

  PROCEDURE help;

    PROCEDURE s(VAL s: ARRAY OF CHAR);
      VAR i: INTEGER; off: CHAR;
    BEGIN
      off:=0c; i:=0;
      WHILE (s[i]#0c) DO
        IF s[i]='`' THEN hii(1); off:=' '; i:=i+1 END;
        tty.Write(s[i]);
        IF s[i]=off THEN hii(0); off:=0c; END;
        i:=i+1;
      END;
      hii(0);
    END s;

  BEGIN
    tty.set_pos(23,0);
    s(" `CR select/unselect   `- unselect all     `C copy   `D delete ");
    tty.set_pos(24,0);
    s(" `. change directory   `X excelsior/msdos  `T time togle       ");
  END help;

  PROCEDURE fill_scr;
    VAR i,j: INTEGER;
  BEGIN
    i:=0; j:=0;
    REPEAT
      j:=lookup(dir,"*.*",j);
      IF j>=0 THEN
        get_entry(dir,j,scr[i]); ref[i]:=j; i:=i+1; j:=j+1
      END;
    UNTIL (i>HIGH(scr)) OR (j<0); last:=i-1;
    WHILE i<=HIGH(scr) DO scr[i].attr:={EMPTY}; i:=i+1 END;
  END fill_scr;

  PROCEDURE fs_check;
  BEGIN
    IF fs.done THEN RETURN END;
    tty.perror(fs.error,'%%s\n'); HALT(1)
  END fs_check;

  PROCEDURE fill_exc;
    VAR T: fs.TREE; i: INTEGER; name: NAME; mod: BITSET;
  BEGIN
    RESIZE(exc,64);
    RESIZE(xmk,64);
    fs.walk(T,'*',TRUE); fs_check;
    i:=0;
    WHILE fs.next_dir(T) DO
      fs_check;
      WHILE (i<=HIGH(exc)) & fs.next_entry(T,name,mod) DO
        fs_check;
        xmk[i]:=FALSE; exc[i]:=name;
        INC(i);
      END;
    END;
    fs.dispose(T); fs_check;
    RESIZE(exc,i);
    RESIZE(xmk,i);
  END fill_exc;

  PROCEDURE show_entry(i: INTEGER);
    VAR ptr: ENTRYptr;
       date: ARRAY [0..23] OF CHAR;
         s: ARRAY [0..63] OF CHAR;
  BEGIN
    tty.set_pos(2+i,1);
    ptr:=SYSTEM.ADR(scr[top+i]);
    IF EMPTY IN ptr^.attr THEN tty.erase_line(0); RETURN END;
    IF sub_dir IN ptr^.attr THEN loi(1) END;
    IF  MARK   IN ptr^.attr THEN rev(1) END;
    date:="";
    IF time_tog THEN
 --     tim.AppTime(date,"   %d-%m3-%y2  %h:%'.%s",ptr^.time);
    END;
    IF ptr^.eof<1000 THEN
      str.print(s,"%-8.8s.%-3.3s %8d%s"
                 ,ptr^.name,ptr^.ext,ptr^.eof,date);
    ELSE
      str.print(s,"%-8.8s.%-3.3s %4d,%3d%s"
                 ,ptr^.name,ptr^.ext,ptr^.eof DIV 1000
                 ,ptr^.eof MOD 1000,date);
    END;
    tty.WriteString(s);
    IF  MARK   IN ptr^.attr THEN rev(0) END;
    IF sub_dir IN ptr^.attr THEN loi(0) END;
    tty.erase_line(0);
  END show_entry;

  PROCEDURE refresh;
    VAR i: INTEGER;
  BEGIN
    tty.set_cursor(0);
    tty.set_pos(1,0);
    i:=total_sec * bytes_per_sec DIV 1024;
    unl(1); tty.print(" MS-DOS volume (%dK) ",i);        unl(0);
    hii(1); tty.print("   %s",dir.entry.name); hii(0);
    tty.erase_line(0);
    FOR i:=0 TO N-1 DO show_entry(i) END;
    tty.set_pos(N+2,0);
    IF last<0 THEN tty.print("no files on this volume") END;
    tty.erase_line(0);
    help;
    tty.set_cursor(1);
  END refresh;

  PROCEDURE up;
  BEGIN
    IF pos=0   THEN RETURN END;
    pos:=pos-1;
    IF pos<top THEN DEC(top);
      tty.set_pos(N+1,0);
      tty.del_line(1); ASSERT(tty.done);
      tty.set_pos(2,0);
      tty.ins_line(1); ASSERT(tty.done);
      show_entry(0);
    END;
  END up;

  PROCEDURE dw;
  BEGIN
    IF pos>=last THEN RETURN END;
    pos:=pos+1;
    IF pos>=top+N THEN INC(top);
      tty.set_pos(2,0);
      tty.del_line(1); ASSERT(tty.done);
      tty.set_pos(N+1,0);
      tty.ins_line(1); ASSERT(tty.done);
      show_entry(N-1);
    END;
  END dw;

  PROCEDURE mark;
  BEGIN
    scr[pos].attr:=scr[pos].attr/{MARK}; show_entry(pos-top);
    dw
  END mark;

  PROCEDURE unmark;
    VAR i: INTEGER;
  BEGIN
    FOR i:=0 TO HIGH(scr) DO EXCL(scr[i].attr,MARK) END; refresh;
  END unmark;

  PROCEDURE change_cd;
    VAR d: Directory;
  BEGIN
    IF NOT (sub_dir IN scr[pos].attr) THEN RETURN END;
    open_dir(d,scr[pos]);
    monitor(d);
    close_dir(d);
  END change_cd;

  PROCEDURE copy_file(VAL e: ENTRY; VAL name: ARRAY OF CHAR);
    VAR buf: ARRAY [0..blk_size-1] OF CHAR;
        clu: INTEGER;                   adr: ADDRESS;
       file: bio.FILE;                    i: INTEGER;
  BEGIN
    bio.create(file,name,'w',0);
    IF NOT bio.done THEN error("can't create"' file "%s"',name); RETURN END;
    i:=0; adr:=SYSTEM.ADR(buf);
    clu:=e.clu;
    WHILE (1<clu) & (clu<0FFFh) DO
      read_cluster(clu,adr);
      adr:=adr+clu_size DIV 4; i:=i+clu_size;
      clu:=FAT(clu);
      IF i>=blk_size THEN
        i:=0; adr:=SYSTEM.ADR(buf);
        bio.write(file,adr,blk_size);
        IF NOT bio.done THEN
          error('write error file "%s"',name)
        END;
      END;
    END;
    IF i>0 THEN
      adr:=SYSTEM.ADR(buf);
      bio.write(file,adr,i);
      IF NOT bio.done THEN
        error('write error file "%s"',name)
      END;
    END;
    bio.end(file,e.eof);
    bio.close(file);
    IF NOT bio.done THEN error("can't close"' file "%s"',name) END;
  END copy_file;

  PROCEDURE copy_one(VAL e: ENTRY): BOOLEAN;
    VAR i,j: INTEGER;   ch: CHAR;
       name: ARRAY [0..31] OF CHAR;
  BEGIN
    i:=0; j:=0;
    WHILE (i<=HIGH(e.name)) & (e.name[i]#0c) DO
      ch:=e.name[i];
      IF ("A"<=ch) & (ch<="Z") THEN INC(ch,40b) END;
      name[j]:=ch; j:=j+1; i:=i+1
    END;
    name[j]:='.'; j:=j+1;
    i:=0;
    WHILE (i<=HIGH(e.ext)) & (e.ext[i]#0c) DO
      ch:=e.ext[i];
      IF ("A"<=ch) & (ch<="Z") THEN INC(ch,40b) END;
      name[j]:=ch;  j:=j+1; i:=i+1
    END;
    name[j]:=0c;
    tty.set_pos(N+2,0); tty.erase(0);
    tty.print("copy MS-DOS file ");
    hii(1);
    tty.print("%.8s.%.3s",e.name,e.ext);
    hii(0);
    tty.print(" to Excelsior-II file: ");
    hii(1);
    tty.print("%s",name);
    hii(0);
    tty.print(" ? ");
    REPEAT
      key.read(ch);
      IF ch=33c THEN RETURN FALSE END;
    UNTIL (ch='y') OR (ch='n');
    tty.print("%c",ch);
    IF ch='y' THEN copy_file(e,name) END;
    RETURN TRUE
  END copy_one;

  PROCEDURE copy;
    VAR i,cou: INTEGER;
  BEGIN cou:=0;
    FOR i:=0 TO last DO
      IF MARK IN scr[i].attr THEN
        cou:=cou+1;
        IF NOT copy_one(scr[i]) THEN
          tty.set_pos(N+2,0); tty.erase(0); RETURN
        END;
      END;
    END;
    IF cou=0 THEN
      IF copy_one(scr[pos]) THEN END;
    END;
    tty.set_pos(N+2,0); tty.erase(0);
  END copy;

  PROCEDURE del_one(entry: INTEGER; VAR e: ENTRY): BOOLEAN;
    VAR i,j: INTEGER;   ch: CHAR;
       name: ARRAY [0..31] OF CHAR;
  BEGIN
    i:=0; j:=0;
    WHILE (i<=HIGH(e.name)) & (e.name[i]#0c) DO
      ch:=e.name[i];
      IF ("A"<=ch) & (ch<="Z") THEN INC(ch,40b) END;
      name[j]:=ch; j:=j+1; i:=i+1
    END;
    name[j]:='.'; j:=j+1;
    i:=0;
    WHILE (i<=HIGH(e.ext)) & (e.ext[i]#0c) DO
      ch:=e.ext[i];
      IF ("A"<=ch) & (ch<="Z") THEN INC(ch,40b) END;
      name[j]:=ch;  j:=j+1; i:=i+1
    END;
    name[j]:=0c;
    tty.set_pos(N+2,0); tty.erase(0);
    tty.print("delete MS-DOS file ");
    hii(1);
    tty.print("%.8s.%.3s",e.name,e.ext);
    hii(0);
    tty.print(" ? ");
    REPEAT
      key.read(ch);
      IF ch=33c THEN RETURN FALSE END;
    UNTIL (ch='y') OR (ch='n');
    tty.print("%c",ch);
    IF ch='n' THEN RETURN TRUE END;
    e.name[0]:=CHAR(deleted);
    e.sta    :=deleted;
    e.attr   :={};
    e.time   :=0;
    put_entry(dir,ref[entry],e);
    save_dir(dir);
    remove_cluster(e.clu);
    saveFATs;
    RETURN TRUE
  END del_one;

  PROCEDURE delete;
    VAR i,cou: INTEGER;
  BEGIN cou:=0;
    FOR i:=0 TO last DO
      IF MARK IN scr[i].attr THEN
        cou:=cou+1;
        IF NOT del_one(i,scr[i]) THEN
          tty.set_pos(N+2,0); tty.erase(0); RETURN
        END;
      END;
    END;
    IF cou=0 THEN
      IF del_one(pos,scr[pos]) THEN END;
    END;
    tty.set_pos(N+2,0); tty.erase(0);
  END delete;

  PROCEDURE copy_out_file(VAL name: ARRAY OF CHAR; VAR e: ENTRY);

    VAR entry: INTEGER;
         file: bio.FILE;
         n,no: INTEGER;
          buf: ARRAY [0..blk_size-1] OF CHAR;
            i: INTEGER;
          adr: ADDRESS;
          clu: INTEGER;

    PROCEDURE close;
    BEGIN
      bio.close(file);
      IF NOT bio.done THEN error("can't close"' file "%s"',name) END;
    END close;

  BEGIN
    entry:=0;
    WHILE (entry<=HIGH(dir.body)) & (ORD(dir.body[entry][0])#empty) DO
      INC(entry);
    END;
    IF entry>HIGH(dir.body) THEN
      entry:=0;
      WHILE (entry<=HIGH(dir.body)) & (ORD(dir.body[entry][0])#deleted) DO
        INC(entry);
      END;
    END;
    IF entry>HIGH(dir.body) THEN
      tty.print("directory overflow!!!"); RETURN
    END;
    bio.open(file,name,'r');
    IF NOT bio.done THEN error("can't"' open file "%s"',name); RETURN END;
    no:=(bio.eof(file)+clu_size-1) DIV clu_size;
    IF alloc_cluster(no,e.clu) THEN
      e.eof :=bio.eof(file);
      e.time:=0;
      e.attr:={non_arch};
      e.sta :=used;
      put_entry(dir,entry,e);
      save_dir(dir);
      saveFATs;
    ELSE
      tty.print("no space on volume!!!"); close; RETURN
    END;
    i:=bio.eof(file); clu:=e.clu;
    WHILE i>0 DO
      adr:=SYSTEM.ADR(buf);
      IF i>blk_size THEN
        bio.read(file,adr,blk_size);
      ELSE
        bio.read(file,adr,i);
      END;
      IF NOT bio.done THEN error('read error file "%s"',name) END;
      n:=blk_size;
      WHILE (n>0) & (i>0) DO
        write_cluster(clu,adr); clu:=FAT(clu);
        n:=n-clu_size;
        i:=i-clu_size;
        adr:=adr + clu_size DIV 4;
      END;
    END;
    close;
  END copy_out_file;

  PROCEDURE copy_out(VAL name: ARRAY OF CHAR): BOOLEAN;
    VAR i,j: INTEGER;   ch: CHAR;    e: ENTRY;
  BEGIN
    i:=0; j:=0;
    WHILE (i<=HIGH(name)) & (name[i]#0c) & (name[i]#'.') DO
      ch:=name[i];
      IF ("a"<=ch) & (ch<="z") THEN DEC(ch,40b) END;
      IF j<=HIGH(e.name) THEN e.name[j]:=ch END;
      j:=j+1; i:=i+1
    END;
    IF j<=HIGH(e.name) THEN e.name[j]:=0c END;
    j:=0;
    IF (i<=HIGH(name)) & (name[i]='.') THEN
      i:=i+1;
      WHILE (i<=HIGH(name)) & (name[i]#0c) DO
        ch:=name[i];
        IF ("a"<=ch) & (ch<="z") THEN DEC(ch,40b) END;
        IF j<=HIGH(e.ext) THEN e.ext[j]:=ch END;
        j:=j+1; i:=i+1
      END;
      IF j<=HIGH(e.ext) THEN e.ext[j]:=0c END;
    END;

    tty.set_pos(N+2,0); tty.erase(0);
    tty.print("copy Excelsior file ");
    hii(1);
    tty.print("%s",name);
    hii(0);
    tty.print(" to MS-DOS file ");
    hii(1);
    tty.print("%.8s.%.3s",e.name,e.ext);
    hii(0);
    tty.print(" ? ");
    REPEAT
      key.read(ch);
      IF ch=33c THEN RETURN FALSE END;
    UNTIL (ch='y') OR (ch='n');
    tty.print("%c",ch);
    IF ch='y' THEN copy_out_file(name,e) END;
    RETURN TRUE
  END copy_out;

  VAR ch: CHAR;

  PROCEDURE exc_monitor;
    CONST M = N+3;
    VAR i,j: INTEGER;
  BEGIN
    tty.set_pos(N+2,0); tty.erase(0); help;
    tty.set_pos(M,0);
    unl(1); tty.print(" Excelsior-II directory "); unl(0);
    tty.set_pos(M+1,0);
    IF HIGH(exc)<0 THEN RETURN END;
    FOR i:=0 TO HIGH(exc) DO
      IF i MOD 8 # 7 THEN
        tty.print("%-9.9s ",exc[i])
      ELSE
        tty.print("%-9.9s\n",exc[i])
      END;
      xmk[i]:=FALSE;
    END;
    i:=0;
    LOOP
      tty.set_pos(M+1+i DIV 8,(i MOD 8)*10);
      loi(1);
      rev(ORD(xmk[i])); tty.print("%-9.9s",exc[i]); rev(0);
      loi(0);
      tty.set_pos(M+1+i DIV 8,(i MOD 8)*10);
      key.read(ch);
      j:=i;
      IF ("a"<=ch) & (ch<="z") THEN ch:=CAP(ch) END;
      IF    ch='C' THEN EXIT
      ELSIF ch=33c THEN RETURN
      ELSIF ch=key.dw    THEN
        IF i+8<=HIGH(exc) THEN j:=i+8 END;
      ELSIF ch=key.up    THEN
        IF i-8>=0      THEN j:=i-8 END;
      ELSIF ch=key.left  THEN
        IF i-1>=0      THEN j:=i-1 END;
      ELSIF ch=key.right THEN
        IF i+1<=HIGH(exc) THEN j:=i+1 END;
      ELSIF ch=15c        THEN
        xmk[i]:=NOT xmk[i];
        rev(ORD(xmk[i])); tty.print("%-9.9s",exc[i]); rev(0);
        IF i+1<=HIGH(exc) THEN j:=i+1 END;
      ELSE
      END;
      IF j#i THEN
        tty.set_pos(M+1+i DIV 8,(i MOD 8)*10);
        rev(ORD(xmk[i])); tty.print("%-9.9s",exc[i]); rev(0);
        i:=j;
      END;
    END;
    FOR i:=0 TO HIGH(exc) DO
      IF xmk[i] THEN
        IF copy_out(exc[i]) THEN  END;
      END;
    END;
  END exc_monitor;

VAR i: INTEGER;

BEGIN
  tty.home; tty.erase(0);
  NEW(scr,HIGH(dir.body)+1);
  NEW(ref,HIGH(dir.body)+1);
  IF (scr^.ADR=NIL) OR (ref^.ADR=NIL) THEN
    tty.print("no enough memory...\n"); HALT(1)
  END;

  top:=0;  pos:=0;
  fill_scr; fill_exc;
  refresh;
  LOOP
    tty.set_pos(2+pos-top,0);
    key.read(ch);
    IF ("a"<=ch) & (ch<="z") THEN ch:=CAP(ch) END;
    IF    ch=key.up    THEN up
    ELSIF ch=key.dw    THEN dw
    ELSIF ch=key.pgup  THEN FOR i:=0 TO 7 DO up END;
    ELSIF ch=key.pgdw  THEN FOR i:=0 TO 7 DO dw END;
    ELSIF ch=15c       THEN mark
    ELSIF ch=33c       THEN EXIT
    ELSIF ch='X'       THEN exc_monitor; fill_scr; refresh
    ELSIF ch='.'       THEN change_cd
    ELSIF ch='C'       THEN copy; fill_exc;
    ELSIF ch='D'       THEN delete; fill_scr; refresh
    ELSIF ch='T'       THEN time_tog:=NOT time_tog; refresh
    ELSIF ch='-'       THEN unmark
    ELSE
    END;
  END;
  DISPOSE(scr);
  DISPOSE(ref);
  DISPOSE(exc);
END monitor;

--------------------------  I N I T  ---------------------------
                          -----------

VAR SAVE: req.REQUEST;

PROCEDURE init;
  VAR r: req.REQUEST;
BEGIN
  IF HIGH(args.words)<0 THEN
    tty.print('usage:\n  msflx device\n');
    HALT(1)
  END;
  mem.allocate(buffer,blk_size DIV 4);
  IF buffer=NIL THEN tty.print('no memory for buffer\n'); HALT(1) END;
  bio.open(drv,args.words[0],'rw');
  IF NOT bio.done THEN
    error("can't open device '%s'",args.words[0]);
  END;
  IF bio.is_disk*bio.kind(drv)={} THEN
    tty.print('%s is not disk\n',args.words[0]); HALT(1);
  END;
  bio.buffers(drv,4,4096);

  r.op:=req.GET_SPEC;
  bio.doio(drv,r);
  IF NOT bio.done THEN error("GET SPEC\n"); HALT END;
  r.op:=req.SET_SPEC;
  SAVE:=r;
  WITH r DO
    dsecs  :=360*2;
    ssc    :=9;
    secsize:=512;
    cyls   :=40;
    heads  :=2;
    minsec :=1;
    maxsec :=9;
    ressec :=0;
  END;
  read_boot;
  bio.doio(drv,r);
  IF NOT bio.done THEN error("SET_SPEC 360K = 40trk 9 sec x 512\n") END;
  IF total_sec>720 THEN
    r.cyls:=80;  r.dsecs:=720*2;
    bio.doio(drv,r);
    IF NOT bio.done THEN error("SET_SPEC 720K = 80trk 9 sec x 512\n") END;
    read_boot;
  END;
  IF DEBUG THEN print_spec END;
  loadFATs;
END init;

PROCEDURE final;
BEGIN
  tty.set_pos(24,0);
  tty.print("\n");
  IF SAVE.op=req.SET_SPEC THEN bio.doio(drv,SAVE); bio.close(drv) END
END final;

VAR root: Directory;

BEGIN
  drv:=bio.null;
  SAVE.op:=req.NOP;
  NEW(exc); NEW(xmk);
  init;
  root.entry.attr:={ROOT};
  root.entry.name:="\";
  open_dir(root,root.entry);
  time_tog:=FALSE;
  monitor(root);
  bio.close(drv);
  IF NOT bio.done THEN error('close "%s"\n',args.words[0]) END;
END msflx.
