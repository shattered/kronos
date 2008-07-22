MODULE msex; (* Leo 12-Mar-91. (c) KRONOS *)
             (* Igo 24-Nov-91. (c) KRONOS *)

IMPORT       SYSTEM;
IMPORT       ASCII;
IMPORT       Heap;
IMPORT  bio: BIO;
IMPORT  req: defRequest;
IMPORT  err: defErrors;
IMPORT  str: Strings;
IMPORT  tim: Time;
IMPORT  tty: Terminal;
IMPORT  key: Keyboard;
IMPORT  arg: tskArgs;
IMPORT  low: lowLevel;
IMPORT  wnd: ttyWindows;
FROM SYSTEM  IMPORT ADR;

WITH STORAGE: Heap;

(*$N+*)

TYPE
  STR32     = ARRAY [0..31] OF CHAR;

  ITEM      = RECORD
                name: STR32;
                dir : BOOLEAN;
                eof : INTEGER;
                time: INTEGER;
                clu : INTEGER; (* cluster  *)  (* for MSDOS only *)
                seld: BOOLEAN; (* selected *)
              END;

  ITEMPTR   = POINTER TO ITEM;

  DIRECTORY = DYNARR OF ITEM;

  STATE     = RECORD
                win: wnd.WINDOW;
                inf: wnd.INFO;
                dir: DIRECTORY;
                top: INTEGER;
                cur: INTEGER;
                cou: INTEGER;
                sum: INTEGER;
                fre: INTEGER;
              END;

VAR  H: wnd.WINDOW;
   MSG: wnd.WINDOW;
   L,R: STATE;                  shadow: INTEGER;
     X: POINTER TO STATE;       normal: INTEGER;
   HID: BOOLEAN;                bright: INTEGER;



PROCEDURE perror(ecod: INTEGER; VAL f: ARRAY OF CHAR; SEQ a: SYSTEM.WORD);
BEGIN
  tty.nop;
  tty.set_pos(tty.state^.lines-2,0);         tty.erase(0);
  tty.print(f,a);  tty.perror(ecod,"%%s\n"); HALT(ecod)
END perror;

PROCEDURE werror(ecod: INTEGER; VAL f: ARRAY OF CHAR; SEQ a: SYSTEM.WORD);
  VAR ch: CHAR;
BEGIN
  wnd.era(MSG,2);
  wnd.setpos(MSG,1,5);   wnd.print(MSG,f,a);
  IF ecod#0 THEN
    wnd.setpos(MSG,2,1); wnd.perror(MSG,ecod,"%%|48.48s")
  END;
  wnd.ontop(MSG);
  wnd.open(MSG);  key.read(ch);  wnd.close(MSG)
END werror;

PROCEDURE query(VAL s0,s1,y,n: ARRAY OF CHAR): BOOLEAN;
  VAR i,j: INTEGER;    ch: CHAR;
      Y,N: INTEGER;
BEGIN
  wnd.era(MSG,2);
  wnd.foreground(MSG,bright);
  wnd.setpos(MSG,0,0);   wnd.print(MSG,"%|48.48s",s0);
  wnd.foreground(MSG,normal);
  wnd.setpos(MSG,1,0);   wnd.print(MSG,"%|48.48s",s1);
  i:=str.len(y);
  j:=str.len(n);
  Y:=(48-(j+i)) DIV 3;
  N:=48-Y-j;
  i:=0;
  wnd.ontop(MSG);
  wnd.open(MSG);
  LOOP
    wnd.reverse(MSG,1-i);   wnd.setpos(MSG,2,Y);   wnd.print(MSG,"%s",y);
    wnd.reverse(MSG,  i);   wnd.setpos(MSG,2,N);   wnd.print(MSG,"%s",n);
    wnd.reverse(MSG,  0);
    key.read(ch);
    IF CAP(ch)=y[1] THEN wnd.close(MSG); RETURN TRUE  END;
    IF CAP(ch)=n[1] THEN wnd.close(MSG); RETURN FALSE END;
    IF ch=15c       THEN wnd.close(MSG); RETURN (i=0) END;
    IF ch=33c       THEN wnd.close(MSG); RETURN FALSE END;
    i:=1-i
  END
END query;

PROCEDURE c(c: INTEGER);
BEGIN
  CASE c OF
  |-1: tty.set_color(shadow)
  | 0: tty.set_color(normal)
  |+1: tty.set_color(bright)
  END
END c;

PROCEDURE p(c: INTEGER); BEGIN tty.set_pos(tty.state^.lines-2,c) END p;

PROCEDURE r(n: INTEGER); BEGIN tty.set_reverse(n) END r;

PROCEDURE wnd_check;
BEGIN
  IF NOT wnd.done THEN perror(wnd.error,"") END
END wnd_check;

PROCEDURE bio_check;
  VAR i: INTEGER;
BEGIN
  IF NOT bio.done THEN i:=bio.error; werror(i,"%s  ",bio.ename); HALT(i) END
END bio_check;

PROCEDURE sort(VAR d: DIRECTORY);

  PROCEDURE names(l,r: INTEGER);
    VAR t: ITEM;
        x: ITEM;
      i,j: INTEGER;
  BEGIN
    i:=l; j:=r; x:=d[(i+j) DIV 2];
    REPEAT
      LOOP
        WITH d[i] DO
          IF NOT ((dir>x.dir) OR (dir=x.dir) & (name<x.name)) THEN EXIT END
        END;
        i:=i+1
      END;
      LOOP
        WITH d[j] DO
          IF NOT ((x.dir>dir) OR (x.dir=dir) & (x.name<name)) THEN EXIT END
        END;
        j:=j-1
      END;
      IF i<=j THEN
        t:=d[i]; d[i]:=d[j]; d[j]:=t; i:=i+1; j:=j-1
      END
    UNTIL i>j;
    IF l<j THEN names(l,j) END;
    IF i<r THEN names(i,r) END
  END names;

BEGIN
  IF HIGH(d)<=0 THEN RETURN END;
  names(0,HIGH(d))
END sort;

PROCEDURE xwalk(VAR d: DIRECTORY);
  VAR h: INTEGER;
   name: STR32;
   mode: BITSET;
BEGIN
  bio.dir_walk(bio.cd,bio.s_name+bio.s_dirfwd);  bio_check;
  h:=0;
  WHILE bio.get_entry(bio.cd,name,mode) DO
    IF HID OR (mode*bio.e_hidden={}) OR (mode*bio.e_dir#{}) THEN
      IF h>HIGH(d) THEN RESIZE(d,h+1) END;
      d[h].time:=-1;      d[h].name:=name;      d[h].seld:=FALSE;
      d[h].eof :=-1;
      IF bio.e_dir*mode#{} THEN d[h].dir:=TRUE ELSE d[h].dir:=FALSE END;
      INC(h)
    END
  END;
  RESIZE(d,h);
  bio.end_walk(bio.cd);  bio_check;
  sort(d)
END xwalk;

PROCEDURE mark(VAL s: STATE; i: INTEGER);
BEGIN
  IF (i<0) OR (i>HIGH(s.dir)) THEN RETURN END;
  IF tty.state^.max_color>0 THEN
    IF s.dir[i].seld THEN
      wnd.foreground(s.win,bright)
    ELSE
      wnd.foreground(s.win,normal)
    END
  ELSE
    wnd.underline(s.win,ORD(s.dir[i].seld))
  END
END mark;

PROCEDURE unmark(VAL s: STATE);
BEGIN
  IF tty.state^.min_color<-1 THEN
    wnd.foreground(s.win,normal)
  ELSE
    wnd.underline(s.win,0)
  END
END unmark;

PROCEDURE fill(VAR s: STATE);
  VAR i,l,c,w,h,w3: INTEGER; name: STR32;
BEGIN
  wnd.freeze:=TRUE;
  h:=s.inf^.h-2;
  w:=s.inf^.w-2; w3:=(w-2) DIV 3;
  l:=0;
  c:=0;
  FOR i:=s.top TO s.top+h*3-1 DO
    wnd.setpos(s.win,l,c); wnd_check;
    IF i<=HIGH(s.dir) THEN
      mark(s,i);
      name:=s.dir[i].name;
      IF s.dir[i].dir THEN str.append(name,"/") END;
      wnd.print(s.win,"%-*.*s",w3,w3,name);
      unmark(s)
    ELSE
      wnd.print(s.win,"%-*.*s",w3,w3,"")
    END;
    INC(l);
    IF l=h THEN l:=0; INC(c,w3+1) END;
    wnd_check
  END;
  FOR l:=0 TO h-1 DO
    c:=w3;
    FOR i:=1 TO 2 DO
      wnd.setpos(s.win,l,c);                 wnd_check;
      wnd.print(s.win,"%c",tty.state^.vbar); wnd_check; INC(c,w3+1);
    END
  END;
  c:=w3;
  FOR i:=1 TO 2 DO
    wnd.frameprint(s.win,0,c+1,"%c",tty.state^.bars[0,1]); wnd_check; INC(c,w3+1)
  END;
  c:=w3;
  FOR i:=1 TO 2 DO
    wnd.frameprint(s.win,h+1,c+1,"%c",tty.state^.bars[2,1]); wnd_check; INC(c,w3+1)
  END;
  wnd.freeze:=FALSE;
  IF wnd.closed(s.win) THEN wnd.open(s.win) ELSE wnd.refresh(s.win) END;
  wnd_check;
  IF s.cur>HIGH(s.dir) THEN s.cur:=HIGH(s.dir) ELSIF s.cur<0 THEN s.cur:=0 END;
  IF tty.state^.min_color=-1 THEN tty.set_underline(0) END
END fill;

---------------------------- MS/DOS ----------------------------
                            --------
VAR   BAD: BOOLEAN;
 msd_done: BOOLEAN;
     disk: bio.FILE;
    bytes_per_sec   (* длина сектора в байтах    *)
   ,bytes_per_clu   (* длина кластера в байтах   *)
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

   FATs: ARRAY [0..16*1024] OF CHAR; (* we assumed 12-bits FATs *)

--------------------  B U F F E R    I / O  --------------------
                    ------------------------

PROCEDURE read_sectors(first,no: INTEGER; to: SYSTEM.ADDRESS);
(* начиная с номера -first-,                    *)
(* считывает с диска подряд -no- секторов       *)
(* и размещает информацию по адресу -to-        *)
BEGIN
  bio.seek(disk,first*512,0); bio_check;
  bio.read(disk,to,no*512);   bio_check;
END read_sectors;

PROCEDURE write_sectors(first,no: INTEGER; from: SYSTEM.ADDRESS);
(* начиная с номера -first-,                    *)
(* записывает на диск подряд -no- секторов      *)
(* информацию берет по адресу -from-            *)
BEGIN
  bio.seek (disk,first*512,0); bio_check;
  bio.write(disk,from,no*512); bio_check;
END write_sectors;

PROCEDURE read_cluster(cl: INTEGER; to: SYSTEM.ADDRESS);
(* считывает с диска кластер -cl-           *)
(* и размещает информацию по адресу -to-    *)
BEGIN
  ASSERT((2<=cl) & (cl<=max_clu));
  read_sectors(offset+(cl-2)*sec_per_clu, sec_per_clu, to);
END read_cluster;

PROCEDURE read_clusters(cl,no: INTEGER; to: SYSTEM.ADDRESS);
(* считывает с диска no кластеров начиная с -cl- *)
(* и размещает информацию по адресу -to-         *)
BEGIN
  ASSERT((no>0) & (2<=cl) & (cl+no-1<=max_clu));
  read_sectors(offset+(cl-2)*sec_per_clu, sec_per_clu*no , to);
END read_clusters;

PROCEDURE write_cluster(cl: INTEGER; from: SYSTEM.ADDRESS);
(* записывает на диск кластер -cl-          *)
(* информацию берет по адресу -from-        *)
BEGIN
  ASSERT((2<=cl) & (cl<=max_clu));
  write_sectors(offset+(cl-2)*sec_per_clu, sec_per_clu, from);
END write_cluster;

PROCEDURE write_clusters(cl,no: INTEGER; from: SYSTEM.ADDRESS);
(* записывает на диск no кластеров начиная с -cl- *)
(* информацию берет по адресу -from-              *)
BEGIN
  ASSERT((no>0) & (2<=cl) & (cl+no-1<=max_clu));
  write_sectors(offset+(cl-2)*sec_per_clu, sec_per_clu*no , from);
END write_clusters;

---------------------  R E A D    B O O T  ---------------------
                     ----------------------

PROCEDURE open_disk;
  VAR name: ARRAY [0..127] OF CHAR;
BEGIN
  IF HIGH(arg.words)<0 THEN name:="/dev/fd0" ELSE str.copy(name,arg.words[0]) END;
  bio.open(disk,name,'rw');    bio_check;
  IF bio.kind(disk)*bio.is_disk={} THEN
    werror(0,'isn`t disk device "%s"',name); HALT
  END
END open_disk;

PROCEDURE read_boot;

  VAR boot: ARRAY [0..1023] OF CHAR;

  PROCEDURE byte(i: INTEGER): INTEGER;
  BEGIN RETURN INTEGER(boot[i]) END byte;

  PROCEDURE word(i: INTEGER): INTEGER;
  BEGIN  RETURN INTEGER(boot[i])+INTEGER(boot[i+1])*256 END word;

  VAR r: req.REQUEST;
   name: ARRAY [0..127] OF CHAR;

BEGIN
  L.fre:=-1;
  IF disk#bio.null THEN bio.close(disk); bio_check END;
  open_disk;
  r.op:=req.GET_SPEC;
  bio.doio(disk,r);  bio_check;
  r.op:=req.SET_SPEC;
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
  bio.doio(disk,r);  bio_check;

  bytes_per_sec:=512;
  sec_per_clu  :=2;
  bytes_per_clu:=bytes_per_sec*sec_per_clu;
  res_sec      :=1;
  no_FATs      :=1;
  root_dir_ent :=112;
  total_sec    :=720;
  media_desc   :=0FDh;
  sec_per_FAT  :=2;
  sec_per_trk  :=9;
  heads        :=2;
  hidden_sec   :=0;
  bio.get(disk,boot,512);
  IF NOT bio.done THEN
    werror(bio.error,'read "%s"',bio.ename); BAD:=TRUE; RETURN
  END;
  read_sectors(0,1,SYSTEM.ADR(boot));
  BAD:=( ORD(boot[0]) # 0E9h ) & ( ORD(boot[0]) # 0EBh );
  IF BAD  THEN
    werror(0,'Illegal MSDOS header %03hh (EBh or E9h expected)',boot[0])
  ELSE
    bytes_per_sec := word(0Bh);
    sec_per_clu   := byte(0Dh);
    res_sec       := word(0Eh);
    no_FATs       := byte(10h);
    root_dir_ent  := word(11h);
    total_sec     := word(13h);
    media_desc    := byte(15h);
    sec_per_FAT   := word(16h);
    sec_per_trk   := word(18h);
    heads         := word(1Ah);
    hidden_sec    := word(1Ch)
  END;
  ---- производные значения ----
  bytes_per_clu:=bytes_per_sec*sec_per_clu;
  clu_size:=bytes_per_sec * sec_per_clu;
  offset  :=res_sec + sec_per_FAT  * no_FATs
                    + root_dir_ent * 32 DIV bytes_per_sec;
  max_clu :=(total_sec - offset) DIV sec_per_clu + 1;
  IF BAD THEN RETURN END;
  IF total_sec>720 THEN
    r.op:=req.GET_SPEC;
    bio.doio(disk,r);   bio_check;
    r.op:=req.SET_SPEC; r.cyls:=80;
    bio.doio(disk,r);   bio_check;
  END;
  bio.close(disk);
  open_disk;
  bio.buffers(disk,4,4096);       bio_check;
END read_boot;

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

VAR r: req.REQUEST;
  i,t: INTEGER;
   ch: CHAR;
  buf: ARRAY [0..4095] OF CHAR;

  PROCEDURE adr(VAL s: ARRAY OF INTEGER): SYSTEM.ADDRESS;
  BEGIN RETURN SYSTEM.ADR(s) END adr;

BEGIN
  IF query('Select disk density','','[360K]','[720K]') THEN
    i:=360; t:=40
  ELSE
    i:=720; t:=80
  END;
  IF NOT query('All data on the disk will be destroied'
              ,'are you realy sure?','[Yes]','[No]')
  THEN RETURN
  END;
  r.op:=req.GET_SPEC;
  bio.doio(disk,r);  bio_check;
  r.op:=req.SET_SPEC;
  WITH r DO
    ssc    :=9;
    secsize:=512;
    cyls   :=t;
    heads  :=2;
    minsec :=1;
    maxsec :=9;
    ressec :=0;
  END;
  bio.doio(disk,r);  bio_check;

  low.zero(buf);
  low.move(SYSTEM.ADR(buf)    ,adr(F000),SIZE(F000));
  low.move(SYSTEM.ADR(buf)+384,adr(F384),SIZE(F384));
  FOR i:=2560 TO HIGH(buf)       DO buf[i]:=CHAR(0F6h) END;
  FOR i:=2560 TO HIGH(buf) BY 32 DO buf[i]:=0c         END;
  bio.seek(disk,0,0);     bio_check;
  bio.put(disk,buf,4096); bio_check;
  bio.close(disk);        bio_check;
  read_boot;
END format;

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
  END
END putFAT;

PROCEDURE remove_cluster(clu: INTEGER);
  VAR next: INTEGER;
BEGIN
  WHILE (clu#0) & (clu#0FFFh) DO
    next:=FAT(clu); putFAT(clu,0); clu:=next
  END
END remove_cluster;

PROCEDURE alloc_cluster(no: INTEGER; VAR first: INTEGER): BOOLEAN;
  VAR i,cl: INTEGER;
BEGIN
  ASSERT(no>0);
  i:=2; first:=0;
  REPEAT
    IF FAT(i)=0 THEN
      IF first = 0 THEN first:=i ELSE putFAT(cl,i) END;
      cl:= i; DEC(no)
    END;
    INC(i);
  UNTIL (no=0) OR (i>max_clu);
  IF no=0 THEN
    putFAT(cl,0FFFh); RETURN TRUE
  ELSIF first#0 THEN
    remove_cluster(first)
  END;
  RETURN FALSE
END alloc_cluster;

PROCEDURE free(): INTEGER;
  VAR i,s: INTEGER;
BEGIN
  IF BAD THEN RETURN -1 END;
  i:=2; s:=0;
  REPEAT
    IF FAT(i)=0 THEN INC(s) END; INC(i)
  UNTIL i=max_clu;
  RETURN s*clu_size
END free;

PROCEDURE saveFATs;
BEGIN
  IF BAD THEN RETURN END;
  write_sectors(1, sec_per_FAT,SYSTEM.ADR(FATs));
  write_sectors(sec_per_FAT+1, sec_per_FAT, SYSTEM.ADR(FATs));
END saveFATs;

PROCEDURE loadFATs;
  VAR s: INTEGER;
BEGIN
  IF BAD THEN RETURN END;
  ASSERT(BYTES(FATs)>=sec_per_FAT*512);
  read_sectors( 1, sec_per_FAT , SYSTEM.ADR(FATs) );
  IF (ORD(FATs[0]) # media_desc) THEN
    werror(0,'%|48s','Illegal Files Allocation Table!'); BAD:=TRUE
  END;
  L.fre:=free()
END loadFATs;

VAR MSDIR: ARRAY [0..1023] OF STR32;
     MSCD: INTEGER;
     MSHI: INTEGER;

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

PROCEDURE get_entry(entry: INTEGER; VAR e: ITEM);
  VAR i: INTEGER;                   s,m,h: INTEGER;
      p: STR32;                     d,n,y: INTEGER;
BEGIN
  p:=MSDIR[entry];
  e.name:="";
  e.dir:=FALSE; e.eof:=00;
  e.time:=00;
  e.seld:=FALSE;
  IF ORD(p[0])=empty   THEN RETURN END;
  IF ORD(p[0])=deleted THEN RETURN END;
  FOR i:=0 TO 11 DO e.name[i]:=' '      END;
  FOR i:=0 TO 7  DO e.name[i]  :=p[i]   END; e.name[8]:='.';
  FOR i:=0 TO 2  DO e.name[i+9]:=p[i+8] END; e.name[12]:=0c;
  IF 4 IN BITSET(p[11]) THEN e.dir:=TRUE END;
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
END get_entry;

PROCEDURE get_free_entry(VAR entry: INTEGER; VAR e: ITEM): BOOLEAN;
  VAR i: INTEGER;                   s,m,h: INTEGER;
      p: STR32;                     d,n,y: INTEGER;
BEGIN
  entry:=0;
  LOOP
    IF entry=MSHI THEN RETURN FALSE END;
    IF (ORD(MSDIR[entry][0])=empty) OR (ORD(MSDIR[entry][0])=deleted) THEN
      EXIT
    END;
    INC(entry)
  END;
  p:=MSDIR[entry];
  e.name:="";
  e.dir:=FALSE; e.eof:=00;
  e.time:=00;
  e.seld:=FALSE;
  e.dir :=FALSE;
  e.time:=tim.time();
  e.clu :=-1;
  e.eof :=0;
  RETURN TRUE
END get_free_entry;

PROCEDURE put_entry(entry: INTEGER; VAL e: ITEM);
  VAR i: INTEGER;                   s,m,h: INTEGER;
      p: STR32;                     d,n,y: INTEGER;
BEGIN
  FOR i:=0 TO 10 DO p[i]:=' ' END;
  FOR i:=0 TO 7 DO p[i]  :=e.name[i]   END;
  FOR i:=0 TO 2 DO p[i+8]:=e.name[i+9] END;
  p[11]:=CHAR({5});
  IF e.dir THEN p[11]:=CHAR({5,4}) END;
  tim.unpack(tim.time(),y,n,d,h,m,s); y:=y-1980;
  s:=s+(m+h*64)*32;
  p[22]:=CHAR(s);       p[23]:=CHAR(s DIV 256);
--d:=d+(n+y*128)*16;
  d:=d+(n+y*16)*32;
  p[24]:=CHAR(d);       p[25]:=CHAR(d DIV 256);
  p[26]:=CHAR(e.clu);   p[27]:=CHAR(e.clu DIV 256);
  p[28]:=CHAR(e.eof);   p[29]:=CHAR(e.eof DIV 256);
  p[30]:=CHAR(e.eof DIV 10000h);  p[31]:=0c;
  MSDIR[entry]:=p;
END put_entry;

PROCEDURE open_dir(CLU: INTEGER);
  VAR n,sec,adr,clu: INTEGER;
BEGIN
  IF BAD THEN RETURN END;
  MSCD:=CLU;
  IF CLU<=0 THEN
    n:=root_dir_ent
  ELSE
    n:=0; clu:=CLU;
    WHILE (1<clu) & (clu<0FFFh) DO INC(n); clu:=FAT(clu) END;
    n:=n * sec_per_clu * bytes_per_sec DIV 32;
  END;
  ASSERT(n<=HIGH(MSDIR)+1);
  IF CLU<=0 THEN
    n:=root_dir_ent * 32 DIV bytes_per_sec;
    sec:=no_FATs * sec_per_FAT + 1;
    read_sectors(sec,n,ADR(MSDIR));
    MSHI:=root_dir_ent;
  ELSE
    MSHI:=n;
    clu:=CLU; adr:=ADR(MSDIR);
    WHILE (1<clu) & (clu<0FFFh) DO
      read_cluster(clu,adr); INC(adr,clu_size DIV 4); clu:=FAT(clu)
    END
  END
END open_dir;

PROCEDURE save_dir;
  VAR n,sec,clu,adr: INTEGER;
BEGIN
  IF BAD THEN RETURN END;
  adr:=ADR(MSDIR);
  IF MSCD<=0 THEN
    n:=root_dir_ent * 32 DIV bytes_per_sec;
    sec:=no_FATs * sec_per_FAT + 1;
    write_sectors(sec,n,adr);
  ELSE
    clu:=MSCD;
    WHILE (1<clu) & (clu<0FFFh) DO
      write_cluster(clu,adr); INC(adr,clu_size DIV 4); clu:=FAT(clu)
    END
  END
END save_dir;

PROCEDURE lookup(name: ARRAY OF CHAR; VAR entry: INTEGER): BOOLEAN;
  VAR i,j: INTEGER; p: STR32;
BEGIN
  FOR i:=8 TO 10 DO name[i]:=name[i+1] END; name[11]:=0c;
  FOR j:=0 TO HIGH(MSDIR) DO
    p:=MSDIR[j];
    IF (ORD(p[0])#empty) & (ORD(p[0])#deleted) THEN
      p[11]:=0c;
      IF p=name THEN entry:=j; RETURN TRUE END
    END
  END;
  RETURN FALSE
END lookup;

PROCEDURE msd_press(VAR name: ARRAY OF CHAR; VAL ename: ARRAY OF CHAR);
  VAR i,j: INTEGER;
BEGIN
  i:=0;
  WHILE (i<=7) & (ename[i]#' ') DO name[i]:=ename[i]; INC(i) END;
  IF ename[9]#' ' THEN
    name[i]:='.'; INC(i);
    j:=9;
    WHILE (j<=11) & (ename[j]#' ') DO name[i]:=ename[j]; INC(i); INC(j) END;
  END;
  name[i]:=0c;
END msd_press;

PROCEDURE msd_unpress(VAR mname: ARRAY OF CHAR; VAL name: ARRAY OF CHAR);
  VAR i,j: INTEGER;
BEGIN
  FOR i:=0 TO 11 DO mname[i]:=' ' END; mname[8]:='.'; mname[12]:=0c;
  i:=0;
  WHILE (i<=7) & (name[i]#0c) & (name[i]#'.') DO
    mname[i]:=CHAR(ORD(name[i]) MOD 128); INC(i)
  END;
  WHILE (name[i]#0c) & (name[i]#'.') DO INC(i) END;
  IF name[i]=0c THEN RETURN END;
  INC(i); j:=0;
  WHILE (j<3) & (name[i]#0c) DO
    mname[9+j]:=CHAR(ORD(name[i]) MOD 128); INC(j); INC(i)
  END
END msd_unpress;

PROCEDURE small(VAR s: ARRAY OF CHAR);
  VAR i: INTEGER;
BEGIN
  i:=0;
  WHILE s[i]#0c DO s[i]:=ASCII.SMALL(s[i]); INC(i) END
END small;

PROCEDURE capital(VAR s: ARRAY OF CHAR);
  VAR i: INTEGER;
BEGIN
  i:=0;
  WHILE s[i]#0c DO s[i]:=ASCII.CAPITAL(s[i]); INC(i) END
END capital;

PROCEDURE termo(VAL name: ARRAY OF CHAR; pos,eof: INTEGER; VAR prc: INTEGER);
  VAR i: INTEGER;
      Y: POINTER TO STATE;
BEGIN
  IF eof<=0 THEN eof:=1 END;
  IF X=ADR(L) THEN Y:=ADR(R) ELSE Y:=ADR(L) END;
  IF pos=0 THEN
    c(-1); r(1);
    tty.set_pos(X^.inf^.l+X^.inf^.h,X^.inf^.c+1);
    tty.print(" %-32s",name);
    tty.set_pos(Y^.inf^.l+Y^.inf^.h,Y^.inf^.c+1);
    tty.print(" %32s",'  ');
    c(0); r(0); prc:=0; RETURN
  END;
  i:=pos*33 DIV eof;
  c(+1); r(1);
  WHILE prc<i DO
    IF prc#0 THEN
      IF Y=ADR(L) THEN
        tty.set_pos(Y^.inf^.l+Y^.inf^.h,Y^.inf^.c+1+32-prc);
        tty.print("< ")
      ELSE
        tty.set_pos(Y^.inf^.l+Y^.inf^.h,Y^.inf^.c+1+prc-1);
        tty.print(" >")
      END
    ELSE
      IF Y=ADR(L) THEN
        tty.set_pos(Y^.inf^.l+Y^.inf^.h,Y^.inf^.c+1+32-prc);
        tty.print("<")
      ELSE
        tty.set_pos(Y^.inf^.l+Y^.inf^.h,Y^.inf^.c+1+prc);
        tty.print(">")
      END
    END;
    INC(prc);
  END;
  c(0); r(0)
END termo;

CONST kr2pc = ARRAY OF CHAR {
  000c,001c,002c,003c,004c,005c,006c,007c,010c,011c,012c,013c,014c,015c,
  016c,017c,020c,021c,022c,023c,024c,025c,026c,027c,030c,031c,032c,033c,
  034c,035c,036c,037c,040c,041c,042c,043c,044c,045c,046c,047c,050c,051c,
  052c,053c,054c,055c,056c,057c,060c,061c,062c,063c,064c,065c,066c,067c,
  070c,071c,072c,073c,074c,075c,076c,077c,100c,101c,102c,103c,104c,105c,
  106c,107c,110c,111c,112c,113c,114c,115c,116c,117c,120c,121c,122c,123c,
  124c,125c,126c,127c,130c,131c,132c,133c,134c,135c,136c,137c,140c,141c,
  142c,143c,144c,145c,146c,147c,150c,151c,152c,153c,154c,155c,156c,157c,
  160c,161c,162c,163c,164c,165c,166c,167c,170c,171c,172c,173c,174c,175c,
  176c,177c,200c,201c,202c,203c,204c,205c,206c,207c,210c,211c,212c,213c,
  214c,215c,216c,217c,220c,221c,222c,223c,224c,225c,226c,227c,230c,231c,
  232c,233c,234c,235c,236c,237c,240c,241c,242c,243c,244c,245c,246c,247c,
  250c,251c,252c,253c,254c,255c,256c,257c,260c,261c,262c,263c,264c,265c,
  266c,267c,270c,271c,272c,273c,274c,275c,276c,277c,356c,240c,241c,346c,
  244c,245c,344c,243c,345c,250c,251c,252c,253c,254c,255c,256c,257c,357c,
  340c,341c,342c,343c,246c,242c,354c,353c,247c,350c,355c,351c,347c,352c,
  236c,200c,201c,226c,204c,205c,224c,203c,225c,210c,211c,212c,213c,214c,
  215c,216c,217c,237c,220c,221c,222c,223c,206c,202c,234c,233c,207c,230c,
  235c,231c,227c,232c};

CONST pc2kr = ARRAY OF CHAR {
  000c,001c,002c,003c,004c,005c,006c,007c,010c,011c,012c,013c,014c,015c,
  016c,017c,020c,021c,022c,023c,024c,025c,026c,027c,030c,031c,032c,033c,
  034c,035c,036c,037c,040c,041c,042c,043c,044c,045c,046c,047c,050c,051c,
  052c,053c,054c,055c,056c,057c,060c,061c,062c,063c,064c,065c,066c,067c,
  070c,071c,072c,073c,074c,075c,076c,077c,100c,101c,102c,103c,104c,105c,
  106c,107c,110c,111c,112c,113c,114c,115c,116c,117c,120c,121c,122c,123c,
  124c,125c,126c,127c,130c,131c,132c,133c,134c,135c,136c,137c,140c,141c,
  142c,143c,144c,145c,146c,147c,150c,151c,152c,153c,154c,155c,156c,157c,
  160c,161c,162c,163c,164c,165c,166c,167c,170c,171c,172c,173c,174c,175c,
  176c,177c,341c,342c,367c,347c,344c,345c,366c,372c,351c,352c,353c,354c,
  355c,356c,357c,360c,362c,363c,364c,365c,346c,350c,343c,376c,373c,375c,
  377c,371c,370c,374c,340c,361c,301c,302c,327c,307c,304c,305c,326c,332c,
  311c,312c,313c,314c,315c,316c,317c,320c,260c,261c,262c,263c,264c,265c,
  266c,267c,270c,271c,272c,273c,274c,275c,276c,277c,300c,301c,302c,303c,
  304c,305c,306c,307c,310c,311c,312c,313c,314c,315c,316c,317c,320c,321c,
  322c,323c,324c,325c,326c,327c,330c,331c,332c,333c,334c,335c,336c,337c,
  322c,323c,324c,325c,306c,310c,303c,336c,333c,335c,337c,331c,330c,334c,
  300c,321c,360c,361c,362c,363c,364c,365c,366c,367c,370c,371c,372c,373c,
  374c,375c,376c,377c};

PROCEDURE msd2exc(VAL e: ITEM; txt: BOOLEAN);
  VAR f: bio.FILE;
     ch: CHAR;
    i,j: INTEGER;
    clu: INTEGER;
    pos: INTEGER;
    prc: INTEGER;
    len: INTEGER;
    eof: INTEGER;
    buf: STRING;
   name: STR32;
   text: STRING;

PROCEDURE read_to_buf;
  VAR fclu,nclu,con: INTEGER;
BEGIN
  fclu:=clu;
  nclu:=1;
  clu :=FAT(clu);
  con :=sec_per_trk*heads;
  con :=con - (offset+(clu-2)*sec_per_clu) MOD con;
  WHILE (nclu<con) & (1<clu) & (clu<0FFFh)&(fclu+nclu=clu) & ((nclu+1)*clu_size<=BYTES(buf)) DO
    clu:=FAT(clu);
    INC(nclu)
  END;
  read_clusters(fclu,nclu,ADR(buf));
  len:=clu_size*nclu;
  IF eof<clu_size*nclu THEN len:=eof END
END read_to_buf;

BEGIN
  IF e.name="" THEN RETURN END;
  msd_press(name,e.name);
  pos:=0; prc:=0;
  termo(name,0,e.eof,prc);
  small(name);
  bio.open(f,name,'w');
  IF bio.done OR (bio.error=err.sec_vio) THEN
    bio.close(f);
    IF NOT query('Such file already exist',name,'[Overwrite]','[Skip]') THEN
      termo("",0,e.eof,prc); RETURN
    END
  END;
  bio.create(f,name,'w',e.eof); bio_check;
  bio.buffers(f,1,4096);        bio_check;
  NEW(buf,sec_per_trk*heads*bytes_per_sec*2);
  IF txt THEN
    pos:=0;
    eof:=e.eof;
    clu:=e.clu;
    NEW(text,eof);
    WHILE (1<clu) & (clu<0FFFh) DO
      read_to_buf;
      low.cmove(ADR(text),pos,ADR(buf),0,len);
      DEC(eof,len); INC(pos,len)
    END;
    pos:=0; i:=0; j:=0;
    eof:=e.eof;
    WHILE eof>0 DO
      ch:=text[i];
      IF ch>=40c   THEN buf[j]:=pc2kr[ORD(ch)]; INC(j)
      ELSIF ch=12c THEN buf[j]:=36c;       INC(j)
      ELSIF ch#15c THEN buf[j]:=pc2kr[ORD(ch)]; INC(j)
      END;
      IF j=4096 THEN
        bio.put(f,buf,j); bio_check; INC(pos,j); j:=0;
        termo(name,pos,e.eof,prc)
      END;
      DEC(eof); INC(i)
    END;
    IF j#0 THEN bio.put(f,buf,j); bio_check; INC(pos,j) END;
    DISPOSE(text)
  ELSE
    eof:=e.eof;
    clu:=e.clu;
    WHILE (1<clu) & (clu<0FFFh) DO
      termo(name,pos,e.eof,prc);
      read_to_buf;
      bio.put(f,buf,len); bio_check; DEC(eof,len); INC(pos,len)
    END
  END;
  DISPOSE(buf);
  termo(name,e.eof,e.eof,prc);
  bio.close(f);     bio_check;
  bio.open(f,name,'r'); bio_check;
  bio.set_attr(f, bio.a_wtime, e.time);     bio_check;
  bio.set_attr(f, bio.a_ctime, e.time);     bio_check;
  bio.close(f);     bio_check;
END msd2exc;

PROCEDURE exc2msd(VAR e: ITEM; txt: BOOLEAN);
  VAR m: ITEM;
      f: bio.FILE;
     ch: CHAR;
    ent: INTEGER;
    i,j: INTEGER;
    clu: INTEGER;
   fclu: INTEGER;
   nclu: INTEGER;
    pos: INTEGER;
    prc: INTEGER;
    len: INTEGER;
   flen: INTEGER;
    eof: INTEGER;
    buf: STRING;
   text: STRING;
   name: STR32;

PROCEDURE calc_fats;
  VAR con: INTEGER;
BEGIN
  fclu:=clu;
  nclu:=1;
  clu :=FAT(clu);
  con :=sec_per_trk*heads;
  con :=con - (offset+(clu-2)*sec_per_clu) MOD con;
  WHILE (nclu<con) & (1<clu) & (clu<0FFFh)&(fclu+nclu=clu) & ((nclu+1)*clu_size<=BYTES(buf)) DO
    clu:=FAT(clu);
    INC(nclu)
  END;
  flen:=clu_size*nclu
END calc_fats;

BEGIN
  IF e.name="" THEN RETURN END;
  msd_unpress(name,e.name);
  pos:=0; prc:=0;
  capital(name);
  termo(name,0,e.eof,prc);
  bio.open(f,e.name,'r');
  IF NOT bio.done THEN
    werror(bio.error,'open "%s"',name); RETURN
  END;
  bio.buffers(f,1,4096);        bio_check;
  e.eof:=bio.eof(f);            bio_check;
  IF lookup(name,ent) THEN
    IF NOT query('Such file already exist',name,'[Overwrite]','[Skip]') THEN
      termo("",0,e.eof,prc); bio.close(f); RETURN
    END;
    get_entry(ent,m);
    remove_cluster(m.clu);
    m.eof:=0;
    put_entry(ent,m);
    save_dir;
    saveFATs;
  ELSE
    IF NOT get_free_entry(ent,m) THEN
      werror(0,'no free entry for "%s"',name); bio.close(f); RETURN
    END
  END;
  eof:=e.eof;
  IF eof>L.fre THEN
    werror(0,'no space for "%s"',name); bio.close(f); RETURN
  END;
  IF txt THEN
    NEW(text,eof);
    bio.get(f,text,eof); bio_check;
    j:=0;
    FOR i:=0 TO HIGH(text) DO (*$<$T-*)
      ch:=text[i]; INC(eof,ORD(ch=36c)); INC(j,ORD(ch=12c)) (*$>*)
    END;
    IF j>0 THEN
      IF text[0]=12c THEN INC(eof) END;
      FOR i:=1 TO HIGH(text) DO
        INC(eof,ORD((text[i]=12c) & (text[i-1]#15c)))
      END
    END
  END;
  IF NOT alloc_cluster((eof+clu_size-1) DIV clu_size,m.clu) THEN
    werror(0,'no space for "%s"',name); bio.close(f); RETURN
  END;
  m.name:=name;
  saveFATs;
  NEW(buf,sec_per_trk*heads*bytes_per_sec);
  IF txt THEN
    i:=0; j:=0; len:=e.eof; clu:=m.clu; calc_fats;
    WHILE len>0 DO
      ch:=text[i];
      IF ch>=40c THEN
        buf[j]:=kr2pc[ORD(ch)]; INC(j)
      ELSIF (ch=36c) OR ((ch=12c) & ((i=0) OR (i>0) & (text[i-1]#15c))) THEN
        buf[j]:=15c; INC(j);
        IF j=flen THEN
          write_clusters(fclu,nclu,ADR(buf)); calc_fats; INC(pos,flen); j:=0;
          termo(name,pos,eof,prc)
        END;
        buf[j]:=12c; INC(j)
      ELSE
        buf[j]:=kr2pc[ORD(ch)]; INC(j)
      END;
      IF j=flen THEN
        write_clusters(fclu,nclu,ADR(buf)); calc_fats; INC(pos,flen); j:=0;
        termo(name,pos,eof,prc)
      END;
      DEC(len); INC(i)
    END;
    IF j#0 THEN write_clusters(fclu,nclu,ADR(buf)); INC(pos,j) END;
    m.eof:=eof;
    DISPOSE(text)
  ELSE
    eof:=e.eof;
    clu:=m.clu;
    WHILE eof>0 DO
      termo(name,pos,e.eof,prc);
      calc_fats;
      IF eof>flen THEN len:=flen ELSE len:=eof END;
      bio.get(f,buf,len); bio_check;
      write_clusters(fclu,nclu,ADR(buf));
      DEC(eof,len); INC(pos,len)
    END;
    m.eof:=e.eof;
  END;
  DISPOSE(buf);
  termo(name,e.eof,e.eof,prc);
  bio.close(f);     bio_check;
  m.time := e.time;
  put_entry(ent,m);
  save_dir
END exc2msd;

PROCEDURE fmtbytes(VAR e: ARRAY OF CHAR; i: INTEGER);
BEGIN
  IF i<=999       THEN str.print(e," %3d",i)
  ELSIF i<=999999 THEN str.print(e," %3d,%03d",i DIV 1000,i MOD 1000)
  ELSE                 str.print(e," %d,%03d,%03d",i DIV 1000000
                             ,i DIV 1000 MOD 1000,i MOD 1000)
  END
END fmtbytes;

PROCEDURE showstat(VAL s: STATE);
  VAR f,e: STR32;
BEGIN
  tty.set_pos(s.inf^.l+s.inf^.h+1,s.inf^.c+1);  c(-1);
  IF s.cou>0 THEN
    fmtbytes(e,s.sum);
    str.print(f,"%s bytes in %d files",e,s.cou)
  ELSIF s.fre>=0 THEN
    fmtbytes(e,s.fre);
    str.print(f,"%s bytes free",e)
  ELSE
    str.print(f,"no volume")
  END;
  tty.print("%-32.32s",f);                       c(0)
END showstat;

PROCEDURE copy(txt: BOOLEAN): BOOLEAN;

  PROCEDURE stat(VAL s: STATE);
    VAR i: INTEGER;
  BEGIN
    bio.du(bio.cd,R.fre,i); bio_check; L.fre:=free();
    showstat(s)
  END stat;

  VAR q: ARRAY [0..63] OF CHAR;
    sys: STR32;
    i,j: INTEGER;
   name: STR32;
   smal: STR32;
BEGIN
  IF HIGH(X^.dir)<0 THEN RETURN FALSE END;
  i:=0; j:=0;
  WHILE i<=HIGH(X^.dir) DO INC(j,ORD(X^.dir[i].seld)); INC(i) END;
  IF j>0 THEN
    str.print(q,'Copy %d selected files?',j);
    IF txt THEN
      IF NOT query(q,'(text)','[Yes]','[No]') THEN RETURN FALSE END
    ELSE
      IF NOT query(q,'(bin)','[Yes]','[No]') THEN RETURN FALSE END
    END
  ELSIF (X^.cur<0) OR (X^.cur>HIGH(X^.dir)) & X^.dir[X^.cur].dir THEN
    RETURN FALSE
  ELSE
    IF X=ADR(L) THEN
      msd_press(name,X^.dir[X^.cur].name); smal:=name; small(smal); sys:="MS/DOS"
    ELSE
      name:=X^.dir[X^.cur].name; msd_unpress(smal,name); capital(smal); sys:="Excelsior"
    END;
    IF txt THEN
      str.print(q,'Copy %s file "%s" to (text)',sys,name)
    ELSE
      str.print(q,'Copy %s file "%s" to (bin)',sys,name)
    END;
    IF NOT query(q,smal,'[Yes]','[No]') THEN RETURN FALSE END
  END;
  IF j>0 THEN
    i:=0;
    WHILE i<=HIGH(X^.dir) DO
      IF X^.dir[i].seld THEN
        IF X=ADR(L) THEN msd2exc(X^.dir[i],txt); stat(R)
        ELSE             exc2msd(X^.dir[i],txt); stat(L)
        END
      END; INC(i);
    END
  ELSE
    IF X=ADR(L) THEN msd2exc(X^.dir[X^.cur],txt)
    ELSE             exc2msd(X^.dir[X^.cur],txt)
    END
  END;
  RETURN TRUE
END copy;

PROCEDURE msd_remove(e: ITEM);
  VAR i: INTEGER;
BEGIN
  IF NOT lookup(e.name,i) THEN
    werror(0,'can`t lookup "%s"',e.name); RETURN
  END;
  get_entry(i,e);
  IF e.dir THEN RETURN END;
  e.name[0]:=CHAR(deleted);
  remove_cluster(e.clu);
  put_entry(i,e);
  saveFATs;
  save_dir;
END msd_remove;

PROCEDURE exc_remove(VAL e: ITEM);
BEGIN
  bio.unlink(e.name);
  IF NOT bio.done THEN
    werror(bio.error,'can`t remove "%s"',e.name); RETURN
  END
END exc_remove;

PROCEDURE remove(): BOOLEAN;
  VAR q: ARRAY [0..63] OF CHAR;
    i,j: INTEGER;
   name: STR32;
BEGIN
  IF HIGH(X^.dir)<0 THEN RETURN FALSE END;
  i:=0; j:=0;
  WHILE i<=HIGH(X^.dir) DO INC(j,ORD(X^.dir[i].seld)); INC(i) END;
  IF j>0 THEN
    str.print(q,'Remove %d selected files? ',j);
    IF NOT query(q,'','[Yes]','[No]') THEN RETURN FALSE END
  ELSIF (X^.cur<0) OR (X^.cur>HIGH(X^.dir)) & X^.dir[X^.cur].dir THEN
    RETURN FALSE
  ELSE
    IF X=ADR(L) THEN
      msd_press(name,X^.dir[X^.cur].name)
    ELSE
      name:=X^.dir[X^.cur].name
    END;
    str.print(q,'Remove file "%s"? ',name);
    IF NOT query(q,'','[Yes]','[No]') THEN RETURN FALSE END
  END;
  IF j>0 THEN
    i:=0;
    WHILE i<=HIGH(X^.dir) DO
      IF X^.dir[i].seld THEN
        IF X=ADR(L) THEN msd_remove(X^.dir[i])
        ELSE             exc_remove(X^.dir[i])
        END
      END; INC(i)
    END
  ELSE
    IF X=ADR(L) THEN msd_remove(X^.dir[X^.cur])
    ELSE             exc_remove(X^.dir[X^.cur])
    END
  END;
  RETURN TRUE
END remove;

PROCEDURE mwalk(VAR d: DIRECTORY);
  VAR i,h: INTEGER;
BEGIN
  RESIZE(d,0);
  read_boot;
  IF BAD THEN msd_done:=FALSE; RETURN END;
  loadFATs;
  IF BAD THEN msd_done:=FALSE; RETURN END;
  low.zero(MSDIR);
  open_dir(MSCD);
  FOR i:=0 TO MSHI-1 DO
    IF (ORD(MSDIR[i][0])#empty) & (ORD(MSDIR[i][0])#deleted) THEN
      h:=HIGH(d)+1; RESIZE(d,h+1); get_entry(i,d[h])
    END
  END;
  sort(d);
  msd_done:=TRUE
END mwalk;

PROCEDURE msd_chdir(VAL name: ARRAY OF CHAR);
  VAR e: ITEM; i: INTEGER;
BEGIN
  msd_done:=FALSE;
  IF BAD OR NOT lookup(name,i) THEN RETURN END;
  get_entry(i,e);
  IF (e.name="") OR NOT e.dir THEN RETURN END;
  MSCD:=e.clu;
  mwalk(L.dir)
END msd_chdir;

----------------------------------------------------------------

PROCEDURE cursor(VAL s: STATE; on: INTEGER);
  VAR name: STR32;
   i,l,c,w: INTEGER;
BEGIN
  IF (s.cur<0) OR (s.cur>HIGH(s.dir)) THEN RETURN END;
  i:=(s.cur-s.top);
  l:=i MOD (s.inf^.h-2);
  c:=i DIV (s.inf^.h-2);
  w:=s.inf^.w-2; w:=(w-2) DIV 3;
  c:=c*(w+1);
  wnd.setpos(s.win,l,c); wnd_check;
  str.copy(name,s.dir[s.cur].name);
  IF s.dir[s.cur].dir THEN str.append(name,"/") END;
  mark(s,s.cur);
  IF on#0 THEN wnd.reverse(s.win,1) END;
  wnd.print(s.win,"%-*.*s",w,w,name);
  IF on#0 THEN wnd.reverse(s.win,0); r(0) END;
  unmark(s)
END cursor;

PROCEDURE help;
  VAR ch: CHAR;
BEGIN
  wnd.ontop(H); wnd.open(H); key.read(ch); wnd.close(H);
END help;

PROCEDURE quit;
  VAR ch: CHAR;
BEGIN
  wnd.era(H,2);             wnd.ontop(H);
  wnd.setpos(H,2,6);        wnd.print(H,"I wonder You realy (!) want to QUIT");
  wnd.setpos(H,4,6);        wnd.print(H,"this beautifull and powerfull tool!");
  wnd.foreground(H,bright); wnd.reverse(H,1);
  wnd.setpos(H,7,9);        wnd.print(H,"[Yes]");
  wnd.foreground(H,normal);
  wnd.setpos(H,7,20);       wnd.print(H,"[Sorry]");
  wnd.foreground(H,shadow);
  wnd.setpos(H,7,31);       wnd.print(H,"[Never]");
  wnd.open(H);              key.read(ch);
  wnd.close(H);
  p(0); tty.erase(0);
END quit;

PROCEDURE monitor;


  PROCEDURE mvup;
  BEGIN
    IF (X^.cur<0) OR (X^.cur>HIGH(X^.dir)) THEN RETURN END;
    cursor(X^,0);
    IF X^.cur=0 THEN X^.cur:=X^.top+X^.inf^.h-2-1;
      IF X^.cur>HIGH(X^.dir) THEN X^.cur:=HIGH(X^.dir) END
    ELSIF X^.cur>X^.top THEN
      DEC(X^.cur)
    ELSE
      X^.top:=X^.top-(X^.inf^.h-2) DIV 2;
      IF X^.top<0 THEN X^.top:=0 END;
      DEC(X^.cur);
      fill(X^)
    END;
    cursor(X^,1)
  END mvup;

  PROCEDURE mvlf;
    VAR h: INTEGER;
  BEGIN
    IF (X^.cur<0) OR (X^.cur>HIGH(X^.dir)) THEN RETURN END;
    h:=X^.inf^.h-2;
    IF HIGH(X^.dir)<h THEN RETURN END;
    cursor(X^,0);
    IF X^.cur<h THEN
      X^.cur:=X^.cur+h*2;
      WHILE X^.cur>HIGH(X^.dir) DO X^.cur:=X^.cur-h END
    ELSIF X^.cur-h>=X^.top THEN
      DEC(X^.cur,h)
    ELSE
      X^.top:=X^.top-h;
      IF X^.top<0 THEN X^.top:=0 END;
      DEC(X^.cur,h);
      fill(X^)
    END;
    cursor(X^,1)
  END mvlf;

  PROCEDURE mvdw;
    VAR h: INTEGER;
  BEGIN
    IF (X^.cur<0) OR (X^.cur>HIGH(X^.dir)) THEN RETURN END;
    cursor(X^,0);
    h:=X^.inf^.h-2;
    IF X^.cur=HIGH(X^.dir) THEN
      X^.cur:=X^.top+(X^.cur-X^.top) DIV h * h
    ELSIF X^.cur<X^.top+h*3-1 THEN
      INC(X^.cur)
    ELSE
      X^.top:=X^.top+h DIV 2;
      INC(X^.cur);
      fill(X^)
    END;
    cursor(X^,1)
  END mvdw;

  PROCEDURE mvrg;
    VAR h: INTEGER;
  BEGIN
    IF (X^.cur<0) OR (X^.cur>HIGH(X^.dir)) THEN RETURN END;
    h:=X^.inf^.h-2;
    IF HIGH(X^.dir)<h THEN RETURN END;
    cursor(X^,0);
    IF X^.cur>HIGH(X^.dir)-h THEN
      X^.cur:=X^.cur-h*2;
      WHILE X^.cur<X^.top DO INC(X^.cur,h) END
    ELSIF X^.cur+h<=X^.top+h*3-1 THEN
      INC(X^.cur,h)
    ELSE
      X^.top:=X^.top+h;
      INC(X^.cur,h);
      fill(X^)
    END;
    cursor(X^,1)
  END mvrg;

  PROCEDURE pgup(n: INTEGER);
  BEGIN
    IF (X^.top=0) & (X^.cur<=0)THEN RETURN END;
    cursor(X^,0);
    IF X^.cur-n>=X^.top THEN DEC(X^.cur,n); cursor(X^,1); RETURN END;
    IF X^.top=0         THEN X^.cur:=0;     cursor(X^,1); RETURN END;
    DEC(X^.cur,n); DEC(X^.top,n);
    IF X^.top<0 THEN X^.top:=0 END;
    IF X^.cur<0 THEN X^.cur:=0 END;
    fill(X^);   cursor(X^,1)
  END pgup;

  PROCEDURE pgdw(n: INTEGER);
    VAR h: INTEGER;
  BEGIN
    h:=X^.inf^.h-2;
    IF X^.cur=HIGH(X^.dir) THEN RETURN END;
    cursor(X^,0);
    IF X^.cur+n>HIGH(X^.dir) THEN n:=HIGH(X^.dir)-X^.cur END;
    IF X^.cur+n<X^.top+h*3 THEN
      INC(X^.cur,n);
      cursor(X^,1); RETURN
    END;
    INC(X^.top,n);
    INC(X^.cur,n);
    fill(X^);   cursor(X^,1)
  END pgdw;

  PROCEDURE enter;
    VAR f: bio.FILE;
        i: INTEGER;
     last: STR32;
   cdname: ARRAY [0..127] OF CHAR;
  BEGIN
    IF (X^.cur<0) OR (X^.cur>HIGH(X^.dir)) THEN RETURN END;
    WITH X^.dir[X^.cur] DO
      IF NOT dir THEN RETURN END;
      IF X=ADR(R) THEN
        IF name=".." THEN
          bio.fname(bio.cd,cdname);
          bio.splitpathname(cdname,last);
        ELSE
          last:=""
        END;
        bio.open(f,name,'rwx');
        IF NOT bio.done THEN RETURN END;
        bio.close(f);
        bio.chdir(name);
        IF NOT bio.done  THEN RETURN END;
        xwalk(X^.dir);
        IF last#"" THEN
          i:=0;
          WHILE (i<=HIGH(X^.dir)) & (X^.dir[i].name#last) DO INC(i) END
        ELSE
          i:=HIGH(X^.dir)+1
        END;
        IF i<=HIGH(X^.dir) THEN
          X^.cur:=i; X^.top:=i-X^.inf^.h-2-1;
          IF X^.top<0 THEN X^.top:=0 END
        ELSE
          X^.top:=0; X^.cur:=0
        END
      ELSE
        msd_chdir(name);
        IF NOT msd_done  THEN RETURN END;
        X^.cur:=0; X^.top:=0; mwalk(X^.dir)
      END;
      fill(X^);  cursor(X^,1)
    END
  END enter;

  PROCEDURE pull(VAR s: STATE);
  BEGIN
    IF s.cur>HIGH(s.dir) THEN
      s.cur:=HIGH(s.dir);
      s.top:=s.cur-(X^.inf^.h-2)*3+1;
      IF s.top<0 THEN s.top:=0 END
    END
  END pull;

  PROCEDURE showeof(VAL s: STATE);
    VAR i: INTEGER;
        e: STR32;
  BEGIN
    IF (s.cur<0) OR (s.cur>HIGH(s.dir)) THEN RETURN END;
    i:=s.dir[s.cur].eof;
    IF i<0 THEN RETURN END;
    fmtbytes(e,i);
    tty.set_pos(s.inf^.l+s.inf^.h,s.inf^.c+1+32-str.len(e));  c(-1); r(1);
    tty.print("%s",e);                                        c( 0); r(0)
  END showeof;

  PROCEDURE showname(VAL s: STATE);
    VAR name: STR32;
  BEGIN
    tty.set_pos(s.inf^.l+s.inf^.h,s.inf^.c+1);  c(-1); r(1);
    IF (s.cur<0) OR (s.cur>HIGH(s.dir)) THEN
      tty.print(" %-32s",'  ');                 r( 0); c(0);
    ELSE
      IF ADR(s)=ADR(L) THEN
        msd_press(name,s.dir[s.cur].name)
      ELSE
        name:=s.dir[s.cur].name
      END;
      tty.print(" %-32s",name);     r( 0); c(0);
      showeof(s)
    END
  END showname;

  PROCEDURE updateattr(i: INTEGER);
    VAR f: bio.FILE;
  BEGIN
    IF X=ADR(L) THEN RETURN END;
    IF (i<0) OR (i>HIGH(R.dir)) THEN RETURN END;
    IF R.dir[i].eof>=0 THEN RETURN END;
    bio.open(f,R.dir[i].name,'');
    IF NOT bio.done THEN RETURN END;
    R.dir[i].eof:=bio.eof(f);
    bio.get_attr(f,bio.a_wtime,R.dir[i].time);
    IF R.cur=i THEN showname(R) END;
    bio.close(f)
  END updateattr;


  PROCEDURE picture(VAL s: STATE);
  BEGIN
    cursor(s,ORD(X=ADR(s)));
    IF X=ADR(s) THEN c(+1); r(1) END;
    tty.set_pos(s.inf^.l+s.inf^.h,s.inf^.c); tty.print(" ");
    IF ADR(s)=ADR(R) THEN updateattr(s.cur) END;
    showname(s);
    r(0);  c(0);
    showstat(s)
  END picture;

  PROCEDURE hidden;
  BEGIN
    cursor(X^,0);
    HID:=NOT HID;
    R.cou:=0;     R.sum:=0;
    xwalk(R.dir); pull(R); fill(R); picture(R); cursor(X^,1)
  END hidden;

  PROCEDURE jump;
  BEGIN
    IF BAD & (X=ADR(R)) THEN
      mwalk(L.dir); fill(L);
      IF BAD THEN picture(L); picture(R); RETURN END
    END;
    cursor(X^,0);
    IF X=ADR(R) THEN X:=ADR(L) ELSE X:=ADR(R) END;
    picture(L); picture(R)
  END jump;

  PROCEDURE home;
  BEGIN
    IF (X^.top=0) & (X^.cur=0) THEN RETURN END;
    cursor(X^,0);
    IF X^.top=0 THEN X^.cur:=0;
    ELSE X^.top:=0;  X^.cur:=0;  fill(X^)
    END;
    cursor(X^,1)
  END home;

  PROCEDURE end;
    VAR h: INTEGER;
  BEGIN
    h:=X^.inf^.h-2;
    IF X^.cur=HIGH(X^.dir) THEN RETURN END;
    cursor(X^,0);
    X^.cur:=HIGH(X^.dir);
    IF X^.top<X^.cur-h*3+1 THEN
      X^.top:=X^.cur-h*3+1;
      IF X^.top<0 THEN X^.top:=0 END;  fill(X^)
    END;
    cursor(X^,1)
  END end;

  PROCEDURE unselect(VAR s: STATE);
    VAR i: INTEGER; done: BOOLEAN;
  BEGIN
    done:=FALSE;
    FOR i:=0 TO HIGH(s.dir) DO
      IF s.dir[i].seld THEN done:=TRUE END;
      s.dir[i].seld:=FALSE
    END;
    s.cou:=0; s.sum:=0;
    IF NOT done THEN RETURN END;
    fill(s); picture(s)
  END unselect;

  PROCEDURE select;
    VAR i: INTEGER;
  BEGIN
    IF (X^.cur<0) OR (X^.cur>HIGH(X^.dir)) THEN RETURN END;
    WITH X^.dir[X^.cur] DO
      IF dir THEN mvdw; RETURN END;
      IF X=ADR(R) THEN updateattr(X^.cur) END;
      seld:=NOT seld;
      IF X=ADR(R) THEN i:=(eof+clu_size-1) DIV clu_size * clu_size
      ELSE             i:=(eof+      4095) DIV 4096     * 4096
      END;
      IF seld THEN INC(X^.cou); INC(X^.sum,i)
      ELSE         DEC(X^.cou); DEC(X^.sum,i)
      END;
      IF seld THEN
        IF X=ADR(L) THEN unselect(R) ELSE unselect(L) END;
      END
    END;
    showstat(X^);
    IF X^.cur<HIGH(X^.dir) THEN mvdw ELSE cursor(X^,1) END
  END select;


  PROCEDURE selectall;
    VAR i,j: INTEGER; done: BOOLEAN;
  BEGIN
    done:=FALSE;
    FOR i:=0 TO HIGH(X^.dir) DO
      WITH X^.dir[i] DO
        IF NOT dir & NOT seld THEN
          done:=TRUE; seld:=TRUE;
          IF X=ADR(R) THEN updateattr(i) END;
          IF X=ADR(R) THEN j:=(eof+clu_size-1) DIV clu_size * clu_size
          ELSE             j:=(eof+      4095) DIV     4096 * 4096
          END;
          INC(X^.cou); INC(X^.sum,j)
        END
      END
    END;
    IF NOT done THEN RETURN END;
    IF X=ADR(L) THEN unselect(R) ELSE unselect(L) END;
    fill(X^); picture(X^); cursor(X^,1)
  END selectall;

  PROCEDURE reread;
    VAR i: INTEGER;
  BEGIN
    L.cou:=0; L.sum:=0;
    R.cou:=0; R.sum:=0;
    mwalk(L.dir); pull(L); fill(L);
    xwalk(R.dir); pull(R); fill(R);
    bio.du(bio.cd,R.fre,i); bio_check; L.fre:=free();
    picture(L);   picture(R)
  END reread;

  PROCEDURE docopy(txt: BOOLEAN);
    VAR done: BOOLEAN;
  BEGIN
    cursor(X^,0);
    done:=copy(txt);
    IF done THEN reread ELSE picture(L); picture(R) END
  END docopy;

  PROCEDURE doremove;
    VAR done: BOOLEAN;
  BEGIN
    cursor(X^,0);
    done:=remove();
    IF done THEN reread ELSE picture(L); picture(R) END
  END doremove;

  VAR ch: CHAR;

BEGIN
  fill(R);
  fill(L);
  X:=ADR(R);
  reread;
  LOOP
    IF (X^.cur>=0) & (X^.cur<=HIGH(X^.dir)) & (key.ready()=0) THEN
      showname(X^)
    END;
    LOOP
      key.wait(200);
      IF key.ready()>0 THEN EXIT END;
      updateattr(X^.cur);
      bio.flush(disk)
    END;
    key.read(ch);
    CASE ch OF
    |key.f1   : help; picture(L); picture(R)
    |key.f2   : hidden
    |key.f4   : docopy(TRUE)
    |key.f5   : docopy(FALSE)
    |key.f8   : doremove
    |'i','I'  : format; reread
    |'-'      : unselect(X^); cursor(X^,1)
    |'*'      : selectall
    |key.center
    ,key.cr   : enter
    |key.tab
    ,33c      : jump
    |key.home : home
    |key.end  : end
    |key.dw   : mvdw
    |key.up   : mvup
    |key.pgup : pgup((X^.inf^.h-2) DIV 2)
    |key.pgdw : pgdw((X^.inf^.h-2) DIV 2)
    |key.ins  : select
    |key.left : mvlf
    |key.right: mvrg
    |key.f10  : quit; picture(L); picture(R); RETURN
    ELSE
    END
  END
END monitor;

PROCEDURE mkwindows;
BEGIN
  NEW(L.dir);   L.top:=0; L.cur:=0;
  wnd.new(L.win);                         wnd_check;
  wnd.resize(L.win,40,16);                wnd_check;
  wnd.move  (L.win,0,0);                  wnd_check;
  wnd.frame (L.win,1);                    wnd_check;
  wnd.cursor(L.win,0);                    wnd_check;
  wnd.frameprint(L.win,0,2,"MS/DOS");     wnd_check;
  wnd.get(L.win,L.inf);                   wnd_check;

  NEW(R.dir);   R.top:=0; R.cur:=0;
  wnd.new(R.win);                         wnd_check;
  wnd.resize(R.win,37,16);                wnd_check;
  wnd.move  (R.win,0,80-37);              wnd_check;
  wnd.frame (R.win,1);                    wnd_check;
  wnd.cursor(R.win,0);                    wnd_check;
  wnd.frameprint(R.win,0,2,"Excelsior");  wnd_check;
  wnd.get(R.win,R.inf);                   wnd_check;

  wnd.new(H);                             wnd_check;
  wnd.resize(H,50,12);                    wnd_check;
  wnd.move  (H, 9,15);                    wnd_check;
  wnd.frame (H, 1);                       wnd_check;
  wnd.cursor(H, 0);                       wnd_check;

  wnd.new(MSG);                           wnd_check;
  wnd.resize (MSG,50, 5);                 wnd_check;
  wnd.move   (MSG, 9,15);                 wnd_check;
  wnd.frame  (MSG, 1);                    wnd_check;
  wnd.cursor (MSG, 0);                    wnd_check;

  wnd.setpos(H,0,10); wnd.foreground(H,bright);
  wnd.print (H,"MS/DOS <- exchange -> Excelsior");

  wnd.setpos(H,2,3);  wnd.foreground(H,shadow);
  wnd.print (H,"Ins");
  wnd.setpos(H,2,10); wnd.foreground(H,normal);
  wnd.print (H,"select/deselect");

  wnd.setpos(H,3,3);  wnd.foreground(H,shadow);
  wnd.print (H," * ");
  wnd.setpos(H,3,10); wnd.foreground(H,normal);
  wnd.print (H,"select all");

  wnd.setpos(H,4,3);  wnd.foreground(H,shadow);
  wnd.print (H," - ");
  wnd.setpos(H,4,10); wnd.foreground(H,normal);
  wnd.print (H,"deselect all");

  wnd.setpos(H,5,3);  wnd.foreground(H,shadow);
  wnd.print (H,"TAB");
  wnd.setpos(H,5,10); wnd.foreground(H,normal);
  wnd.print (H,"change panel");

  wnd.setpos(H,6,3);  wnd.foreground(H,shadow);
  wnd.print (H," I ");
  wnd.setpos(H,6,10); wnd.foreground(H,normal);
  wnd.print (H,"initialize volume");

  wnd.setpos(H,9,19); wnd.foreground(H,bright);
  wnd.print (H,"hacked by Leo 11-Apr-91");
END mkwindows;

PROCEDURE barline;
BEGIN
  p( 0); tty.erase_line(0);
  p( 1); c(+1);        tty.print("F1");              c(0);
  p( 3); c(-1); r(1);  tty.print(" Help ");    r(0); c(0);

  p(10); c(+1);        tty.print("F2");              c(0);
  p(12); c(-1); r(1);  tty.print(" Hidden ");  r(0); c(0);

  p(21); c(+1);        tty.print("F4");              c(0);
  p(23); c(-1); r(1);  tty.print(" CopyTxt "); r(0); c(0);

  p(33); c(+1);        tty.print("F5");              c(0);
  p(35); c(-1); r(1);  tty.print(" Copy "); r(0); c(0);

  p(42); c(+1);        tty.print("F6");              c(0);
  p(44); c(-1); r(1);  tty.print(" Rename ");  r(0); c(0);

  p(53); c(+1);        tty.print("F7");              c(0);
  p(55); c(-1); r(1);  tty.print(" Mkdir ");   r(0); c(0);

  p(63); c(+1);        tty.print("F8");              c(0);
  p(65); c(-1); r(1);  tty.print(" Del ");     r(0); c(0);

  p(71); c(+1);        tty.print("F10");             c(0);
  p(74); c(-1); r(1);  tty.print(" Quit ");    r(0); c(0);
END barline;

VAR s: req.REQUEST;

BEGIN
  tty.set_pos(0,0);
  tty.erase(0);
  MSCD:=-1;
  disk:=bio.null;
  tty.set_awp(0);
  tty.set_cursor(0);
  CASE tty.state^.max_color-tty.state^.min_color+1 OF
  |2: shadow:= 0; normal:=0; bright:= 0;
  |3: shadow:= 0; normal:=0; bright:=+1;
  |4: shadow:=-1; normal:=0; bright:=+1
  |8: shadow:=-1; normal:=0; bright:=+3
  ELSE
     shadow:= 0; normal:=0; bright:=0;
  END;
  msd_done:=TRUE; HID:=FALSE;
  mkwindows;
  barline;
  open_disk; s.op:=req.GET_SPEC; bio.doio(disk,s);  bio_check; bio.close(disk);
  monitor;
  bio.close(disk);
  open_disk; s.op:=req.SET_SPEC; bio.doio(disk,s);  bio_check; bio.close(disk);
END msex.
