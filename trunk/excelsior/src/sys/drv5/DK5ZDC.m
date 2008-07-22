MODULE DK5ZDC; (*$T-$N-$I- Leo 03-Nov-89. (c) KRONOS *)

IMPORT  cod: defCodes;          IMPORT       SYSTEM;
IMPORT  err: defErrors;         IMPORT   os: osKernel;
IMPORT  req: defRequest;        IMPORT   fs: osFiles;
IMPORT  env: tskEnv;            IMPORT  tim: Time;

CONST F0=0; F1=1; (* first and last FLOPPY     drives *)
      W0=4; W1=5; (* first and last WINCHESTER drives *)


TYPE
  bs      = BITSET;
  int     = INTEGER;
  ADDRESS = SYSTEM.ADDRESS;
  SPEC    = RECORD
              dmode  : BITSET;
              dsecs  : INTEGER; (* device size in secs *)
              ssc    : INTEGER; (* sector size code    *)
              secsize: INTEGER; (* sector len 2**ssc   *)
              cyls   : INTEGER; (* # cylinders         *)
              heads  : INTEGER; (* # heads             *)
              minsec : INTEGER; (* min sector # on trk *)
              maxsec : INTEGER; (* max sector # on trk *)
              ressec : INTEGER; (* reserved sectors    *)
              precomp: INTEGER; (* precompensation     *)
              rate   : INTEGER; (* heads stepping      *)
            END;

PROCEDURE move(to,from: ADDRESS; size: INTEGER); CODE cod.move END move;

VAR ports: ARRAY [0..6] OF SPEC;

CONST BASE = 80000h;
      HALF = 10000h;
      ok   = err.ok;
      cash =   0FFh;

VAR fdc0: POINTER TO BITSET;
    fdc1: POINTER TO BITSET;
    fdc2: POINTER TO BITSET;
     HEI: ADDRESS;
     DMA: ADDRESS;
  BUFFER: ADDRESS;
  F0000h: POINTER TO ARRAY [0..0FFFFh] OF CHAR;

CONST
  BUFSIZE = 12*1024;

  IGNORE=bs(00FFh);        RDTRK =bs(07FFh);
  RETRY =bs(01FFh);        WRTRK =bs(08FFh);
  ABORT =bs(02FFh);        FORMAT=bs(09FFh);
  RESET =bs(03FFh);        GDS   =bs(0AFFh);
  FLUSH =bs(04FFh);        PDS   =bs(0BFFh);
  READ  =bs(05FFh);        RDS   =bs(0CFFh);
  WRITE =bs(06FFh);        SEEK  =bs(0DFFh);
  RAWRD =bs(0EFFh);

PROCEDURE wait;
BEGIN
  WHILE (fdc0^*{0..7}+bs(HEI^))#{} DO os.delay(0) END
END wait;

PROCEDURE abort;
BEGIN
  REPEAT fdc0^:=ABORT;  HEI^:=5;  wait UNTIL fdc0^*{8..15}={}
END abort;

PROCEDURE flush;
BEGIN
  fdc0^:=FLUSH; HEI^:=5; wait;
  IF fdc0^*{8..15}#{} THEN abort END
END flush;

PROCEDURE errorw(): BITSET;  (* decode error for Winchester drive *)
 CONST mw_error = ARRAY OF INTEGER
               {err.io_error,   err.io_error, err.io_error,  err.io_error
               ,err.seek_err,   err.hw_fail,  err.not_ready, err.io_error
               ,err.miss_did,   err.seek_0,   err.ipted_op,  err.io_error
               ,err.miss_sid,   err.head_crc, err.data_crc,  err.bad_block};
  VAR i: INTEGER;
    e,r: BITSET;
BEGIN
  r:=(fdc0^>>8)*{0..7}+(fdc1^>>8)*{8..15};
  IF r*{6}#{} THEN
    i:=int((fdc0^>>16)*{0..7});
    IF (i>=0) & (i<=6) THEN ports[i].dmode:=ports[i].dmode-req.ready END
  END;
  e:=bs(err.io_error);
  FOR i:=0 TO 15 DO
    IF r*{0}#{} THEN e:=e+bs(mw_error[i]) END; r:=r>>1
  END;
  RETURN e
END errorw;

PROCEDURE errorf(): BITSET;  (* decode error for Floppy drive *)
 CONST fd_error = ARRAY OF INTEGER
               {err.io_error, err.seek_err,   err.inv_dma,   err.data_crc
               ,err.miss_sid, err.not_ready,  err.write_pro, err.io_error};
  VAR i: INTEGER;
    e,r: BITSET;
BEGIN
  r:=(fdc0^>>8)*{0..7};
  IF r*{5,6}#{} THEN   i:=int((fdc0^>>16)*{0..7});
    IF (i>=0) & (i<=6) THEN
      IF r*{5}#{} THEN ports[i].dmode:=ports[i].dmode-req.ready END;
      IF r*{6}#{} THEN ports[i].dmode:=ports[i].dmode-req.wpro  END
    END
  END;
  e:=bs(err.io_error);
  FOR i:=0 TO 6 DO
    IF r*{0}#{} THEN e:=e+bs(fd_error[i]) END; r:=r>>1
  END;
  RETURN e
END errorf;

PROCEDURE error(): INTEGER;
BEGIN
  IF int( (fdc0^>>16)*{0..7} )<4 THEN RETURN int(errorf())
  ELSE                                RETURN int(errorw())
  END
END error;

PROCEDURE exec(drn: INTEGER; VAR res: INTEGER);
  VAR sta: BITSET;
BEGIN
  wait;
  sta:=fdc0^;
  IF (sta>>16)*{0..7}#bs(drn) THEN res:=int(bs(res)+bs(err.hw_fail)) END;
  IF (sta>> 8)*{0..7}={}      THEN RETURN END;
  IF drn<4 THEN res:=int(bs(res)+errorf())
  ELSE          res:=int(bs(res)+errorw())
  END;
  REPEAT abort; os.delay(50) UNTIL fdc0^*{8..15}={};
  flush
END exec;

PROCEDURE _get_spec(drn: INTEGER; VAR res: INTEGER);
  VAR tag: BITSET;
BEGIN
  res:=ok;
  WITH ports[drn] DO
    DMA^ :=(BUFFER-BASE)*4;
    fdc1^:={};
    fdc0^:=GDS + bs(drn<<16);
    HEI^ :=5; exec(drn,res);
    IF res#ok THEN RETURN END;
    tag:=(fdc1^>>8)*{0..7};
    IF drn>=4 THEN dmode:=dmode+req.wint
    ELSE           dmode:=dmode+req.floppy;
      IF    tag*{7}#{} THEN cyls:=77;
        IF  tag*{6}#{} THEN heads:=1 ELSE heads:=2 END
      ELSIF tag*{5}#{} THEN cyls:=40  (* it isn't possible to determine *)
      ELSE                  cyls:=80  (* number of sides for 5.25"      *)
      END
    END;
    IF tag*{3}#{} THEN dmode:=dmode+req.wpro ELSE dmode:=dmode-req.wpro END;
    maxsec :=int( (fdc2^)*{0..7} );
    minsec :=int( (fdc2^>>8)*{0..7} );
    ssc    :=int( (fdc1^<<8)*{0..7} ) + 7;
    secsize:=1<<ssc;
    dsecs  :=(maxsec-minsec+1)*heads*cyls-ressec
  END;
  res:=ok
END _get_spec;

PROCEDURE _set_spec(drn: INTEGER; VAR res: INTEGER);
  VAR i: INTEGER;
    den: INTEGER;
    tag: BITSET;
BEGIN
  res:=ok;
  WITH ports[drn] DO
    DMA^ :=(BUFFER-BASE)*4;
    fdc1^:={};
    fdc0^:=GDS + bs(drn<<16);
    HEI^ :=5; exec(drn,res);
    IF res#ok THEN RETURN END;
    tag:=(fdc1^>>8)*{0..7};
    IF drn>=4 THEN     den:=000;     tag:={};  (* fixed parameters *)
      minsec:=0;    maxsec:=15;      ssc:=9;   secsize:=512;
      IF    cyls=697  THEN precomp:=0
      ELSIF cyls<=612 THEN precomp:=32
      ELSE                 precomp:=255
      END
    ELSE  den:=0FFh;   precomp:=0;
      IF cyls=40 THEN tag:=tag+{5} ELSE tag:=tag-{5} END;
      i:=(maxsec-minsec+1)<<ssc;
      IF (cyls=77) & (i>4096) OR (cyls#77) & (i>3000) THEN den:=0 END
    END;
    dsecs:=(maxsec-minsec+1)*heads*cyls;
    IF ressec>=dsecs THEN ressec:=0 ELSE DEC(dsecs,ressec) END;
    fdc2^:= bs(maxsec) + bs(minsec<<8) + bs(precomp<<16);
    fdc1^:= bs(den)    + bs(tag<<8)    + bs(0<<16)   + bs(ssc-7)>>8;
    fdc0^:= PDS        + bs(drn<<16)   + bs(0FFh>>8);
    HEI^ :=5;  exec(drn,res)
  END
END _set_spec;

PROCEDURE _readw(VAR r: req.REQUEST);
  VAR sec: INTEGER;   dma: ADDRESS;
      trk: INTEGER;   buf: ADDRESS;
      hed: INTEGER;   len: INTEGER;
     size: INTEGER;   tai: INTEGER;
BEGIN
  WITH ports[r.drn] DO
    len:=r.len;
    IF len<=0 THEN RETURN END;
    buf:=r.buf;
    sec:=r.ofs+ressec;
    trk:=sec DIV 16;    hed:=trk MOD heads;
    sec:=sec MOD 16;    trk:=trk DIV heads;
    LOOP
      IF buf>=BASE THEN dma:=buf ELSE dma:=BUFFER END;
      tai:=len;
      IF sec+tai>16 THEN tai:=16-sec END;
      size :=tai*128;
      DMA^ :=(dma-BASE)*4;
      fdc1^:=bs(trk DIV 256 + hed*16) + bs(sec<<8)    + bs(size<<18);
      fdc0^:=  READ  +  bs(r.drn<<16) + bs(trk MOD 256)>>8;
      HEI^ :=5;      exec(r.drn,r.res);
      IF r.res#ok    THEN RETURN END;
      IF buf<BASE    THEN move(buf,BUFFER,size) END;
      DEC(len,tai);
      IF len=0       THEN RETURN END;
      INC(sec,tai);  INC(buf,size);
      IF sec=16      THEN sec:=0; INC(hed);
        IF hed=heads THEN hed:=0; INC(trk) END
      END
    END
  END
END _readw;

PROCEDURE readw(VAR r: req.REQUEST);
  VAR i: INTEGER;
BEGIN
  i:=4;
  REPEAT r.res:=ok; _readw(r); DEC(i) UNTIL (r.res=ok) OR (i=0)
END readw;

PROCEDURE _writew(VAR r: req.REQUEST);
  VAR sec: INTEGER;   dma: ADDRESS;
      trk: INTEGER;   buf: ADDRESS;
      hed: INTEGER;   len: INTEGER;
     size: INTEGER;   tai: INTEGER;
BEGIN
  WITH ports[r.drn] DO
    len:=r.len;
    IF len<=0 THEN RETURN END;
    buf:=r.buf;
    sec:=r.ofs+ressec;
    trk:=sec DIV 16;    hed:=trk MOD heads;
    sec:=sec MOD 16;    trk:=trk DIV heads;
    LOOP
      tai:=len;
      IF sec+tai>16 THEN tai:=16-sec END;
      size :=tai*128;
      IF buf>=BASE THEN dma:=buf ELSE dma:=BUFFER; move(dma,buf,size) END;
      DMA^ :=(dma-BASE)*4;
      fdc1^:=bs(trk DIV 256 + hed*16) + bs(sec<<8)    + bs(size<<18);
      fdc0^:=WRITE  +   bs(r.drn<<16) + bs(trk MOD 256)>>8;
      HEI^ :=5;      exec(r.drn,r.res);
      IF r.res#ok    THEN RETURN END;
      DEC(len,tai);
      IF len=0       THEN RETURN END;
      INC(sec,tai);  INC(buf,size);
      IF sec=16      THEN sec:=0; INC(hed);
        IF hed=heads THEN hed:=0; INC(trk) END
      END
    END
  END
END _writew;

PROCEDURE writew(VAR r: req.REQUEST);
  VAR i: INTEGER;
BEGIN
  i:=4;
  REPEAT r.res:=ok; _writew(r); DEC(i) UNTIL (r.res=ok) OR (i=0)
END writew;

TYPE  WD_BUF = POINTER TO ARRAY [0..63] OF CHAR;

PROCEDURE prepare_w(buf: WD_BUF);
  CONST skew=3; minsec=0; maxsec=15;
  VAR i,s: INTEGER;
      ref: ARRAY [0..15] OF INTEGER;
BEGIN
  FOR i:=minsec TO maxsec DO ref[i]:=-1 END;
  s:=minsec;
  FOR i:=minsec TO maxsec DO
    WHILE ref[s]#-1 DO
      IF s<maxsec THEN s:=s+1 ELSE s:=minsec END;
    END;
    ref[s]:=i;
    s:=s+skew;
    IF s>maxsec THEN s:=s-(maxsec-minsec+1) END;
  END;
  i:=0;
  FOR s:=minsec TO maxsec DO
    buf^[i]:=0c; INC(i);  buf^[i]:=CHAR(ref[s]); INC(i)
  END;
  WHILE i<=HIGH(buf^) DO  buf^[i]:=377c; INC(i) END
END prepare_w;

PROCEDURE format_w(VAR r: req.REQUEST);
  CONST wMAGIC = 4D464457h;
  VAR buf: WD_BUF;
      trk: INTEGER;
      hed: INTEGER;
BEGIN
  IF (r.ofs<0) & (r.len<BYTES(buf^)+4) THEN
    r.len:=BYTES(buf^)+4; r.res:=err.not_enough; RETURN
  END;
  IF (r.buf=NIL) OR (r.len<BYTES(buf^)+4) THEN
    r.res:=err.bad_parm; RETURN
  END;
  buf:=r.buf+1;
  IF r.ofs<0 THEN
    prepare_w(buf); r.buf^:=wMAGIC; _set_spec(r.drn,r.res); RETURN
  END;
  IF r.buf^#wMAGIC THEN r.res:=err.bad_parm; RETURN END;
  WITH ports[r.drn] DO
    trk:=r.ofs DIV (maxsec-minsec+1);
    hed:=trk   MOD heads;
    trk:=trk   DIV heads
  END;
  move(BUFFER,buf,SIZE(buf^));
  DMA^ :=(BUFFER-BASE)*4;
  fdc1^:=bs(trk DIV 256 + hed*16) + bs(1<<8) + bs(BYTES(buf^)<<16);
  fdc0^:=FORMAT  +  bs(r.drn<<16) + bs(trk MOD 256)>>8;
  HEI^ :=5;  exec(r.drn,r.res)
END format_w;

PROCEDURE readf(VAR r: req.REQUEST);
  VAR sec: INTEGER;
      trk: INTEGER;   dma: ADDRESS;
      hed: INTEGER;   buf: ADDRESS;
      tai: INTEGER;   len: INTEGER;
     size: INTEGER;  secs: INTEGER;
BEGIN
  WITH ports[r.drn] DO
    len:=r.len;
    IF len<=0 THEN RETURN END;
    buf:=r.buf;
    sec:=r.ofs+ressec;  secs:=maxsec-minsec+1;
    trk:=sec DIV secs;    hed:=trk MOD heads;
    sec:=sec MOD secs;    trk:=trk DIV heads;
    LOOP
      IF buf>=BASE THEN dma:=buf ELSE dma:=BUFFER END;
      tai:=len;
      IF sec+tai>secs THEN tai:=secs-sec END;
      size :=tai<<(ssc-2);
      DMA^ :=(dma-BASE)*4;
      fdc1^:=bs(hed*16) + bs(sec+minsec)<<8  + bs(size<<18);
      fdc0^:= READ      + bs(r.drn<<16) + bs(trk MOD 256)>>8;
      HEI^ :=5;      exec(r.drn,r.res);
      IF r.res#ok    THEN RETURN END;
      IF buf<BASE    THEN move(buf,BUFFER,size) END;
      DEC(len,tai);
      IF len=0       THEN RETURN END;
      INC(sec,tai);  INC(buf,size);
      IF sec=secs    THEN sec:=0; INC(hed);
        IF hed=heads THEN hed:=0; INC(trk) END
      END
    END
  END
END readf;

PROCEDURE writef(VAR r: req.REQUEST);
  VAR sec: INTEGER;
      trk: INTEGER;   dma: ADDRESS;
      hed: INTEGER;   buf: ADDRESS;
      tai: INTEGER;   len: INTEGER;
     size: INTEGER;  secs: INTEGER;
BEGIN
  WITH ports[r.drn] DO
    len:=r.len;
    IF len<=0 THEN RETURN END;
    buf:=r.buf;
    sec:=r.ofs+ressec; secs:=maxsec-minsec+1;
    trk:=sec DIV secs;   hed:=trk MOD heads;
    sec:=sec MOD secs;   trk:=trk DIV heads;
    LOOP
      tai:=len;
      IF sec+tai>secs THEN tai:=secs-sec END;
      size :=tai<<(ssc-2);
      IF buf>=BASE THEN dma:=buf ELSE dma:=BUFFER; move(dma,buf,size) END;
      DMA^ :=(dma-BASE)*4;
      fdc1^:=bs(hed*16) + bs(sec+minsec)<<8  + bs(size<<18);
      fdc0^:= WRITE     + bs(r.drn<<16) + bs(trk MOD 256)>>8;
      HEI^:=5;        exec(r.drn,r.res);
      IF r.res#ok     THEN RETURN END;
      DEC(len,tai);
      IF len=0        THEN RETURN END;
      INC(sec,tai);   INC(buf,size);
      IF sec=secs     THEN sec:=0; INC(hed);
        IF hed=heads  THEN hed:=0; INC(trk) END
      END
    END
  END
END writef;

CONST fMAGIC=4D464446h;

TYPE FD_BUF = POINTER TO ARRAY [0..BUFSIZE-1] OF CHAR;
     FD_SID = POINTER TO ARRAY [0..31] OF INTEGER;

PROCEDURE prepare_f(VAR r: req.REQUEST; id: FD_SID; buf: FD_BUF);

  PROCEDURE set_gaps(single: BOOLEAN; VAR gap1,gap3: INTEGER);
  BEGIN
    WITH ports[r.drn] DO
      IF single THEN
        IF cyls=77 THEN (* 8" floppy *)
          CASE ssc OF
            |7: gap3:=27  |8: gap3:=42  |9: gap3:=50  ELSE gap3:=80
          END
        ELSE (* 5.25" floppy *)
          gap3:=10;
          IF ssc<=8 THEN gap1:=40 ELSE gap1:=20 END
        END
      ELSE (* double density *)
        IF cyls=77 THEN (* 8" floppy *)
          CASE ssc OF
            |7: gap3:=34  |10: gap3:=116  ELSE gap3:=54
          END
        ELSE (* 5.25" floppy *)
          gap1:=32; gap3:=32; -- default for 9x512, 128, 256
          IF ssc=9 THEN
            CASE maxsec OF |8: gap1:=60  |10: gap3:=30  ELSE END
          ELSIF ssc=10 THEN  gap1:=48;      gap3:=40
          END
        END
      END
    END
  END set_gaps;

  VAR p: INTEGER;     single: BOOLEAN;

  PROCEDURE app(val: INTEGER; count: INTEGER);
  BEGIN
    WHILE count>0 DO buf^[p]:=CHAR(val); INC(p); DEC(count) END;
  END app;

  PROCEDURE prepare_DD(cyls,minsec,maxsec,ssc,gap3: INTEGER);
    VAR sector: INTEGER;
  BEGIN
    FOR sector:=minsec TO maxsec DO
      IF cyls#77 THEN app(000h,10) ELSE app(000h,12) END;
      app(0F5h,3); app(0FEh,1); -- id address mark
      id^[sector]:=p;   -- address of id field
      app(000h,1); app(000h,1); app(sector,1); app(ssc-7,1);
      --==track========side=========sector=========size==--
      app(0F7h,1); -- 2 CRCs

      app(04Eh,22); app(000h,12); -- GAP
      app(0F5h, 3); app(0FBh, 1); -- data address mark
  (*  data[sector]:=p;  -- address of data field  *)
      app(0E5h,1<<ssc); app(0F7h,1);
      --==data==============2 CRCs====--
      app(04Eh,gap3);
    END;
    app(04Eh,BYTES(buf^)-p-1)
  END prepare_DD;

  PROCEDURE prepare_SD(minsec,maxsec,ssc,gap3: INTEGER);
    VAR sector: INTEGER;
  BEGIN
    FOR sector:=minsec TO maxsec DO
      app(000h, 6);    app(0FEh, 1); -- id address mark
      id^[sector]:=p;  -- address of id field
      app(000h,1);     app(000h,1);  app(sector,1);  app(ssc-7,1);
      --==track============side==========sector==========size==--
      app(0F7h,1);     -- 2 CRCs
      app(0FFh,11);    app(000h, 6);   -- GAP
      app(0FBh, 1);    -- data address mark
  (*  data[sector]:=p; -- address of data field  *)
      app(0E5h,1<<ssc); app(0F7h,1);
      --==data==============2 CRCs====--
      app(0FFh,gap3);
    END;
    app(0FFh,BYTES(buf^)-p-1)
  END prepare_SD;

  VAR i,gap1,gap3: INTEGER;

BEGIN
  WITH ports[r.drn] DO
    i:=(maxsec-minsec+1)*secsize;
    single:=NOT ((cyls=77) & (i>4096) OR (cyls#77) & (i>3000));
    set_gaps(single,gap1,gap3);
    p:=0;
    IF single THEN
      IF cyls#77 THEN    app(0FFh,gap1)
      ELSE app(0FFh,40); app(000h,6); app(0FCh,1);  app(0FFh,26)
      END;
      prepare_SD(minsec,maxsec,ssc,gap3)
    ELSE (* double density *)
      IF cyls#77 THEN    app(04Eh,gap1)
      ELSE app(04Eh,80); app(000h,12); app(0F6h,3); app(0FCh,1); app(04Eh,50)
      END;
      prepare_DD(cyls,minsec,maxsec,ssc,gap3)
    END
  END
END prepare_f;

PROCEDURE format_f(VAR r: req.REQUEST);
  VAR id: FD_SID;
     buf: FD_BUF;
     i,p: INTEGER;
     trk: INTEGER;
     hed: INTEGER;
BEGIN
  IF (r.ofs<0) & (r.len<BYTES(buf^)+BYTES(id^)+4) THEN
    r.len:=BYTES(buf^)+BYTES(id^)+4; r.res:=err.not_enough; RETURN
  END;
  IF (r.buf=NIL) OR (r.len<BYTES(buf^)+BYTES(id^)+4) THEN
    r.res:=err.bad_parm; RETURN
  END;
  id :=r.buf+1;
  buf:=r.buf+1+SIZE(id^);
  IF r.ofs<0       THEN
    prepare_f(r,id,buf); r.buf^:=fMAGIC; _set_spec(r.drn,r.res); RETURN
  END;
  IF r.buf^#fMAGIC THEN r.res:=err.bad_parm; RETURN END;
  WITH ports[r.drn] DO
    trk:=r.ofs DIV (maxsec-minsec+1);
    hed:=trk   MOD heads;
    trk:=trk   DIV heads;
    FOR i:=minsec TO maxsec DO
      p:=id^[i];
      buf^[p]:=CHAR(trk); buf^[p+1]:=CHAR(hed); buf^[p+2]:=CHAR(i)
    END
  END;
  move(BUFFER,buf,SIZE(buf^));
  DMA^ :=(BUFFER-BASE)*4;
  fdc1^:=bs(hed*16) + bs(1<<8)      + bs(BYTES(buf^)<<16);
  fdc0^:= FORMAT    + bs(r.drn<<16) + bs(trk MOD 256)>>8;
  HEI^ :=5;  exec(r.drn,r.res)
END format_f;

PROCEDURE seek(VAR r: req.REQUEST);
  VAR trk: INTEGER;
BEGIN
  WITH ports[r.drn] DO trk:=r.ofs DIV ((maxsec-minsec+1)*heads) END;
  DMA^ :=(BUFFER-BASE)*4;
  fdc1^:=bs(trk DIV 256);
  fdc0^:=SEEK + bs(r.drn<<16) + bs(trk MOD 256)>>8;
  HEI^ :=5;  exec(r.drn,r.res)
END seek;

PROCEDURE park(VAR r: req.REQUEST);
  VAR trk: INTEGER;
BEGIN
  trk  :=ports[r.drn].cyls+19;
  IF trk>1023 THEN trk:=1023 END;
  DMA^ :=(BUFFER-BASE)*4;
  fdc1^:=bs(trk DIV 256);
  fdc0^:= SEEK + bs(r.drn<<16) + bs(trk MOD 256)>>8;
  HEI^ :=5;  exec(r.drn,r.res)
END park;

PROCEDURE mount(VAR r: req.REQUEST);

  VAR ign: INTEGER;
      ptr: POINTER TO ARRAY [0..3Fh] OF CHAR;
      sav: SPEC;
      bad: BOOLEAN;
      sec: INTEGER;

  PROCEDURE unpack(VAL s: ARRAY OF CHAR; p,l: INTEGER): INTEGER;
    VAR i,c: INTEGER;
  BEGIN
    i:=0;
    FOR p:=p TO p+l-1 DO
      c:=ORD(s[p]);
      IF    (ORD("0")<=c) & (c<=ORD("9")) THEN i:=i<<4+(c-ORD("0"))
      ELSIF (ORD("A")<=c) & (c<=ORD("F")) THEN i:=i<<4+(c-ORD("A")+10)
      ELSE bad:=TRUE
      END
    END;
    RETURN i
  END unpack;

BEGIN
  WITH ports[r.drn] DO
    DMA^ :=(BUFFER-BASE)*4;
    fdc1^:=bs(ORD(r.drn<4)<<8 ) + bs(40h<<16);
    fdc0^:=READ + bs(r.drn<<16);
    HEI^ :=5;   exec(r.drn,r.res);
    IF r.res#ok THEN
      DMA^ :=(BUFFER-BASE)*4;
      fdc1^:=bs(ORD(r.drn<4)<<8 ) + bs(40h<<16);
      fdc0^:=READ + bs(r.drn<<16);
      HEI^ :=5;   exec(r.drn,r.res);
    END;
    IF r.res#ok THEN r.res:=ok; RETURN END;
    ptr:=BUFFER;
    IF (ptr^[8]="x") & (ptr^[9]="2") & (ptr^[10]="d") THEN
      sec    :=ORD(ptr^[11]);
      cyls   :=ORD(ptr^[12])+ORD(ptr^[13])*256;
      heads  :=ORD(ptr^[14]) MOD 16;
      ssc    :=ORD(ptr^[14]) DIV 16;
      ressec :=ORD(ptr^[15]);
      IF cyls>80 THEN minsec:=0; maxsec:=sec-1 ELSE minsec:=1; maxsec:=sec END
      (* ptr^[16..19] = time *)
    ELSIF (ptr^[8+0]='X') & (ptr^[8+1]='D') & (ptr^[8+2]='0') THEN
      cyls  :=ORD(ptr^[8+4])+ORD(ptr^[8+5])*256;
      minsec:=ORD(ptr^[8+6]);      heads:=ORD(ptr^[8+8]);
      maxsec:=ORD(ptr^[8+7]);      ssc  :=ORD(ptr^[8+9]);
      ressec:=ORD(ptr^[8+10])
    ELSIF (ptr^[35h]='x') & (ptr^[36h]='d') THEN
      sav  :=ports[r.drn];
      bad  :=FALSE;
      cyls :=unpack(ptr^,37h,3);
      sec  :=unpack(ptr^,3Ah,2);
      heads:=unpack(ptr^,3Ch,1);
      ssc  :=unpack(ptr^,3Dh,1);      ressec:=unpack(ptr^,3Eh,2);
      IF bad THEN ports[r.drn]:=sav; _set_spec(r.drn,ign); RETURN END;
      IF cyls>80 THEN minsec:=0; maxsec:=sec-1 ELSE minsec:=1; maxsec:=sec END
    ELSE
      RETURN
    END;
    secsize:=1<<ssc;
    _set_spec(r.drn,r.res); r.res:=ok
  END
END mount;

PROCEDURE get_spec(VAR r: req.REQUEST);
BEGIN
  WITH ports[r.drn] DO
    r.dmode  :=dmode;
    r.dsecs  :=dsecs;
    r.ssc    :=ssc;
    r.secsize:=secsize;
    r.cyls   :=cyls;
    r.heads  :=heads;
    r.minsec :=minsec;
    r.maxsec :=maxsec;
    r.ressec :=ressec;
    r.rate   :=rate;
    r.precomp:=precomp;
    IF r.drn>=4 THEN r.precomp:=precomp*4 END
  END
END get_spec;

PROCEDURE set_spec(VAR r: req.REQUEST);
BEGIN
  _get_spec(r.drn,r.res); r.res:=ok;
  WITH ports[r.drn] DO
    dmode  :=r.dmode;
    dsecs  :=r.dsecs;
    ssc    :=r.ssc;
    secsize:=r.secsize;
    cyls   :=r.cyls;
    heads  :=r.heads;
    minsec :=r.minsec;
    maxsec :=r.maxsec;
    ressec :=r.ressec;
    rate   :=r.rate;
    precomp:=r.precomp;
    IF r.drn>=4 THEN precomp:=precomp DIV 4 END
  END;
  _set_spec(r.drn,r.res)
END set_spec;

VAR inited: BOOLEAN;

PROCEDURE init;
  VAR i,ign: INTEGER; adr: ADDRESS;
BEGIN
  inited:=TRUE;
  F0000h:=ADDRESS( 0F0000h DIV 4 + BASE );
  BUFFER:=ADDRESS( 0D4000h DIV 4 + BASE );
  HEI   :=ADDRESS( 0D0000h DIV 4 + BASE );
  adr :=BASE+(0F8010h+1*4) DIV 4; (* int(adr^) DIV HALF MOD 256 M.B.=4 *)
  adr :=int(adr^) MOD HALF;
  fdc0:=ADDRESS( F0000h ) + adr DIV 4;
  fdc1:=ADDRESS( fdc0 ) + 1;
  fdc2:=ADDRESS( fdc0 ) + 2;
  DMA :=ADDRESS( fdc2 );

  REPEAT UNTIL HEI^=0;
  abort; flush;
  FOR i:=W0 TO W1 DO
    WITH ports[i] DO
      dmode :=req.ready+req.wint+req.fmttrk;
      maxsec:= 15;      minsec :=0;
      heads :=  4;      ressec:=(maxsec-minsec+1);
      cyls  :=612;      secsize:=1<<ssc;
      ssc   :=  9;      dsecs  :=(maxsec-minsec+1)*heads*cyls-ressec;
      rate  :=  0;      precomp:=0;
      _set_spec(i,ign);
      fdc1^:={};
      fdc0^:=SEEK + bs(i<<16);
      HEI^ :=5;   exec(i,ign)
    END
  END;
  FOR i:=F0 TO F1 DO
    WITH ports[i] DO
      dmode:=req.ready+req.floppy+req.fmttrk;
      heads:=2;          ressec :=0;
      _get_spec(i,ign);  minsec :=1;
      ssc  :=10;         secsize:=1<<ssc;
      rate :=0;          precomp:=0;
      IF cyls=77 THEN maxsec:=8 ELSE cyls:=80; maxsec:=5 END;
      dsecs:=(maxsec-minsec+1)*heads*cyls-ressec;
      _set_spec(i,ign)
    END
  END;
END init;

PROCEDURE doiof(VAR r: req.REQUEST);
  VAR try: INTEGER;
BEGIN
  IF NOT inited THEN init END;
  try:=2;
  WITH ports[r.drn] DO
   REPEAT
    dmode:=dmode+(req.ready+req.fmttrk+req.floppy);
    r.res:=ok;
    CASE r.op OF
    |req.MOUNT    : mount(r)
    |req.UNMOUNT  :
    |req.READ     : readf(r)
    |req.WRITE    : writef(r)
    |req.POWER_OFF:
    |req.FORMAT   : format_f(r); RETURN
    |req.SET_SPEC : set_spec(r)
    |req.GET_SPEC : get_spec(r)
    |req.SEEK     : seek(r)
    ELSE
      r.res:=err.inv_op; RETURN
    END;
    IF r.res=ok           THEN RETURN END;
    IF dmode*req.ready={} THEN RETURN END;
    IF bs(r.res)*bs(err.hw_fail)#bs(err.hw_fail) THEN RETURN END;
    abort;
    try:=try-1
   UNTIL try=0
  END
END doiof;

PROCEDURE doiow(VAR r: req.REQUEST);
  VAR try: INTEGER;
BEGIN
  IF NOT inited THEN init END;
  try:=3;
  WITH ports[r.drn] DO
   REPEAT
    dmode:=dmode+(req.ready+req.fmttrk+req.wint);
    r.res:=ok;
    CASE r.op OF
    |req.MOUNT    : mount(r)
    |req.UNMOUNT  :
    |req.READ     : readw(r)
    |req.WRITE    : writew(r)
    |req.POWER_OFF: park(r);     RETURN
    |req.FORMAT   : format_w(r); RETURN
    |req.SET_SPEC : set_spec(r)
    |req.GET_SPEC : get_spec(r)
    |req.SEEK     : seek(r)
    ELSE
      r.res:=err.inv_op; RETURN
    END;
    IF r.res=ok           THEN RETURN END;
    IF dmode*req.ready={} THEN RETURN END;
    abort;
    try:=try-1
   UNTIL try=0
  END
END doiow;

PROCEDURE read_rt_clock;

  CONST BASE=80000h; RT_CLOCK = 4;

  VAR adr: SYSTEM.ADDRESS;
     time: INTEGER;  time0: INTEGER;
      sec: INTEGER;    day: INTEGER;
      min: INTEGER;  month: INTEGER;
     hour: INTEGER;   year: INTEGER;
        b: ARRAY [0..11] OF CHAR;

  PROCEDURE bcd(VAR int: INTEGER; byte: CHAR);
  BEGIN int:=(ORD(byte) DIV 16) * 10 + (ORD(byte) MOD 16) END bcd;

BEGIN
  adr:=BASE+(0F8010h+RT_CLOCK*4) DIV 4;
  adr:=INTEGER(adr^) MOD 10000h;
  adr:=BASE + 0F0000h DIV 4 + adr DIV 4;
  time0:=-1;
  LOOP
    move(SYSTEM.ADR(b),adr,3);
    bcd( sec,b[3]);         bcd(  day,b[7]);
    bcd( min,b[4]);         bcd(month,b[8]);
    bcd(hour,b[5]);         bcd( year,b[9]);
    time:=tim.pack(year+1978,month,day,hour,min,sec);
    IF (time#-1) & (time=time0) THEN EXIT END;
    time0:=time
  END;
  tim.set_time(time)
END read_rt_clock;

PROCEDURE insert;
  VAR i,r: INTEGER;  main,name: ARRAY [0..3] OF CHAR;
BEGIN
  main:="";  r:=ok;
  FOR i:=0 TO HIGH(ports) DO
    IF    i IN {F0..F1} THEN
      name:="fd0"; name[2]:=CHAR(ORD("0")+i);
      IF main="" THEN main:=name END;
      r:=fs.define_driver(name,main,i,fs.disk,doiof)
    ELSIF i IN {W0..W1} THEN
      name:="wd0"; name[2]:=CHAR(ORD("0")+i-4);
      IF main="" THEN main:=name END;
      r:=fs.define_driver(name,main,i,fs.disk,doiow)
    END;
    IF r#ok THEN HALT(r) END
  END
END insert;

BEGIN
  inited:=FALSE;
  read_rt_clock;
  insert;
  env.become_ipr;
  os.suspend(os.active(),-1)
END DK5ZDC.
