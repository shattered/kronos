IMPLEMENTATION MODULE PCX; (* John 07-Dec-90. (c) KRONOS *)

IMPORT       SYSTEM;
IMPORT       defBMG;
IMPORT  err: defErrors;
IMPORT       BIO;
IMPORT  mem: Heap;

IMPORT  tty: Terminal; -- FOR debug purposes

WITH STORAGE (NEW    : mem.ALLOCATE;
              DISPOSE: mem.DEALLOCATE;
              RESIZE : mem.REALLOCATE);

TYPE ADDRESS=SYSTEM.ADDRESS;

CONST swap = ARRAY OF CHAR
    { 000c, 200c, 100c, 300c, 040c, 240c, 140c, 340c, 020c, 220c,
120c, 320c, 060c, 260c, 160c, 360c, 010c, 210c, 110c, 310c, 050c,
250c, 150c, 350c, 030c, 230c, 130c, 330c, 070c, 270c, 170c, 370c,
004c, 204c, 104c, 304c, 044c, 244c, 144c, 344c, 024c, 224c, 124c,
324c, 064c, 264c, 164c, 364c, 014c, 214c, 114c, 314c, 054c, 254c,
154c, 354c, 034c, 234c, 134c, 334c, 074c, 274c, 174c, 374c, 002c,
202c, 102c, 302c, 042c, 242c, 142c, 342c, 022c, 222c, 122c, 322c,
062c, 262c, 162c, 362c, 012c, 212c, 112c, 312c, 052c, 252c, 152c,
352c, 032c, 232c, 132c, 332c, 072c, 272c, 172c, 372c, 006c, 206c,
106c, 306c, 046c, 246c, 146c, 346c, 026c, 226c, 126c, 326c, 066c,
266c, 166c, 366c, 016c, 216c, 116c, 316c, 056c, 256c, 156c, 356c,
036c, 236c, 136c, 336c, 076c, 276c, 176c, 376c, 001c, 201c, 101c,
301c, 041c, 241c, 141c, 341c, 021c, 221c, 121c, 321c, 061c, 261c,
161c, 361c, 011c, 211c, 111c, 311c, 051c, 251c, 151c, 351c, 031c,
231c, 131c, 331c, 071c, 271c, 171c, 371c, 005c, 205c, 105c, 305c,
045c, 245c, 145c, 345c, 025c, 225c, 125c, 325c, 065c, 265c, 165c,
365c, 015c, 215c, 115c, 315c, 055c, 255c, 155c, 355c, 035c, 235c,
135c, 335c, 075c, 275c, 175c, 375c, 003c, 203c, 103c, 303c, 043c,
243c, 143c, 343c, 023c, 223c, 123c, 323c, 063c, 263c, 163c, 363c,
013c, 213c, 113c, 313c, 053c, 253c, 153c, 353c, 033c, 233c, 133c,
333c, 073c, 273c, 173c, 373c, 007c, 207c, 107c, 307c, 047c, 247c,
147c, 347c, 027c, 227c, 127c, 327c, 067c, 267c, 167c, 367c, 017c,
217c, 117c, 317c, 057c, 257c, 157c, 357c, 037c, 237c, 137c, 337c,
077c, 277c, 177c, 377c};

PROCEDURE min(a,b: INTEGER): INTEGER;
BEGIN IF a<b THEN RETURN a ELSE RETURN b END;
END min;

PROCEDURE check(VAL h: HEADER);

  PROCEDURE e(SEQ r: INTEGER);
    VAR i: INTEGER;
  BEGIN
    tty.print('******* ERROR ');
    FOR i:=0 TO HIGH(r) DO tty.print('%d ',r[i]) END;
    tty.print('*******\n');
    error:=err.inconsistency;
  END e;

BEGIN
  done:=FALSE;
  WITH h DO
    IF (vers#2) & (vers#3) & (vers#5)    THEN e(00); RETURN END;
    IF (encod#0) & (encod#1)             THEN e(01); RETURN END;
    IF (bitpx#1) & (bitpx#2) &
       (bitpx#4) & (bitpx#8)             THEN e(02,bitpx); RETURN END;
    IF (x1<0) OR (x1>0FFFFh)             THEN e(03); RETURN END;
    IF (x2<0) OR (x2>0FFFFh)             THEN e(04); RETURN END;
    IF (y1<0) OR (y1>0FFFFh)             THEN e(05); RETURN END;
    IF (y2<0) OR (y2>0FFFFh)             THEN e(06); RETURN END;
    IF x1>x2                             THEN e(07); RETURN END;
    IF y1>y2                             THEN e(08); RETURN END;
    IF (hres<0) OR (hres>0FFFFh)         THEN e(09); RETURN END;
    IF (vres<0) OR (vres>0FFFFh)         THEN e(10); RETURN END;
    IF (nplanes>8) OR (nplanes<1)        THEN e(11); RETURN END;
    IF (bplin<1) OR (bplin>0FFFFh)       THEN e(12); RETURN END;
    IF (palinfo<0) OR (palinfo>0FFFFh)   THEN e(13); RETURN END;
    IF (shres<0) OR (shres>0FFFFh)       THEN e(14); RETURN END;
    IF (svres<0) OR (svres>0FFFFh)       THEN e(15); RETURN END;
  (* This condition check faults for Microsoft
   * Windows 3.0 Paint Brush output, so excluded 25-Jun-91 Leo, John, Nick
   *IF (((x2-x1+1)*bitpx+7) DIV 8#bplin) THEN e(16,bitpx,bplin,x1,x2);
   *                                                 RETURN END;
   *)
  END;
  done:=TRUE;
  error:=err.ok;
END check;

PROCEDURE word(VAL s: ARRAY OF CHAR; ofs: INTEGER): INTEGER;
BEGIN
(*$<$T-*)
 RETURN ORD(s[ofs]) + ORD(s[ofs+1])*100h
(*$>*)
END word;

PROCEDURE get_header(f: BIO.FILE; VAR h: HEADER): INTEGER;
  VAR buf: ARRAY [0..127] OF CHAR; i: INTEGER;
BEGIN
  done:=FALSE;
  BIO.get(f,buf,BYTES(buf));
  IF NOT BIO.done THEN error:=BIO.error; RETURN MAX(INTEGER) END;
  WITH h DO
    vers   := ORD(buf[1]);
    encod  := ORD(buf[2]);
    bitpx  := ORD(buf[3]);
    x1     := word(buf,4);
    y1     := word(buf,6);
    x2     := word(buf,8);
    y2     := word(buf,10);
    hres   := word(buf,12);
    vres   := word(buf,14);
    FOR i:=0 TO HIGH(clrma) DO clrma[i]:=buf[16+i] END;
    vmode  := ORD(buf[64]);
    nplanes:= ORD(buf[65]);
    bplin  := word(buf,66);
    palinfo:= word(buf,68);
    shres  := word(buf,70);
    svres  := word(buf,72);
    FOR i:=0 TO HIGH(xtra) DO xtra[i]:=buf[74+i] END;
    IF buf[0]#12c THEN
      error:=err.inconsistency;
      RETURN MAX(INTEGER)
    END;
    check(h);
    RETURN ((bplin+3) DIV 4)*(y2-y1+1)*nplanes
  END
END get_header;

PROCEDURE unpack(f: BIO.FILE; VAL h: HEADER; a: ADDRESS);
  VAR buf  : ARRAY [0..8191] OF CHAR;
      f_pos: INTEGER;
      pos  : INTEGER;

  PROCEDURE get(): CHAR;
  BEGIN
    IF pos>HIGH(buf) THEN
      BIO.get(f,buf,min(BYTES(buf),BIO.eof(f)-BIO.pos(f)));
      f_pos:=BIO.pos(f);
      pos:=0
    END;
    INC(pos);
    RETURN buf[pos-1]
  END get;

VAR line   : STRING;
    W,H,WPL: INTEGER;
    lay_sz : INTEGER;
    col,lin: INTEGER;
    cou,lay: INTEGER;
    last,i : INTEGER;
    val: CHAR;

BEGIN
  done:=FALSE;
  IF (a>=NIL) OR (a<1000h) THEN error:=err.bad_parm; RETURN END;
  pos:=HIGH(buf)+1;
  done:=TRUE;
  f_pos:=BIO.pos(f);
  IF NOT BIO.done THEN done:=FALSE; error:=BIO.error; RETURN END;
  WITH h DO
    W:=x2-x1+1;
    H:=y2-y1+1;
    WPL:=(bplin+3) DIV 4;
    IF encod#1 THEN
      BIO.read(f,a,WPL*H*4*nplanes);
      done:=BIO.done;
      error:=BIO.error;
      RETURN
    END;
    cou:=0;
    val:=0c;
(*$U+*)
    line^.HIGH:=bplin;
    FOR lin:=0 TO H - 1 DO
      line^.ADR:=a + WPL*lin;
      FOR lay:=0 TO nplanes-1 DO
        col:=0;
        REPEAT
          IF cou=0 THEN
            val:=get();
            IF BITSET(val)*{6..7}={6..7} THEN
              cou:=ORD(val) MOD 64; val:=get()
            ELSE
              cou:=1
            END;
            IF NOT BIO.done THEN done:=FALSE; error:=BIO.error; RETURN END;
            val:=swap[ORD(val)];
          END;
          IF cou#0 THEN
            last:=min(col+cou,bplin);
            DEC(cou,(last-col));
            FOR i:=col TO last-1 DO line[i]:=val END;
            col:=last;
          END;
        UNTIL col=bplin;
        line^.ADR:=line^.ADR + lay_sz;
(*$U-*)
      END
    END;
    BIO.seek(f,0,f_pos+pos);
  END;
  done:=TRUE;
  error:=err.ok;
END unpack;

PROCEDURE put_header(f: BIO.FILE; VAL h: HEADER);
  VAR buf: ARRAY [0..127] OF CHAR; i: INTEGER;

  PROCEDURE byte(val,pos: INTEGER);
  BEGIN
    buf[pos]:=CHAR(val);
  END byte;

  PROCEDURE word(val,pos: INTEGER);
  BEGIN
    buf[pos  ]:=CHAR(val MOD 100h);
    buf[pos+1]:=CHAR(val DIV 100h);
  END word;

BEGIN
  check(h);
  IF NOT done THEN error:=err.bad_parm; RETURN END;
  WITH h DO
    byte(0Ah,0);
    byte(vers,1);
    byte(encod,2);
    byte(bitpx,3);
    word(x1,4);
    word(y1,6);
    word(x2,8);
    word(y2,10);
    word(hres,12);
    word(vres,14);
    FOR i:=0 TO HIGH(clrma) DO buf[i+16]:=clrma[i] END;
    byte(vmode,64);
    byte(nplanes,65);
    word(bplin,66);
    word(palinfo,68);
    word(shres,70);
    word(svres,72);
    FOR i:=0 TO HIGH(xtra) DO buf[i+74]:=xtra[i] END
  END;
  BIO.put(f,buf,BYTES(buf));
  done:=BIO.done;
  error:=BIO.error;
END put_header;

PROCEDURE pack(f: BIO.FILE; VAL h: HEADER; a: ADDRESS);
  VAR buf: ARRAY [0..8191] OF CHAR; pos: INTEGER;

PROCEDURE put(xx: SYSTEM.WORD);
BEGIN
  IF pos>HIGH(buf) THEN
    BIO.put(f,buf,BYTES(buf));
    pos:=0;
  END;
  buf[pos]:=CHAR(xx); INC(pos);
END put;

VAR line   : STRING;
    H,WPL  : INTEGER;
    col,lin: INTEGER;
    cou,lay: INTEGER;
    i,last : INTEGER;
    val,c  : CHAR;
    lay_sz : INTEGER;

BEGIN
  check(h);            IF NOT done THEN RETURN END;
  pos :=0;
  WITH h DO
    H:=y2-y1+1;
    WPL:=(bplin+3) DIV 4;
    lay_sz:=WPL*H;
    cou:=0;
    val:=0c;
    FOR lin:=0 TO H-1 DO
(*$U+$T-*)
      line^.ADR:=a + lin*WPL;
      FOR lay:=0 TO nplanes-1 DO
        col:=0;
        REPEAT
          c:=swap[ORD(line[col])]; INC(col);
          IF c=val THEN
            INC(cou);
            IF cou=63 THEN put(0FFh); put(val); cou:=0 END
          ELSE
            IF cou#0 THEN
              IF (cou=1) & (BITSET(val)*{6..7}#{6..7}) THEN
                put(val)
              ELSE
                put(0C0h+cou); put(val)
              END
            END;
            val:=c;
            cou:=1;
          END;
          IF NOT BIO.done THEN done:=FALSE; error:=BIO.error; RETURN END
        UNTIL col>=bplin;
        line^.ADR:=line^.ADR + lay_sz;
      END
(*$U+$T-*)
    END;
    IF cou#0 THEN put(0C0h+cou); put(val) END;
  END;
  BIO.put(f,buf,pos);
  done:=BIO.done;
  error:=BIO.error;
END pack;

PROCEDURE unpackBMP(f: BIO.FILE; VAL h: HEADER; b: defBMG.BITMAP);
  VAR buf  : ARRAY [0..8191] OF CHAR;
      f_pos: INTEGER;
      pos  : INTEGER;

  PROCEDURE get(): CHAR;
  BEGIN
    IF pos>HIGH(buf) THEN
      f_pos:=BIO.pos(f);
      BIO.get(f,buf,min(BYTES(buf),BIO.eof(f)-BIO.pos(f)));
      pos:=0
    END;
    INC(pos);
    RETURN buf[pos-1]
  END get;

VAR line    : STRING;
    offs    : INTEGER;
    lay_sz  : INTEGER;
    col,lin : INTEGER;
    cou,lay : INTEGER;
    last,i  : INTEGER;
    val     : CHAR;


BEGIN
  done:=FALSE;
  pos:=HIGH(buf)+1;
  done:=TRUE;
  WITH h DO
    IF encod#1 THEN
      FOR lay:=0 TO HIGH(b^.layers) DO
        IF b^.layers[lay]#NIL THEN
          BIO.read(f,b^.layers[lay],b^.WPL*b^.H*4);
          IF NOT BIO.done THEN done:=BIO.done; error:=BIO.error END
        END
      END;
      RETURN
    END;
    cou:=0;
    val:=0c;
(*$<$U+*)
    line^.HIGH:=bplin-1;
(*$>*)
    FOR lin:=0 TO b^.H - 1 DO
      offs:=lin*b^.WPL;
      FOR lay:=0 TO HIGH(b^.layers) DO
        IF b^.layers[lay]#NIL THEN
(*$<$U+*)
          line^.ADR:=b^.layers[lay] + offs;
(*$>*)
          col:=0;
          REPEAT
            IF cou=0 THEN
              val:=get();
              IF BITSET(val)*{6..7}={6..7} THEN
                cou:=ORD(val) MOD 64; val:=get()
              ELSE
                cou:=1
              END;
              IF NOT BIO.done THEN done:=FALSE; error:=BIO.error; RETURN END;
              val:=swap[ORD(val)];
            END;
            IF cou#0 THEN
              REPEAT
                line[col]:=val;
                INC(col);
                DEC(cou);
              UNTIL (cou=0) OR (col=bplin)
            END
          UNTIL col=bplin
        END
      END
    END;
    BIO.seek(f,0,f_pos+pos);
  END;
  done:=TRUE;
  error:=err.ok;
END unpackBMP;

PROCEDURE packBMP(f: BIO.FILE; VAL h: HEADER; b: defBMG.BITMAP);
  VAR buf: ARRAY [0..8191] OF CHAR; pos: INTEGER;

PROCEDURE put(xx: SYSTEM.WORD);
BEGIN
  IF pos>HIGH(buf) THEN
    BIO.put(f,buf,BYTES(buf));
    pos:=0;
  END;
  buf[pos]:=CHAR(xx); INC(pos);
END put;

VAR line   : STRING;
    offs   : INTEGER;
    col,lin: INTEGER;
    cou,lay: INTEGER;
    i,last : INTEGER;
    val,c  : CHAR;
    lay_sz : INTEGER;
    col0,col1: INTEGER;

BEGIN
  check(h);            IF NOT done THEN RETURN END;
  pos:=0;
  cou:=0;
  val:=0c;
  WITH h DO
    col0:=x1 DIV 8;
    col1:=x2 DIV 8;
    FOR lin:=y1 TO y2 DO
(*$<$U+$T-*)
      offs:=lin*b^.WPL;
      FOR lay:=0 TO HIGH(b^.layers) DO
        IF b^.layers[lay]#NIL THEN
          line^.ADR:=b^.layers[lay] + offs;
          col:=col0;
          REPEAT
            c:=swap[ORD(line[col])]; INC(col);
(*$>*)
            IF c=val THEN
              INC(cou);
              IF cou=63 THEN put(0FFh); put(val); cou:=0 END
            ELSE
              IF cou#0 THEN
                IF (cou=1) & (BITSET(val)*{6..7}#{6..7}) THEN
                  put(val)
                ELSE
                  put(0C0h+cou); put(val)
                END
              END;
              val:=c;
              cou:=1;
            END;
            IF NOT BIO.done THEN done:=FALSE; error:=BIO.error; RETURN END
          UNTIL col>col1;
          IF cou#0 THEN
            IF (cou=1) & (BITSET(val)*{6..7}#{6..7}) THEN
              put(val)
            ELSE
              put(0C0h+cou); put(val)
            END
          END;
          val:=0c;
          cou:=0;
        END
      END
    END;
    IF cou#0 THEN put(0C0h+cou); put(val) END;
  END;
  BIO.put(f,buf,pos);
  done:=BIO.done;
  error:=BIO.error;
END packBMP;

PROCEDURE write(f: BIO.FILE; B: defBMG.BITMAP; h: HEADER);
  VAR i,save,sz: INTEGER;
BEGIN
  done:=TRUE; error:=err.ok;
  WITH h DO
    vers   :=5;
    encod  :=1;
    bitpx  :=1;
    x1     :=0;
    y1     :=0;
    x2     :=B^.W-1;
    y2     :=B^.H-1;
    hres   :=480;
    vres   :=360;
    vmode  := 0;
    nplanes:= 0;
    FOR i:=0 TO HIGH(B^.layers) DO
      IF B^.layers[i]#NIL THEN INC(nplanes) END
    END;
    bplin  := (B^.W+7) DIV 8;
    palinfo:= 0;
    shres  := 0;
    svres  := 0;
  END;
  save:=BIO.pos(f);
  IF NOT BIO.done THEN done:=FALSE; error:=BIO.error; RETURN END;
  put_header(f,h);
  IF NOT done THEN
    BIO.seek(f,0,save)
  ELSE
    packBMP(f,h,B);
    IF NOT done THEN BIO.seek(f,0,save); RETURN END
  END;
  sz:= B^.WPL*4*B^.H*h.nplanes;
  IF (BIO.pos(f)-save>sz) THEN

   (* packed > original *)

    BIO.seek(f,0,save);
    h.encod:=0;
    put_header(f,h);
    IF NOT done THEN
      BIO.seek(f,0,save)
    ELSE
      BIO.write(f,B^.BASE,sz)
    END;
    IF NOT BIO.done THEN
      done:=FALSE;
      error:=BIO.error;
      BIO.seek(f,0,save)
    END
  END
END write;

PROCEDURE new_map(w,h,l: INTEGER): defBMG.BITMAP;
  VAR m: defBMG.BITMAP; i: INTEGER;
BEGIN
  NEW(m);
  WITH m^ DO
    W:=w;
    H:=h;
    WPL:=(w+31) DIV 32;
    PATTERN:={0..31};
    mask:={};
    FOR i:=0 TO HIGH(layers) DO layers[i]:=NIL END;
    FOR i:=0 TO l-1          DO
      mem.ALLOCATE(layers[i],WPL*H);
      mask:=mask+{i};
    END;
    BASE:=layers[0]
  END;
  RETURN m
END new_map;

PROCEDURE read(f: BIO.FILE; VAR B: defBMG.BITMAP; VAR h: HEADER);
  VAR save,sz: INTEGER;
BEGIN
  save:=BIO.pos(f);
  IF NOT BIO.done THEN done:=FALSE; error:=BIO.error; RETURN END;
  sz:=get_header(f,h);
  IF NOT done THEN BIO.seek(f,0,save); RETURN END;
  WITH h DO B:=new_map((x2-x1+1)*bitpx,y2-y1+1,nplanes) END;
  unpackBMP(f,h,B);
  IF NOT done THEN BIO.seek(f,0,save); RETURN END
END read;

BEGIN
END PCX.
