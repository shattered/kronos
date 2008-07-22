IMPLEMENTATION MODULE ip[1]; (* $X+$N+ Igo&John 02-Oct-91. (c) KRONOS *)
                             (*        Igo      13-Dec-91. (c) KRONOS *)

IMPORT  SYSTEM;
IMPORT  drv: netdrv;
IMPORT  os : osKernel;
IMPORT  nos: netKernel;
IMPORT  err: defErrors;
IMPORT  cod: defCodes;

--IMPORT  spo: SPOws;
--IMPORT  tty: Terminal;

WITH STORAGE (NEW    : nos.allocate;
              DISPOSE: nos.deallocate;
              RESIZE : nos.reallocate);

TYPE int      = INTEGER;
     bs       = BITSET;

PROCEDURE di(): BITSET;  CODE cod.getm cod.copt 3 cod.bic cod.setm END di;
PROCEDURE ei(m: BITSET); CODE cod.setm END ei;
PROCEDURE TR(VAR f: BOOLEAN): BOOLEAN; CODE cod.tr END TR;

CONST fragment_size      = 8;
      min_header_size    = 20;
      max_header_size    = 60;
      min_datagramm_size = min_header_size+fragment_size;
      max_buffers        = 64;
      more_frag          = {18};

      stopped_data_tout = 5 * 50;

      MAGIC           = 5049h;

TYPE CHANNEL = POINTER TO channel;
     HOST    = POINTER TO host;

     START   = POINTER TO start;
     start   = RECORD
                 f,b : START;
                 proc: drv.startproc;
                 chan: CHANNEL
               END;

     channel = RECORD
                 did  : INTEGER;
                 node : drv.NODE;
                 stat : INTEGER;
                 len  : INTEGER;

                 buf : nos.buffer;
                 hsz : INTEGER;
                 hdr : ARRAY [0..14] OF WORD;

                 host: HOST;

                 doio: drv.REFDOIO;
                 up  : start

               END;

     host    = RECORD
                 f,b : HOST;
                 dest: DYNARR OF range;  (* available hosts or net list *)
                 name: DYNARR OF CHAR;   (* driver name                 *)
                 chan: CHANNEL;          (* driver channel              *)
               END;

     PATH    = POINTER TO path;
     path    = RECORD
                 f,b : PATH;;
                 net : INTEGER;          (* net  where gate is *)
                 host: INTEGER;          (* host where gate is *)
                 dest: DYNARR OF range   (* available net list *)
               END;


VAR

  m0FFFF: BITSET;

(* output *)

  odsg  : os.signal_rec;
  od    : os.process;
  oqued : REQUEST;

  (* input *)

  itsg  : os.signal_rec;
  it    : os.process;
  itque : nos.TIME;

  make  : DATA;

  freebf: drv.BUFFER; (* free buffers list *)

  freedg: DATA;       (* free datagram list *)

  istart: START;

  hlock : os.signal_rec;
  ilock : os.signal_rec;
  ILOCK : BOOLEAN;

  nets  : HOST;
  gates : HOST;
  paths : PATH;

  mynet : INTEGER;
  myhost: INTEGER;

  prot  : ARRAY [0..255] OF inputproc;

PROCEDURE chsm(VAL h: ARRAY OF SYSTEM.WORD; sz: INTEGER): BITSET;
  TYPE int=INTEGER; bs=BITSET;

  VAR i,sum,x: INTEGER;

BEGIN

(*$<$T-*)

  sum:=0;
  sum:=sum+int(bs(h[0]    )*m0FFFF);
  sum:=sum+int(bs(h[0]>>16)*m0FFFF);
  sum:=sum+int(bs(h[1]    )*m0FFFF);
  sum:=sum+int(bs(h[1]>>16)*m0FFFF);
  sum:=sum+int(bs(h[2]    )*m0FFFF);
  FOR i:=3 TO sz-1 DO
    x:=h[i];
    sum:=sum+int(bs(x    )*m0FFFF);
    sum:=sum+int(bs(x>>16)*m0FFFF)
  END;
  sum:=int(bs(sum)*m0FFFF) + int((bs(sum)>>16)*m0FFFF);
  sum:=int(bs(sum)*m0FFFF) + int((bs(sum)>>16)*m0FFFF);
  RETURN BITSET(sum)

(*$>*)

END chsm;

PROCEDURE removechannel(c: CHANNEL);

  PROCEDURE remove(VAR h: HOST; c: CHANNEL): BOOLEAN;
    VAR p: HOST;
  BEGIN
    IF h=NIL THEN RETURN FALSE END;
    p:=h;
    REPEAT
      IF p^.chan=c THEN nos.untien(h,p); RETURN TRUE END;
      p:=p^.f
    UNTIL p=h;
    RETURN FALSE
  END remove;

  PROCEDURE removestart(c: CHANNEL);
    VAR m: BITSET;
        s: START;
  BEGIN
    m:=di();
      IF istart=NIL THEN ei(m); RETURN END;
      s:=istart;
      REPEAT
        IF s^.chan=c THEN nos.untien(istart,s); ei(m); RETURN END;
        s:=s^.f
      UNTIL s=istart;
    ei(m)
  END removestart;

BEGIN
  os.wait(hlock);
  removestart(c);
  IF remove(nets ,c) THEN os.send(hlock); RETURN END;
  IF remove(gates,c) THEN END;
  os.send(hlock)
END removechannel;

PROCEDURE ready(c: CHANNEL; n: drv.NODE; res: INTEGER);
  VAR m: BITSET;
      r: REQUEST;
BEGIN
  ASSERT(c#NIL);
  IF n=NIL THEN
    removechannel(c);
    RETURN
  END;
  IF c^.node=n THEN c^.node:=NIL END;
  r     :=REQUEST(n^.obj);
  r^.res:=res;
  m:=di();
    nos.tie(oqued,r);
    IF odsg.queue#os.null THEN os.send(odsg) END;
  ei(m)
END ready;

PROCEDURE next(c: CHANNEL; n: drv.NODE; VAR last: BOOLEAN; VAR len: INTEGER);
  VAR r   : REQUEST;
      s   : BITSET;
      stat: INTEGER;
BEGIN

(*$<$T-*)

--  ASSERT(len>min_datagramm_size);
--  ASSERT(n#NIL); ASSERT(len    > 0);
--  ASSERT(c#NIL); ASSERT(n^.stat>=0);
  r:=REQUEST(n^.obj);
  IF n#c^.node THEN
    (* make new header *)
    c^.hdr[2]:=r^.ttl MOD 100h + (r^.prot MOD 100h)*100h;
    c^.hdr[3]:=r^.hadr;
    c^.hdr[4]:=r^.dst;
    c^.hsz   :=min_header_size;
    c^.stat  :=0;
    c^.buf   :=r^.buf;
    c^.node  :=n
  END;
  ASSERT(n^.stat*fragment_size<r^.len);
  IF c^.stat#n^.stat THEN
    c^.buf:=r^.buf;
    nos.bfind(c^.buf,n^.stat*fragment_size)
  END;
  len:=(len-min_header_size) DIV fragment_size;
  stat:=n^.stat;
  INC(n^.stat,len);
  IF n^.stat*fragment_size>=r^.len THEN
    c^.node:=NIL;
    last   :=TRUE;
    len    :=r^.len-stat*fragment_size;
    c^.hdr[1]:=bs(r^.did)             + (bs(stat)<<19)*{19..31}
  ELSE
    c^.hdr[1]:=bs(r^.did) + more_frag + (bs(stat)<<19)*{19..31};
    len   :=len*fragment_size;
    last  :=FALSE
  END;

  c^.len:=len;
  len   :=len+c^.hsz;

  (* pack total length *)
  c^.hdr[0]:=( bs(54h) + (bs(len)*m0FFFF)<<16);

  s:=chsm(c^.hdr,c^.hsz DIV 4);
  c^.hdr[2]:=bs(c^.hdr[2])*m0FFFF + (s*m0FFFF)<<16;

  c^.stat:=stat

(*$>*)

END next;

PROCEDURE omove(c: CHANNEL; a: ADDRESS; do: INTEGER);
BEGIN
  nos.move(a,do,SYSTEM.ADR(c^.hdr),0,c^.hsz);
  INC(do,c^.hsz);
  nos.bmove(a,do,c^.buf,c^.len);
  IF c^.node#NIL THEN c^.stat:=c^.node^.stat ELSE c^.stat:=-1 END
END omove;

PROCEDURE odone;
  VAR m: BITSET;
      r: REQUEST;
BEGIN
  LOOP
    m:=di();
      WHILE oqued=NIL DO os.wait(odsg) END;
      r:=oqued;
      nos.untie(oqued);
    ei(m);
    r^.done(r^.obj)
  END
END odone;

PROCEDURE unpackadr(ia: INTEGER; VAR net,host: INTEGER): BOOLEAN;
  VAR a: BITSET;
BEGIN
  a:=BITSET(ia);
  IF    a*{31}={} THEN net:=INTEGER((a>>24)*{0.. 6}); host:=INTEGER(a*{0..23})
  ELSIF a*{30}={} THEN net:=INTEGER((a>>16)*{0..13}); host:=INTEGER(a*{0..15})
  ELSIF a*{29}={} THEN net:=INTEGER((a>> 8)*{0..20}); host:=INTEGER(a*{0.. 7})
  ELSE
    RETURN FALSE
  END;
  RETURN TRUE
END unpackadr;

PROCEDURE getchannel(VAR c: CHANNEL; VAR dest: INTEGER; ipadr: INTEGER);

  PROCEDURE finddriver(VAR c: CHANNEL; VAR dest: INTEGER; n,h: INTEGER);
    VAR p: HOST;
        i: INTEGER;
  BEGIN
    c:=NIL;
    IF n=mynet THEN
      IF nets#NIL THEN
        p:=nets;
        REPEAT
          FOR i:=0 TO HIGH(p^.dest) DO
            IF (p^.dest[i].from<=h)&(h<=p^.dest[i].to) THEN
              c:=p^.chan; dest:=h; RETURN
            END
          END;
          p:=p^.f
        UNTIL p=nets
      END
    ELSIF gates#NIL THEN
      p:=gates;
      REPEAT
        FOR i:=0 TO HIGH(p^.dest) DO
          IF (p^.dest[i].from<=n)&(n<=p^.dest[i].to) THEN
            c:=p^.chan; dest:=0; RETURN
          END
        END;
        p:=p^.f
      UNTIL p=gates
    END
  END finddriver;

  PROCEDURE findpath(VAR c: CHANNEL; VAR dest: INTEGER; n: INTEGER);
    VAR p: PATH;
        i: INTEGER;
  BEGIN
    c:=NIL;
    IF paths=NIL THEN RETURN END;
    p:=paths;
    REPEAT
      FOR i:=0 TO HIGH(p^.dest) DO
        IF (p^.dest[i].from<=n)&(n<=p^.dest[i].to) THEN
          finddriver(c,dest,p^.net,p^.host);
          RETURN
        END
      END;
      p:=p^.f
    UNTIL p=paths
  END findpath;

  VAR h: INTEGER;
      n: INTEGER;
      l: PATH;
BEGIN
  c:=NIL;
  IF NOT unpackadr(ipadr,n,h) THEN RETURN END;
  finddriver(c,dest,n,h);
  IF c#NIL THEN RETURN END;
  findpath  (c,dest,n)
END getchannel;

PROCEDURE transmit(VAR r: request): INTEGER;
  VAR dr : drv.request;
      c  : CHANNEL;
      res: INTEGER;
BEGIN
  IF hostadr=-1 THEN r.res:=err.undef   ; RETURN r.res END;
  IF r.len<=0   THEN r.res:=err.bad_parm; RETURN r.res END;
  r.res:=err.ok;
  IF os.wait_del(-1,hlock)#0 THEN r.res:=err.ipted_op; RETURN r.res END;
    getchannel(c,r.node.dest,r.dst);
    IF c=NIL THEN os.send(hlock); r.res:=err.no_entry; RETURN r.res END;
    r.hadr:=hostadr;
    r.did :=c^.did;
    c^.did:=(c^.did+1) MOD 10000h;
    WITH r.node DO
      stat:=0;
      prot:=drv.IP;
      tout:=r.tout;
      obj :=SYSTEM.ADR(r)
    END;
    dr.prot:=drv.IP;
    dr.op  :=drv._transmit;
    dr.node:=SYSTEM.ADR(r.node);
    res:=c^.doio^(dr);
    IF res#err.ok THEN r.res:=res END;
  os.send(hlock);
  RETURN res
END transmit;

PROCEDURE transmitdg(d: DATA);
BEGIN
END transmitdg;

PROCEDURE empty(d: DATA);
BEGIN
END empty;

PROCEDURE install (p: INTEGER; input: inputproc): INTEGER;
BEGIN
  IF (p<0) OR (p>HIGH(prot)) THEN RETURN err.bad_parm END;
  IF os.wait_del(-1,hlock)#0 THEN RETURN err.ipted_op END;
  IF prot[p]#empty THEN os.send(hlock); RETURN err.duplicate END;
  prot[p]:=input;
  os.send(hlock);
  RETURN err.ok
END install;

PROCEDURE remove  (p: INTEGER): INTEGER;
BEGIN
  IF hostadr=-1              THEN RETURN err.undef    END;
  IF (p<0) OR (p>HIGH(prot)) THEN RETURN err.bad_parm END;
  IF os.wait_del(-1,hlock)#0 THEN RETURN err.ipted_op END;
  prot[p]:=empty;
  os.send(hlock);
  RETURN err.ok
END remove;

PROCEDURE find(h: HOST; VAR name: ARRAY OF CHAR): HOST;
  VAR l: HOST;
BEGIN
  IF h=NIL THEN RETURN NIL END;
  l:=h;
  REPEAT
    IF name=l^.name THEN RETURN l END;
    l:=l^.f
  UNTIL l=h;
  RETURN NIL
END find;

PROCEDURE alloc(c: CHANNEL; VAR b: drv.BUFFER; is: drv.startproc; adr: ADDRESS; p,l: INTEGER);
FORWARD;

PROCEDURE input(c: CHANNEL; VAR b: drv.BUFFER);
FORWARD;

PROCEDURE definedriver(VAL nm: ARRAY OF CHAR;
                       VAL d : ARRAY OF range; g: BOOLEAN): INTEGER;


  PROCEDURE installme(c: CHANNEL): INTEGER;
    VAR d: drv.request;
  BEGIN
    d.op      :=drv._install;
    d.prot    :=drv.IP;
    d.alloc   :=alloc;
    d.input   :=input;
    d.move    :=omove;
    d.next    :=next;
    d.ready   :=ready;
    d.channel :=c;
    d.res:=c^.doio^(d);
    RETURN d.res
  END installme;

  VAR p: HOST;

  PROCEDURE u;
  BEGIN
    DISPOSE(p^.name);
    DISPOSE(p^.dest);
    DISPOSE(p^.chan);
    DISPOSE(p);
    os.send(hlock)
  END u;

  VAR r: INTEGER;

BEGIN
  IF hostadr=-1 THEN RETURN err.undef    END;
  IF HIGH(nm)<0 THEN RETURN err.bad_parm END;
  IF HIGH(d )<0 THEN RETURN err.bad_parm END;
  IF os.wait_del(-1,hlock)#0 THEN RETURN err.ipted_op END;
  NEW(p);       IF p=NIL THEN os.send(hlock); RETURN err.no_memory END;
  NEW(p^.name);
  NEW(p^.dest);
  NEW(p^.chan); IF p^.chan=NIL THEN u; RETURN err.no_memory END;
  NEW(p^.name,HIGH(nm)+2); IF HIGH(p^.name)<0 THEN u; RETURN err.no_memory END;
  NEW(p^.dest,HIGH(d )+1); IF HIGH(p^.dest)<0 THEN u; RETURN err.no_memory END;
  p^.name:=nm; p^.name[HIGH(p^.name)]:=0c;
  p^.dest:=d;
  IF find(nets ,p^.name)#NIL THEN u; RETURN err.duplicate END;
  IF find(gates,p^.name)#NIL THEN u; RETURN err.duplicate END;

  p^.chan^.node:=NIL;
  p^.chan^.host:=p;
  p^.chan^.did :=os.timer MOD 10000h;

--tty.print('try to open driver\n');


  r:=drv.open (p^.name,p^.chan^.doio); IF r#err.ok THEN u; RETURN r END;
  r:=installme(p^.chan);               IF r#err.ok THEN u; RETURN r END;

  IF g THEN nos.tie(gates,p) ELSE nos.tie(nets,p) END;

  os.send(hlock);
  RETURN err.ok
END definedriver;

PROCEDURE definepath(n,h: INTEGER; VAL d: ARRAY OF range): INTEGER;
  VAR p: PATH;
BEGIN
  IF hostadr=-1              THEN RETURN err.undef    END;
  IF HIGH(d)<0               THEN RETURN err.bad_parm END;
  IF os.wait_del(-1,hlock)#0 THEN RETURN err.ipted_op END;
  NEW(p);
  IF p=NIL           THEN os.send(hlock); RETURN err.no_memory END;
  NEW(p^.dest,HIGH(d)+1);
  IF HIGH(p^.dest)<0 THEN os.send(hlock); RETURN err.no_memory END;
  p^.net :=n;
  p^.host:=h;
  p^.dest:=d;
  os.send(hlock);
  RETURN err.ok
END definepath;

PROCEDURE sethost(hadr: INTEGER): INTEGER;
  VAR n,h: INTEGER;
BEGIN
  IF NOT unpackadr(hadr,n,h) THEN RETURN err.bad_parm END;
  IF os.wait_del(-1,hlock)#0 THEN RETURN err.ipted_op END;
  IF hostadr#-1 THEN os.send(hlock); RETURN err.duplicate END;
  hostadr:=hadr;
  mynet  :=n;
  myhost :=h;
  os.send(hlock);
  RETURN err.ok
END sethost;

----------------------------- INPUT ----------------------------
                             -------

PROCEDURE deltout(VAR t: nos.time);
  VAR m: BITSET;
BEGIN
  m:=di();
    IF    t.tout>=0   THEN nos.deltout(t);
    ELSIF t.f    #NIL THEN nos.untien (itque,SYSTEM.ADR(t))
    END;
    t.f:=NIL;
  ei(m)
END deltout;

PROCEDURE dgtimer(t: nos.TIME);
BEGIN
  nos.tie(itque,t);
  IF itsg.queue#os.null THEN os.send(itsg) END
END dgtimer;

PROCEDURE settout(VAR t: nos.time; tout: INTEGER);
  VAR m: BITSET;
BEGIN
  t.tout:=-1;
  t.f   :=NIL;
  nos.settout(t,tout)
END settout;

(* must be called only with desabled interrupts *)
PROCEDURE startinput;
  VAR s: START;
BEGIN
  WHILE (istart#NIL) & (freebf#NIL) DO
    s:=istart;
    nos.untie(istart);
    s^.proc
  END
END startinput;

PROCEDURE alloc(c: CHANNEL; VAR b: drv.BUFFER; is: drv.startproc; adr: ADDRESS; p,l: INTEGER);
  VAR m: BITSET;
BEGIN
  m:=di();
    WHILE freebf=NIL DO
      c^.up.proc:=is;
      nos.tie(istart,SYSTEM.ADR(c^.up));
      nos.wait
    END;
    b     :=freebf;
    freebf:=freebf^.f;
  ei(m);
  b^.len:=l;
  b^.pos:=0;
  nos.move(b^.buf,0,adr,p,l)
END alloc;

PROCEDURE brem(VAR b: drv.BUFFER);
  VAR m: BITSET;
BEGIN
  m:=di();
    b^.f:=freebf; freebf:=b;
    startinput;
  ei(m)
END brem;

PROCEDURE binsert(d: DATA; b: drv.BUFFER): BOOLEAN;
  VAR c,f: drv.BUFFER;
      rp : INTEGER;
BEGIN
  IF d^.cque=NIL THEN
    b^.f:=b; b^.cf:=b;
    b^.b:=b; b^.cb:=b;
    d^.cque:=b
  ELSE
    rp:=b^.ofs+b^.len;
    c:=d^.cque;
    IF rp<=c^.ofs THEN
      d^.cque:=b;
      b^.cb:=c^.cb;                                     b^.cb^.cf:=b;
      IF rp=c^.ofs THEN b^.cf:=c^.cf ELSE b^.cf:=c END; b^.cf^.cb:=b;
      b^.f:=c;
      b^.b:=c^.b;
    ELSE
      c:=d^.cque^.cb;
      WHILE rp<=c^.ofs DO c:=c^.cb END;
      f   :=c^.cf;
      b^.f:=f;
      b^.b:=f^.b;
      IF rp=f^.ofs THEN b^.cf:=f^.cf ELSE b^.cf:=f END; b^.cf^.cb:=b;
      f:=b^.b;
      IF    b^.ofs=f^.ofs+f^.len THEN
        c^.cf:=b^.cf; c^.cf^.cb:=c
      ELSIF b^.ofs<f^.ofs+f^.len THEN
        RETURN FALSE
      ELSE
        b^.cb:=c; b^.cb^.cf:=b
      END
    END;
    b^.b^.f:=b;
    b^.f^.b:=b
  END;
  RETURN TRUE
END binsert;

PROCEDURE dfind(VAR d: DATA; dst,s,i,p: INTEGER);
  VAR n: DATA;
BEGIN
  IF make#NIL THEN
    n:=make;
    REPEAT
      IF (n^.src=s) & (n^.did=i) & (n^.prot=p) & (n^.dst=dst) THEN
        d:=n;
        RETURN
      END;
      n:=n^.f
    UNTIL n=make;
  END;
  d:=NIL
END dfind;

PROCEDURE drem(VAR d: DATA);
  VAR m: BITSET;
BEGIN
  m:=di();
    IF d^.cque#NIL THEN
      d^.cque^.b^.f:=freebf; freebf:=d^.cque; d^.cque:=NIL;
      startinput
    END;
    d^.f:=freedg; freedg:=d;
  ei(m)
END drem;

PROCEDURE dstop(d: DATA);
  VAR m: BITSET;
BEGIN
  IF d^.stop THEN RETURN END;
  d^.stop:=TRUE;
  IF d^.cque#NIL THEN
    m:=di();
      d^.cque^.b^.f:=freebf; freebf:=d^.cque; d^.cque:=NIL;
      startinput;
    ei(m)
  END
END dstop;

PROCEDURE lock;
  VAR m: BITSET;
BEGIN
  m:=di();
    IF ILOCK THEN os.wait(ilock) END;
    ILOCK:=TRUE;
  ei(m)
END lock;

PROCEDURE iunlock;
  VAR m: BITSET;
BEGIN
  m:=di();
    IF ilock.queue#os.null THEN os.send(ilock) ELSE ILOCK:=FALSE END;
  ei(m)
END iunlock;

PROCEDURE itout;
  VAR m: BITSET;
      t: nos.TIME;
      d: DATA;
BEGIN
  LOOP
    m:=di();
      LOOP
        WHILE itque=NIL DO os.wait(itsg) END;
        lock;
        IF itque#NIL THEN EXIT END;
        iunlock
      END;
    ei(m);
    t:=itque;
    nos.untie(itque);
    t^.f:=NIL;
    d:=DATA(t^.obj);
    m:=di();
      IF d^.cque#NIL THEN
        d^.cque^.b^.f:=freebf; freebf:=d^.cque; d^.cque:=NIL;
        startinput
      END;
      nos.untien(make,d);
      d^.f:=freedg; freedg:=d;
    ei(m);
    iunlock
  END
END itout;

PROCEDURE biunlock(VAR b: drv.BUFFER);
BEGIN
  brem(b); iunlock
END biunlock;

PROCEDURE input(c: CHANNEL; VAR b: drv.BUFFER);
  TYPE WHEAD = POINTER TO ARRAY [0..max_header_size DIV 4-1] OF BITSET;
       BHEAD = POINTER TO ARRAY [0..max_header_size      -1] OF CHAR;

  VAR wh : WHEAD;
      bh : BHEAD;
      d  : DATA;
      n  : drv.BUFFER;
      m,s: BITSET;
      dst: INTEGER;
  src,did,pro: INTEGER;
      len,hsz: INTEGER;

BEGIN
  IF b=NIL THEN RETURN END;
  wh :=b^.buf;
  bh :=b^.buf;
  hsz:=int(  wh^[0]*{4 .. 7} )>>2;    (* pos in b^.buf *)
  src:=int( (wh^[2]>>16)*m0FFFF );    (* check sum     *)
  s:=chsm(wh^,hsz DIV 4);
  IF bs(src)#s THEN brem(b); RETURN END;

  lock;

  did:=int( wh^[1]*m0FFFF );
  src:=int( wh^[3] );
  dst:=int( wh^[4] );
  pro:=int( bh^[9] );

  dfind(d,dst,src,did,pro);

  IF d=NIL THEN

    len:=int( bh^[8] );
    IF len=0 THEN biunlock(b); RETURN END;
    m:=di();
      d:=freedg;
      IF d=NIL THEN ei(m); biunlock(b); RETURN END;
      freedg:=freedg^.f;
    ei(m);
    d^.last:=FALSE;
    d^.src :=src;
    d^.dst :=dst;
    d^.did :=did;
    d^.prot:=pro;
    d^.cque:=NIL;
    d^.stop:=FALSE;

    IF len>5 THEN len:=5 END;

    settout(d^.time,len*50);

    nos.tie(make,d)
  ELSIF d^.stop THEN biunlock(b); RETURN
  END;
  b^.ofs:=int(  wh^[1]*{19..31} )>>16;  (* offs in datagramm   *)
  len   :=int( (wh^[0]>>16)*m0FFFF );   (* length of fragment  *)

  IF (len>b^.len) OR (len=0) THEN dstop(d); biunlock(b); RETURN END;

  b^.pos:=b^.pos + hsz;
  b^.len:=len    - hsz;

  IF wh^[1]*more_frag={} THEN
    IF d^.last THEN dstop(d); biunlock(b); RETURN END;
    d^.last:=TRUE;
    d^.tlen:=b^.ofs+b^.len
  ELSIF bs(b^.len)*{0..2}#{} THEN dstop(d); biunlock(b); RETURN
  END;
  IF (d^.last)&(b^.ofs+b^.len>d^.tlen) THEN dstop(d); biunlock(b); RETURN END;
  IF NOT binsert(d,b) THEN dstop(d); biunlock(b); RETURN END;
  b:=d^.cque^.cb;
  IF d^.last & (b=d^.cque) & (b^.b^.ofs+b^.b^.len=d^.tlen) THEN

    deltout   (d^.time);
    nos.untien(make,d );

    IF d^.dst#hostadr THEN
      transmitdg(d)
    ELSE
      prot[d^.prot](d);
    END;
    drem(d)
  END;
  iunlock
END input;

PROCEDURE stop;
BEGIN
  os.stop(it,TRUE);
  os.stop(od,TRUE)
END stop;

PROCEDURE init;

  VAR i: INTEGER;
      b: drv.BUFFER;
      d: DATA;
BEGIN
  oqued :=NIL;
  itque :=NIL;
  make  :=NIL;
  freebf:=NIL;
  freedg:=NIL;
  nets  :=NIL;
  gates :=NIL;
  paths :=NIL;          hostadr:=-1; mynet:=-1; myhost:=-1;
  istart:=NIL;
  ILOCK :=FALSE;

  os.ini_signal(odsg ,{}                 ,0);
  os.ini_signal(itsg ,{}                 ,0);
  os.ini_signal(hlock,os.break + os.guard,1);
  os.ini_signal(ilock,           os.guard,0);

  IF os.make_process(od,odone,512)#err.ok THEN HALT(1) END;
  IF os.make_process(it,itout,512)#err.ok THEN HALT(1) END;

  FOR i:=0 TO max_buffers-1 DO
    nos.allocate(b,SIZE(b^));   IF b=NIL THEN HALT(err.no_memory) END;
    b^.sz:=(512+64) DIV 4;
    nos.allocate(b^.buf,b^.sz); IF b^.buf=NIL THEN HALT(err.no_memory) END;
    b^.f :=freebf; freebf:=b;

    nos.allocate(d,SIZE(d^));   IF d=NIL THEN HALT(err.no_memory) END;
    d^.f  :=freedg;
    freedg:=d;
    d^.time.obj :=d;
    d^.time.done:=dgtimer
  END;

  FOR i:=0 TO HIGH(prot) DO prot[i]:=empty END;

  IF os.final(os.self(),stop)#err.ok THEN (* HALT(1)*) END;
  os.start(it);
  os.start(od)
END init;

PROCEDURE set;
VAR r: INTEGER;
    h: ARRAY [0..1] OF range;
BEGIN
  IF sethost(02h)#err.ok THEN HALT(1) END;
  h[0].from:=001h;
  h[0].to  :=001h;
  h[1].from:=003h;
  h[1].to  :=003h;
  r:=definedriver("arc0",h,FALSE);
  IF r#err.ok THEN HALT(r) END
END set;


BEGIN
  m0FFFF:={0..15};
  init
  --;set
END ip.
