IMPLEMENTATION MODULE tcp[1]; (* $N+ Igo 18-Oct-91. (c) KRONOS *)

IMPORT  SYSTEM;
IMPORT  err: defErrors;
IMPORT  nos: netKernel;
IMPORT   os:  osKernel;
IMPORT  drv: netdrv;
IMPORT   ip: ip;
IMPORT  cod: defCodes;

IMPORT  tty: Terminal;
--IMPORT  spo: SPOws;

PROCEDURE di (): BITSET;  CODE cod.getm cod.copt 3 cod.bic cod.setm END di;
PROCEDURE ei (m: BITSET); CODE cod.setm END ei;

CONST

    LISTEN       = 0;  (* represents waiting for a connection request
                          from any remote TCP and port. *)

    SYN_SENT     = 1;  (* represents waiting for a matching connection
                          request after having sent a connection request. *)

    SYN_RECEIVED = 2;  (* represents waiting for a confirming connection
                          request acknowledgment after having both received
                          and sent a connection request. *)

    ESTABLISHED  = 3;  (* represents an open connection, data received can be
                          delivered to the user.  The normal state for the data
                          transfer phase of the connection. *)

    FIN_WAIT_1   = 4;  (* represents waiting for a connection termination
                          request from the remote TCP, or an acknowledgment
                          of the connection termination request previously
                          sent. *)

    FIN_WAIT_2   = 5;  (* represents waiting for a connection termination
                          request from the remote TCP. *)

    CLOSE_WAIT   = 6;  (* represents waiting for a connection termination
                          request from the local user. *)

    CLOSING      = 7;  (* represents waiting for a connection termination
                          request acknowledgment from the remote TCP. *)

    LAST_ACK     = 8;  (* represents waiting for an acknowledgment of the
                          connection termination request previously sent
                          to the remote TCP (which includes an acknowledgment
                          of its connection termination request). *)

    TIME_WAIT    = 9;  (* represents waiting for enough time to pass
                          to be sure the remote TCP received the
                          acknowledgment of its connection termination
                          request. *)

    CLOSED       = 10; (* represents no connection state at all. *)

    magic        = 706374h; (* tcp *)


TYPE SESSION = POINTER TO session;
     session = RECORD
                 f,b : SESSION;

                magic: INTEGER;

                 lnid: INTEGER;
                 rnid: INTEGER;
                 radr: INTEGER;

                 (* transmitter variables *)

                 tran: REQUEST;        (* request for retransmit        *)
                 tbuf: nos.buffer;     (* current buffer for retransmit *)
                 tack: INTEGER;        (* expected acknolegment         *)
                 tof1: INTEGER;        (* offset to tack in tran        *)

                 ctra: REQUEST;        (* request to transmit           *)
                 cbuf: nos.buffer;     (* current buffer for transmit   *)
                 tnxt: INTEGER;        (* sequence num to transmit      *)
                 tof2: INTEGER;        (* offset to tnxt in ctra        *)

                 tdon: REQUEST;        (* request for done              *)

                 twin: INTEGER;        (* reported window            *)
                 tpsz: INTEGER;        (* reported max fragment size *)
                 twl1: INTEGER;        (* seq used for last twin upd.*)
                 twl2: INTEGER;        (* ack used for last twin upd.*)
                 time: nos.time;       (* retransmission timeout     *)
                 rcnt: INTEGER;        (* number of issued request   *)

                 (* reciever variables *)

                 rack: INTEGER;        (* sequence num expected to recieve *)
                 rwin: INTEGER;        (* bytes available to recieve       *)

                 rque: REQUEST;        (* recieve queue *)

                 rof1: INTEGER;        (* ofsset in rque           *)

                 rbuf: SYSTEM.ADDRESS; (* receive data buffer      *)
                 rbsz: INTEGER;        (* receive data buffer size *)
                 rbeg: INTEGER;
                 rend: INTEGER;

                 pbuf: POINTER TO ARRAY [0..0] OF INTEGER;
                                       (* receive push buffer      *)
                 pwsz: SYSTEM.ADDRESS; (* receive push buffer size *)
                 pbeg: INTEGER;
                 pend: INTEGER;

                 iss : INTEGER;        (* initial sequence number  *)
                 irs : INTEGER;        (* initial receive  number  *)

                 stat: INTEGER;        (* state of session *)

                 (* events handling *)

                 epro: eventproc;
                 eobj: WORD;



                srnid: INTEGER;
                sradr: INTEGER;
               END;

TYPE    oREQUEST = POINTER TO orequest;
        orequest = RECORD
                     next: oREQUEST;
                     s   : SESSION;
                     r   : ip.request;
                     buf : nos.buffer;
                     freq: REQUEST;    (* first tcprequest for this operation *)
                     fpos: INTEGER;    (* position in this tcprequest         *)
                     h   : ARRAY [0..15] OF BITSET
                   END;

CONST   minhdrsz = 20;
        maxsgmsz = 1024*16;

        _urg = {0};
        _ack = {1};
        _psh = {2};
        _rst = {3};
        _syn = {4};
        _fin = {5};

        timeout2MSL = 100;
        deadtimeout = 10 ;

        synsendtout = 50;

        requestsno  = 16;

        tcpotimeout = 100;
        tcprtimeout = 100;
        tcpottl     = 4;


VAR m0FFFF      : BITSET;
    main        : SESSION;
    conn        : SESSION;
    dead        : SESSION;
    ilock       : os.signal_rec;
    tcptime     : os.signal_rec;
    usertime    : os.signal_rec;
    freersg     : os.signal_rec;
    freer       : oREQUEST;
    tcptimeque  : nos.TIME;
    usertimeque : nos.TIME;
    tcptp       : os.process;
    usertp      : os.process;
    rarr        : ARRAY [0..requestsno-1] OF orequest;

PROCEDURE findsession(h: SESSION; VAR s: SESSION; lnid,rnid,radr: INTEGER);
BEGIN
  s:=h;
  IF s=NIL THEN RETURN END;
  REPEAT
    IF (s^.lnid=lnid) & (s^.rnid=rnid) & (s^.radr=radr) THEN RETURN END;
    s:=s^.f
  UNTIL s=h;
  s:=NIL
END findsession;

PROCEDURE findlsession(VAR s: SESSION; lnid,rnid,radr: INTEGER);
BEGIN
  s:=conn;
  IF s=NIL THEN RETURN END;
  REPEAT
    IF (s^.lnid=lnid) & (s^.rnid=rnid) & (s^.radr=radr) THEN RETURN END;
    s:=s^.f
  UNTIL s=conn;
  REPEAT
    IF (s^.lnid=lnid) & (s^.rnid=0) & (s^.radr=radr) THEN RETURN END;
    s:=s^.f
  UNTIL s=conn;
  REPEAT
    IF (s^.lnid=lnid) & (s^.rnid=rnid) & (s^.radr=0) THEN RETURN END;
    s:=s^.f
  UNTIL s=conn;
  REPEAT
    IF (s^.lnid=lnid) & (s^.rnid=0) & (s^.radr=0) THEN RETURN END;
    s:=s^.f
  UNTIL s=conn;
  s:=NIL
END findlsession;

PROCEDURE deltout(VAR t: nos.time);
  VAR m: BITSET;
BEGIN

--tty.print('del tout t=%d f=%h\n',t.tout,t.f);

  m:=di();
    IF    t.tout>=0   THEN nos.deltout(t);
    ELSIF t.f    #NIL THEN nos.untien (tcptimeque,SYSTEM.ADR(t))
    END;
    t.f:=NIL;
  ei(m)
END deltout;

PROCEDURE usertimer(t: nos.TIME);
BEGIN
  nos.tie(usertimeque,t);
  IF usertime.queue#os.null THEN os.send(usertime) END
END usertimer;

PROCEDURE setutimer(s: SESSION; VAR r: request);
BEGIN
  r.time.tout:=-1;
  r.time.f   :=NIL;
  IF r.tout>0 THEN
    r.time.done:=usertimer;
    r.time.obj :=s;
    nos.settout(r.time,r.tout)
  END
END setutimer;

PROCEDURE delutimer(VAR t: nos.time);
  VAR m: BITSET;
BEGIN

--tty.print('del user timer\n');


  m:=di();
    IF    t.tout>=0   THEN nos.deltout(t);
    ELSIF t.f    #NIL THEN nos.untien(usertimeque,SYSTEM.ADR(t))
    END;
    t.f:=NIL;
  ei(m)
END delutimer;

PROCEDURE dodone(s: SESSION);
  VAR m: BITSET;
      n: REQUEST;
BEGIN


--tty.print('dodone\n');

  WHILE s^.tdon#s^.tran DO
    n:=s^.tdon;
    m:=di();
    IF n^.rcnt>0 THEN
        n^.rm:=TRUE;
        ei(m);
        RETURN
      END;
    ei(m);
    s^.tdon:=n^.f;
    IF s^.tdon#NIL THEN s^.tdon^.b:=n^.b END;
    delutimer(n^.time);
    n^.done  (n^.obj )
  END
END dodone;

PROCEDURE tcpodone(r: oREQUEST);
  VAR m  : BITSET;
      l,p: INTEGER;
      s  : BOOLEAN;
      t  : REQUEST;
BEGIN

--tty.perror(r^.r.res,'done %%s\n');

  DEC(r^.s^.rcnt);


  t:=r^.freq;
  p:=r^.fpos;
  l:=r^.r.len-r^.r.buf.len;
  s:=FALSE;
  WHILE l>0 DO
    m:=di();
      DEC(t^.rcnt);
      s:=s OR (t^.rcnt=0)&(t^.rm);
    ei(m);
    DEC(l,t^.len-p);
    t:=t^.f;
    p:=0
  END;

  m:=di();
    r^.next:=freer; freer:=r;
    IF freersg.queue#os.null THEN os.send(freersg) END;
  ei(m);

  IF s THEN
    os.wait(ilock);
      dodone(r^.s);
    os.send(ilock)
  END
END tcpodone;

PROCEDURE tcpbadodone(r: oREQUEST);
  VAR m  : BITSET;
      l,p: INTEGER;
      s  : BOOLEAN;
      t  : REQUEST;
BEGIN

--tty.perror(r^.r.res,'baddone %%s\n');

  IF r^.r.res=err.write_fail THEN tty.print('bad: write_fail\n') END;

  DEC(r^.s^.rcnt);

  t:=r^.freq;
  p:=r^.fpos;
  l:=r^.r.len-r^.r.buf.len;
  s:=FALSE;
  WHILE l>0 DO
    m:=di();
      DEC(t^.rcnt);
      s:=s OR (t^.rcnt=0)&(t^.rm);
    ei(m);
    DEC(l,t^.len-p);
    t:=t^.f;
    p:=0
  END;

  m:=di();
    r^.next:=freer; freer:=r;
    IF freersg.queue#os.null THEN os.send(freersg) END;
  ei(m);

  IF s THEN dodone(r^.s) END
END tcpbadodone;

PROCEDURE sendheader(s: SESSION;
                     lnid,rnid,radr,seq,ack: INTEGER; flags: BITSET);
  VAR r: oREQUEST;
      m: BITSET;
BEGIN

  m:=di();
    WHILE freer=NIL DO os.wait(freersg) END;
    r:=freer;
    freer:=freer^.next;
  ei(m);

  IF flags*_syn#{} THEN
    s^.tnxt:=s^.iss;
    s^.tack:=s^.tnxt;
    seq    :=s^.tnxt
  END;

--tty.print('send f=%{} seq=%d ack=%d radr=%h\n',flags,seq,ack,radr);

  r^.h[0]:=BITSET(lnid)*m0FFFF+(BITSET(rnid)*m0FFFF)<<16;
  r^.h[1]:=BITSET(seq);
  r^.h[2]:=BITSET(ack);
  r^.h[3]:=BITSET(5)+(flags*{0..5})<<10+BITSET(s^.rwin)<<16;
  r^.h[4]:={};

  r^.freq:=NIL;
  r^.r.buf.next:=NIL;

  r^.s:=s;
  INC(s^.rcnt);

  r^.r.dst:=radr;
  r^.r.len:=20;

  IF flags*_syn#{} THEN INC(s^.tnxt) END;
  IF flags*_fin#{} THEN INC(s^.tnxt) END;

  IF ip.transmit(r^.r)#err.ok THEN tcpbadodone(r) END

END sendheader;

PROCEDURE synsend(s: SESSION);
BEGIN
  sendheader(s,s^.lnid,s^.rnid,s^.radr,s^.tnxt,0,_syn)
END synsend;

PROCEDURE synacksend(s: SESSION);
BEGIN
  sendheader(s,s^.lnid,s^.rnid,s^.radr,s^.tnxt,s^.rack,_syn+_ack)
END synacksend;

PROCEDURE acksend(s: SESSION);
BEGIN
  sendheader(s,s^.lnid,s^.rnid,s^.radr,s^.tnxt,s^.rack,_ack)
END acksend;

PROCEDURE ackfinsend(s: SESSION);
BEGIN
  sendheader(s,s^.lnid,s^.rnid,s^.radr,s^.tnxt,s^.rack,_ack+_fin)
END ackfinsend;

PROCEDURE finsend(s: SESSION);
BEGIN
  sendheader(s,s^.lnid,s^.rnid,s^.radr,s^.tnxt,s^.rack,_fin)
END finsend;

PROCEDURE rstsend(s: SESSION);
BEGIN
  sendheader(s,s^.lnid,s^.rnid,s^.radr,s^.tnxt,s^.rack,_rst)
END rstsend;

PROCEDURE rtimeout(s: SESSION; t: INTEGER);
  VAR m: BITSET;
BEGIN
  m:=di();
    IF (s^.time.tout>=0) OR (s^.time.f#NIL) THEN ei(m); RETURN END;
  ei(m);
  nos.settout(s^.time,t)
END rtimeout;

PROCEDURE fillheader(s: SESSION; r: oREQUEST; VAR tbuf: nos.buffer;
                               tran: REQUEST; tpos: INTEGER);
BEGIN
  r^.h[0]:=BITSET(s^.lnid)*m0FFFF+(BITSET(s^.rnid)*m0FFFF)<<16;
  r^.h[1]:=BITSET(s^.tack);
  r^.h[2]:=BITSET(s^.rack);
  r^.h[4]:={};

  IF tran^.op=_closeses THEN
    r^.freq:=NIL; r^.r.buf.next:=NIL
  ELSE
    r^.freq:=tran;
    r^.buf :=tbuf;
    r^.r.buf.next:=SYSTEM.ADR(r^.buf)
  END;
  r^.fpos:=tpos;

  r^.s:=s;
  INC(s^.rcnt);

  r^.r.dst:=s^.radr
END fillheader;

PROCEDURE tcpretransmit(s: SESSION);
  VAR m,f   : BITSET;
      at,l,p: INTEGER;
      r     : oREQUEST;
      tran  : REQUEST;
      tof1  : INTEGER;
BEGIN

--tty.print('tcp retransmit ttrl=%d twin=%d\n',s^.ttrl,s^.twin);

  IF s^.tran=s^.ctra THEN RETURN END;

  m:=di();
    WHILE freer=NIL DO os.wait(freersg) END;
    r:=freer;
    freer:=freer^.next;
  ei(m);

  fillheader(s,r,s^.tbuf,s^.tran,s^.tof1);

  at:=s^.tack+s^.twin;
  IF s^.tnxt<at THEN at:=s^.tnxt END;
  at:=at-s^.tack;
  IF at>s^.tpsz THEN at:=s^.tpsz END;

  f:=_ack;
  l:=0;

  tran:=s^.tran;
  tof1:=s^.tof1;
  LOOP
    IF tran^.op=_closeses THEN f:=f+_fin; EXIT END;
    IF at=0 THEN EXIT END;
    INC(tran^.rcnt);
    p:=tran^.len-tof1;
    IF p<=at THEN
      DEC(at,p);
      INC(l ,p);
      IF tran^.mode*_pushm#{} THEN f:=f+_psh; at:=0 END;
      tran:=tran^.f; tof1:=0;
      IF tran=s^.ctra THEN EXIT END
    ELSE
      INC(l,at);
      EXIT
    END
  END;

  r^.r.len:=l + r^.r.buf.len;
  r^.h[3]:=BITSET(5)+(f*{0..5})<<10+BITSET(s^.rwin)<<16;

  IF ip.transmit(r^.r)#err.ok THEN tcpbadodone(r) END

END tcpretransmit;

PROCEDURE tcptransmit(s: SESSION; ACK: BOOLEAN);
  VAR m,f      : BITSET;
      at,l,p,lt: INTEGER;
      r        : oREQUEST;
BEGIN

--tty.print('tcp transmit tnxt=%d twin=%d\n',s^.tnxt,s^.twin);

  IF s^.ctra=NIL THEN
    IF ACK THEN acksend(s) END;
    RETURN
  END;

  lt:=s^.tack+s^.twin;

  WHILE (s^.ctra#NIL)&((lt>s^.tnxt)OR(s^.ctra^.op=_closeses)) DO

    m:=di();
      WHILE freer=NIL DO os.wait(freersg) END;
      r:=freer;
      freer:=freer^.next;
    ei(m);

    fillheader(s,r,s^.cbuf,s^.ctra,s^.tof2);

    at:=lt-s^.tnxt;
    IF at>s^.tpsz THEN at:=s^.tpsz END;

    f:=_ack;
    l:=0;

    LOOP
      IF s^.ctra^.op=_closeses THEN
        s^.ctra:=s^.ctra^.f; s^.tof2:=0;
        f:=f+_fin;
        EXIT
      END;
      IF at=0 THEN EXIT END;
      INC(s^.ctra^.rcnt);
      p:=s^.ctra^.len-s^.tof2;
      IF p<=at THEN
        DEC(at,p);
        INC(l ,p);
        IF s^.ctra^.mode*_pushm#{} THEN f:=f+_psh; at:=0 END;
        s^.ctra:=s^.ctra^.f;
        s^.tof2:=0;
        IF s^.ctra=NIL THEN EXIT END;
        s^.cbuf:=s^.ctra^.obuf
      ELSE
        INC(s^.tof2,at);
        INC(l,at);
        nos.bfind(s^.cbuf,s^.tof2);
        EXIT
      END
    END;

    r^.r.len:=l + r^.r.buf.len;
    r^.h[3]:=BITSET(5)+(f*{0..5})<<10+BITSET(s^.rwin)<<16;

    INC(s^.tnxt,l);

    IF f*_fin#{} THEN
      INC(s^.tnxt);
      CASE s^.stat OF
      |CLOSE_WAIT : s^.stat:=LAST_ACK
      |ESTABLISHED: s^.stat:=FIN_WAIT_1
      END
    END;

    IF ip.transmit(r^.r)#err.ok THEN tcpbadodone(r) END

  END;
  rtimeout(s,tcprtimeout)
END tcptransmit;

PROCEDURE tcpreceive(s: SESSION; d: ip.DATA; seq: INTEGER; push: BOOLEAN);
  VAR b    : drv.BUFFER;
      l1,l2: INTEGER;
      n    : REQUEST;
BEGIN
  b:=d^.cque;
  WHILE (seq<s^.rack)&(d^.tlen>0) DO
    l1:=s^.rack-seq;
    IF l1>b^.len THEN l1:=b^.len END;
    INC(seq,l1);
    DEC(d^.tlen,l1);
    INC(b^.pos,l1);
    DEC(b^.len,l1);
    IF b^.len=0 THEN b:=b^.f END
  END;
  WHILE (s^.rque#NIL) & (d^.tlen>0) DO
    l1:=s^.rque^.len-s^.rof1;
    WHILE (l1>0)&(d^.tlen>0) DO
      l2:=b^.len;
      IF l2>l1 THEN l2:=l1 END;
      nos.move(s^.rque^.ibuf,s^.rque^.ipos+s^.rof1,b^.buf,b^.pos,l2);
      INC(b^.pos ,l2); DEC(l1,l2);
      DEC(b^.len ,l2); DEC(d^.tlen,l2);
      INC(s^.rof1,l2); INC(s^.rack,l2);
      IF b^.len=0 THEN b:=b^.f END
    END;
    IF l1=0 THEN
      s^.rof1:=0;
      n      :=s^.rque;
      n^.ilen:=n^.len;
      nos.untie(s^.rque);
      delutimer(n^.time);
      n^.done  (n^.obj )
    END
  END;
  IF (s^.rque#NIL)&push&(l1>0) THEN
    n      :=s^.rque;
    n^.ilen:=s^.rof1;
    n^.mode:=_pushm;
    s^.rof1:=0;
    nos.untie(s^.rque);
    delutimer(n^.time);
    n^.done  (n^.obj )
  END;
  IF (d^.tlen<=0)OR(s^.rwin<=0) THEN RETURN END;
  IF push&(d^.tlen<=s^.rwin) THEN
    l1:=(s^.pend+1) MOD s^.pwsz;
    IF l1=s^.pbeg THEN RETURN END;
    (*$>$T-*)
      s^.pbuf^[s^.pend]:=(s^.rend+d^.tlen-1) MOD s^.pwsz;
    (*$<*)
    s^.pend:=l1
  END;
  REPEAT
    IF s^.rend>=s^.rbeg THEN l1:=s^.rbsz-s^.rend ELSE l1:=s^.rbeg-s^.rend END;
    IF l1>d^.tlen THEN l1:=d^.tlen END;
    WHILE (l1>0)&(d^.tlen>0) DO
      l2:=b^.len;
      IF l2>l1 THEN l2:=l1 END;
      nos.move(s^.rbuf,s^.rend,b^.buf,b^.pos,l2);
      INC(b^.pos ,l2); DEC(l1,l2);
      DEC(b^.len ,l2); DEC(d^.tlen,l2);
      DEC(s^.rwin,l2); INC(s^.rack,l2);
      s^.rend:=(s^.rend+l2) MOD s^.rbsz;
      IF b^.len=0 THEN b:=b^.f END
    END
  UNTIL (d^.tlen<=0) OR (s^.rwin<=0)
END tcpreceive;

PROCEDURE tcptrandone(s: SESSION; len: INTEGER);
  VAR l: INTEGER;
BEGIN

--tty.print('tcp trandone %d bytes\n',len);

  IF s^.tran=NIL THEN

--  tty.print('tran=NIL\n');

                      RETURN END;
  IF len>0 THEN deltout(s^.time); nos.settout(s^.time,tcprtimeout) END;
  WHILE len>0 DO
    l:=s^.tran^.len-s^.tof1;
    IF l>len THEN
      INC(s^.tof1,len);
      nos.bfind(s^.tbuf,s^.tof1);
      len:=0
    ELSE
      s^.tran:=s^.tran^.f;
      s^.tof1:=0;
      IF s^.tran#NIL THEN s^.tbuf:=s^.tran^.obuf END;
      DEC(len,l)
    END
  END;

  dodone(s)

END tcptrandone;

PROCEDURE processack(s: SESSION; seq,ack,win: INTEGER; flags: BITSET): BOOLEAN;
  VAR i: INTEGER;
BEGIN
  IF flags*_ack={} THEN             RETURN FALSE END;
  IF ack>s^.tnxt   THEN acksend(s); RETURN TRUE  END;
  IF s^.tack<=ack THEN
    IF (s^.twl1<seq) OR (s^.twl1=seq)&(s^.twl2<=ack) THEN
      s^.twin:=win;
      s^.twl1:=seq;
      s^.twl2:=ack
    END
  END;
  IF s^.tack<ack THEN
    i:=ack-s^.tack;
    s^.tack:=ack;
    tcptrandone(s,i)
  END;
  RETURN TRUE
END processack;

PROCEDURE removequeue(s: SESSION; e: INTEGER): BOOLEAN;
  VAR n: REQUEST;
BEGIN
  IF (s^.rque#NIL)&(s^.rof1#0) THEN
    n:=s^.rque;
    nos.untie(s^.rque);
    n^.ilen:=s^.rof1;
    s^.rof1:=0;
    delutimer(n^.time);
    n^.done  (n^.obj )
  END;
  WHILE s^.rque#NIL DO
    n:=s^.rque;
    nos.untie(s^.rque);
    n^.ilen:=0;
    n^.res :=e;
    delutimer(n^.time);
    n^.done  (n^.obj )
  END;
  IF s^.rcnt>0 THEN RETURN FALSE END;
  WHILE s^.tdon#NIL DO
    n:=s^.tdon;
    s^.tdon:=s^.tdon^.f;
    n^.res :=e;
    delutimer(n^.time);
    n^.done  (n^.obj )
  END;
  RETURN TRUE
END removequeue;

PROCEDURE removesession(s: SESSION; res,event: INTEGER);
  VAR epro: eventproc;
      eobj: WORD;
BEGIN
  epro:=s^.epro;
  eobj:=s^.eobj;
  deltout(s^.time);
  IF    s^.stat IN {LISTEN,SYN_SENT,SYN_RECEIVED} THEN
    nos.untien(conn,s)
  ELSIF s^.stat=CLOSED THEN
    nos.untien(dead,s)
  ELSE
    nos.untien(main,s)
  END;
  IF removequeue(s,res) THEN
    nos.deallocate(s^.rbuf,(s^.rbsz+3) DIV 4);
    nos.deallocate(s^.pbuf,s^.pwsz);
    nos.deallocate(s,SIZE(s^));
    os.send(ilock);
    epro(NIL,eobj,event);
    RETURN
  END;
  s^.stat:=CLOSED;
  s^.rnid:=res;
  s^.radr:=event;
  nos.tie    (dead,s);
  nos.settout(s^.time,deadtimeout);
  os.send(ilock)
END removesession;

PROCEDURE finsession(s: SESSION);
  VAR n: REQUEST;
BEGIN
  IF (s^.rque#NIL)&(s^.rof1#0) THEN
    n:=s^.rque;
    nos.untie(s^.rque);
    n^.ilen:=s^.rof1;
    s^.rof1:=0;
    delutimer(n^.time);
    n^.done  (n^.obj )
  END;
  WHILE s^.rque#NIL DO
    n:=s^.rque;
    nos.untie(s^.rque);
    n^.ilen:=0;
    n^.res :=drv.closing;
    delutimer(n^.time);
    n^.done  (n^.obj )
  END;
  INC(s^.rack);
  acksend(s);
  os.send(ilock);
  s^.epro(s,s^.eobj,_close)
END finsession;

PROCEDURE tcpinput(d: ip.DATA);
  VAR rnid,lnid: INTEGER;
      seq , ack: INTEGER;
      win , hsz: INTEGER;
      len      : INTEGER;
      flags    : BITSET;
      b        : drv.BUFFER;
      a        : ADDRESS;
      s        : SESSION;
      ackok    : BOOLEAN;
      seqok    : BOOLEAN;
      i        : INTEGER;

PROCEDURE rstsend0(seq,ack: INTEGER; ACK: BOOLEAN);
BEGIN
  IF ACK THEN
    sendheader(s,lnid,rnid,d^.src,seq,ack,_rst+_ack)
  ELSE
    sendheader(s,lnid,rnid,d^.src,seq,ack,_rst     )
  END
END rstsend0;

PROCEDURE established(s: SESSION): BOOLEAN;
  VAR s0: SESSION;
BEGIN

  --tty.print('established\n');

  findsession(main,s0,s^.lnid,s^.rnid,s^.radr);
  IF s0#NIL THEN removesession(s,err.duplicate,_reset); RETURN FALSE END;

  s^.stat:=ESTABLISHED;
  nos.untien(conn,s);
  nos.tie   (main,s);
  delutimer    (s^.tran^.time);

--tty.print('extablished done\n');

  s^.tran^.done(s^.tran^.obj );
  s^.tran     :=s^.tran^.f;
  s^.tdon     :=s^.tdon^.f;
  s^.rwin     :=s^.rbsz;
  acksend(s);
  RETURN TRUE
END established;

BEGIN

--tty.print('tcpinput\n');


  b:=d^.cque;
  IF (b^.len<minhdrsz) OR (b^.pos MOD 4 # 0) THEN

 -- tty.print('tcp bad buffer\n');

                                                  RETURN END;

  os.wait(ilock);

  a    :=b^.buf+ b^.pos DIV 4;
  rnid :=INTEGER(BITSET(a^    )*m0FFFF);
  lnid :=INTEGER(BITSET(a^>>16)*m0FFFF); INC(a);
  seq  :=INTEGER(a^);                    INC(a);
  ack  :=INTEGER(a^);                    INC(a);
  win  :=INTEGER(BITSET(a^>>16)*m0FFFF);
  flags:=        BITSET(a^>>10)*{0..5} ;
  hsz  :=INTEGER(BITSET(a^    )*{0..3})*4;
  IF hsz#minhdrsz THEN os.send(ilock); RETURN END;

  len    :=d^.tlen-hsz;
  d^.tlen:=len;
  INC(d^.cque^.pos,hsz);
  DEC(d^.cque^.len,hsz);

--tty.print('tcpin flags=%{} seq=%d ack=%d\n',flags,seq,ack);

  findsession(main,s,lnid,rnid,d^.src);
  IF s=NIL THEN
    findlsession(s,lnid,rnid,d^.src);
    IF s=NIL THEN os.send(ilock); RETURN END;
    CASE s^.stat OF
    |LISTEN:
             IF flags*_fin#{} THEN os.send(ilock); RETURN END;
             IF flags*_rst#{} THEN os.send(ilock); RETURN END;
             IF flags*_ack#{} THEN
               rstsend0(ack,0,FALSE);
               os.send (ilock);
               RETURN
             END;
             IF flags*_syn#{} THEN
               s^.rack :=seq+1;
               s^.rwin :=win;
               s^.srnid:=s^.rnid; s^.rnid:=rnid;
               s^.sradr:=s^.radr; s^.radr:=d^.src;
               synacksend(s);
               s^.stat:=SYN_RECEIVED
             END;
             os.send(ilock);
             RETURN

    (* the processing of listen state is not yet finished
       we must continue processing in the next session state
    *)

    |SYN_SENT:
             IF flags*_fin#{} THEN os.send(ilock); RETURN END;
             IF flags*_ack#{} THEN
               ackok:=(ack>s^.tack)&(ack<=s^.tnxt);
               IF NOT ackok THEN
                 IF flags*_rst={} THEN rstsend0(ack,0,FALSE) END;
                 os.send (ilock);
                 RETURN
               END;
               s^.tack:=ack  (* the is acceptable *)
             END;
             IF flags*_rst#{} THEN
               IF ackok THEN removesession(s,drv.reset,_reset) END;
               os.send(ilock);
               RETURN
             END;

  (* security and precedence must be checked here but not implement yet  *)


  (* this step should be reached only if the ACK is ok, or thre is no ack
     and it the segment did not contain the RST                          *)

             IF flags*_syn#{} THEN
               deltout(s^.time);
               s^.rack:=seq+1;
               s^.irs :=seq  ;
               IF s^.tack>s^.iss THEN  (* our SYN has been ACKed *)
                 acksend    (s);
                 IF NOT established(s) THEN RETURN END;
               ELSE
                 synacksend (s);
                 s^.stat:=SYN_RECEIVED
               END;
             END;

             (* if there are other controls or text in the segment
                we must queue them for processing after the
                ESTABLISHED state has been reached               *)

             os.send(ilock);
             RETURN

    |SYN_RECEIVED:

             (* check sequence number *)

             ASSERT(s^.rwin=0);

             IF (len>0)OR(seq#s^.rack) THEN
               IF flags*_rst={} THEN acksend(s) END;
               os.send(ilock);
               RETURN
             END;

             IF flags*_rst#{} THEN
               IF seq=s^.rack THEN
                 IF s^.tran^.op=_waitses THEN
                   s^.stat:=LISTEN;
                   s^.rnid:=s^.srnid;
                   s^.radr:=s^.sradr
                 ELSE
                   removesession(s,drv.reset,_reset)
                 END
               END;
               os.send(ilock);
               RETURN
             END;

  (* security and precedence must be checked here but not implement yet  *)


             IF flags*_syn#{} THEN
               rstsend0(seq,0,FALSE);
               removesession(s,drv.reset,_reset);
               os.send(ilock);
               RETURN
             END;

             IF flags*_ack={} THEN
               os.send(ilock);
               RETURN
             END;
             ackok:=(ack>s^.tack)&(ack<=s^.tnxt);
             IF NOT ackok THEN
               rstsend0(ack,0,FALSE);
               os.send(ilock);
               RETURN
             END;

             s^.tack:=ack;
             IF NOT established(s) THEN RETURN END;
             IF flags*_fin#{} THEN
               s^.stat:=CLOSE_WAIT;
               finsession(s);
               RETURN
             END;

             os.send(ilock);
             RETURN

             (* if there are other controls or text in the segment
                we must queue them for processing after the
                ESTABLISHED state has been reached               *)

    ELSE
      ASSERT(FALSE)
    END
  ELSE

 --tty.print('find in main\n');

    (* check receive sequence number *)

    IF len=0 THEN
      IF s^.rwin=0 THEN
        seqok:=seq=s^.rack
      ELSE
        seqok:=(s^.rack<=seq)&(seq<s^.rack+s^.rwin)
      END
    ELSE
      seqok:=(s^.rwin>0)&(seq<s^.rack+s^.rwin)&(s^.rack<seq+len)
    END;

    IF seq>s^.rack THEN seqok:=FALSE END;

    IF NOT seqok THEN
      acksend(s);
      os.send(ilock);
      RETURN
    END;

    IF flags*_rst#{} THEN
      removesession(s,drv.reset,_reset);
      RETURN
    END;

  (* security and precedence must be checked here but not implement yet  *)

    IF flags*_syn#{} THEN
      rstsend0(seq,0,FALSE);
      removesession(s,drv.reset,_reset);
      RETURN
    END;

    IF flags*_ack={} THEN             os.send(ilock); RETURN END;
    IF ack>s^.tnxt   THEN acksend(s); os.send(ilock); RETURN END;
    IF s^.tack<=ack THEN
      IF (s^.twl1<seq) OR (s^.twl1=seq)&(s^.twl2<=ack) THEN
        s^.twin:=win;
        s^.twl1:=seq;
        s^.twl2:=ack
      END
    END;
    IF s^.tack<ack THEN
      i:=ack-s^.tack;
      s^.tack:=ack;
      tcptrandone(s,i)
    END;

    CASE s^.stat OF
    |ESTABLISHED:

              i:=s^.rack;
              tcpreceive (s,d,seq,flags*_psh#{});
              tcptransmit(s,i#s^.rack);

              IF flags*_fin#{} THEN
                s^.stat:=CLOSE_WAIT;
                finsession(s);
                RETURN
              END;

              os.send(ilock);
              RETURN

    |FIN_WAIT_1:
              IF s^.tack=s^.tnxt THEN


--tty.print('my fin acked\n');

                deltout(s^.time);
                s^.stat:=FIN_WAIT_2
              END;

              i:=s^.rack;
              tcpreceive (s,d,seq,flags*_psh#{});
              IF flags*_fin#{} THEN
                IF s^.stat=FIN_WAIT_1 THEN
                  s^.stat:=CLOSING;
                  finsession(s)
                ELSE
                  s^.stat:=TIME_WAIT;
                  deltout(s^.time);
                  nos.settout(s^.time,timeout2MSL);
                  finsession(s)
                END;
                RETURN
              END;

              IF i#s^.rack THEN acksend(s) END

    |FIN_WAIT_2:
              i:=s^.rack;
              tcpreceive (s,d,seq,flags*_psh#{});

              IF flags*_fin#{} THEN

--tty.print('get fin go to TIME_WAIT\n');

                s^.stat:=TIME_WAIT;
                deltout    (s^.time);
                nos.settout(s^.time,timeout2MSL);
                finsession(s);
                RETURN
              END;

              IF i#s^.rack THEN acksend(s) END

    |CLOSE_WAIT:

    |CLOSING   :
              IF s^.tack=s^.tnxt THEN
                deltout(s^.time);
                s^.stat:=TIME_WAIT;
                nos.settout(s^.time,timeout2MSL)
              END


    |LAST_ACK  :
              IF s^.tack=s^.tnxt THEN
                deltout(s^.time);
                removesession(s,drv.reset,_removed)
              END;
              os.send(ilock);
              RETURN

    |TIME_WAIT :
              acksend(s);
              deltout(s^.time);
              nos.settout(s^.time,timeout2MSL);
              os.send(ilock);
              RETURN
    ELSE
      ASSERT(FALSE);
    END
  END;
  os.send(ilock)
END tcpinput;

PROCEDURE tcptimeout;
  VAR m: BITSET;
      t: nos.TIME;
      s: SESSION;
BEGIN
  LOOP
    m:=di();
      LOOP
        WHILE tcptimeque=NIL DO os.wait(tcptime) END;
        os.wait(ilock);
        IF tcptimeque#NIL THEN EXIT END;
        os.send(ilock)
      END;
    ei(m);
    t:=tcptimeque;
    nos.untie(tcptimeque);
    t^.f:=NIL;
    s:=SESSION(t^.obj);
    CASE s^.stat OF
    |SYN_SENT: synsend(s);
               nos.settout(s^.time,synsendtout)
    |ESTABLISHED,
     FIN_WAIT_1 ,
     FIN_WAIT_2 :
               tcpretransmit(s);
               nos.settout(s^.time,tcprtimeout)
    |TIME_WAIT  :
               removesession(s,drv.reset,_removed);
               os.wait(ilock)
    |CLOSED     :
               removesession(s,s^.rnid,s^.radr);
               os.wait(ilock)
    ELSE
      ASSERT(FALSE)
    END;
    os.send(ilock)
  END
END tcptimeout;

PROCEDURE tcptimer(t: nos.TIME);
BEGIN
  nos.tie(tcptimeque,t);
  IF tcptime.queue#os.null THEN os.send(tcptime) END
END tcptimer;

PROCEDURE usertimeout;
  VAR m: BITSET;
      t: nos.TIME;
      s: SESSION;
BEGIN
  LOOP
    m:=di();
      LOOP
        WHILE usertimeque=NIL DO os.wait(usertime) END;
        os.wait(ilock);
        IF usertimeque#NIL THEN EXIT END;
        os.send(ilock)
      END;
    ei(m);
    t:=usertimeque;
    nos.untie(usertimeque);
    t^.f:=NIL;
    s:=SESSION(t^.obj);
    IF NOT (s^.stat IN {LISTEN,CLOSED}) THEN rstsend(s) END;
    removesession(s,err.time_out,_timeout)
  END
END usertimeout;

PROCEDURE newsession(VAR r: request; VAR s: SESSION);
  VAR n,h: INTEGER;
BEGIN
  s:=NIL;
  IF (r.ibsz <=0)OR(r.ibsz >0FFFFh)    THEN r.res:=err.bad_parm ; RETURN END;
  IF (r.ipbsz<=0)OR(r.ipbsz>0FFFFh)    THEN r.res:=err.bad_parm ; RETURN END;
  IF NOT ip.unpackadr(r.radr,n,h)    THEN r.res:=err.bad_parm ; RETURN END;
  nos.allocate(s,SIZE(s^));
  IF s=NIL                         THEN r.res:=err.no_memory; RETURN END;
  nos.allocate(s^.rbuf,(r.ibsz+3) DIV 4);
  IF s^.rbuf=NIL THEN
    nos.deallocate(s,SIZE(s^));         r.res:=err.no_memory; RETURN
  END;
  nos.allocate(s^.pbuf,r.ipbsz+1);
  IF s^.rbuf=NIL THEN
    nos.deallocate(s      ,SIZE(s^)       );
    nos.deallocate(s^.rbuf,(r.ibsz+3) DIV 4);
    r.res:=err.no_memory;
    RETURN
  END;
  WITH s^ DO
    epro:=r.event;
    eobj:=r.eobj;
    rbsz:=r.ibsz;
    pwsz:=r.ipbsz+1;
    rbeg:=0; pbeg:=0;
    rend:=0; pend:=0;
    ctra:=NIL;
    rque:=NIL;
    tpsz:=maxsgmsz;
    rcnt:=0;
    tack:=os.timer*os.tick*2;
    tnxt:=tack;
    iss :=tack;
    twl1:=0;
    twl2:=tack;
    irs :=0;
    tof1:=0;
    tof2:=0;
    rof1:=0;
    rwin:=0;
    twin:=0;
    lnid:=r.lnid;
    rnid:=r.rnid;
    radr:=r.radr;
    time.tout:=-1;
    time.f   :=NIL;
    time.done:=tcptimer;
    time.obj :=s;
    tran     :=SYSTEM.ADR(r);
    tran^.f  :=NIL;
    tran^.b  :=s^.tran;
    tdon     :=s^.tran
  END
END newsession;

PROCEDURE waitsession(VAR r: request);
  VAR s: SESSION;
BEGIN
  IF r.lnid=0 THEN r.res:=err.bad_parm; RETURN END;
  newsession(r,s);
  IF s=NIL THEN RETURN END;
  r.sid  :=s;
  s^.stat:=LISTEN;
  os.wait(ilock);
    setutimer(s,r);
    nos.tie(conn,s);
  os.send(ilock)
END waitsession;

PROCEDURE callsession(VAR r: request);
  VAR s: SESSION;
BEGIN
  IF (r.radr=0) OR (r.lnid=0) OR (r.rnid=0) THEN r.res:=err.bad_parm; RETURN END;
  findsession(main,s,r.lnid,r.rnid,r.radr);
  IF s#NIL THEN r.res:=err.duplicate; RETURN END;
  newsession(r,s);
  IF s=NIL THEN RETURN END;
  r.rcnt :=0;
  r.rm   :=FALSE;
  r.sid  :=s;
  s^.stat:=SYN_SENT;
  os.wait(ilock);
    nos.tie(conn,s);
    synsend(s);
    nos.settout(s^.time,synsendtout);
    setutimer(s,r);
  os.send(ilock)
END callsession;

PROCEDURE closesession(VAR r: request);
  VAR s: SESSION;
BEGIN
  s:=r.sid;
  IF s=NIL    THEN r.res:=err.bad_parm; RETURN END;
  os.wait(ilock);
    r.f  :=NIL;
    r.len:=1;
    CASE s^.stat OF
    |LISTEN    ,SYN_SENT  : removesession(s,drv.closing,_reset);
                            RETURN
    |FIN_WAIT_1,FIN_WAIT_2,
     CLOSING,LAST_ACK,
     TIME_WAIT            : r.res:=drv.closing;
                            os.send(ilock);
                            RETURN
    ELSE
    END;
    IF (s^.tdon#NIL)&(s^.tdon^.b^.op=_closeses) THEN
      r.res:=drv.closing;
      os.send(ilock);
      RETURN
    END;
    IF s^.ctra=NIL THEN s^.tof2:=0; s^.ctra:=SYSTEM.ADR(r) END;
    IF s^.tran=NIL THEN s^.tof1:=0; s^.tran:=SYSTEM.ADR(r) END;
    IF s^.tdon=NIL THEN
      s^.tdon:=SYSTEM.ADR(r);
      s^.tdon^.b:=s^.tdon
    ELSE
      r.b:=s^.tdon^.b;
      s^.tdon^.b^.f:=SYSTEM.ADR(r);
      s^.tdon^.b   :=SYSTEM.ADR(r)
    END;
    tcptransmit(s,FALSE);
    setutimer(s,r);
  os.send(ilock)
END closesession;

PROCEDURE send(VAR r: request);
  VAR s: SESSION;
BEGIN
  s:=r.sid;
  IF s=NIL    THEN r.res:=err.bad_parm; RETURN END;
  IF r.len<=0 THEN r.res:=err.bad_parm; RETURN END;
  r.rm  :=FALSE;
  r.rcnt:=0;
  os.wait(ilock);
    IF NOT(s^.stat IN {ESTABLISHED}) THEN
      os.send(ilock);
      r.res:=err.bad_parm;
      RETURN
    END;
    r.f:=NIL;
    IF s^.ctra=NIL THEN s^.tof2:=0; s^.ctra:=SYSTEM.ADR(r); s^.cbuf:=r.obuf END;
    IF s^.tran=NIL THEN s^.tof1:=0; s^.tran:=SYSTEM.ADR(r); s^.tbuf:=r.obuf END;
    IF s^.tdon=NIL THEN
      s^.tdon:=SYSTEM.ADR(r);
      s^.tdon^.b:=s^.tdon
    ELSE
      r.b:=s^.tdon^.b;
      s^.tdon^.b^.f:=SYSTEM.ADR(r);
      s^.tdon^.b   :=SYSTEM.ADR(r)
    END;
    setutimer(s,r);
    tcptransmit(s,FALSE);
  os.send(ilock)
END send;

PROCEDURE receive(VAR r: request);
  VAR s        : SESSION;
      l,len,pos: INTEGER;
      rlen,lwin: INTEGER;
BEGIN
  s:=r.sid;
  IF s=NIL    THEN r.res:=err.bad_parm; RETURN END;
  IF r.len<=0 THEN r.res:=err.bad_parm; RETURN END;
  os.wait(ilock);
    r.mode:={};
    IF s^.rque#NIL THEN
      nos.tie(s^.rque,SYSTEM.ADR(r));
      setutimer(s,r);
    ELSE
      lwin:=s^.rwin;
      len :=r.len;
      rlen:=len;
      pos :=r.ipos;
      IF s^.pbeg#s^.pend THEN
        (*$>$T-*)
          l:=s^.pbuf^[s^.pbeg];
        (*$<*)
        IF l>=s^.rbeg THEN l:=l-s^.rbeg+1 ELSE l:=s^.rbsz-s^.rbeg+l+1 END;
        IF l<=rlen    THEN
          s^.pbeg:=(s^.pbeg+1) MOD s^.pwsz;
          rlen   :=len;
          r.mode :=_pushm
        END
      END;
      WHILE ((s^.rwin=0) OR (s^.rbeg#s^.rend)) & (len>0) DO
        IF s^.rbeg>=s^.rend THEN
          l:=s^.rbsz-s^.rbeg
        ELSE
          l:=s^.rend-s^.rbeg
        END;
        IF l>len THEN l:=len END;
        nos.move(r.ibuf,pos,s^.rbuf,s^.rbeg,l);
        INC(s^.rwin,l);
        s^.rbeg:=(s^.rbeg+l) MOD s^.rbsz;
        INC(pos,l);
        DEC(len,l)
      END;
      IF len=0 THEN
        r.ilen:=rlen;
        r.done(r.obj)
      ELSE
        nos.tie(s^.rque,SYSTEM.ADR(r));
        s^.rof1:=pos-r.ipos;
        setutimer(s,r)
      END;
      IF (lwin=0)&(s^.rwin#0) THEN tcptransmit(s,TRUE) END
    END;
  os.send(ilock)
END receive;

PROCEDURE doio(VAR r: request);
BEGIN
  r.res:=err.ok;
  CASE r.op OF
    |_waitses : waitsession (r)
    |_callses : callsession (r)
    |_closeses: closesession(r)
    |_send    : send        (r)
    |_receive : receive     (r)
  ELSE
    r.res:=err.inv_op
  END
END doio;

PROCEDURE install;
BEGIN
  IF ip.install(ip.TCP,tcpinput)#err.ok THEN HALT(1) END
END install;

PROCEDURE tcpstop;
BEGIN
  os.stop(tcptp ,TRUE);
  os.stop(usertp,TRUE)
END tcpstop;

PROCEDURE init;
  VAR i: INTEGER;
BEGIN
  main       :=NIL;
  conn       :=NIL;
  dead       :=NIL;
  freer      :=NIL;
  tcptimeque :=NIL;
  usertimeque:=NIL;
  os.ini_signal(freersg ,os.sendup,0);
  os.ini_signal(ilock   ,os.sendup,1);
  os.ini_signal(tcptime ,{}       ,0);
  os.ini_signal(usertime,{}       ,0);
  FOR i:=0 TO HIGH(rarr) DO
    rarr[i].next:=freer; freer:=SYSTEM.ADR(rarr[i]);
    freer^.r.obj :=freer;
    freer^.r.done:=tcpodone;
    freer^.r.prot:=ip.TCP;
    freer^.r.dntf:=FALSE;
    freer^.r.ttl :=tcpottl;
    freer^.r.tout:=tcpotimeout;
    WITH freer^.r.buf DO
      buf:=SYSTEM.ADR(freer^.h);
      pos:=0;
      len:=20
    END
  END;

  IF os.make_process(tcptp , tcptimeout,512)#err.ok THEN HALT(1) END;
  IF os.make_process(usertp,usertimeout,512)#err.ok THEN HALT(1) END;
  IF os.final(os.self(),tcpstop)       #err.ok THEN HALT(1) END;
  os.start( tcptp);
  os.start(usertp);
  install
END init;

BEGIN
  m0FFFF:={0..15};
  init
END tcp.
