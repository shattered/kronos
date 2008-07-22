IMPLEMENTATION MODULE rpc; (* $N+ $U+ Igo 28-Nov-91. (c) KRONOS *)

(* Remote Procedure Call *)

IMPORT  SYSTEM;
IMPORT  cod: defCodes;
IMPORT  os : osKernel;
IMPORT  nos: netKernel;
IMPORT  err: defErrors;
IMPORT  drv: netdrv;
IMPORT   ip;
IMPORT  udp;

--IMPORT  tty: Terminal;
--IMPORT  key: Keyboard;


PROCEDURE di(): BITSET;  CODE cod.getm cod.copt 3 cod.bic cod.setm END di;
PROCEDURE ei(m: BITSET); CODE cod.setm END ei;

CONST MAGIC = 531h;

------------------------------ XDR -----------------------------
                              -----

-- auth_flavor

 CONST AUTH_NULL  = 0;
       AUTH_UNIX  = 1;
       AUTH_SHORT = 2;
       AUTH_DES   = 3;

(*
  struct opaque_auth
    {
      auth_flavor flavor;
      opaque      body<400>;
    }
*)

-- msg_type

CONST CALL  = 0;
      REPLY = 1;

-- auth_stat

CONST AUTH_BADCRED      = 1; -- bad credentials (seal broken)
      AUTH_REJECTEDCRED = 2; -- client must begin new session
      AUTH_BADVERF      = 3; -- bad verifier (seal broken)
      AUTH_REJECTEDVERF = 4; -- verifier expired or replayed
      AUTH_TOOWEAK      = 5; -- rejected for security reasons

(* struct rpc_msg
     {
       unsigned int xid;
       union switch (msg_type mtype)
         {
           case CALL:
             call_body cbody;
           case REPLY:
             reply_body rbody;
         } body;
     }

   struct call_body
     {
       unsigned int rpcvers; -- must be equal two
       unsigned int prog;
       unsigned int vers;
       unsigned int proc;
       opque_auth cred;
       opque_auth verf;

     procedure specific parameters start here

    }
*)
----------------------------------------------------------------


TYPE inpTRANS = POINTER TO inptrans;
     inptrans = RECORD
                  f,b : inpTRANS;

                  xid : INTEGER;        (* transactio ident for reply        *)
                  port: INTEGER;        (* udp port         for reply        *)
                  iadr: INTEGER;        (* internet address for reply        *)
                  proc: INTEGER;        (* rpc procedure for transaction     *)
                  vers: INTEGER;        (* program version                   *)
                  parm: DYNARR OF WORD; (* input parameters body             *)
                  wsz : INTEGER;        (* word size of memory allocated for
                                           input transaction                 *)
                END;

     PORT     = POINTER TO port;
     port     = RECORD
                  f,b  : PORT;
                  magic: INTEGER;
                  port : INTEGER;          (* udp port                  *)
                  prog : INTEGER;          (* rpc program               *)
                  vlow : INTEGER;          (* lowest  supported version *)
                  vhigh: INTEGER;          (* highest supported version *)
                  nproc: INTEGER;
                  doit : DOIT;
                  obj  : WORD;

                  prs  : os.process;       (* caller process            *)
                  trans: inpTRANS;         (* transactions for process  *)
                  call : os.signal_rec;    (* call exist for processing *)

                  rbody: DYNARR OF WORD;   (* reply body                *)
                  xid  : INTEGER;          (* awaited transaction ident *)
                  aport: INTEGER;          (* awaited udp port          *)
                  aiadr: INTEGER;          (* awaited ip  address       *)
                  reply: os.signal_rec;    (* transaction ready         *)
                  rlock: os.signal_rec;
                  astat: INTEGER;
                  rstat: INTEGER;
                  rres : INTEGER;
                  rlow : INTEGER;
                  rhigh: INTEGER;

                  plock: os.signal_rec;    (* port lock                 *)
                END;

VAR ports: PORT;       (* all installed ports *)

TYPE READER = POINTER TO reader;
     reader = RECORD
                d : ip .DATA;   (* input datagramm       *)
                b : drv.BUFFER; (* current buffer        *)
                l : INTEGER;    (* datagramm length      *)
                p : PORT;       (* port to input         *)
                sa: INTEGER;    (* source ip address     *)
                sp: INTEGER;    (* source udp port       *)
                sx: INTEGER     (* source transaction id *)
              END;

     cREQUEST = POINTER TO crequest;
     crequest = RECORD
                  r   : udp.request;
                  hdr : ARRAY [0..9] OF WORD;
                  hsz : INTEGER;
                  done: os.signal_rec
                END;

     rREQUEST = POINTER TO rrequest;
     rrequest = RECORD
                  r   : udp.request;
                  hdr : ARRAY [0..7] OF WORD;
                  hsz : INTEGER;
                  buf : buffer;
                  dbuf: nos.buffer
                END;

CONST _replytout  = 200;
      _sendtout   = 200;

PROCEDURE calldone(r: cREQUEST);
BEGIN

--tty.print('calldone res=%h\n',r^.r.res);

  os.send(r^.done)
END calldone;

PROCEDURE replydone(r: rREQUEST);
BEGIN
--tty.print('reply done res=%h\n',r^.r.res);

  IF r^.buf.sz>0 THEN
    nos.deallocate(r^.buf.adr,r^.buf.sz);
  END;
  nos.deallocate(r,SIZE(r^))
END replydone;

PROCEDURE sendrrequest(r: rREQUEST; ia,dup,sup: INTEGER);
  VAR res: INTEGER;
BEGIN
  WITH r^.r DO
    dst :=ia;
    dprt:=dup;
    sprt:=sup;
    tout:=_replytout;
    done:=replydone
  END;
  WITH r^.r.buf DO
    buf :=SYSTEM.ADR(r^.hdr);
    pos :=0;
    len :=r^.hsz*4;
    next:=SYSTEM.ADR(r^.dbuf)
  END;
  WITH r^.dbuf DO
    buf :=r^.buf.adr;
    pos :=0;
    len :=r^.buf.sz*4;
    next:=NIL
  END;
  r^.r.len:=(r^.hsz+r^.buf.sz)*4;
  r^.r.obj:=r;
  res:=udp.transmit(r^.r);
  IF res#err.ok THEN replydone(r^.r.obj) END
END sendrrequest;

PROCEDURE sendcrequest(VAR r: crequest; ia,dup,sup,to: INTEGER;
                       VAR b: nos.buffer; len: INTEGER): INTEGER;
BEGIN
  WITH r.r DO
    dst :=ia;
    dprt:=dup;
    sprt:=sup;
    tout:=to;
    done:=calldone
  END;
  WITH r.r.buf DO
    buf :=SYSTEM.ADR(r.hdr);
    pos :=0;
    len :=r.hsz*4;
    next:=SYSTEM.ADR(b)
  END;
  r.r.obj:=SYSTEM.ADR(r);
  r.r.len:=r.hsz*4+len;
  RETURN udp.transmit(r.r)
END sendcrequest;

PROCEDURE writenum(VAR s: ARRAY OF WORD; VAR p: INTEGER; n: INTEGER);
BEGIN
  ASSERT(p<=HIGH(s));
  s[p]:=((BITSET(n)*{00..07})>>08)+
        ((BITSET(n)*{08..15})<<08)+
        ((BITSET(n)*{16..23})>>08)+
        ((BITSET(n)*{23..31})<<08);
  INC(p)
END writenum;

PROCEDURE putnum(VAR s: ARRAY OF WORD; VAR p: INTEGER; n: INTEGER);
BEGIN
  s[p]:=((BITSET(n)*{00..07})>>08)+
        ((BITSET(n)*{08..15})<<08)+
        ((BITSET(n)*{16..23})>>08)+
        ((BITSET(n)*{23..31})<<08);
  INC(p)
END putnum;

PROCEDURE writeshort(VAR s: ARRAY OF WORD; VAR p: INTEGER; n: INTEGER);
BEGIN
  ASSERT(p<=HIGH(s));
  s[p]:=((BITSET(n)*{00..07})>>08);
  INC(p);
END writeshort;

PROCEDURE putshort(VAR s: ARRAY OF WORD; VAR p: INTEGER; n: INTEGER);
BEGIN
  s[p]:=((BITSET(n)*{00..07})>>08);
  INC(p)
END putshort;

PROCEDURE putverf(VAR s: ARRAY OF WORD; VAR p: INTEGER);
BEGIN
  putshort(s,p,AUTH_NULL);
  s[p]:=0; INC(p)
END putverf;

PROCEDURE sendbadreply(VAR r: reader; s,e,ae: INTEGER);
  VAR rq : rREQUEST;
      pos: INTEGER;
BEGIN
  pos:=0;
  nos.allocate(rq,SIZE(rq^));
  IF rq=NIL THEN RETURN END;
  rq^.buf.adr:=NIL;
  rq^.buf.sz :=  0;
  putnum  (rq^.hdr,pos, r.sx);
  putshort(rq^.hdr,pos,REPLY);
  putshort(rq^.hdr,pos,    s);
  CASE s OF
  |MSG_ACCEPTED:
    putverf (rq^.hdr,pos  );
    putshort(rq^.hdr,pos,s);
    CASE e OF
    |PROG_MISMATCH: putnum(rq^.hdr,pos,r.p^.vlow );
                    putnum(rq^.hdr,pos,r.p^.vhigh);
    |PROG_UNAVAIL,
     PROC_UNAVAIL,
     GARBAGE_ARGS:
    END
  |MSG_DENIED  :
    putshort(rq^.hdr,pos,s);
    CASE e OF
    |RPC_MISMATCH: putshort(rq^.hdr,pos,2);
                   putshort(rq^.hdr,pos,2);
    |AUTH_ERROR  : ASSERT(ae IN {AUTH_BADCRED,
                                 AUTH_REJECTEDCRED,
                                 AUTH_BADVERF,
                                 AUTH_REJECTEDVERF,
                                 AUTH_TOOWEAK
                                }
                          );
                   putshort(rq^.hdr,pos,ae)
    END
  END;
  rq^.hsz:=pos;
  sendrrequest(rq,r.sa,r.sp,r.p^.port)
END sendbadreply;

PROCEDURE garbageparm(VAR r: reader);
BEGIN
  sendbadreply(r,MSG_ACCEPTED,GARBAGE_ARGS,0)
END garbageparm;

PROCEDURE badauth(VAR r: reader);
BEGIN
  sendbadreply(r,MSG_DENIED,AUTH_ERROR,AUTH_BADCRED)
END badauth;

PROCEDURE badrpcvers(VAR r: reader);
BEGIN
  sendbadreply(r,MSG_DENIED,RPC_MISMATCH,0)
END badrpcvers;

PROCEDURE badprog(VAR r: reader);
BEGIN
  sendbadreply(r,MSG_ACCEPTED,PROG_UNAVAIL,0)
END badprog;

PROCEDURE badprogvers(VAR r: reader);
BEGIN
  sendbadreply(r,MSG_ACCEPTED,PROG_MISMATCH,0)
END badprogvers;

PROCEDURE badproc(VAR r: reader);
BEGIN
  sendbadreply(r,MSG_ACCEPTED,PROC_UNAVAIL,0)
END badproc;

PROCEDURE getnum(VAR r: reader; VAR i: INTEGER): BOOLEAN;
  VAR a: POINTER TO ARRAY [0..0] OF CHAR;
      p: INTEGER;
BEGIN
  IF r.l<4      THEN RETURN FALSE END;
  IF r.b^.len =0 THEN
    r.b:=r.b^.f;
    IF r.b^.len MOD 4 # 0 THEN RETURN FALSE END;
    IF r.b^.pos MOD 4 # 0 THEN RETURN FALSE END
  END;
  a:=r.b^.buf;
  p:=r.b^.pos;

(*$<$T-*)

  i:=INTEGER(
     BITSET(a^[p+3]<<00)+
     BITSET(a^[p+2]<<08)+
     BITSET(a^[p+1]<<16)+
     BITSET(a^[p+0]<<24));

(*$>*)

  INC(r.b^.pos,4);
  DEC(r.b^.len,4);
  DEC(r.l     ,4);
  RETURN TRUE
END getnum;

PROCEDURE readnum(VAR s: ARRAY OF WORD; VAR p,n: INTEGER): BOOLEAN;
  VAR w: BITSET;
BEGIN
  IF p>HIGH(s) THEN RETURN FALSE END;
  w:=BITSET(s[p]);
  n:=INTEGER((w*{00..07})>>08+
             (w*{08..15})<<08+
             (w*{16..23})>>08+
             (w*{24..31})<<08
            );
  INC(p);
  RETURN TRUE
END readnum;

PROCEDURE skipopaque(VAR r: reader; f: BOOLEAN): BOOLEAN;
  VAR len,l: INTEGER;
BEGIN
  IF NOT getnum(r,len) THEN RETURN FALSE END;
  IF len<0             THEN IF f THEN garbageparm(r) END; RETURN FALSE END;
  len:=(len+3) DIV 4 * 4;
  IF r.l<len THEN IF f THEN garbageparm(r) END; RETURN FALSE END;
  WHILE len>0 DO
    IF len<=r.b^.len THEN
      INC(r.b^.pos,len);
      DEC(r.b^.len,len);
      DEC(r.l     ,len);
      len:=0
    ELSE
      DEC(len,r.b^.len);
      DEC(r.l,r.b^.len);
      r.b^.len:=0;
      r.b:=r.b^.f;
      IF r.b^.len MOD 4 # 0 THEN IF f THEN garbageparm(r) END; RETURN FALSE END;
      IF r.b^.pos MOD 4 # 0 THEN IF f THEN garbageparm(r) END; RETURN FALSE END
    END
  END;
  RETURN TRUE
END skipopaque;

PROCEDURE checkauth(VAR r: reader): BOOLEAN;
  VAR fl: INTEGER;
BEGIN
  IF NOT getnum(r,fl)        THEN RETURN FALSE END;
  IF NOT (fl IN {AUTH_NULL}) THEN badauth(r); RETURN FALSE END;
  IF NOT skipopaque(r,FALSE) THEN RETURN FALSE END;
  RETURN TRUE
END checkauth;

PROCEDURE copyparm(VAR p: ARRAY OF WORD; VAR r: reader): BOOLEAN;
  VAR l,pos: INTEGER;
BEGIN
  ASSERT(r.l DIV 4 - 1 = HIGH(p));
  pos:=0;
  WHILE r.l>0 DO
    IF r.b^.len=0 THEN
      r.b:=r.b^.f;
      IF r.b^.len MOD 4 # 0 THEN garbageparm(r); RETURN FALSE END;
      IF r.b^.pos MOD 4 # 0 THEN garbageparm(r); RETURN FALSE END
    END;
    IF r.b^.len>r.l THEN l:=r.l ELSE l:=r.b^.len END;
    nos.move(SYSTEM.ADR(p),pos,r.b^.buf,r.b^.pos,l);
    INC(pos,l);
    DEC(r.l,l);
    INC(r.b^.pos,l);
    DEC(r.b^.len,l)
  END;
  RETURN TRUE
END copyparm;

PROCEDURE inputcall(VAR r: reader);
  VAR proc: INTEGER;
      vers: INTEGER;
      t   : inpTRANS;
      psz : INTEGER;
BEGIN

--tty.print('input call\n');

  IF NOT getnum(r,vers) THEN RETURN END;
  IF vers#2             THEN badrpcvers (r); RETURN END;
  IF NOT getnum(r,vers) THEN RETURN END;
  IF vers#r.p^.prog     THEN badprog    (r); RETURN END;
  IF NOT getnum(r,vers) THEN RETURN END;
  IF vers<r.p^.vlow     THEN badprogvers(r); RETURN END;
  IF vers>r.p^.vhigh    THEN badprogvers(r); RETURN END;
  IF NOT getnum(r,proc) THEN RETURN END;
  IF proc>=r.p^.nproc   THEN badproc    (r); RETURN END;
  IF NOT checkauth(r)   THEN RETURN END;
  IF NOT checkauth(r)   THEN RETURN END;

--tty.print('good call psz=%d\n',r.l);
--RETURN ;

  ASSERT(r.l MOD 4 = 0);
  psz:=r.l DIV 4;
  nos.allocate(t,SIZE(t^) + psz);
  IF t=NIL THEN RETURN END;
  t^.xid :=r.sx;
  t^.port:=r.sp;
  t^.iadr:=r.sa;
  t^.vers:=vers;
  t^.proc:=proc;
  t^.wsz :=SIZE(t^)+psz;
  IF psz=0 THEN
    NEW(t^.parm)
  ELSE
    t^.parm^.ADR :=SYSTEM.ADR(t^)+SIZE(t^);
    t^.parm^.HIGH:=psz-1;
    IF NOT copyparm(t^.parm,r) THEN nos.deallocate(t,t^.wsz); RETURN END
  END;
  nos.tie(r.p^.trans,t);
  os.send(r.p^.call)

--;tty.print('call\n');

END inputcall;

PROCEDURE inputreply(VAR r: reader);
  VAR m: BITSET;
BEGIN
  IF r.p^.reply.queue=os.null THEN RETURN END;
  IF r.p^.xid#r.sx            THEN RETURN END;
  IF r.p^.aiadr#r.sa          THEN RETURN END;
  IF r.p^.aport#r.sp          THEN RETURN END;
  IF NOT getnum(r,r.p^.rstat) THEN RETURN END;
  r.p^.rbody^.ADR:=NIL;
  IF    r.p^.rstat=MSG_ACCEPTED THEN
    IF NOT checkauth(r)        THEN RETURN END;
    IF NOT getnum(r,r.p^.rres) THEN RETURN END;
    CASE r.p^.rres OF
    |SUCCESS      :
      ASSERT(r.l MOD 4 = 0);
      IF r.l>0 THEN
        nos.allocate(r.p^.rbody^.ADR,r.l DIV 4);
        IF r.p^.rbody^.ADR=NIL THEN RETURN END;
        r.p^.rbody^.HIGH:=r.l DIV 4 - 1;
        IF NOT copyparm(r.p^.rbody,r) THEN
          nos.deallocate(r.p^.rbody^.ADR,r.p^.rbody^.HIGH+1);
          r.p^.rbody^.HIGH:=-1;
          RETURN
        END
      ELSE
        r.p^.rbody^.ADR :=NIL;
        r.p^.rbody^.HIGH:=-1
      END
    |PROG_MISMATCH:
      IF NOT getnum(r,r.p^.rlow ) THEN RETURN END;
      IF NOT getnum(r,r.p^.rhigh) THEN RETURN END;
    |PROG_UNAVAIL,
     PROC_UNAVAIL,
     GARBAGE_ARGS:
    ELSE
      RETURN
    END;
    os.send(r.p^.reply)

--;tty.print('reply\n');

  ELSIF r.p^.rstat=MSG_DENIED   THEN
    IF NOT getnum(r,r.p^.rres) THEN RETURN END;
    CASE r.p^.rres OF
    |RPC_MISMATCH:
      IF NOT getnum(r,r.p^.rlow ) THEN RETURN END;
      IF NOT getnum(r,r.p^.rhigh) THEN RETURN END;
    |AUTH_ERROR:
      IF NOT getnum(r,r.p^.astat) THEN RETURN END;
      IF NOT ( r.p^.astat IN {AUTH_BADCRED,
                              AUTH_REJECTEDCRED,
                              AUTH_BADVERF,
                              AUTH_REJECTEDVERF,
                              AUTH_TOOWEAK}     ) THEN RETURN END;

    ELSE
      RETURN
    END;
    os.send(r.p^.reply)

--;tty.print('reply\n');

  ELSE
    RETURN
  END
END inputreply;

PROCEDURE rpcinput(d: ip.DATA; p: PORT; sa,sp,len: INTEGER);
  VAR r  : reader;
      msg: INTEGER;
BEGIN

  --tty.print('rpc input \n');

  r.b :=d^.cque;  r.l :=len;
  r.d :=d;        r.sa:=sa;
  r.sp:=sp;       r.p :=p;
  IF r.b^.pos MOD 4 # 0 THEN RETURN END;
  IF r.b^.len MOD 4 # 0 THEN RETURN END;
  IF r.l      MOD 4 # 0 THEN RETURN END;
  IF NOT getnum(r,r.sx) THEN RETURN END;
  IF NOT getnum(r,msg ) THEN RETURN END;
  IF    msg=CALL  THEN inputcall (r)
  ELSIF msg=REPLY THEN
    os.wait(r.p^.rlock);
      inputreply(r);
    os.send(r.p^.rlock)
  ELSE
    RETURN
  END
END rpcinput;

VAR callerstarted: os.signal_rec;
    portforcaller: PORT;

PROCEDURE caller;
  VAR p: PORT;
      t: inpTRANS;
BEGIN
  p:=portforcaller;
  os.send(callerstarted);
  LOOP
    os.wait(p^.call);
    t:=p^.trans;
    p^.doit(p^.obj,t^.proc,t^.vers,t^.parm);
    nos.untie(p^.trans);
    nos.deallocate(t,t^.wsz)
  END
END caller;

PROCEDURE call   (VAR a: rpcadr; p: PORT; VAR pb: nos.buffer; len: INTEGER;
                  VAR rb: buffer): INTEGER;

  PROCEDURE u;
  BEGIN
    INC(p^.xid);
    os.send(p^.rlock);
    os.send(p^.plock)
  END u;

  VAR r  : crequest;
      ttm: INTEGER;
      to : INTEGER;
      i  : INTEGER;
      m  : BITSET;
      brk: os.signal_rec;

  PROCEDURE sendcall(): INTEGER;
    VAR pos: INTEGER;
  BEGIN
    pos:=0;
    putnum  (r.hdr,pos,p^.xid);
    putshort(r.hdr,pos, CALL );
    putshort(r.hdr,pos,  2   );
    putnum  (r.hdr,pos,a.prog);
    putnum  (r.hdr,pos,a.vers);
    putnum  (r.hdr,pos,a.proc);
    putverf (r.hdr,pos);
    putverf (r.hdr,pos);
    p^.aiadr:=a.iadr;
    p^.aport:=a.port;
    r.hsz   :=pos;
    os.ini_signal(r.done,os.break,0);
    RETURN sendcrequest(r,a.iadr,a.port,p^.port,to,pb,len)
  END sendcall;

BEGIN

  IF p^.magic#MAGIC THEN RETURN err.bad_desc END;

  ttm:=os.timer+a.tout;
  i:=os.wait_del(a.tout,p^.plock);
  IF i>0 THEN RETURN err.ipted_op END;
  IF i<0 THEN RETURN err.time_out END;
  os.ini_signal(brk,os.break,1);
  os.wait(p^.rlock);
  LOOP
    to:=_sendtout;
    IF a.tout>=0 THEN
      to:=ttm-os.timer;
      IF to<=0 THEN u; RETURN err.time_out END
    END;
    IF sendcall()=err.ok THEN
      os.wait(r.done);
      IF r.r.res=err.ok THEN
        IF a.tout>=0 THEN
          to:=ttm-os.timer;
          IF to<=0 THEN u; RETURN err.time_out END;
          IF to>_replytout THEN to:=_replytout END
        ELSE
          to:=_replytout
        END;
        m:=di();
          os.send(p^.rlock);
          i:=os.wait_del(to,p^.reply);
        ei(m);
        IF i>0 THEN
          os.wait(p^.rlock);
          p^.reply.cou:=0;
          IF p^.rbody^.ADR#NIL THEN
            nos.deallocate(p^.rbody^.ADR,p^.rbody^.HIGH+1)
          END;
          u; RETURN err.ipted_op
        END;
        IF i=0 THEN
          rb.adr :=p^.rbody^.ADR   ; p^.rbody^.ADR :=NIL;
          rb.sz  :=p^.rbody^.HIGH+1; p^.rbody^.HIGH:=-1;
          a.stat :=p^.rstat;
          a.res  :=p^.rres;
          INC(p^.xid);
          os.send(p^.plock);
          RETURN err.ok
        ELSE
          os.wait(p^.rlock);
          IF p^.reply.cou=1 THEN
            os.wait(p^.reply);
            rb.adr :=p^.rbody^.ADR   ; p^.rbody^.ADR :=NIL;
            rb.sz  :=p^.rbody^.HIGH+1; p^.rbody^.HIGH:=-1;
            a.stat :=p^.rstat;
            a.res  :=p^.rres;
            u; RETURN err.ok
          END
        END
      END
    END;

    IF os.wait_del(-1,brk)>0 THEN u; RETURN err.ipted_op END;
    os.send(brk)

  END
END call;

PROCEDURE reply  (p: PORT; VAR pb: buffer);
  VAR rq : rREQUEST;
      pos: INTEGER;
BEGIN
  nos.allocate(rq,SIZE(rq^));
  IF rq=NIL THEN RETURN END;
  pos:=0;
  putnum  (rq^.hdr,pos,p^.trans^.xid);
  putshort(rq^.hdr,pos,REPLY);
  putshort(rq^.hdr,pos,MSG_ACCEPTED);
  putverf (rq^.hdr,pos);
  putshort(rq^.hdr,pos,SUCCESS);
  rq^.hsz:=pos;
  rq^.buf:=pb;
  sendrrequest(rq,p^.trans^.iadr,p^.trans^.port,p^.port)
END reply;

PROCEDURE garbageargs(p: PORT);
  VAR rq : rREQUEST;
      pos: INTEGER;
BEGIN
  nos.allocate(rq,SIZE(rq^));
  IF rq=NIL THEN RETURN END;
  pos:=0;
  putnum  (rq^.hdr,pos,p^.trans^.xid);
  putshort(rq^.hdr,pos,REPLY);
  putshort(rq^.hdr,pos,MSG_ACCEPTED);
  putverf (rq^.hdr,pos);
  putshort(rq^.hdr,pos,GARBAGE_ARGS);
  rq^.hsz:=pos;
  rq^.buf.adr:=NIL;
  rq^.buf.sz :=0;
  sendrrequest(rq,p^.trans^.iadr,p^.trans^.port,p^.port)
END garbageargs;

PROCEDURE create(VAR p: PORT; VAR ins: install): INTEGER;

  PROCEDURE u;
  BEGIN
    nos.deallocate(p,SIZE(p^));
    os.unlock
  END u;

  PROCEDURE inisignals;
  BEGIN
    os.ini_signal(p^.call ,{}                ,0);
    os.ini_signal(p^.reply,os.sendup+os.break,0);
    os.ini_signal(p^.plock,os.guard +os.break,1);
    os.ini_signal(p^.rlock,          os.break,1)
  END inisignals;

  VAR i: INTEGER;

BEGIN
  IF ins.nproc<0 THEN RETURN err.bad_parm END;
  os.lock;
  nos.allocate(p,SIZE(p^));
  IF p=NIL THEN u; RETURN err.no_memory END;
  p^.magic:=0;
  p^.nproc:=ins.nproc;
  p^.vlow :=ins.vlow;
  p^.vhigh:=ins.vlow;
  p^.doit :=ins.doit;
  p^.port :=ins.port;
  p^.prog :=ins.prog;
  p^.obj  :=ins.obj;
  p^.xid  :=os.timer;
  p^.trans:=NIL;
  p^.rbody^.ADR :=NIL;
  p^.rbody^.HIGH:=-1;
  inisignals;
  i:=udp.install(ins.port,rpcinput,p);
  IF i#err.ok THEN u; RETURN i END;
  i:=os.make_process(p^.prs,caller,1024);
  IF i#err.ok THEN
    IF udp.remove(ins.port)#err.ok THEN END; u; RETURN i
  END;
  p^.magic:=MAGIC;
  nos.tie(ports,p);
  portforcaller:=p;
  os.start(p^.prs);
  os.wait(callerstarted);
  os.unlock;
  RETURN err.ok
END create;

PROCEDURE remove (VAR p: PORT): INTEGER;
BEGIN


END remove;

PROCEDURE stop;
  VAR p: PORT;
BEGIN

  RETURN ;

  p:=ports;
  IF p#NIL THEN
    REPEAT
      IF udp.remove(p^.port)#err.ok THEN END;
      p:=p^.f
    UNTIL p=ports
  END
END stop;

BEGIN
  ports:=NIL;
  os.ini_signal(callerstarted,{},0);
  IF os.final(os.self(),stop)#err.ok THEN (*HALT(1)*) END
END rpc.
