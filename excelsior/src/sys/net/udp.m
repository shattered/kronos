IMPLEMENTATION MODULE udp[1]; (* Igo 25-Nov-91. (c) KRONOS *)
                              (* Igo 22-Dec-91. (c) KRONOS *)

IMPORT  SYSTEM;
IMPORT  err: defErrors;
IMPORT  os : osKernel;
IMPORT  nos: netKernel;
IMPORT  drv: netdrv;
IMPORT  ip;

TYPE PORT = POINTER TO port;
     port = RECORD
              port : INTEGER;      (* udp port number              *)
              input: inputproc;
              obj  : WORD;
              next : PORT;
            END;

CONST hashsize  = 256;

TYPE  hashtable = ARRAY [0..hashsize-1] OF PORT;

VAR hash   : hashtable;
    udplock: os.signal_rec;

PROCEDURE udpdone(r: REQUEST);
BEGIN
  r^.res:=r^.ip.res;
  r^.done(r^.obj)
END udpdone;

PROCEDURE transmit(VAR r: request): INTEGER;
  VAR res: INTEGER;
BEGIN
  r.res:=err.ok;
  IF (r.len+8)>0FFFFh THEN r.res:=err.bad_parm; RETURN r.res END;
  r.hdr[0]:=((BITSET(r.dprt)*{0..15})<<16)+BITSET(r.sprt)*{0..15};
  r.hdr[1]:=BITSET(r.len+8)*{0..15};
  WITH r.ip.buf DO
    buf :=SYSTEM.ADR(r.hdr);
    pos :=0;
    len :=8;
    next:=SYSTEM.ADR(r.buf)
  END;
  WITH r.ip DO
    dst :=r.dst;
    len :=r.len+8;
    prot:=ip.UDP;
    tout:=r.tout;
    ttl :=(tout+49) DIV 50;
    dntf:=FALSE;
    done:=udpdone;
    obj :=SYSTEM.ADR(r)
  END;
  res:=ip.transmit(r.ip);
  IF res#err.ok THEN r.res:=res END;
  RETURN res
END transmit;

PROCEDURE udpinput(d: ip.DATA);

  TYPE int  = INTEGER;
       BREF = POINTER TO ARRAY [0..0] OF CHAR;

  VAR b  : drv.BUFFER;
      pd : INTEGER;
      ps : INTEGER;
      l,h: INTEGER;
      p  : PORT;
      pb : BREF;
BEGIN
  IF d^.tlen<=8 THEN RETURN END;
  b:=d^.cque;
  IF b^.len<8 THEN RETURN END;
  pb:=BREF(b^.buf);
  (*$<$T-*)
  ps:=int(pb^[b^.pos+0])+int(pb^[b^.pos+1])*100h;
  pd:=int(pb^[b^.pos+2])+int(pb^[b^.pos+3])*100h;
  l :=int(pb^[b^.pos+4])+int(pb^[b^.pos+5])*100h;
  (*$>*)
  IF (l>d^.tlen) OR (l<=8) THEN RETURN END;
  h:=(pd MOD hashsize + pd DIV hashsize) MOD hashsize;
  os.wait(udplock);
    p:=hash[h];
    WHILE (p#NIL)&(p^.port#pd) DO p:=p^.next END;
    IF p#NIL THEN
      INC(b^.pos,8); DEC(d^.tlen,8); DEC(l,8); DEC(b^.len,8);
      p^.input(d,p^.obj,d^.src,ps,l)
    END;
  os.send(udplock)
END udpinput;

PROCEDURE addhash(port: INTEGER; in: inputproc; w: WORD): INTEGER;
  VAR h: INTEGER;
      p: PORT;
BEGIN
  h:=(port MOD hashsize + port DIV hashsize) MOD hashsize;
  p:=hash[h];
  WHILE (p#NIL)&(p^.port#port) DO p:=p^.next END;
  IF p#NIL THEN RETURN err.duplicate END;
  nos.allocate(p,SIZE(p^));
  IF p=NIL THEN RETURN err.no_memory END;
  p^.next :=hash[h];
  hash[h] :=p;
  p^.obj  :=w;
  p^.port :=port;
  p^.input:=in;
  RETURN err.ok
END addhash;

PROCEDURE install(port: INTEGER; input: inputproc; obj: WORD): INTEGER;
  VAR res: INTEGER;
BEGIN
  IF (port<0) OR (port>0FFFFh) THEN RETURN err.bad_parm END;
  IF os.wait_del(-1,udplock)#0 THEN RETURN err.ipted_op END;
    res:=addhash(port,input,obj);
  os.send(udplock);
  RETURN res
END install;

PROCEDURE remove(port: INTEGER): INTEGER;
  TYPE int = INTEGER;
       bit = BITSET ;

  VAR h   : INTEGER;
      p,pn: PORT;
      res : INTEGER;
BEGIN
  res:=err.ok;
  IF (port<0) OR (port>0FFFFh) THEN RETURN err.bad_parm END;
  h:=(port MOD hashsize + port DIV hashsize) MOD hashsize;
  IF os.wait_del(-1,udplock)#0 THEN RETURN err.ipted_op END;
    p :=NIL;
    pn:=hash[h];
    WHILE (pn#NIL)&(pn^.port#port) DO p:=pn; pn:=pn^.next END;
    IF pn=NIL THEN
      res:=err.no_entry
    ELSE
      IF p=NIL THEN
        hash[h]:=pn^.next
      ELSE
        p^.next:=pn^.next
      END;
      nos.deallocate(pn,SIZE(pn^))
    END;
  os.send(udplock);
  RETURN res
END remove;

PROCEDURE stop;
BEGIN
  IF ip.remove(ip.UDP)#err.ok THEN END
END stop;

PROCEDURE installme;
  VAR r: INTEGER;
BEGIN
  r:=ip.install(ip.UDP,udpinput);
  IF r#err.ok THEN HALT(r) END
END installme;

PROCEDURE initudp;
  VAR i: INTEGER;
BEGIN
  os.ini_signal(udplock,os.break,1);
  FOR i:=0 TO HIGH(hash) DO hash[i]:=NIL END;
  IF os.final(os.self(),stop)#err.ok THEN (*HALT(1)*) END;
  installme
END initudp;

BEGIN
  initudp
END udp.
