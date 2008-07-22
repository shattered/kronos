IMPLEMENTATION MODULE vocVoc; (* Sem 21-Aug-89. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADR, ADDRESS;
FROM FileNames  IMPORT  ChangeExt;
FROM FsPublic   IMPORT  FileName, NoSuchBlock;
FROM ASCII      IMPORT  NL;
FROM Pattern    IMPORT  Match;
FROM Misc       IMPORT  Letter?;
FROM BIO        IMPORT  bRead, bWrite, OpenOnDir, CD, Close,
                        SetEof, GetEof, Create, Link;


FROM Terminal   IMPORT  print;

CONST
  dummy=164b;
  Stop=BOOLEAN(1000);

TYPE
  ptrs = (left, right, ptrn, trnl);
  wrd  = ARRAY ptrs OF INTEGER;
  dictionary=POINTER TO dict_rec;
  dict_rec=RECORD
    index: INTEGER;
    keys : INTEGER;
    trns : INTEGER;
    i_bf : ARRAY [0..4095] OF CHAR;
    k_bf : ARRAY [0..4095] OF CHAR;
    t_bf : ARRAY [0..4095] OF CHAR;
    i_blk: INTEGER;
    k_blk: INTEGER;
    t_blk: INTEGER;
    map  : ARRAY [0..dummy] OF INTEGER;
  END;
  rel_bits=(lss,equ,gtr);
  rel=SET OF rel_bits;

VAR
  wsp  : ARRAY [0..1] OF dict_rec;
  coder: ARRAY CHAR OF CHAR;

PROCEDURE open(VAR d: dictionary; name: ARRAY OF CHAR): BOOLEAN;
  VAR nm: FileName; i: INTEGER; r: BOOLEAN;
BEGIN
  i:=0;
  WHILE wsp[i].index>=0 DO
    INC(i); IF i>HIGH(wsp) THEN RETURN TRUE END;
  END;
  d:=ADR(wsp[i]);
  i:=0;
  WHILE (i<HIGH(nm)) & (i<=HIGH(name)) & (name[i]#0c) DO
    nm[i]:=name[i]; INC(i);
  END;
  nm[i]:=0c; nm[HIGH(nm)]:=CHAR(i);
  r:=FALSE;
  ChangeExt(nm,'x');
  r:=r OR OpenOnDir(CD(),d^.index,nm);
  ChangeExt(nm,'r');
  r:=r OR OpenOnDir(CD(),d^.keys,nm);
  ChangeExt(nm,'t');
  r:=r OR OpenOnDir(CD(),d^.trns,nm);
  IF r THEN
    d^.index:=-1; d:=NIL; RETURN r;
  END;
  r:=bRead(d^.index,0,ADR(d^.map),SIZE(d^.map)*4);
  d^.i_blk:=-1; d^.k_blk:=-1; d^.t_blk:=-1;
  RETURN r;
END open;

PROCEDURE close(VAR d: dictionary): BOOLEAN;
  VAR r: BOOLEAN;
BEGIN
  IF d=NIL THEN RETURN FALSE END;
  r:=Close(d^.index);
  r:=Close(d^.keys) OR r;
  r:=Close(d^.trns) OR r;
  d^.index:=-1; d:=NIL;
  RETURN r;
END close;

PROCEDURE create(VAR d: dictionary; name: ARRAY OF CHAR): BOOLEAN;
  VAR nm: FileName; i: INTEGER; r: BOOLEAN;
BEGIN
  i:=0;
  WHILE wsp[i].index>=0 DO
    INC(i); IF i>HIGH(wsp) THEN RETURN TRUE END;
  END;
  d:=ADR(wsp[i]);
  i:=0;
  WHILE (i<HIGH(nm)) & (i<=HIGH(name)) & (name[i]#0c) DO
    nm[i]:=name[i]; INC(i);
  END;
  nm[i]:=0c; nm[HIGH(nm)]:=CHAR(i);
  r:=Create(d^.index) OR Create(d^.keys) OR Create(d^.trns);
  ChangeExt(nm,'x');
  r:=r OR Link(CD(),nm,d^.index);
  ChangeExt(nm,'r');
  r:=r OR Link(CD(),nm,d^.keys);
  ChangeExt(nm,'t');
  r:=r OR Link(CD(),nm,d^.trns);
  IF r THEN
    d^.index:=-1; d:=NIL; RETURN r;
  END;
  FOR i:=0 TO HIGH(d^.map) DO d^.map[i]:=0 END;
  r:=bWrite(d^.index,0,ADR(d^.map),SIZE(d^.map)*4);
  SetEof(d^.index,SIZE(d^.map)*4);
  d^.i_blk:=-1; d^.k_blk:=-1; d^.t_blk:=-1;
  RETURN r;
END create;

PROCEDURE read_key(d: dictionary; pos: INTEGER; VAR s: ARRAY OF CHAR): BOOLEAN;
  VAR i,b: INTEGER; r: BOOLEAN;
BEGIN
  i:=0; b:=pos DIV 4096; pos:=pos MOD 4096;
  LOOP
    IF d^.k_blk#b THEN
      r:=bRead(d^.keys,b,ADR(d^.k_bf),4096); d^.k_blk:=b;
      IF r THEN s[i]:=0c; d^.k_blk:=-1; RETURN r END;
    END;
    WHILE pos<4096 DO
      IF d^.k_bf[pos]=NL THEN s[i]:=0c; RETURN FALSE END;
      IF i=HIGH(s) THEN s[i]:=0c; RETURN FALSE END;
      s[i]:=d^.k_bf[pos]; INC(i); INC(pos);
    END;
    pos:=0; INC(b);
  END;
END read_key;

PROCEDURE read_tns(d: dictionary; pos: INTEGER; VAR s: ARRAY OF CHAR): BOOLEAN;
  VAR i,b: INTEGER; r: BOOLEAN;
BEGIN
  i:=0; b:=pos DIV 4096; pos:=pos MOD 4096;
  LOOP
    IF d^.t_blk#b THEN
      r:=bRead(d^.trns,b,ADR(d^.t_bf),4096); d^.t_blk:=b;
      IF r THEN s[i]:=0c; d^.t_blk:=-1; RETURN r END;
    END;
    WHILE pos<4096 DO
      IF d^.t_bf[pos]=NL THEN s[i]:=0c; RETURN FALSE END;
      IF i=HIGH(s)   THEN s[i]:=0c; RETURN FALSE END;
      s[i]:=d^.t_bf[pos]; INC(i); INC(pos);
    END;
    pos:=0; INC(b);
  END;
END read_tns;

PROCEDURE read_item(d: dictionary; pos: INTEGER; VAR item: wrd): BOOLEAN;
  VAR i,b: INTEGER; r: BOOLEAN;
      ptr: POINTER TO ARRAY [0..SIZE(item)*4-1] OF CHAR;
BEGIN
  ptr:=ADR(item); i:=0; b:=pos DIV 4096; pos:=pos MOD 4096;
  LOOP
    IF d^.i_blk#b THEN
      r:=bRead(d^.index,b,ADR(d^.i_bf),4096); d^.i_blk:=b;
      IF r THEN d^.i_blk:=-1; RETURN r END;
    END;
    WHILE pos<4096 DO
      ptr^[i]:=d^.i_bf[pos]; INC(i); INC(pos);
      IF i>HIGH(ptr^) THEN RETURN FALSE END;
    END;
    pos:=0; INC(b);
  END;
END read_item;

PROCEDURE cmp(VAR nm: ARRAY OF CHAR): rel;
  PROCEDURE Match(pi,si: INTEGER): BOOLEAN;
  BEGIN
    WHILE key[pi]#'*' DO
      IF key[pi]=0c THEN RETURN nm[si]=0c
      ELSIF key[pi]=nm[si] THEN INC(si); INC(pi)
      ELSE RETURN FALSE
      END;
    END;
    WHILE key[pi]='*' DO INC(pi) END;
    IF key[pi]=0c THEN RETURN TRUE END;
    WHILE nm[si]#0c DO
      IF Match(pi,si) THEN RETURN TRUE END; INC(si);
    END;
    RETURN FALSE;
  END Match;
  VAR pi,si: INTEGER;
BEGIN
  pi:=0; si:=0;
  WHILE key[pi]#'*' DO
    IF key[pi]=0c THEN
      IF nm[si]=0c THEN RETURN rel{equ} ELSE RETURN rel{lss} END;
    ELSIF nm[si]=0c THEN RETURN rel{gtr};
    ELSIF key[pi]=nm[si] THEN INC(si); INC(pi)
    ELSIF key[pi]>nm[si] THEN RETURN rel{gtr};
    ELSE  RETURN rel{lss}
    END;
  END;
  WHILE key[pi]='*' DO INC(pi) END;
  IF key[pi]=0c THEN RETURN rel{lss,equ,gtr} END;
  WHILE nm[si]#0c DO
    IF Match(pi,si) THEN RETURN rel{lss,equ,gtr} END; INC(si);
  END;
  RETURN rel{lss,gtr};
END cmp;

PROCEDURE find_scan(d: dictionary; ind_pos: INTEGER; ip: iter_proc): BOOLEAN;
  VAR w: wrd; nm: ARRAY [0..79] OF CHAR; r: BOOLEAN; s: rel;
BEGIN
  IF ind_pos<=0 THEN RETURN FALSE END; -- empty tree
  r:=read_item(d,ind_pos,w) OR read_key(d,w[ptrn],nm);
  IF r THEN RETURN r END;
  s:=cmp(nm);
  IF lss IN s THEN r:=find_scan(d,w[left],ip); IF r THEN RETURN r END END;
  IF (equ IN s) & (w[trnl]>=0) THEN
    word:=nm;
    r:=read_tns(d,w[trnl],trn); IF r THEN RETURN r END;
    IF ip() THEN RETURN Stop END;
  END;
  IF gtr IN s THEN r:=find_scan(d,w[right],ip); IF r THEN RETURN r END END;
  RETURN FALSE;
END find_scan;

PROCEDURE find(d: dictionary; ip: iter_proc): BOOLEAN;
  VAR r: BOOLEAN; c: CHAR;
BEGIN
  IF d=NIL THEN RETURN FALSE END;
  IF key[0]=0c THEN RETURN FALSE END;
  IF key[0]='*' THEN
    FOR c:=0c TO CHAR(dummy) DO
      r:=find_scan(d,d^.map[INTEGER(c)],ip);
      IF r=Stop THEN RETURN FALSE END;
      IF r THEN RETURN r END;
    END;
  ELSE
    r:=find_scan(d,d^.map[INTEGER(coder[key[0]])],ip);
    IF r=Stop THEN RETURN FALSE END;
    IF r THEN RETURN r END;
  END;
  RETURN FALSE;
END find;

PROCEDURE put_ind(d: dictionary; pos: INTEGER;
                  a: ADDRESS; n: INTEGER): BOOLEAN;
  VAR i,b: INTEGER; r: BOOLEAN;
      ptr: POINTER TO ARRAY [0..4095] OF CHAR;
BEGIN
  IF n<=0 THEN RETURN FALSE END;
  ptr:=a; i:=0; b:=pos DIV 4096; pos:=pos MOD 4096;
  LOOP
    IF d^.i_blk#b THEN
      r:=bRead(d^.index,b,ADR(d^.i_bf),4096); d^.i_blk:=b;
      IF r=NoSuchBlock THEN r:=FALSE END;
      IF r THEN d^.i_blk:=-1; RETURN r END;
    END;
    WHILE pos<4096 DO
      d^.i_bf[pos]:=ptr^[i]; INC(i); INC(pos);
      IF i>=n THEN
        r:=bWrite(d^.index,b,ADR(d^.i_bf),4096); RETURN r;
      END;
    END;
    r:=bWrite(d^.index,b,ADR(d^.i_bf),4096);
    IF r THEN RETURN r END;
    pos:=0; INC(b);
  END;
END put_ind;

PROCEDURE remove(d: dictionary): BOOLEAN;
  PROCEDURE rem_scan(ind_pos: INTEGER): BOOLEAN;
    VAR w: wrd; nm: ARRAY [0..79] OF CHAR; r: BOOLEAN; i: INTEGER;
  BEGIN
    IF ind_pos<=0 THEN RETURN FALSE END; -- empty tree
    r:=read_item(d,ind_pos,w) OR read_key(d,w[ptrn],nm);
    IF r THEN RETURN r END;
    IF nm>key THEN r:=rem_scan(w[left])
    ELSIF nm=key THEN
      i:=-1;
      r:=put_ind(d,ind_pos+12,ADR(i),4);
    ELSE r:=rem_scan(w[right]);
    END;
    RETURN r;
  END rem_scan;
  VAR r: BOOLEAN; c: CHAR;
BEGIN
  IF d=NIL THEN RETURN FALSE END;
  IF key[0]=0c THEN RETURN FALSE END;
  RETURN rem_scan(d^.map[INTEGER(coder[key[0]])]);
END remove;

PROCEDURE put(d: dictionary): BOOLEAN;
  PROCEDURE put_key(): BOOLEAN;
    VAR i,b,pos: INTEGER; r: BOOLEAN;
        ptr: POINTER TO ARRAY [0..4095] OF CHAR;
  BEGIN
    pos:=GetEof(d^.keys);
    i:=0; b:=pos DIV 4096; pos:=pos MOD 4096;
    LOOP
      IF d^.k_blk#b THEN
        IF pos#0 THEN
          r:=bRead(d^.keys,b,ADR(d^.k_bf),4096);
          IF r THEN d^.k_blk:=-1; RETURN r END;
        END;
        d^.k_blk:=b;
      END;
      WHILE pos<4096 DO
        IF word[i]=0c THEN d^.k_bf[pos]:=NL ELSE d^.k_bf[pos]:=word[i] END;
        IF word[i]=0c THEN
          INC(i); INC(pos);
          r:=bWrite(d^.keys,b,ADR(d^.k_bf),4096);
          SetEof(d^.keys,b*4096+pos);
          RETURN r;
        END;
        INC(i); INC(pos);
      END;
      r:=bWrite(d^.keys,b,ADR(d^.k_bf),4096);
      IF r THEN RETURN r END;
      pos:=0; INC(b);
    END;
  END put_key;
  PROCEDURE put_trn(): BOOLEAN;
    VAR i,b,pos: INTEGER; r: BOOLEAN;
        ptr: POINTER TO ARRAY [0..4095] OF CHAR;
  BEGIN
    pos:=GetEof(d^.trns);
    i:=0; b:=pos DIV 4096; pos:=pos MOD 4096;
    LOOP
      IF d^.t_blk#b THEN
        IF pos#0 THEN
          r:=bRead(d^.trns,b,ADR(d^.t_bf),4096);
          IF r THEN d^.t_blk:=-1; RETURN r END;
        END;
        d^.t_blk:=b;
      END;
      WHILE pos<4096 DO
        IF trn[i]=0c THEN d^.t_bf[pos]:=NL ELSE d^.t_bf[pos]:=trn[i] END;
        IF trn[i]=0c THEN
          INC(i); INC(pos);
          r:=bWrite(d^.trns,b,ADR(d^.t_bf),4096);
          SetEof(d^.trns,b*4096+pos);
          RETURN r;
        END;
        INC(i); INC(pos);
      END;
      r:=bWrite(d^.trns,b,ADR(d^.t_bf),4096);
      IF r THEN RETURN r END;
      pos:=0; INC(b);
    END;
  END put_trn;
  PROCEDURE new_item(host: INTEGER): BOOLEAN;
    VAR w: wrd; e: INTEGER; r: BOOLEAN;
  BEGIN
    w[left]:=0; w[right]:=0;
    w[ptrn]:=GetEof(d^.keys); w[trnl]:=GetEof(d^.trns);
    e:=GetEof(d^.index);
    IF host<SIZE(d^.map)*4 THEN d^.map[host DIV 4]:=e END;
    r:=put_ind(d,host,ADR(e),4) OR
       put_ind(d,e,ADR(w),SIZE(w)*4) OR
       put_key() OR put_trn();
    SetEof(d^.index,e+SIZE(w)*4);
    RETURN r;
  END new_item;
  PROCEDURE change_item(pos: INTEGER): BOOLEAN;
    VAR ek,et: INTEGER;
  BEGIN
    ek:=GetEof(d^.keys); et:=GetEof(d^.trns);
    RETURN put_ind(d,pos+8,ADR(ek),4) OR put_ind(d,pos+12,ADR(et),4) OR
           put_key() OR put_trn();
  END change_item;
  PROCEDURE find_free(pos: INTEGER): BOOLEAN;
    VAR w: wrd; nm: ARRAY [0..79] OF CHAR; r: BOOLEAN;
  BEGIN
    r:=read_item(d,pos,w) OR read_key(d,w[ptrn],nm);
    IF r THEN RETURN r END;
    IF nm>word THEN
      IF w[left]<=0 THEN
        RETURN new_item(pos)
      ELSE
        RETURN find_free(w[left])
      END;
    ELSIF nm=word THEN
      RETURN change_item(pos)
    ELSE
      IF w[right]<=0 THEN
        RETURN new_item(pos+4)
      ELSE
        RETURN find_free(w[right])
      END;
    END;
  END find_free;
  VAR p,n: INTEGER;
BEGIN
  IF d=NIL THEN RETURN TRUE END;
  IF word[0]=0c THEN RETURN FALSE END;
  p:=INTEGER(coder[word[0]]);
  n:=d^.map[p];
  IF n<=0 THEN
    RETURN new_item(p*4);
  ELSE
    RETURN find_free(n);
  END;
END put;

VAR i: INTEGER;
    c,ch: CHAR;

BEGIN
  FOR i:=0 TO HIGH(wsp) DO wsp[i].index:=-1 END;
  ch:=0c;
  FOR c:=0c TO 377c DO
    IF Letter?(c) THEN
      coder[c]:=ch; INC(ch);
    ELSE
      coder[c]:=CHAR(dummy);
    END;
  END;
END vocVoc.
