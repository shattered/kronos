IMPLEMENTATION MODULE exScreen; (* Leo 05-Jun-87. (c) KRONOS *)

IMPORT  cod: defCodes;
IMPORT  tty: Terminal;
IMPORT  key: Keyboard;
IMPORT  mem: Heap;

FROM SYSTEM     IMPORT  ADR, ADDRESS;
FROM ASCII      IMPORT  CR, LF, NL;

PROCEDURE MOVE(to,from: ADDRESS; size: INTEGER); CODE cod.move END MOVE;

TYPE String=ARRAY [0..maxcol+1] OF CHAR;
     StrPtr=POINTER TO String;

VAR   max_l: INTEGER;
      max_c: INTEGER;
     bottom: INTEGER;
      DL_IL: BOOLEAN;

  min_color: INTEGER;

VAR lp: ARRAY [0..63] OF StrPtr;
    sz: ARRAY [0..63] OF INTEGER;
  mode: ARRAY [0..63] OF BITSET;
   att: ARRAY [0..63] OF INTEGER;
infstr: String;
    ln: INTEGER;
    cl: INTEGER;
    lb: INTEGER;  (* left bound *)
 stack: ARRAY [0..31] OF INTEGER;
    sp: INTEGER;
please: BOOLEAN;

 clpos: INTEGER;
 lnpos: INTEGER;                info_off: BOOLEAN;
 flpos: INTEGER;                 bell_on: BOOLEAN;
 blpos: INTEGER;                  ins_on: BOOLEAN;
 irpos: INTEGER;            infoONscreen: BOOLEAN; (* info on screen state *)
  bump: String;

PROCEDURE coff; BEGIN tty.set_cursor(0) END coff;
PROCEDURE con;  BEGIN tty.set_cursor(1)  END con;

PROCEDURE roff; BEGIN tty.set_reverse(0) END roff;
PROCEDURE ron ; BEGIN tty.set_reverse(1)  END ron;

PROCEDURE lon; BEGIN tty.set_color(min_color); ron END lon;

PROCEDURE loff; BEGIN tty.set_color(0); roff END loff;

PROCEDURE Bell; (* must use tty.Bell *)
BEGIN IF bell_on THEN key.bell(0) END END Bell;

PROCEDURE bell(on: BOOLEAN);
BEGIN bell_on:=on; IF blpos>=0 THEN set_on_off(blpos,on) END;  END bell;

PROCEDURE bell?(): BOOLEAN;  BEGIN RETURN bell_on END bell?;

PROCEDURE ins(on: BOOLEAN);
BEGIN ins_on:=on; IF irpos>=0 THEN set_on_off(irpos,on) END;  END ins;

PROCEDURE ins?(): BOOLEAN;   BEGIN RETURN  ins_on END ins?;

PROCEDURE dispose;
BEGIN
  IF sz[ln]<=0 THEN RETURN END;
  IF lp[ln]#NIL THEN
    mem.DEALLOCATE(lp[ln],(sz[ln]+4) DIV 4);
  END;
  sz[ln]:=0; lp[ln]:=NIL; mode[ln]:={}; att[ln]:=-1;
END dispose;

PROCEDURE stringsize(VAL s: ARRAY OF CHAR): INTEGER;
  VAR i: INTEGER;
BEGIN
  IF s[0]=0c THEN RETURN 0 END;
  i:=0;
  REPEAT i:=i+1 UNTIL (i>HIGH(s)) OR (s[i]=0c); DEC(i);
  IF (i<0) OR (s[i]#' ') THEN RETURN i+1 END;
  REPEAT i:=i-1 UNTIL (i<0) OR (s[i]#' '); INC(i);
  RETURN i
END stringsize;

PROCEDURE new(VAL s: ARRAY OF CHAR);
  VAR szB: INTEGER; ptr: StrPtr;
BEGIN
  IF s[0]=0c THEN dispose; RETURN END;
  szB:=stringsize(s);
  IF szB=0 THEN dispose; RETURN END;
  IF szB>maxcol THEN szB:=maxcol+1 END;
  IF sz[ln]#szB THEN dispose;
    mem.ALLOCATE(ptr,(szB+4) DIV 4);
    lp[ln]:=ptr; sz[ln]:=szB; mode[ln]:={}; att[ln]:=-1;
  ELSE ptr:=lp[ln]
  END;
  IF ptr=NIL THEN sz[ln]:=0; RETURN END;
  MOVE(ptr,ADR(s),(szB+4) DIV 4);
  ptr^[szB]:=0c;
END new;

PROCEDURE newfix(VAL s: ARRAY OF CHAR; szB: INTEGER);
  VAR ptr: StrPtr;
BEGIN
  IF szB=0 THEN dispose; RETURN END;
  IF (s[szB-1]=' ') THEN
    REPEAT szB:=szB-1 UNTIL (szB<0) OR (s[szB]#' '); INC(szB);
  END;
  IF szB=0 THEN dispose; RETURN END;
  IF szB>maxcol THEN szB:=maxcol+1 END;
  IF sz[ln]#szB THEN dispose;
    mem.ALLOCATE(ptr,(szB+4) DIV 4);
    lp[ln]:=ptr; sz[ln]:=szB; mode[ln]:={}; att[ln]:=-1;
  ELSE ptr:=lp[ln]
  END;
  IF ptr=NIL THEN sz[ln]:=0; RETURN END;
  MOVE(ptr,ADR(s),(szB+4) DIV 4);
  ptr^[szB]:=0c;
END newfix;

(****************************************************************************)

PROCEDURE NotStop(): BOOLEAN;
BEGIN RETURN FALSE END NotStop;

PROCEDURE info(VAL s: ARRAY OF CHAR; l,c,f,i,b: INTEGER);
BEGIN
  infstr:=s; infstr[max_c]:=0c;
  clpos:=c; lnpos:=l; flpos:=f; blpos:=b; irpos:=i
END info;

CONST defaultinfo=
"<<< SCREEN 00:000  INS off  BELL on               00000 FILE _____________  >>>";
      defaultinfo1=
"<<< 00:000 INS off BELL on              00000 FILE                >>>";
      defaultinfo2=
" 00:000 INS off BELL on              00000                ";
      defaultinfo3=
" 00:000 Ioff B on             00000              ";

PROCEDURE frame(nl,nc: INTEGER);
BEGIN
  IF nl>HIGH(lp) THEN nl:=HIGH(lp) END;
  IF nc>maxcol   THEN nc:=maxcol   END;
  nolines:=nl;   nocolumns:=nc;
  max_l  :=nl-1; bottom:=max_l-1; infoline_pos?:=bottom+1;
  max_c  :=nc-1;
  IF nc>=80 THEN
    info(defaultinfo,11,14,50,23,33); timepos:=38; fnpos:=61
  ELSIF nc>=70 THEN
    info(defaultinfo1,4,7,40,15,24); timepos:=28; fnpos:=51
  ELSIF nc>=56 THEN
    info(defaultinfo2,1,4,38,12,21); timepos:=25; fnpos:=43
  ELSE
    info(defaultinfo3,1,4,30,9,14); timepos:=18; fnpos:=36
  END;
END frame;

(****************************************************************************)

VAR empty: ARRAY [0..0] OF CHAR;

PROCEDURE adrline(l: INTEGER): ADDRESS;
BEGIN
  IF sz[l]=0 THEN RETURN ADR(empty) ELSE RETURN lp[l] END;
END adrline;

PROCEDURE getline(l: INTEGER; VAR s: ARRAY OF CHAR);
  VAR len: INTEGER;
BEGIN len:=sz[l];
  ASSERT(len>=0);
  IF len=0 THEN s[0]:=0c; RETURN END;
  IF HIGH(s) < len THEN len:=HIGH(s) END;
  MOVE(ADR(s),lp[l],(len+4) DIV 4);
  s[len]:=0c;
END getline;

PROCEDURE get(VAR s: ARRAY OF CHAR); BEGIN getline(ln,s) END get;
PROCEDURE adr(): ADDRESS;     BEGIN RETURN adrline(ln)   END adr;

PROCEDURE size(): INTEGER;           BEGIN RETURN sz[ln] END size;

PROCEDURE linesize(l: INTEGER): INTEGER;
BEGIN RETURN sz[l] END linesize;

PROCEDURE push;
BEGIN
  IF sp>=HIGH(stack)-2 THEN RETURN END;
  stack[sp]:=ln; INC(sp);
  stack[sp]:=cl; INC(sp);
END push;

PROCEDURE pop;
  VAR l,c: INTEGER;
BEGIN
  ln:=-512; cl:=-512;
  IF sp<2 THEN pos(0,0); RETURN END;
  DEC(sp); c:=stack[sp];
  DEC(sp); l:=stack[sp]; pos(l,c);
END pop;

PROCEDURE copt;
  VAR l,c: INTEGER;
BEGIN
  c:=stack[sp-1];  l:=stack[sp-2];
  stack[sp]:=l; INC(sp);
  stack[sp]:=c; INC(sp);
END copt;

PROCEDURE drop;
BEGIN
  DEC(sp,2);
END drop;

(****************************************************************************)


PROCEDURE setinfo(col: INTEGER; ch: CHAR);
BEGIN
  IF infstr[col]=ch THEN RETURN END; infstr[col]:=ch;
  IF info_off THEN RETURN END;
  IF NOT infoONscreen THEN RETURN END;
  IF col>max_c        THEN RETURN END;
  push; tty.set_pos(bottom+1,col); tty.Write(ch); pop;
END setinfo;

PROCEDURE setinfostr(col: INTEGER; VAL s: ARRAY OF CHAR);
  VAR i: INTEGER; ch: CHAR; pos: INTEGER;
BEGIN
  i:=0; pos:=-1;
  IF NOT info_off THEN push END;
  LOOP
    WHILE (i<=HIGH(s)) & (s[i]#0c) & (s[i]=infstr[col+i]) DO INC(i) END;
    IF (i>HIGH(s)) OR (s[i]=0c) THEN EXIT END;
    ch:=s[i];
    IF infoONscreen & (infstr[col+i]#ch) & NOT info_off & (col+i<=max_c) THEN
      IF pos=col+i-1  THEN tty.right(1)
      ELSIF pos#col+i THEN tty.set_pos(bottom+1,col+i)
      ELSE (* we stay at correct column *)
      END;
      IF pos<0 THEN lon END;
      tty.Write(ch); pos:=col+i+1;
    END;
    infstr[col+i]:=ch; INC(i);
  END;
  IF pos>=0 THEN loff END;
  IF NOT info_off THEN pop END;
END setinfostr;

PROCEDURE set_on_off(col: INTEGER; tog: BOOLEAN);
BEGIN
  IF tog THEN setinfostr(col,"on ")
  ELSE        setinfostr(col,"off")
  END;
END set_on_off;

PROCEDURE Clear; BEGIN tty.erase(0); infoONscreen:=FALSE END Clear;

PROCEDURE showinfo;
BEGIN
  IF info_off     THEN infoONscreen:=FALSE; RETURN END;
  IF infoONscreen THEN RETURN END;
  push;
  lon; coff;
  bump:=infstr;  bump[max_c]:=0c;
  tty.set_pos(bottom+1,0); tty.write(bump,0,max_c);
  loff; Clear;  infoONscreen:=TRUE;
  pop; con;
END showinfo;

PROCEDURE pushandclearinfo;
BEGIN push; tty.set_pos(bottom+1,0); Clear END pushandclearinfo;

PROCEDURE pushandposinfo;
BEGIN push; tty.set_pos(bottom+1,0); infoONscreen:=FALSE END pushandposinfo;

PROCEDURE infomode(on: BOOLEAN);
BEGIN info_off:=NOT on;
  IF info_off THEN pushandclearinfo; pop ELSE showinfo END;
END infomode;

PROCEDURE infomode?(): BOOLEAN; BEGIN RETURN NOT info_off END infomode?;

PROCEDURE showint(val,pos,width: INTEGER);
  VAR s: ARRAY [0..7] OF CHAR; w: INTEGER;
BEGIN
  w:=width; s[w]:=0c;
  WHILE w>0 DO DEC(w);
    s[w]:=CHAR(ORD('0') + val MOD 10); val:=val DIV 10;
  END;
  setinfostr(pos,s);
END showint;

PROCEDURE showcl;
BEGIN showint(cl,clpos,3) END showcl;

PROCEDURE showfln(fln: INTEGER);
BEGIN showint(fln,flpos,5) END showfln;

PROCEDURE showln;
BEGIN showint(ln,lnpos,2) END showln;

VAR oldln,oldfln,oldcl: INTEGER;

PROCEDURE fixpos(fln: INTEGER);
  VAR x: BOOLEAN;
BEGIN
  IF (oldln=ln) & (oldfln=fln) & (oldcl=cl) THEN RETURN END;
  coff;
  oldln:=ln; oldfln:=fln; oldcl:=cl;
  IF clpos>=0 THEN showcl END;
  IF lnpos>=0 THEN showln END;
  IF flpos>=0 THEN showfln(fln) END;
  con;
END fixpos;

(****************************************************************************)

PROCEDURE clearscr;
  VAR i: INTEGER;
BEGIN
  push; pos(0,0); Clear;
  FOR i:=0 TO HIGH(lp) DO ln:=i; dispose END;
  pop; showinfo;
END clearscr;

PROCEDURE clearln;
  VAR t: String;
BEGIN
  IF sz[ln]<=cl THEN RETURN END;
  IF cl=0 THEN dispose;
  ELSE
    MOVE(ADR(t),lp[ln],(cl+3) DIV 4);
    t[cl]:=0c; newfix(t,cl);
  END;
  tty.erase_line(0);
END clearln;

PROCEDURE clean?(): BOOLEAN;
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO bottom DO
    IF sz[i]>0 THEN RETURN FALSE END;
  END;  RETURN TRUE
END clean?;

PROCEDURE refresh;
  VAR i,n,size,pressed,p,len: INTEGER;  t: String;  clean: BOOLEAN;
BEGIN
  clean:=clean?();
  IF clean THEN
    push; tty.home; Clear; pop;
  END;
  please:=TRUE;
  pressed:=key.ready();
  IF pressed>0 THEN
    IF stop() THEN stop:=NotStop; RETURN END;
  END;
  IF clean THEN showinfo; please:=FALSE; RETURN END;
  push; coff;
  FOR i:=0 TO bottom DO
    pos(i,0);
    size:=sz[i]-lb;
    IF size>0 THEN
      IF size>max_c THEN size:=max_c END;
      MOVE(ADR(t),ADDRESS(lp[i])+(lb DIV 4),(size+3) DIV 4);
      t[size]:=0c;
      n:=size-1;   (* tailed blanks *)
      WHILE (n>=0) & (t[n]=' ') DO DEC(n) END;  len:=n+1; t[len]:=0c;
      n:=0;        (* leading blanks *)
      WHILE (t[n]=' ') DO INC(n) END;
      IF t[n]#0c THEN
        IF n<12 THEN tty.write(t,0,len)
        ELSE tty.erase_line(0); posc(lb+n); tty.write(t,n,len-n)
        END;
        cl:=size+lb; posc?:=cl
      END
    END;
    tty.erase_line(0);
    IF (i MOD 4=0) & (i<=max_l DIV 2) THEN
      p:=key.ready();
      IF p>pressed THEN pressed:=p;
        IF stop() THEN pop; stop:=NotStop; con; RETURN END
      END
    END
  END;
  pop; stop:=NotStop; please:=FALSE; showinfo; con
END refresh;

PROCEDURE PleaseRefresh(): BOOLEAN;
BEGIN stop:=NotStop; RETURN please END PleaseRefresh;

PROCEDURE refreshline;
  VAR size,n,len: INTEGER;  t: String;
BEGIN
  push;
  size:=sz[ln]-lb;
  pos(ln,0);
  IF size>0 THEN
    IF size>max_c THEN size:=max_c END;
    MOVE(ADR(t),ADDRESS(lp[ln])+(lb DIV 4),(size+3) DIV 4);
    t[size]:=0c;
    n:=size-1;   (* tailed blanks *)
    WHILE (n>=0) & (t[n]=' ') DO DEC(n) END; len:=n+1; t[len]:=0c;
    n:=0;        (* leading blanks *)
    WHILE (t[n]=' ') DO INC(n) END;
    IF t[n]#0c THEN
      IF n<12 THEN tty.write(t,0,len)
      ELSE tty.erase_line(0); posc(lb+n); tty.write(t,n,len-n)
      END;
      cl:=size+lb; posc?:=cl
    END
  END;
  tty.erase_line(0);
  pop
END refreshline;

(****************************************************************************)

PROCEDURE leftbound(col: INTEGER);
BEGIN
  IF (col<0) THEN col:=0 END;
  IF (col>255-max_c+8) THEN col:=(256-max_c+8) DIV 8 * 8 END;
  ASSERT(col MOD 8 = 0);
  IF lb#col THEN lb:=col; refreshline; refresh END;
END leftbound;

PROCEDURE leftbound?(): INTEGER;
BEGIN RETURN lb END leftbound?;

(****************************************************************************)

PROCEDURE smode(l: INTEGER; s: BITSET);
BEGIN mode[l]:=s END smode;

PROCEDURE gmode(l: INTEGER): BITSET;
BEGIN RETURN mode[l] END gmode;

PROCEDURE sattr(l: INTEGER; val: INTEGER);
BEGIN att[l]:=val END sattr;

PROCEDURE gattr(l: INTEGER): INTEGER;
BEGIN RETURN att[l] END gattr;

PROCEDURE ws(VAL s: ARRAY OF CHAR; szB: INTEGER);
BEGIN newfix(s,szB); refreshline END ws;

PROCEDURE setline(no: INTEGER; VAL s: ARRAY OF CHAR; szB: INTEGER);
  VAR sv: INTEGER;
BEGIN sv:=ln;
  IF (no<0) OR (no>bottom) THEN RETURN END; ln:=no; newfix(s,szB); ln:=sv;
END setline;

PROCEDURE w(ch: CHAR);
  VAR t: String;

  PROCEDURE wch;
    VAR i: INTEGER;
  BEGIN tty.Write(ch);
    IF sz[ln]<=cl THEN
      get(t);
      FOR i:=sz[ln] TO cl DO t[i]:=' ' END;
      t[cl]:=ch; t[cl+1]:=0c; newfix(t,cl+1);
    ELSE
      lp[ln]^[cl]:=ch;
      IF (ch=' ') & (cl=sz[ln]-1) THEN get(t); newfix(t,sz[ln]) END;
    END;
    cl:=cl+1; posc?:=cl;
    IF cl>maxcol THEN posc(maxcol);
      IF ch#' '  THEN Bell END;
    END;
  END wch;

BEGIN
  ASSERT(cl<=maxcol);
  IF ins_on & (sz[ln]>cl) THEN ic(ch) END;
  IF cl-lb<max_c-1 THEN wch; RETURN END;
  lb:=lb+8;
  refreshline; wch;
  please:=TRUE;
END w;

(*

PROCEDURE reverse(i: INTEGER): INTEGER;
  VAR inv: INTEGER;
BEGIN
  inv:=tty.state^.reverse;
  tty.set_reverse(i);
  RETURN INTEGER(inv)
END reverse;

PROCEDURE cursor(i: INTEGER): INTEGER;
  VAR cur: INTEGER;
BEGIN
  cur:=tty.state^.cursor;
  tty.set_cursor(i);
  RETURN cur
END cursor;
*)

PROCEDURE markline(l,c0,c1: INTEGER; on: INTEGER);
  VAR t: String; i,j,size: INTEGER; p: StrPtr;
BEGIN
  IF (l<0)      THEN RETURN END;
  IF (l>bottom) THEN RETURN END;
  IF on=0 THEN
    push; pos(l,0); refreshline; pop; RETURN
  END;
  tty.set_reverse(1);
  IF NOT tty.done THEN RETURN END;
  IF c0<lb THEN c0:=lb END;
  IF c1>=lb+max_c THEN c1:=lb+max_c-1 END;
  push; pos(l,c0);
  j:=0; size:=sz[l]; p:=lp[l];
  FOR i:=c0 TO c1 DO
    IF i>=size THEN t[j]:=' ' ELSE t[j]:=p^[i] END; INC(j)
  END;
  t[j]:=0c;
  tty.write(t,0,j);
  pop;
  tty.set_reverse(0);
END markline;

PROCEDURE mark(l0,c0,l1,c1: INTEGER; on: INTEGER);
  VAR ch: CHAR; i,l,c: INTEGER;
BEGIN
  tty.set_reverse(0);
  IF NOT tty.done THEN RETURN END;
  tty.set_cursor(0);
  IF l0=l1      THEN markline(l0,c0,c1,on); tty.set_cursor(1); RETURN END;
  IF l0>=0      THEN markline(l0,c0,c1,on); INC(l0) ELSE l0:=0 END;
  IF l1<=bottom THEN markline(l1,c0,c1,on); DEC(l1) ELSE l1:=bottom END;
  IF l0>l1      THEN tty.set_cursor(1); RETURN END;
  tty.set_reverse(on);
  push;
  IF c1=c0 THEN i:=1 ELSE i:=0 END;
  FOR i:=i TO 1 DO
    IF i#0 THEN c:=c1 ELSE c:=c0 END;
    IF (c>=lb) & (c-lb<max_c) THEN
      FOR l:=l0 TO l1 DO
        pos(l,c);
        IF c>=sz[l] THEN ch:=' ' ELSE ch:=lp[l]^[c] END;
        tty.Write(ch); INC(cl); INC(posc?);
      END;
    END;
  END;
  pop;
  tty.set_cursor(1);
  tty.set_reverse(0);
END mark;

(****************************************************************************)

PROCEDURE pos(l,col: INTEGER);
  VAR c,l0,c0: INTEGER;
BEGIN
  IF col>maxcol THEN col:=maxcol END;
  IF l>=max_l   THEN l:=bottom   END;
  IF l<0        THEN l:=0        END;
  c:=col-lb;
  IF c<00          THEN c:=00          END;
  IF c>max_c-1 THEN c:=max_c-1 END;
  c0:=cl-lb; l0:=ln;
  cl:=lb+c;  posc?:=cl;
  ln:=l;     posl?:=ln;
  IF (l0=l) THEN
    IF    (c=c0) THEN RETURN
    ELSIF (c=00) THEN tty.Write(CR); RETURN
    ELSIF (ABS(c-c0)=1) THEN
      IF c>c0 THEN tty.right(1) ELSE tty.left(1) END; RETURN
    ELSE tty.set_pos(l,c); RETURN
    END;
  ELSIF (l0+1=l) THEN
    IF    (c=c0) THEN tty.Write(LF); RETURN
    ELSIF (c=00) THEN tty.Write(NL); RETURN
    ELSE tty.set_pos(l,c); RETURN
    END;
  ELSIF (c=c0) & (ABS(l-l0)=1) THEN
    IF l>l0 THEN tty.down(1) ELSE tty.up(1)  END; RETURN
  ELSE
    tty.set_pos(l,c);
  END;
END pos;

PROCEDURE pos?(VAR l,col: INTEGER);
BEGIN l:=ln; col:=cl END pos?;

(*
PROCEDURE posc?(): INTEGER; BEGIN RETURN cl END posc?;
PROCEDURE posl?(): INTEGER; BEGIN RETURN ln END posl?;
*)

PROCEDURE posc(col: INTEGER);
BEGIN pos(ln,col) END posc;

PROCEDURE posl(l: INTEGER);
BEGIN pos(l,cl) END posl;

(****************************************************************************)

PROCEDURE ic(ch: CHAR);
  VAR t: String;  len,i: INTEGER;   r: BOOLEAN;
BEGIN
  IF sz[ln]<=cl THEN RETURN END;
  len:=sz[ln];
  IF ((len-lb)>=max_c) & (lp[ln]^[lb+max_c-1]#' ') THEN
    push; posc(lb+max_c-1); tty.Write(' '); pop;
  END;
  IF (len>=maxcol) THEN
    push; posc(maxcol); tty.Write(' '); pop;
  END;
  coff;
  tty.ins_char(1); r:=NOT tty.done;
  push; tty.Write(ch); pop;
  get(t);
  IF len<=maxcol THEN INC(len) ELSE Bell END;
  i:=len;
  WHILE i>cl DO t[i]:=t[i-1]; DEC(i) END;
  t[cl]:=ch; t[len]:=0c; newfix(t,len);
  IF r THEN refreshline END;
  con
END ic;

PROCEDURE dc;
  VAR t: String;  len,i: INTEGER;   r: BOOLEAN;
BEGIN
  IF sz[ln]<=cl THEN RETURN END;
  coff;
  tty.del_char(1); r:=NOT tty.done;
  get(t);
  len:=sz[ln]-1; i:=cl;
  WHILE i<=len DO t[i]:=t[i+1]; INC(i) END;
  t[len]:=0c; newfix(t,len);
  IF r THEN refreshline
  ELSIF ((sz[ln]-lb)>=max_c) & (t[lb+max_c-1]#' ') THEN
    push; posc(lb+max_c-1); tty.Write(t[lb+max_c-1]); pop;
  END;
  con;
END dc;

CONST A=ORD("A"); O=ORD("0");

PROCEDURE alphanum(ch: CHAR): BOOLEAN;
BEGIN
  RETURN (ch>=300c) OR (ORD(CAP(ch))-A IN {0..25}) OR (ORD(ch)-O IN {0..9})
      OR (ch='_');
END alphanum;

PROCEDURE other(ch: CHAR): BOOLEAN;
BEGIN
  RETURN (ch#' ') & (NOT alphanum(ch) OR (ch='_'))
END other;

PROCEDURE delword1;
  VAR r: BOOLEAN;
    len: INTEGER;
    i,j: INTEGER;
BEGIN
  IF sz[ln]<=cl THEN RETURN END;
  get(bump);
  len:=sz[ln]; i:=cl;
  WHILE (i<len) & (bump[i]=' ') DO INC(i) END;
  IF (i<len) & alphanum(bump[i]) THEN
    WHILE (i<len) & alphanum(bump[i]) DO INC(i) END;
  ELSIF (i<len) & other(bump[i]) THEN
    WHILE (i<len) & other(bump[i]) DO INC(i) END;
  ELSIF (i<len) THEN
    INC(i)
  END;
  IF i=cl THEN RETURN END;
  j:=i-cl;
  tty.del_char(j);
  j:=i; i:=cl;
  WHILE j<=len DO bump[i]:=bump[j]; INC(i); INC(j) END;
  new(bump);
  IF (NOT tty.done) OR (len-lb>=max_c-1) THEN refreshline END;
END delword1;

PROCEDURE skipwordleft1;
  VAR len: INTEGER;
      i,j: INTEGER;
BEGIN
  IF cl=0 THEN RETURN END;
  get(bump);
  len:=sz[ln]; i:=cl-1;
  IF i>=len THEN i:=len-1 END;

  WHILE (i>0) & (bump[i]=' ') DO DEC(i) END;
  IF (i>0) & alphanum(bump[i]) THEN
    WHILE (i>0) & alphanum(bump[i-1]) DO DEC(i) END;
  ELSIF (i>0) & other(bump[i]) THEN
    WHILE (i>0) & other(bump[i-1]) DO DEC(i) END;
  ELSIF (i>0) THEN
    DEC(i)
  END;
  IF i>=cl THEN RETURN END;
  IF i<lb THEN
    leftbound(i DIV 8 * 8); cl:=-999;
  END;
  posc(i); showcl;
END skipwordleft1;

PROCEDURE skipwordright1;
  VAR len: INTEGER;
      i,j: INTEGER;
BEGIN
  IF sz[ln]<=cl THEN RETURN END;
  get(bump);
  len:=sz[ln]; i:=cl;
  WHILE (i<len) & (bump[i]=' ') DO INC(i) END;
  IF (i<len) & alphanum(bump[i]) THEN
    WHILE (i<len) & alphanum(bump[i]) DO INC(i) END;
  ELSIF (i<len) & other(bump[i]) THEN
    WHILE (i<len) & other(bump[i]) DO INC(i) END;
  ELSIF i<len THEN
    INC(i)
  END;
  IF i=cl THEN RETURN END;
  IF i>max_c-1+lb-9 THEN
    leftbound((i-max_c+9) DIV 8 * 8); cl:=-999;
  END;
  posc(i); showcl;
END skipwordright1;

PROCEDURE ruboutword1;
  VAR r: BOOLEAN;
    len: INTEGER;
    i,j: INTEGER;
    c,n: INTEGER;
BEGIN
  IF cl=0 THEN RETURN END;
  get(bump);
  len:=sz[ln]; i:=cl-1;
  IF i>=len THEN i:=len-1 END;
  IF i<0 THEN i:=0 END;
  WHILE (i>0) & (bump[i]=' ') DO DEC(i) END;
  IF (i>0) & alphanum(bump[i]) THEN
    WHILE (i>0) & alphanum(bump[i-1]) DO DEC(i) END;
  ELSIF (i>0) & other(bump[i]) THEN
    WHILE (i>0) & other(bump[i-1]) DO DEC(i) END;
  ELSIF (i>0) THEN
    DEC(i)
  END;
  c:=i;
  IF c>=cl THEN RETURN END;
  j:=cl; IF j>=len THEN j:=len END;
  IF j<0 THEN j:=0 END;
  WHILE j<=len DO bump[i]:=bump[j]; INC(i); INC(j) END;
  new(bump);
  IF i<lb THEN
    leftbound(i DIV 8 * 8); cl:=-999; posc(c)
  ELSE
    n:=cl-c;  cl:=-999;  posc(c);
    tty.del_char(n);
    IF NOT tty.done THEN refreshline END;
  END;
  showcl;
END ruboutword1;

PROCEDURE skipwordright0;
  VAR i,j: INTEGER;
      len: INTEGER;
BEGIN
  i:=cl;   len:=sz[ln];
  IF i>len THEN RETURN END;
  get(bump);
  WHILE (i<len) & (bump[i]#' ') DO INC(i) END;
  WHILE (i<len) & (bump[i]=' ') DO INC(i) END;
  IF i=cl THEN RETURN END;
  IF i>max_c-1+lb-9 THEN
    leftbound((i-max_c+9) DIV 8 * 8); cl:=-999;
  END;
  posc(i); showcl;
END skipwordright0;

PROCEDURE skipwordleft0;
  VAR i,j: INTEGER;
      len: INTEGER;
BEGIN
  i:=cl; len:=sz[ln];
  IF i>=len THEN i:=len-1 ELSE i:=i-1 END;
  IF i<=0 THEN i:=0
  ELSE get(bump);
    WHILE (i>=0) & (bump[i]=' ') DO DEC(i) END;
    WHILE (i>=0) & (bump[i]#' ') DO DEC(i) END; INC(i);
  END;
  IF i=cl THEN RETURN END;
  IF i<lb THEN
    leftbound(i DIV 8 * 8); cl:=-999;
  END;
  posc(i); showcl;
END skipwordleft0;

PROCEDURE delword0;
  VAR len: INTEGER;
      i,j: INTEGER;
BEGIN
  IF sz[ln]<=cl THEN RETURN END;
  get(bump);
  len:=sz[ln]; i:=cl;
  WHILE (i<len) & (bump[i]#' ') DO INC(i) END;
  WHILE (i<len) & (bump[i]=' ') DO INC(i) END;
  IF i=cl THEN RETURN END;
  j:=i-cl;
  tty.del_char(j);
  j:=i; i:=cl;
  WHILE j<=len DO bump[i]:=bump[j]; INC(i); INC(j) END;
  new(bump);
  IF (NOT tty.done) OR (len-lb>=max_c-1) THEN refreshline END;
END delword0;

PROCEDURE ruboutword0;
BEGIN
  skipwordleft0; delword0
END ruboutword0;

PROCEDURE rubout;
  VAR t: String;
BEGIN
  IF cl=0 THEN RETURN END;
  IF sz[ln]<cl THEN
    IF cl=lb THEN lb:=lb-8; refreshline; please:=TRUE END;
    posc(cl-1); RETURN
  END;
  get(t);
  IF (cl=maxcol) & (sz[ln]>cl) THEN t[cl]:=' ';
    push; tty.Write(' '); pop; new(t);
  ELSE t[cl-1]:=' '; new(t);
    IF cl=lb THEN lb:=lb-8; refreshline; please:=TRUE; posc(cl-1);
    ELSE posc(cl-1); push; tty.Write(' '); pop;
    END;
    IF ins_on THEN dc END;
  END;
END rubout;

VAR svu,svd: String;

PROCEDURE undel(VAR up,dw: ARRAY OF CHAR);
BEGIN
  up:=svu; dw:=svd;
END undel;

PROCEDURE cleantobottom?(): BOOLEAN;
  VAR i: INTEGER;
BEGIN
  FOR i:=ln+1 TO bottom DO
    IF sz[i]>0 THEN RETURN FALSE END;
  END;
  RETURN TRUE
END cleantobottom?;

PROCEDURE memdl;
  VAR i: INTEGER;
BEGIN
  INC(ln); dispose; DEC(ln);
  FOR i:=ln+1 TO bottom-1 DO
    sz[i]:=sz[i+1]; lp[i]:=lp[i+1]; mode[i]:=mode[i+1]; att[i]:=att[i+1];
  END;
  lp[bottom]:=NIL; sz[bottom]:=0; mode[bottom]:={}; att[bottom]:=-1;
END memdl;

PROCEDURE reshup;
  VAR i,p,l,pressed: INTEGER;
BEGIN l:=ln-1;
  pos(l,0);
  please:=TRUE; pressed:=0;
  FOR i:=l TO 0 BY -1 DO
    posl(i); refreshline;
    IF ODD(i) & (i<l-1) THEN
      p:=key.ready();
      IF p>pressed THEN pressed:=p;
        IF stop() THEN stop:=NotStop; RETURN END;
      END;
    END;
  END;
  please:=FALSE; stop:=NotStop;
END reshup;

PROCEDURE resh;
  VAR i,p,l,pressed: INTEGER;
BEGIN l:=ln+1;
  pos(l,0);
  please:=TRUE; pressed:=0;
  FOR i:=l TO bottom DO
    posl(i); refreshline;
    IF ODD(i) & (i>l+1) THEN
      p:=key.ready();
      IF p>pressed THEN pressed:=p;
        IF stop() THEN stop:=NotStop; RETURN END;
      END;
    END;
  END;
  please:=FALSE; stop:=NotStop;
END resh;

PROCEDURE dl(VAL s: ARRAY OF CHAR; szB: INTEGER);
  VAR tu,td: String;   i,szu,szd: INTEGER;   r,up,clean: BOOLEAN;
BEGIN
  IF ln=bottom THEN RETURN END;
  clean:=cleantobottom?();
  IF NOT clean THEN
    IF infoONscreen THEN pushandclearinfo; pop END;
    tty.del_line(1); r:=NOT tty.done;
    up:= r & (ln < bottom DIV 2);
    IF up THEN push; tty.roll_up(1); pop END;
  ELSE r:=FALSE; Clear;
  END;
  IF (cl=0) & NOT r THEN
    (* Simple case: terminal has DL capability *)
    (* and whole line delleted                 *)
    getline(ln  ,tu); svu:=tu; szu:=sz[ln];
    getline(ln+1,td); svd:=td; szd:=sz[ln+1];
    newfix(td,szd);
  ELSE (* (cl#0) OR r *)
    getline(ln  ,tu); svu:=tu; szu:=sz[ln];
    getline(ln+1,td); svd:=td; szd:=sz[ln+1];
    FOR i:=szu TO cl-1 DO tu[i]:=' '   END;
    FOR i:=cl  TO szd  DO tu[i]:=td[i] END;
    IF szd>cl THEN tu[szd]:=0c; szu:=szd ELSE tu[cl]:=0c; szu:=cl END;
    newfix(tu,szu); refreshline;
  END;
  push;
  IF NOT clean THEN memdl END;
  IF (szB=0) & (clean OR (sz[bottom-1]=0)) THEN (* Empty string rolling *)
    (* All screen bottom is clear or bottom line *)
    (* (moved to bottom-1 by -memdl- was clear   *)
    setline(bottom,s,0);
  ELSE
    posl(bottom); ws(s,szB);
  END;
  IF NOT clean THEN
    IF   up THEN pop; push; reshup
    ELSIF r THEN pop; push; resh
    END;
  END;
  pop;
END dl;

PROCEDURE memil;
  VAR i: INTEGER;
BEGIN i:=ln;
  ln:=bottom; dispose; ln:=i; posl?:=ln;
  FOR i:=bottom TO ln+1  BY -1 DO
    sz[i]:=sz[i-1]; lp[i]:=lp[i-1]; mode[i]:=mode[i-1]; att[i]:=att[i-1];
  END;
  lp[ln]:=NIL; sz[ln]:=0; mode[ln]:={}; att[ln]:=-1;
END memil;

PROCEDURE il;
  VAR t: String;   i,len: INTEGER;  up,r,clean: BOOLEAN;
BEGIN
  IF ln=bottom THEN RETURN END;
  clean:=cleantobottom?();
  IF NOT clean THEN
    IF infoONscreen THEN
      push; tty.set_pos(bottom,0); Clear; pop;
    END;
    tty.ins_line(1);
    r:=NOT tty.done; up:= r & (ln < bottom DIV 2);
    IF up THEN push; tty.roll_down(1); pop END;
  ELSE Clear; r:=FALSE;
  END;
  get(t); len:=sz[ln];
  memil; (* if clean TRUE to dup work line *)
  IF (cl=0) & NOT r & NOT clean THEN
    (* Simple case: Terminal has IL capability & *)
    (* we stay at the beginnig of the line       *)
    clearln; RETURN
  END;
  push;
  IF cl<len THEN
    t[cl]:=0c; newfix(t,cl); refreshline;
  ELSE newfix(t,len);        refreshline;
  END;
  posl(ln+1); get(t);
  IF sz[ln]>cl THEN
    FOR i:=0 TO cl-1 DO t[i]:=' ' END; len:=sz[ln];
  ELSE  t[0]:=0c; len:=0;
  END;
  newfix(t,len); refreshline; posl(ln-1);

  IF clean THEN pop; RETURN END;
  IF   up  THEN pop; push; reshup
  ELSIF r  THEN pop; push; resh
  END; pop;
END il;

PROCEDURE swap(VAR tu,td: String; szu,szd: INTEGER);
  VAR i,n: INTEGER;  c: CHAR;
BEGIN
  n:=cl;
  IF szd>n THEN n:=szd END;
  IF szu>n THEN n:=szu END;
  FOR i:=szu TO n DO tu[i]:=' ' END;
  FOR i:=szd TO n DO td[i]:=' ' END;
  FOR i:=cl TO n DO
    c:=td[i]; td[i]:=tu[i]; tu[i]:=c
  END;
  tu[n]:=0c; td[n]:=0c;
END swap;

PROCEDURE swapup;
  VAR tu,td: String; szu,szd: INTEGER;
BEGIN
  IF ln=0 THEN RETURN END;
  szu:=sz[ln-1];  szd:=sz[ln];
  IF (szu<=cl) & (szd<=cl) THEN posl(ln-1); RETURN END;
  getline(ln-1,tu);
  getline(ln  ,td);
  swap(tu,td,szu,szd);
  new(td); refreshline; posl(ln-1);
  new(tu); refreshline;
END swapup;

PROCEDURE swapdw;
  VAR tu,td: String; szu,szd: INTEGER;
BEGIN
  IF ln=bottom THEN RETURN END;
  szu:=sz[ln];  szd:=sz[ln+1];
  IF (szu<=cl) & (szd<=cl) THEN posl(ln+1); RETURN END;
  getline(ln  ,tu);
  getline(ln+1,td);
  swap(tu,td,szu,szd);
  new(tu); refreshline; posl(ln+1);
  new(td); refreshline;
END swapdw;

PROCEDURE rollup(VAL s: ARRAY OF CHAR; szB: INTEGER);
  VAR i: INTEGER; show: BOOLEAN;
BEGIN
  IF (s[0]=0c) & clean?() THEN RETURN END;
  show:=NOT ((szB=0) & (sz[bottom]=0));
  coff;
  pushandclearinfo;
  tty.scroll_up(1);
  IF NOT tty.done THEN tty.roll_up(1) END;
  ln:=0; dispose;
  IF NOT show THEN pop END; (* line will not be shown! *)
  IF show THEN pos(bottom,0) END;
  FOR i:=1 TO bottom DO
    sz[i-1]:=sz[i];  lp[i-1]:=lp[i]; mode[i-1]:=mode[i]; att[i-1]:=att[i]
  END;
  sz[bottom]:=0;  lp[bottom]:=NIL; mode[bottom]:={}; att[bottom]:=-1;
  IF show THEN ws(s,szB); pop
  ELSE (* bottom line already empty! & position poped *)
  END;
  con
END rollup;

PROCEDURE rolldw(VAL s: ARRAY OF CHAR; szB: INTEGER);
  VAR i: INTEGER;
BEGIN
  IF (s[0]=0c) & clean?() THEN RETURN END;
  coff;
  push;
  tty.scroll_down(1);
  IF NOT tty.done THEN tty.roll_down(1) END;
  ln:=bottom; dispose;
  pos(0,0);
  FOR i:=bottom TO 1 BY -1 DO
    sz[i]:=sz[i-1];  lp[i]:=lp[i-1]; mode[i]:=mode[i-1]; att[i]:=att[i-1];
  END;
  sz[0]:=0; lp[0]:=NIL; mode[0]:={}; att[0]:=-1; ws(s,szB);
  tty.set_pos(bottom+1,0); Clear;
  pop;
  con;
END rolldw;

PROCEDURE dup;
  VAR t: String;  i: INTEGER;
BEGIN
  IF ln=bottom THEN RETURN END;
  push; pos(ln+1,0); il; pop;
  get(t);
  FOR i:=0 TO cl-1 DO t[i]:=' ' END;
  IF cl>sz[ln] THEN t[cl]:=0c END;
  posl(ln+1);
  new(t); refreshline;
END dup;

PROCEDURE dupover;
  VAR tu,td: String;  i,szu,szd: INTEGER;
BEGIN
  IF ln=bottom THEN RETURN END;
  get(tu); szu:=sz[ln];
  posl(ln+1);
  get(td); szd:=sz[ln];
  FOR i:=szd TO cl-1  DO td[i]:=' ' END;
  FOR i:=cl  TO szu-1 DO td[i]:=tu[i] END;
  td[szu]:=0c; ws(td,szu);
END dupover;

PROCEDURE pull;
  VAR i,j: INTEGER;  n,t: String;
BEGIN
  IF sz[ln]<=cl THEN RETURN END;
  get(t); n:=t;
  i:=cl;
  WHILE t[i]=' ' DO INC(i) END;
  IF t[i]=0c THEN RETURN END;
  j:=cl;
  IF j=i THEN RETURN END;
  FOR i:=i TO sz[ln] DO
    n[j]:=t[i]; INC(j);
  END; new(n);
END pull;

PROCEDURE pullleft;
BEGIN pull; refreshline END pullleft;

PROCEDURE pullright;
  VAR i,j: INTEGER;  t,n: String;
BEGIN
  IF sz[ln]=0 THEN RETURN END;
  i:=cl; cl:=0; posc?:=cl; pull; cl:=i; posc?:=cl; get(t); n:=t;
  j:=0;
  WHILE (j<sz[ln]) & (i<=maxcol) DO n[i]:=t[j]; INC(i); INC(j) END;
  n[i]:=0c;
  IF j<sz[ln] THEN Bell END;
  FOR i:=0 TO cl-1 DO n[i]:=' ' END;
  new(n); refreshline;
END pullright;

(****************************************************************************)

VAR i: INTEGER;



BEGIN
  infoONscreen:=FALSE;
  ASSERT(HIGH(lp)=HIGH(sz));
  FOR i:=0 TO HIGH(lp) DO
    lp[i]:=NIL; sz[i]:=0; mode[i]:={}; att[i]:=-1
  END;
  stop:=NotStop; please:=FALSE; empty[0]:=0c;
  sp:=0; cl:=0; ln:=0; lb:=0; posl?:=ln; posc?:=cl;
  oldln:=-1; oldfln:=-1; oldcl:=-1;
  tty.bottom;
  min_color:=tty.state^.min_color+1;
  IF min_color<0 THEN min_color:=-1 ELSE min_color:=0 END;
  tty.ins_line(1);
  DL_IL:=tty.done;
  tty.del_line(1);
  DL_IL:=DL_IL AND tty.done;
  tty.set_awp(0);
  tty.home;
  frame(tty.state^.lines,tty.state^.columns); infomode(FALSE);
  bell(TRUE);   ins(FALSE);
  clearscr
END exScreen.
