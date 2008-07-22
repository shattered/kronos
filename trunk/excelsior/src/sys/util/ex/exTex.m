IMPLEMENTATION MODULE exTex; (* Ned 18-Nov-87. (c) KRONOS *)

FROM exMem      IMPORT  cur, jump, adr, size?, delete, insert, put;

TYPE String  = ARRAY [0..255] OF CHAR;
    pString  = POINTER TO String;

PROCEDURE Letter?(ch: CHAR): BOOLEAN;
BEGIN RETURN (ch>=300c) OR ("a"<=ch) & (ch<="z") END Letter?;

PROCEDURE centre(c0,c1: INTEGER);
  VAR s: pString; line: String;
    i,l,width,sz,s0,s1: INTEGER;
BEGIN width:=c1-c0+1;
  s:=adr(); sz:=size?();
  s0:=0; s1:=sz-1;
  WHILE (s0<sz) & (s^[s0]=' ') DO INC(s0) END;
  WHILE (s1>=0) & (s^[s1]=' ') DO DEC(s1) END;
  sz:=s1-s0+1;
  IF (sz<0) OR (sz>width) THEN RETURN END;
  sz:=c0+(width-sz) DIV 2;
  FOR l:=0  TO sz-1 DO line[l]:=' ' END; l:=sz;
  FOR i:=s0 TO s1   DO line[l]:=s^[i]; INC(l) END;
  line[l]:=0c;
  put(line,l);
END centre;

PROCEDURE dummy; END dummy;

PROCEDURE form(par,l0,c0: INTEGER; VAR l1,c1: INTEGER);
  VAR words,line: String; first: BOOLEAN;
      pos,wc,width,wlen,ins: INTEGER;

  PROCEDURE newline(sz: INTEGER);
  BEGIN
    jump(ins); insert(1); put(line,sz); INC(ins);
  END newline;

  PROCEDURE spaces(): INTEGER;
    VAR i,l: INTEGER;
  BEGIN
    IF first THEN l:=par ELSE l:=c0 END;
    FOR i:=0 TO l-1 DO line[i]:=' ' END;
    RETURN l
  END spaces;

  PROCEDURE complete;
    VAR com,add,res,i,l,k: INTEGER;
  BEGIN ASSERT(wc#0);
    l:=spaces();
    IF wc=1 THEN
      FOR i:=0 TO pos-1 DO line[l]:=words[i]; INC(l) END;
    ELSE
      com:=width-pos;
      add:=com DIV (wc-1);
      res:=com MOD (wc-1);
      FOR i:=0 TO pos-1 DO
        line[l]:=words[i]; INC(l);
        IF words[i]=' ' THEN
          FOR k:=1 TO add DO line[l]:=' '; INC(l) END;
          IF res>0 THEN line[l]:=' '; INC(l); DEC(res) END;
        END;
      END;
--    ASSERT(l=c1+1);
    END;
    line[l]:=0c; newline(l);
    wc:=0; pos:=0;
    IF first THEN width:=c1-c0+1; first:=FALSE END;
  END complete;

  PROCEDURE lastline(): INTEGER;
    VAR i,l: INTEGER;
  BEGIN
    IF wc=0 THEN RETURN c1 END;
    l:=spaces();
    FOR i:=0 TO pos-1 DO line[l]:=words[i]; INC(l) END;
    line[l]:=0c; newline(l);
    RETURN l
  END lastline;

  PROCEDURE appword(VAL w: ARRAY OF CHAR);
    VAR i,sz,wfrom: INTEGER;
  BEGIN sz:=pos+wlen+1; wfrom:=0;
    IF sz>width THEN
      IF (wc#0) & (wlen<=width) THEN complete END;
      IF wc#0 THEN words[pos]:=' '; INC(pos) END;
      sz:=wlen;
      WHILE sz>width DO
        WHILE pos<width DO
          words[pos]:=w[wfrom]; INC(pos); INC(wfrom); DEC(sz)
        END; INC(wc);
        complete;
      END;
      IF sz=0 THEN RETURN END;
    END;
    IF wc#0 THEN words[pos]:=' '; INC(pos) END;
    FOR i:=wfrom TO wlen-1 DO words[pos]:=w[i]; INC(pos) END;
    INC(wc); words[pos]:=0c;
    IF sz=width THEN complete END;
  END appword;

  VAR sou: INTEGER; -- position in source line

  PROCEDURE getword(VAR s,n,w: ARRAY OF CHAR): BOOLEAN;
    VAR i,j: INTEGER;
  BEGIN
    WHILE s[sou]=' ' DO INC(sou) END;
    IF s[sou]=0c THEN sou:=0; RETURN FALSE END;
    i:=0;
    WHILE (s[sou]#0c) & (s[sou]#' ') DO
      w[i]:=s[sou]; INC(sou); INC(i);
    END; w[i]:=0c; wlen:=i;
    ASSERT(w[0]#0c);

    WHILE s[sou]=' ' DO INC(sou) END;
    IF (s[sou]=0c) & (w[i-1]='-') & (i>2) & Letter?(w[i-2]) & (cur#l1) THEN
      j:=0;
      WHILE n[j]=' ' DO INC(j) END;
      IF NOT Letter?(n[j]) THEN RETURN TRUE END;
      DEC(i);
      WHILE (n[j]#0c) & (n[j]#' ') DO
        w[i]:=n[j]; n[j]:=' '; INC(j); INC(i);
      END; w[i]:=0c; wlen:=i;
    END;

    RETURN TRUE
  END getword;

  VAR w: String; s,n: pString; l,sav: INTEGER;
BEGIN sav:=cur;
  pos:=0; wc:=0; sou:=0; ins:=l1+1;
  width:=c1-par+1; first:=TRUE;
  FOR l:=l0 TO l1 DO
    jump(l+1); n:=adr();
    jump(l);   s:=adr();
    WHILE getword(s^,n^,w) DO appword(w) END;
  END;
  c1:=lastline(); l:=l1-l0+1;
  jump(l0); delete(l,100,dummy);
  l1:=ins-l-1;
  jump(sav);
END form;

END exTex.
