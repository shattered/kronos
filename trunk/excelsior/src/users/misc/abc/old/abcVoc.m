IMPLEMENTATION MODULE abcVoc; (* Hady. 18-Apr-88. (c) KRONOS *)

FROM Misc       IMPORT  Letter?, Bold;
FROM Streams    IMPORT  Stream, Open, Close, Read, GetS, PutS
                      , PutC, Create, LookUp, Write, Seek, Why?;
FROM FileNames  IMPORT  AppExt, DelExt, ChangeExt;
FROM SYSTEM     IMPORT  ADR;
FROM Strings    IMPORT  Str1, Str0, AppStr;
FROM ASCII      IMPORT  NL;
FROM Pattern    IMPORT  Match;
FROM abcDefs    IMPORT  NoSuchWord, ExistOpened, NotOpened;

VAR coder : ARRAY CHAR OF CHAR;
    INITED: BOOLEAN;

CONST dummy=164b;
      MyName='vvv';

TYPE Ptrs = (left, right, ptrn, trnl);
     wrd  = ARRAY Ptrs OF INTEGER;

PROCEDURE InitCoder();
  VAR c,ch: CHAR;
BEGIN
  ch:=0c;
  FOR c:=0c TO 377c DO
    IF Letter?(c) THEN
      coder[c]:=ch;
      INC(ch);
    ELSE
      coder[c]:=CHAR(dummy);
    END;
  END;
END InitCoder;

VAR Map: ARRAY [0..dummy] OF INTEGER;
    index, orgs, trns: Stream;

PROCEDURE InitMap();
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO HIGH(Map) DO Map[i]:=0 END;
END InitMap;

PROCEDURE map?(c: CHAR): INTEGER;
  VAR ch: CHAR;
BEGIN ch:=coder[c];
  IF ch#CHAR(dummy) THEN
    RETURN Map[INTEGER(ch)]
  ELSE RETURN 0
  END
END map?;

PROCEDURE map!(c:CHAR; i:INTEGER);
  VAR ch: CHAR;
BEGIN ch:=coder[c];
  IF ch#CHAR(dummy) THEN Map[INTEGER(ch)]:=i END;
END map!;

PROCEDURE ReadWrd(start: INTEGER; VAR v: wrd): INTEGER;
  VAR r: INTEGER;
BEGIN
  ASSERT(start>=dummy*4, 4Ah);
  IF start#0 THEN
    r:=Seek(index,start,0);
    IF r<0 THEN RETURN r END;
    r:=Read(index, ADR(v), BYTES(v));
  ELSE r:=-1
  END;
  RETURN r;
END ReadWrd;

PROCEDURE WriteWrd(start: INTEGER; VAR v: wrd): INTEGER;
BEGIN
  ASSERT(start>=dummy*4);
  start:=Seek(index, start,0);
  IF start<0 THEN RETURN start END;
  RETURN Write(index, ADR(v), BYTES(v));
END WriteWrd;

(*
PROCEDURE ShowWrd(VAR v: wrd);
BEGIN
(*
012345678901234567890123456789012345678901234567890123456789
right     left      pattern   trans
%$5d      %$5d      %$5d      %$5d
*)
print('right    left     pattern     trans\n');
print('%$5hh   %$5hh  %$5hh   %$5hh\n', v[right], v[left], v[ptrn], v[trnl] );
END ShowWrd;
*)

PROCEDURE ReadS(f: Stream; start: INTEGER; VAR s: ARRAY OF CHAR): INTEGER;
  VAR r: INTEGER;
BEGIN
  IF start>=0 THEN
    r:=Seek(f,start,0);
    IF r<0 THEN RETURN r END;
    RETURN GetS(f,s);
  ELSE RETURN start
  END;
END ReadS;

PROCEDURE OpenVoc(name: ARRAY OF CHAR): INTEGER;
  VAR s: ARRAY [0..255] OF CHAR;
      r: INTEGER;
BEGIN
  Str1(s,name);
  ChangeExt(s,'x'); index:=LookUp(s);
  IF index>=0 THEN
    ChangeExt(s,'r'); orgs :=LookUp(s);
    IF orgs>=0 THEN
      ChangeExt(s,'t'); trns :=LookUp(s);
      IF trns>=0 THEN
        r:=Read(index,ADR(Map),BYTES(Map));
        IF (r>=0) (* & (Map[dummy]=0) *) THEN INITED:=TRUE;
          RETURN 0
        ELSE r:=-1
        END;
      ELSE r:=trns
      END;
    ELSE r:=orgs
    END;
  ELSE r:=index
  END;
  RETURN r
END OpenVoc;

PROCEDURE CreateVoc(name: ARRAY OF CHAR): INTEGER;
  VAR s: ARRAY [0..255] OF CHAR;
      r: INTEGER;
BEGIN
  IF INITED THEN RETURN ExistOpened END;
  Str1(s,name);
  ChangeExt(s,'x'); index:=Create(s);
  IF index>=0 THEN ChangeExt(s,'r'); orgs :=Create(s);
    IF orgs>=0 THEN ChangeExt(s,'t'); trns :=Create(s);
      IF trns>=0 THEN InitMap();
        r:=Write(index, ADR(Map), BYTES(Map));
        IF r>=0 THEN
          INITED:=TRUE;
          RETURN 0;
        END
      ELSE r:=trns END;
    ELSE r:=orgs END;
  ELSE r:=index
  END;
  RETURN r
END CreateVoc;

PROCEDURE CloseVoc(): INTEGER;
  VAR r: INTEGER;
BEGIN
  IF NOT INITED THEN RETURN NotOpened END;
  r:=Seek(index,0,0);
  IF r>=0 THEN
    r:=Write(index, ADR(Map), BYTES(Map));
    IF r>=0 THEN
    r:=Close(index);
      IF r>=0 THEN
        r:=Close(orgs);
        IF r>=0 THEN
          r:=Close(trns);
        END;
      END;
    END;
  END;
  INITED:=FALSE;
  RETURN r;
END CloseVoc;

(***************** W O R D S **********************)

PROCEDURE FindWrd(pat: ARRAY OF CHAR; start: INTEGER;
                  VAR p: wrd; VAR ptr: ARRAY OF CHAR): INTEGER;
  VAR r: INTEGER;
BEGIN
  IF start=0 THEN RETURN 0 END;
  r:=ReadWrd(start,p);
  IF r>=0 THEN r:=ReadS(orgs,p[ptrn],ptr);
    IF r>=0 THEN
      IF ptr=pat THEN RETURN 1
      ELSIF pat<ptr THEN RETURN FindWrd(pat,p[left],p,ptr)
      ELSE RETURN FindWrd(pat,p[right],p,ptr)
      END;
    END;
  END;
  RETURN r;
END FindWrd;

PROCEDURE FindWord(pat: ARRAY OF CHAR; VAR w: Word): INTEGER;
  VAR v: wrd;
      r: INTEGER;
BEGIN
  IF NOT INITED THEN RETURN NotOpened END;
  r:=map?(pat[0]);
  IF r#0 THEN r:=FindWrd(pat,r,v,w.org);
    IF r>0 THEN r:=ReadS(trns,v[trnl],w.trn);
      IF r>=0 THEN r:=1 END;
    END
  END;
  RETURN r;
END FindWord;

PROCEDURE FindPlace(pat:ARRAY OF CHAR; start:INTEGER; VAR ptr:Ptrs): INTEGER;
  VAR i: INTEGER;
      v: wrd;
      s: ARRAY [0..255] OF CHAR;
BEGIN
  IF start=0 THEN RETURN 0 END;
  i:=ReadWrd(start,v);
  IF i>=0 THEN
    i:=ReadS(orgs, v[ptrn], s);
    IF i>=0 THEN
      IF s=pat THEN RETURN 0
      ELSIF pat<s THEN ptr:=left; i:=FindPlace(pat, v[left], ptr)
      ELSE ptr:=right; i:=FindPlace(pat, v[right], ptr)
      END;
      IF i=0 THEN RETURN start
      ELSE RETURN i
      END;
    END;
  END;
  RETURN i
END FindPlace;

PROCEDURE PutWord(VAL w: Word): INTEGER;
  VAR v: wrd;
      s: ARRAY [0..255] OF CHAR;
    i,r: INTEGER;
pointer: Ptrs;
      W: Word;
BEGIN
  IF NOT INITED THEN RETURN NotOpened END;
  IF FindWord(w.org,W)>0 THEN RETURN 1 END;
  v[ptrn]:=Seek(orgs,0,2);
  IF v[ptrn]>=0 THEN v[trnl]:=Seek(trns,0,2);
    IF v[trnl]>=0 THEN
      PutS(orgs,w.org); PutC(orgs,NL);
      PutS(trns,w.trn); PutC(trns,NL);
      v[left]:=0; v[right]:=0;
      i:=Seek(index,0,2);
      IF i>=0 THEN r:=Write(index, ADR(v), BYTES(v));
        IF r>0 THEN r:=map?(w.org[0]);
          IF r#0 THEN r:=FindPlace(w.org,r,pointer);
            IF r>0 THEN r:=ReadWrd(r,v);
              IF r>=0 THEN v[pointer]:=i; r:=Seek(index,-BYTES(v),1);
                IF r>=0 THEN
                  r:=Write(index, ADR(v), BYTES(v));
                END;
              END;
            END;
          ELSE map!(w.org[0],i); r:=0;
          END;
        END;
      ELSE r:=i;
      END;
    ELSE r:=v[trnl];
    END;
  ELSE r:=v[ptrn];
  END;
  RETURN r;
END PutWord;

PROCEDURE IterTree(pat: ARRAY OF CHAR; proc: IterProc): INTEGER;
  VAR w: Word;
      c: INTEGER;
      i: INTEGER;

  PROCEDURE IterOne(start: INTEGER): INTEGER;
    VAR r: INTEGER;
        v: wrd;
     stop: BOOLEAN;
  BEGIN
    IF start>0 THEN r:=ReadWrd(start,v);
      IF r>=0  THEN r:=IterOne(v[left]);
        IF r>=0 THEN r:=ReadS(orgs, v[ptrn], w.org);
          IF (r>=0) & Match(pat,w.org) THEN
            r:=ReadS(trns, v[trnl], w.trn);
            IF r>=0 THEN
              stop:=proc(w);
              IF stop THEN RETURN NoSuchWord END;
            END;
          END;
        END;
      END;
      IF r>=0 THEN r:=IterOne(v[right]) END;
    ELSE r:=0
    END;
    RETURN r
  END IterOne;

BEGIN
  IF NOT INITED THEN RETURN NotOpened END;
  IF pat[0]='*' THEN
    c:=0;
    REPEAT i:=IterOne(map?(CHAR(c)));  INC(c);
    UNTIL (i<0) OR (c>377b);
    IF i=NoSuchWord THEN i:=0 END;
  ELSE
    i:=IterOne(map?(pat[0]));
    IF i=NoSuchWord THEN i:=0 END;
  END;
  RETURN i;
END IterTree;

PROCEDURE FindDel(pat: ARRAY OF CHAR;
              VAR dir: Ptrs;
                start: INTEGER;
                VAR v: wrd): INTEGER;
  VAR r: INTEGER;
      s: ARRAY [0..255] OF CHAR;
BEGIN
  IF start>0 THEN
    r:=ReadWrd(start,v);
    IF r>=0 THEN
      r:=ReadS(orgs, v[ptrn], s);
      IF r>=0 THEN
        IF s=pat THEN RETURN 1
        ELSIF s<pat THEN dir:=right;
        ELSE             dir:=left ;
        END;
        r:=FindDel(pat,dir,v[dir],v);
        IF r=1 THEN RETURN start
        ELSE RETURN r
        END;
      END;
    END;
  ELSE r:=0
  END;
  RETURN r
END FindDel;

PROCEDURE Connect(i: INTEGER): INTEGER;
  VAR v: wrd;
    t,r: INTEGER;
      s: ARRAY [0..255] OF CHAR;
    dir: Ptrs;
BEGIN
  IF i=0 THEN RETURN 1 END;
  r:=ReadWrd(i,v);
  IF r<0 THEN RETURN r END;
  r:=ReadS(orgs, v[ptrn], s);
  IF r<0 THEN RETURN r END;
  r:=map?(s[0]);
  ASSERT(r>0, 4Bh);
  t:=FindPlace(s, r, dir);
  IF t<0 THEN RETURN t END;
  r:=ReadWrd(t,v);
  IF r<0 THEN RETURN r END;
  v[dir]:=i;
  RETURN WriteWrd(t,v)
END Connect;

PROCEDURE GetWord(pat: ARRAY OF CHAR; VAR w: Word): INTEGER;
  VAR       v: wrd;
      i,r,L,R: INTEGER;
          dir: Ptrs;
            s: ARRAY [0..255] OF CHAR;
BEGIN
  IF NOT INITED THEN RETURN NotOpened END;
  r:=map?(pat[0]);
  IF r>0 THEN
    i:=FindDel(pat, dir, r, v);
    IF i>0 THEN L:=v[left]; R:=v[right];
      r:=ReadS(orgs, v[ptrn], w.org);
      IF r>=0 THEN
        r:=ReadS(trns, v[trnl], w.trn);
        IF r>=0 THEN
          IF i#1 THEN r:=ReadWrd(i, v);
            IF r>=0 THEN v[dir]:=0; r:=WriteWrd(i,v);
              IF r<0 THEN (*  ExitGate(Door); *) RETURN r END;
            END;
          ELSE
            IF R>0 THEN    map!(pat[0],R); R:=0;
            ELSIF L>0 THEN map!(pat[0],L); RETURN 1
            ELSE           map!(pat[0],0); RETURN 1
            END;
          END;
          r:=Connect(R);
          IF r>=0 THEN r:=Connect(L); END;
        END;
      END;
    END;
  END;
  RETURN r
END GetWord;

BEGIN
  InitCoder;
  INITED:=FALSE;
END abcVoc.
