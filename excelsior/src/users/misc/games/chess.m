MODULE chess; (* Leo 10-Oct-89. (c) KRONOS *)

IMPORT  cb: chessBoard;

-------------------  GLOBAL STATE VARIABLES  -------------------
                   --------------------------

TYPE arr6   = ARRAY [1..6]   OF INTEGER;
     arr16  = ARRAY [1..16]  OF INTEGER;
     arr10  = ARRAY [1..10]  OF INTEGER;
     arr100 = ARRAY [1..100] OF INTEGER;

CONST
     jbig=10000;
    jvbig=30000;

VAR board,pboard: ARRAY [1..120] OF INTEGER;

    mypce,mytype: arr16; myval: arr6; mymen,myqn,mykg: INTEGER;
    ispce,istype: arr16; isval: arr6; ismen,isqn,iskg: INTEGER;
    matbal,lev: INTEGER;

    ink : arr16;
    fld : arr100;
    kent: arr100;
    ijk : arr10;
    pri : arr6;

    ngame,limit: INTEGER;
    mark: arr100;
    nmove: INTEGER;

    moves: ARRAY [1..100],[1..4] OF INTEGER;

-------------------------  INIT BOARD  -------------------------
                         --------------

PROCEDURE INIT;
  CONST
    _ink     = ARRAY OF INTEGER {
               -9,-11,9,11,1,10,-1,-10,8,12,19,21,-8,-12,-19,-21};
    _board  = ARRAY OF INTEGER {
               1111,1111,1111,1111,1111,1111,1111,1111,1111,1111
              ,1111,1111,1111,1111,1111,1111,1111,1111,1111,1111
              ,1111,  50,  33,  35,  90, 900,  35,  33,  50,1111
              ,1111,  10,  10,  10,  10,  10,  10,  10,  10,1111
              ,1111,   0,   0,   0,   0,   0,   0,   0,   0,1111
              ,1111,   0,   0,   0,   0,   0,   0,   0,   0,1111
              ,1111,   0,   0,   0,   0,   0,   0,   0,   0,1111
              ,1111,   0,   0,   0,   0,   0,   0,   0,   0,1111
              ,1111, -10, -10, -10, -10, -10, -10, -10, -10,1111
              ,1111, -50, -33, -35, -90,-900, -35, -33, -50,1111
              ,1111,1111,1111,1111,1111,1111,1111,1111,1111,1111
              ,1111,1111,1111,1111,1111,1111,1111,1111,1111,1111};

     _mypce  = ARRAY OF INTEGER {
               36,35,23,28,34,37,24,27,25,22,29,33,38,32,39,26};
     _mytype = ARRAY OF INTEGER {
                6, 6, 4, 4, 6, 6, 3, 3, 1, 2, 2, 6, 6, 6, 6, 5};

     _myval  = ARRAY OF INTEGER {
                                90,50,35,33,900,10};

     _ispce  = ARRAY OF INTEGER {
               86,85,93,98,84,87,94,97,95,92,99,83,88,82,89,96};

     _istype = ARRAY OF INTEGER {
                6, 6, 4, 4, 6, 6, 3, 3, 1, 2, 2, 6, 6, 6, 6, 5};
     _isval  = ARRAY OF INTEGER {
                                -90,-50,-35,-33,-900,-10};
     _kent   = ARRAY OF INTEGER {
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                , 0, 0, 1, 2, 3, 3, 2, 1, 0, 0
                , 0, 1, 3, 4, 5, 5, 4, 3, 1, 0
                , 0, 2, 4, 6, 7, 7, 6, 4, 2, 0
                , 0, 3, 5, 7, 8, 8, 7, 5, 3, 0
                , 0, 3, 5, 7, 8, 8, 7, 5, 3, 0
                , 0, 2, 4, 6, 7, 7, 6, 4, 2, 0
                , 0, 1, 3, 4, 5, 5, 4, 3, 1, 0
                , 0, 0, 1, 2, 3, 3, 2, 1, 0, 0};

      _pri  = ARRAY OF INTEGER {2,0,3,4,0,1};
      _ijk   = ARRAY OF INTEGER {10,8,2,0,0,0,0,0,0,0};

  PROCEDURE copy(VAR d: ARRAY OF INTEGER; VAL s: ARRAY OF INTEGER);
    VAR i: INTEGER;
  BEGIN
    ASSERT(HIGH(s)=HIGH(d));
    FOR i:=0 TO HIGH(d) DO d[i]:=s[i] END;
  END copy;

  VAR i: INTEGER;
BEGIN
  FOR i:=1 TO HIGH(fld) DO fld[i]:=0 END;
  ismen:=16; isqn:=1; iskg:=1;
  mymen:=16; myqn:=1; mykg:=1;
  ngame:=0;  limit:=1090; matbal:=0; nmove:=0;
  copy(ink   ,_ink);
  copy(board ,_board);
  copy(mypce ,_mypce);
  copy(mytype,_mytype);
  copy(myval ,_myval);
  copy(ispce ,_ispce);
  copy(istype,_istype);
  copy(isval ,_isval);
  copy(kent  ,_kent);
  copy(pri   ,_pri);
  copy(ijk   ,_ijk);
  pboard:=board;
END INIT;

PROCEDURE zzz(SEQ VAR vars: INTEGER);
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO HIGH(vars) DO vars[i]:=0 END;
END zzz;

---------------------------  WMOVE  ----------------------------
                           ---------

PROCEDURE wmove(VAR man,jv,lsq,nsq,kon,ipt,ifn,kc,next,ip,more: INTEGER;
                kp: INTEGER);

  VAR name: INTEGER;  goto_82: BOOLEAN;

  PROCEDURE p37;
  BEGIN
    mypce[man]:=nsq;  board[lsq]:=0;   board[nsq]:=jv;  matbal:=matbal-kon
  END p37;

  PROCEDURE p51;
  BEGIN
    matbal:=matbal+myval[1]-myval[6];
    jv:=myval[1];     mytype[man]:=1;  ip:=1;  p37
  END p51;

  PROCEDURE p10(): BOOLEAN;
  BEGIN
    REPEAT
      ipt:=ipt+1;
      IF ipt>ifn THEN goto_82:=TRUE; RETURN FALSE END;
      kc:=ink[ipt];   nsq:=lsq+kc;   kon:=board[nsq]
    UNTIL kon<=0;
    IF (kon<0) OR (kon=0)&(kp<=0) THEN
      p37; RETURN TRUE
    END;
    RETURN FALSE
  END p10;

  PROCEDURE p24(): BOOLEAN;
  BEGIN
    REPEAT
      ipt:=ipt+1;
      IF ipt>ifn THEN goto_82:=TRUE; RETURN FALSE END;
      nsq:=lsq+ink[ipt]; kon:=board[nsq]
    UNTIL kon<=0;
    IF (kon<0) OR (kon=0)&(kp<=0) THEN
      p37; RETURN TRUE
    END;
    RETURN FALSE
  END p24;

  PROCEDURE p29(): BOOLEAN;
  BEGIN
    LOOP
      ipt:=ipt+1;
      IF (ipt>4) THEN goto_82:=TRUE; RETURN FALSE END;
      IF ipt<2 THEN
        nsq:=lsq+10;  kon:=board[nsq];
        IF kon=0 THEN
          IF nsq>=90 THEN p51; RETURN TRUE END;
          IF kp <=0  THEN p37; RETURN TRUE ELSE EXIT END;
        ELSE
          ipt:=2;
        END;
      ELSIF ipt=2 THEN
        IF lsq<=40 THEN
          nsq:=lsq+20;  kon:=board[nsq];
          IF kon=0 THEN
            IF kp<=0 THEN p37; RETURN TRUE ELSE EXIT END;
          END;
        END;
      ELSE (* ipt>2 *)
        nsq:=lsq+ink[ipt];  kon:=board[nsq];
        IF kon<0 THEN
          IF nsq<90 THEN p37 ELSE p51 END;
          RETURN TRUE
        END;
      END;
    END;
    RETURN FALSE
  END p29;

  VAR ret: BOOLEAN;

BEGIN
  IF man>0 THEN
    IF ip>0 THEN
      matbal:=matbal-myval[1]+myval[6];
      jv:=myval[6];  mytype[man]:=6;  ip:=0;
    END;
    mypce[man]:=lsq;  board[lsq]:=jv;  board[nsq]:=kon;
    matbal:=matbal+kon;
    IF more<=0 THEN RETURN END;
    goto_82:=FALSE;
  ELSE
    ip:=0;  more:=1;  goto_82:=TRUE;
  END;

  LOOP (* MAIN *)
    (*83*)
    WHILE NOT goto_82 DO
      IF next<0 THEN
        IF kon=0 THEN
          nsq:=nsq+kc;  kon:=board[nsq];
          IF kon<=0 THEN
            IF (kon<0) OR (kon=0)&(kp<=0) THEN p37; RETURN END;
          ELSE
            IF p10() THEN RETURN END
          END
        ELSE
          IF p10() THEN RETURN END
        END;
      ELSIF next=0 THEN
        IF p24() THEN RETURN END
      ELSE (*next>0*)
        IF p29() THEN RETURN END
      END;
    END;
    goto_82:=FALSE;
    (*82*)
    REPEAT
      man:=man+1;
      IF man>mymen THEN more:=-1; RETURN END;
      lsq:=mypce[man];  jv:=board[lsq];  name:=mytype[man]
    UNTIL jv=myval[name];
    CASE name OF
    |1: (*41*) -- QUEEN,ROOK OR BISHOP MOVE
        ipt:=0; ifn:=8;  next:=-1; ret:=p10();
    |2: (*8*)
        ipt:=4; ifn:=8;  next:=-1; ret:=p10();
    |3: (*9*)
        ipt:=0; ifn:=4;  next:=-1; ret:=p10();
    |4: (*17*)           -- KING OR KNIGHT MOVE
        ipt:=8; ifn:=16; next:=0;  ret:=p24();
    |5: (*18*)
        ipt:=0; ifn:=8;  next:=0;  ret:=p24();
    |6: (*40*)         --  PAWN MOVE
        ipt:=0;          next:=1;  ret:=p29();
    END;
    IF ret THEN RETURN END;
  END; (*MAIN LOOP*)
END wmove;

---------------------------  BMOVE  ----------------------------
                           ---------

(* GENERATE A BLACK MOVE *)

PROCEDURE bmove(VAR man,jv,lsq,nsq,kon,ipt,ifn,kc,next,ip,more: INTEGER;
                    kp: INTEGER);

  VAR name: INTEGER; goto_82: BOOLEAN;

  PROCEDURE p37;
  BEGIN
    ispce[man]:=nsq;
    board[lsq]:=0;   board[nsq]:=jv;   matbal:=matbal-kon
  END p37;

  PROCEDURE p51;
  BEGIN
    matbal:=matbal+isval[1]-isval[6];
    jv:=isval[1];    istype[man]:=1;   ip:=1;   p37
  END p51;

  PROCEDURE p10(): BOOLEAN;
  BEGIN
    REPEAT
      ipt:=ipt+1;
      IF ipt>ifn THEN goto_82:=TRUE; RETURN FALSE END;
      kc:=ink[ipt];   nsq:=lsq+kc;   kon:=board[nsq]
    UNTIL (kon>=0) & (kon<1000);
    IF ((kon>0)&(kon<1000) OR (kon=0)&(kp<=0)) THEN
      p37; RETURN TRUE
    END;
    RETURN FALSE
  END p10;

  PROCEDURE p24(): BOOLEAN;
  BEGIN
    REPEAT
      ipt:=ipt+1;
      IF ipt>ifn THEN goto_82:=TRUE; RETURN FALSE END;
      nsq:=lsq+ink[ipt];
      kon:=board[nsq]
    UNTIL (kon>=0) & (kon<1000);
    IF ((kon>0)&(kon<1000) OR (kon=0)&(kp<=0)) THEN
      p37; RETURN TRUE
    END;
    RETURN FALSE
  END p24;

  PROCEDURE p29(): BOOLEAN;
  BEGIN
    LOOP
      ipt:=ipt+1;
      IF ipt>4 THEN goto_82:=TRUE; RETURN FALSE END;
      IF ipt<2 THEN
        nsq:=lsq-10;  kon:=board[nsq];
        IF kon=0 THEN
          IF nsq<=30 THEN p51; RETURN TRUE END;
          IF kp<=0 THEN p37; RETURN TRUE ELSE EXIT END;
        ELSE
          ipt:=2;
        END;
      ELSIF ipt=2 THEN
        IF lsq>=80 THEN
          nsq:=lsq-20;  kon:=board[nsq];
          IF kon=0 THEN
            IF kp<=0 THEN p37; RETURN TRUE ELSE EXIT END;
          END;
        END;
      ELSE (* ipt>2 *)
        nsq:=lsq-ink[ipt];  kon:=board[nsq];
        IF (kon>0) & (kon<1000) THEN
          IF nsq<=30 THEN p51 ELSE p37 END;
          RETURN TRUE
        END;
      END;
    END;
    RETURN FALSE
  END p29;

  VAR ret: BOOLEAN;

BEGIN
  IF man>0 THEN
    IF ip>0 THEN
      matbal:=matbal-isval[1]+isval[6];
      jv:=isval[6];    istype[man]:=6;  ip:=0
    END;
    ispce[man]:=lsq;  board[lsq]:=jv;  board[nsq]:=kon;  matbal:=matbal+kon;
    IF more<=0 THEN RETURN END;
    goto_82:=FALSE;
  ELSE
    ip:=0;  more:=1;  goto_82:=TRUE;
  END;
  LOOP (* MAIN *)
    (*83*)
    WHILE NOT goto_82 DO
      IF next<0 THEN
        IF kon=0 THEN
          nsq:=nsq+kc;  kon:=board[nsq];
          IF (kon>=0) & (kon<1000) THEN
            IF (kon>0) OR (kon=0)&(kp<=0) THEN p37; RETURN END
          ELSE
            IF p10() THEN RETURN END
          END
        ELSE
          IF p10() THEN RETURN END
        END;
      ELSIF next=0 THEN
        IF p24() THEN RETURN END
      ELSE (*next>0*)
        IF p29() THEN RETURN END
      END;
    END (*LOOP83*);
    (*82*)
    goto_82:=FALSE;
    REPEAT
      man:=man+1;
      IF man>ismen THEN more:=-1; RETURN END;
      lsq:=ispce[man];  jv:=board[lsq]; name:=istype[man];
    UNTIL jv=isval[name];
    CASE name OF
    |1: (*41*) -- QUEEN,ROOK OR BISHOP MOVE
        ipt:=0; ifn:=8;  next:=-1; ret:=p10();
    |2: (*8*)
        ipt:=4; ifn:=8;  next:=-1; ret:=p10();
    |3: (*9*)
        ipt:=0; ifn:=4;  next:=-1; ret:=p10();
    |4: (*17*)           -- KING OR KNIGHT MOVE
        ipt:=8; ifn:=16; next:=0;  ret:=p24();
    |5: (*18*)
        ipt:=0; ifn:=8;  next:=0;  ret:=p24();
    |6: (*40*)         --  PAWN MOVE
        ipt:=0;          next:=1;  ret:=p29();
    END;
    IF ret THEN RETURN END;
  END; (*MAIN LOOP*)
END bmove;

---------------------------  FMOVE  ----------------------------
                           ---------

PROCEDURE fmove(VAR jv,lsq,nsq,kon,nm,kastle,iprom,more: INTEGER);
  VAR i: INTEGER;
    man: INTEGER;
BEGIN
  IF more>0 THEN
    IF nm>0 THEN
      IF kastle>0 THEN
        board[26]:=myval[5];
        board[27]:=0;
        board[28]:=0;
        board[29]:=myval[2];
        ASSERT(mymen>0);
        FOR i:=1 TO mymen DO
          IF mypce[i]=28 THEN mypce[i]:=26 END;
          IF mypce[i]=27 THEN mypce[i]:=29 END;
        END;
      ELSIF kastle<0 THEN
        board[22]:=myval[2];
        board[24]:=0;
        board[25]:=0;
        board[26]:=myval[5];
        ASSERT(mymen>0);
        FOR i:=1 TO mymen DO
          IF mypce[i]=24 THEN mypce[i]:=26 END;
          IF mypce[i]=25 THEN mypce[i]:=22 END;
        END;
      ELSE
        ASSERT(mymen>0);
        man:=1;
        WHILE (man<mymen) & (mypce[man]#nsq) DO INC(man) END;
        IF iprom>0 THEN
          matbal:=matbal-myval[1]+myval[6];
          jv:=myval[6];  mytype[man]:=6;
        END;
        mypce[man]:=lsq; matbal:=matbal+kon;
        board[lsq]:=jv;  board[nsq]:=kon;
      END
    END;
    nm:=nm+1;
    IF nm>nmove THEN more:=-1; RETURN END;
  END;
  iprom:=0;    kastle:=0;  jv:=moves[nm,1];
  IF jv=0 THEN kastle:=1;
    board[26]:=0;  board[27]:=myval[2];  board[28]:=myval[5];  board[29]:=0;
    ASSERT(mymen>0);
    FOR i:=1 TO mymen DO
      IF mypce[i]=26 THEN mypce[i]:=28 END;
      IF mypce[i]=29 THEN mypce[i]:=27 END;
    END;
    RETURN
  ELSIF jv<0 THEN kastle:=-1;
    board[22]:=0;  board[24]:=myval[5];  board[25]:=myval[2];  board[26]:=0;
    ASSERT(mymen>0);
    FOR i:=1 TO mymen DO
      IF mypce[i]=26 THEN mypce[i]:=24 END;
      IF mypce[i]=22 THEN mypce[i]:=25 END;
    END;
    RETURN
  ELSE
    lsq:=moves[nm,2];  nsq:=moves[nm,3];  kon:=moves[nm,4];
    ASSERT(mymen>0);
    man:=1;
    WHILE (man<mymen) & (mypce[man]#lsq) DO INC(man) END;
    IF mytype[man]=6 THEN
      IF nsq>=90 THEN
        matbal:=matbal+myval[1]-myval[6];
        jv:=myval[1];   mytype[man]:=1;  iprom:=1;
      END;
    END;
    mypce[man]:=nsq;  board[lsq]:=0;   board[nsq]:=jv;
    matbal:=matbal-kon;
    RETURN
  END;
END fmove;


---------------------------  MYCAS  ----------------------------
                           ---------

MODULE MYCAS;

IMPORT moves, myqn, mykg, board, nmove, mark;
IMPORT zzz, bmove;
EXPORT mycas;


VAR   man,jv,kon: INTEGER;
   more,kc,nx,ip: INTEGER;
 lsq,nsq,ipt,ifn: INTEGER;

PROCEDURE mycas;
BEGIN
  IF (myqn>0) & (board[23]=0) & (board[24]=0) & (board[25]=0) THEN
    man:=0;
    LOOP
      bmove(man,jv,lsq,nsq,kon,ipt,ifn,kc,nx,ip,more,0);
      IF more<=0 THEN EXIT END;
      IF (nsq>=24) & (nsq<=26) THEN more:=0 END;
    END;
    IF more<0 THEN
      nmove:=nmove+1;  moves[nmove,1]:=-1;  mark[nmove]:=40;
    END
  END;
  IF (mykg>0) & (board[27]=0) & (board[28]=0) THEN
    man:=0;
    LOOP
      bmove(man,jv,lsq,nsq,kon,ipt,ifn,kc,nx,ip,more,0);
      IF more<=0 THEN EXIT END;
      IF (nsq>=26) & (nsq<=28) THEN more:=0 END;
    END;
    IF more<0 THEN
      nmove:=nmove+1;  moves[nmove,1]:=0;  mark[nmove]:=90;
    END
  END;
END mycas;

BEGIN
  zzz(man,jv,kon,more,kc,nx,ip,lsq,nsq,ipt,ifn);
END MYCAS;

---------------------------  ISCAS  ----------------------------
                           ---------

MODULE ISCAS;

IMPORT moves, isqn, iskg, isval, ismen, ispce, board, nmove, mark;
IMPORT zzz, wmove;
EXPORT iscas;

VAR    jv,man: INTEGER;
  lsq,nsq,kon: INTEGER;
   ipt,ifn,kc: INTEGER;
   nx,ip,more: INTEGER;

PROCEDURE iscas(VAR kastle,illcas: INTEGER);
  VAR i: INTEGER;
BEGIN
  IF kastle=0 THEN RETURN END;
  IF kastle<0 THEN
    IF isqn<=0 THEN illcas:=1; RETURN END;
    FOR i:=93 TO 95 DO
      IF board[i]#0 THEN illcas:=1; RETURN END
    END;
    man:=0;
    LOOP
      wmove(man,jv,lsq,nsq,kon,ipt,ifn,kc,nx,ip,more,0); (* loop here!!!??? *)
      IF more=9 THEN illcas:=1; RETURN END;
      IF more<0 THEN EXIT END;
      IF (nsq>=94) & (nsq<=96) THEN more:=0 END;
    END;
    board[92]:=0;
    board[94]:=isval[5];
    board[95]:=isval[2];
    board[96]:=0;
    ASSERT(ismen>0);
    FOR i:=1 TO ismen DO
      IF ispce[i]=96 THEN ispce[i]:=94 END;
      IF ispce[i]=92 THEN ispce[i]:=95 END;
    END;
    RETURN
  ELSE (* kastle>0 *)
    IF iskg<=0 THEN illcas:=1; RETURN END;
    FOR i:=97 TO 98 DO
      IF board[i]#0 THEN illcas:=1; RETURN END
    END;
    man:=0;
    LOOP
      wmove(man,jv,lsq,nsq,kon,ipt,ifn,kc,nx,ip,more,0);
      IF more=0 THEN illcas:=1; RETURN END;
      IF more<0 THEN EXIT END;
      IF (nsq>=96) & (nsq<=98) THEN more:=0 END;
    END;
    board[96]:=0;
    board[97]:=isval[2];
    board[98]:=isval[5];
    board[99]:=0;
    ASSERT(ismen>0);
    FOR i:=1 TO ismen DO
      IF ispce[i]=96 THEN ispce[i]:=98 END;
      IF ispce[i]=99 THEN ispce[i]:=97 END;
    END;
  END;
END iscas;

BEGIN
  zzz(jv,man,lsq,nsq,kon,ipt,ifn,kc,nx,ip,more)
END ISCAS;

----------------------------  HEUR  ----------------------------
                            --------

MODULE HEUR;

IMPORT  board, fld, pri, ngame, mymen, ismen, myval, isval, mytype, istype
      , limit, nmove, ispce, moves, ijk, mark, jbig, kent;
IMPORT  zzz, mycas, wmove;

EXPORT heur;

VAR       ki,kj,jt: INTEGER;
    man,ipr,jv,kon: INTEGER;
    kc2,nx2,ip2,jb: INTEGER;
   lsq,nsq,ipt,ifn: INTEGER;
   mytotal,istotal: INTEGER;
 kc,nx,ip,jv2,kon2: INTEGER;
         lsq2,nsq2: INTEGER;
         more,mor2: INTEGER;
 ipt2,ifn2,ibon,m2: INTEGER;

PROCEDURE heur(VAR move: INTEGER);
  VAR inter,i,j,k,l: INTEGER;
BEGIN
  move:=move+1;
  IF move>=9 THEN
    IF move=9 THEN
      pri[5]:=1; pri[2]:=2; pri[6]:=3
    END;
    IF ngame#1 THEN
      mytotal:=0;
      ASSERT(mymen>0);
      FOR i:=1 TO mymen DO (* mymen=0 ??? *)
        mytotal:=mytotal+myval[mytype[i]];
      END;
      IF mytotal-limit>0 THEN
        istotal:=0;
        ASSERT(ismen>0);
        FOR i:=1 TO ismen DO (* ismen=0??? *)
          istotal:=istotal+isval[istype[i]];
        END;
        IF istotal+limit>=0 THEN
          ngame:=1; pri[1]:=1; pri[2]:=1; pri[5]:=4; pri[6]:=0;
        END;
      ELSE
        ngame:=1; pri[1]:=1; pri[2]:=1; pri[5]:=4; pri[6]:=0;
      END;
    END;
    i:=1;
    WHILE (i<=ismen) & (istype[i]#5) DO INC(i) END;
    ASSERT(i<=ismen);
    ki:=ispce[i];
    fld[ki]:=99;
    kj:=1;
    WHILE (kj<=10) & (ki>10) DO ki:=ki-10; kj:=kj+1 END;
    ipr:=0;
    LOOP
      ipr:=ipr+1;
      IF ipr>10 THEN EXIT END;
      i:=ki-ipr;
      IF i>0 THEN
        FOR l:=1 TO 10 DO fld[10*(l-1)+i]:=ijk[ipr] END;
      END;
      i:=ki+ipr;
      IF i<=10 THEN
        FOR l:=1 TO 10 DO fld[10*(l-1)+i]:=ijk[ipr] END;
      END;
      j:=kj-ipr;
      IF j>0 THEN
        FOR l:=1 TO 10 DO fld[10*(j-1)+l]:=ijk[ipr] END;
      END;
      j:=kj+ipr;
      IF j<=10 THEN
        FOR l:=1 TO 10 DO fld[10*(j-1)+l]:=ijk[ipr] END;
      END;
    END;
  END;

  nmove:=0;
  mycas();
  man:=0;
  LOOP
    wmove(man,jv,lsq,nsq,kon,ipt,ifn,kc,nx,ip,more,0);
    IF more<=0 THEN EXIT END;
    nmove:=nmove+1;
    moves[nmove,1]:=jv;
    moves[nmove,2]:=lsq;
    moves[nmove,3]:=nsq;
    moves[nmove,4]:=kon;
    jt:=mytype[man];
    mark[nmove]:=pri[jt]*(kent[nsq]-kent[lsq]+fld[nsq]-fld[lsq]);
    ibon:=0;
    IF jt-6>=0 THEN
      IF kon=0 THEN
        IF nsq-56=0 THEN
          IF lsq-36=0 THEN
            ibon:=30;
            IF (board[65]=isval[6]) OR (board[67]=isval[6]) THEN
              ibon:=5
            END;
          ELSE
            IF lsq=46 THEN ibon:=2 END;
          END;
        ELSE
          IF mark[nmove]>=0 THEN ibon:=5 ELSE ibon:=-5 END;
          IF board[nsq-10]=myval[6] THEN ibon:=ibon-10 END;
          IF board[nsq+10]=myval[6] THEN ibon:=ibon-10 END;
        END;
      ELSE (* kon#0 *)
        IF nsq-55=0 THEN
          IF lsq-35=0 THEN ibon:=20;
            IF (board[64]=isval[6]) OR (board[66]=isval[6]) THEN
              ibon:=5
            END;
          ELSE
            IF lsq=45 THEN ibon:=2 END;
          END;
        ELSE
          IF (lsq=32) OR (lsq=39) THEN ibon:=-5 END;
          IF (lsq=51) OR (lsq=36) THEN ibon:=10 END;
        END;
      END;
    ELSE
      IF move<9 THEN
        IF (jt=4) & ( (nsq=42) OR (nsq=49) ) THEN ibon:=-15 END;
        IF (nsq=45) & (board[35]=myval[6])  THEN ibon:=-50 END;
        IF (nsq=46) & (board[36]=myval[6])  THEN ibon:=-50 END;
        IF (jt=3) AND (lsq=27) THEN ibon:=ibon+2 END;
        IF (jt=4) AND (lsq=28) THEN ibon:=ibon+2 END;
      END;
    END;
    IF move>=9 THEN
      m2:=0;
      LOOP
        wmove(m2,jv2,lsq2,nsq2,kon2,ipt2,ifn2,kc2,nx2,ip2,mor2,0);
        IF mor2<=0 THEN EXIT END;
        ibon:=ibon+1;
      END;
      IF (lsq=44) & (board[34]=myval[6]) THEN ibon:=ibon+5 END;
      IF (lsq=47) & (board[37]=myval[6]) THEN ibon:=ibon+5 END;
      IF (ngame>0) & (jt=6) THEN
        ibon:=ibon+10;
        IF nsq-lsq=20 THEN ibon:=ibon+5 END;
      END;
    END;
    mark[nmove]:=mark[nmove]+ibon
  END; (* LOOP *)
  ASSERT(nmove>0);
  FOR i:=1 TO nmove DO (* nmove=0??? *)
    jb:=-jbig;
    FOR j:=i TO nmove DO
      IF mark[j]>=jb THEN ip:=j; jb:=mark[j] END;
    END;
    FOR k:=1 TO 4 DO
      inter:=moves[i,k];
      moves[i,k]:=moves[ip,k];
      moves[ip,k]:=inter
    END;
    mark[ip]:=mark[i]
  END;
END heur;

BEGIN
  zzz(ki,kj,mytotal,istotal,man,ipr,jv,kon,lsq,nsq,ipt,ifn
     ,kc,nx,ip,more,mor2,jv2,kon2,lsq2,nsq2,ipt2,ifn2
     ,kc2,nx2,ip2,ibon,jt,m2,jb);
END HEUR;


----------------------------  TREE  ----------------------------
                            --------

MODULE TREE;

(* CREATE AND SEARCH MOVE TREE          *)
(* ADJUST DEPTH TO SUIT COMPUTER SPEED  *)
(* lev:=0 FOR SLOW COMPUTERS; lev:=1    *)
(* FOR FASTER COMPUTERS                 *)

IMPORT zzz, board, matbal, jbig, jvbig, lev
     , mypce, ispce, myval, isval, mytype, istype;
IMPORT wmove, bmove, fmove;

EXPORT tree;

VAR
  myking : INTEGER;
  isking : INTEGER;

  jab1,   jv1,lsq1,nsq1,kon1,nm,kas           ,ip1,mor1: INTEGER;
  jab2,m2,jv2,lsq2,nsq2,kon2,ipt2,ifn2,kc2,nx2,ip2,mor2: INTEGER;
  jab3,m3,jv3,lsq3,nsq3,kon3,ipt3,ifn3,kc3,nx3,ip3,mor3: INTEGER;
  jab4,m4,jv4,lsq4,nsq4,kon4,ipt4,ifn4,kc4,nx4,ip4,mor4: INTEGER;
  jab5,m5,jv5,lsq5,nsq5,kon5,ipt5,ifn5,kc5,nx5,ip5,mor5: INTEGER;
  jab6,m6,jv6,lsq6,nsq6,kon6,ipt6,ifn6,kc6,nx6,ip6,mor6: INTEGER;
  jab7,m7,jv7,lsq7,nsq7,kon7,ipt7,ifn7,kc7,nx7,ip7,mor7: INTEGER;


PROCEDURE tree(VAR mov,mate: INTEGER);

  VAR ij,ik: INTEGER;

  PROCEDURE p7;
  BEGIN
    LOOP
      wmove(m7,jv7,lsq7,nsq7,kon7,ipt7,ifn7,kc7,nx7,ip7,mor7,1);
      IF mor7<=0 THEN EXIT END;
      IF matbal>jab7 THEN jab7:=matbal END;
      IF jab7>=jab6  THEN mor7:=0 END;
    END;
  END p7;

  PROCEDURE p6;
  BEGIN
    LOOP
      bmove(m6,jv6,lsq6,nsq6,kon6,ipt6,ifn6,kc6,nx6,ip6,mor6,1);
      IF mor6<=0 THEN EXIT END;
      IF kon6=myking THEN
        jab6:=-jbig
      ELSE
        jab7:=jab5; m7:=0;
        IF matbal>jab7 THEN jab7:=matbal END;
        IF jab7<jab6 THEN
          p7;
          IF mor7<0 THEN jab6:=jab7 END;
        END;
      END;
      IF jab6<=jab5 THEN mor6:=0 END;
    END;
  END p6;

  PROCEDURE p5;
  BEGIN
    LOOP
      wmove(m5,jv5,lsq5,nsq5,kon5,ipt5,ifn5,kc5,nx5,ip5,mor5,1);
      IF mor5<=0 THEN EXIT END;
      IF lev<=0 THEN
        IF (matbal-jab5)>0 THEN jab5:=matbal END;
      ELSE
        IF kon5=isking THEN
          jab5:=jbig
        ELSE
          jab6:=jab4; m6:=0;
          IF matbal<jab6 THEN jab6:=matbal END;
          IF jab6>jab5  THEN
            p6;
            IF mor6<0 THEN jab5:=jab6 END;
          END;
        END;
      END;
      IF jab5>=jab4 THEN mor5:=0 END;
    END;
  END p5;

  PROCEDURE p4;
  BEGIN
    LOOP
      bmove(m4,jv4,lsq4,nsq4,kon4,ipt4,ifn4,kc4,nx4,ip4,mor4,1);
      IF mor4<=0 THEN EXIT END;
      IF kon4=myking THEN
         jab4:=-jbig
      ELSE
        jab5:=jab3; m5:=0;
        IF matbal>jab5 THEN jab5:=matbal END;
        IF jab5<jab4 THEN
          p5;
          IF mor5<0 THEN jab4:=jab5 END
        END;
      END;
      IF jab4<=jab3 THEN mor4:=0 END;
    END
  END p4;

  PROCEDURE p3;
  BEGIN
    LOOP
      wmove(m3,jv3,lsq3,nsq3,kon3,ipt3,ifn3,kc3,nx3,ip3,mor3,0);
      IF mor3<=0 THEN EXIT END;
      IF kon3=isking THEN
        mor3:=0; ik:=m3
      ELSE
        jab4:=jab2; m4:=0;
        IF matbal<jab4 THEN jab4:=matbal END;
        IF jab4>jab3 THEN
          p4;
          IF mor4<0 THEN jab3:=jab4; ik:=m3 END;
        END;
        IF jab3>=jab2 THEN mor3:=0 END;
      END;
    END
  END p3;

  PROCEDURE p2;
    VAR in3,in4: INTEGER;
  BEGIN
    LOOP
      bmove(m2,jv2,lsq2,nsq2,kon2,ipt2,ifn2,kc2,nx2,ip2,mor2,0);
      IF mor2<=0 THEN EXIT END;
      IF kon2=myking THEN
        mor2:=0; ij:=m2
      ELSE
        jab3:=jab1; m3:=0;
        p3;
        IF mor3<0 THEN jab2:=jab3; ij:=m2 END;
        in3:=mypce[ik]; in4:=mytype[ik];
        WHILE ik#1 DO
          mypce[ik]:=mypce[ik-1]; mytype[ik]:=mytype[ik-1];
          ik:=ik-1;
        END;
        mypce[1]:=in3; mytype[1]:=in4;
        IF jab2<=jab1 THEN mor2:=0 END;
      END;
    END;
  END p2;

  VAR in1,in2: INTEGER;

BEGIN
  ij:=1;  mor1:=1;
  ik:=1;  mate:=0;
  nm:=0;  jab1:=-jvbig;
  myking:=myval[5];
  isking:=isval[5];
  LOOP
    fmove(jv1,lsq1,nsq1,kon1,nm,kas,ip1,mor1);
    IF mor1<=0 THEN
      IF jab1=-jvbig THEN mate:=+1 END;
      IF jab1= jvbig THEN mate:=-1 END;
      RETURN
    END;
    jab2:=jvbig;
    m2:=0;
    p2;
    IF mor2<0 THEN jab1:=jab2; mov:=nm END;
    in1:=ispce[ij]; in2:=istype[ij];
    WHILE ij#1 DO
      ispce[ij]:=ispce[ij-1]; istype[ij]:=istype[ij-1];
      ij:=ij-1
    END;
    ispce[1]:=in1; istype[1]:=in2;
  END; (* LOOP *)
END tree;

BEGIN
  zzz(
  jab1,   jv1,lsq1,nsq1,kon1,nm,kas           ,ip1,mor1,
  jab2,m2,jv2,lsq2,nsq2,kon2,ipt2,ifn2,kc2,nx2,ip2,mor2,
  jab3,m3,jv3,lsq3,nsq3,kon3,ipt3,ifn3,kc3,nx3,ip3,mor3,
  jab4,m4,jv4,lsq4,nsq4,kon4,ipt4,ifn4,kc4,nx4,ip4,mor4,
  jab5,m5,jv5,lsq5,nsq5,kon5,ipt5,ifn5,kc5,nx5,ip5,mor5,
  jab6,m6,jv6,lsq6,nsq6,kon6,ipt6,ifn6,kc6,nx6,ip6,mor6,
  jab7,m7,jv7,lsq7,nsq7,kon7,ipt7,ifn7,kc7,nx7,ip7,mor7);


END TREE;

----------------------------  MYGO  ----------------------------
                            --------

MODULE MYGO;

IMPORT  isval, myqn, mykg, iskg, isqn, ismen, ispce, istype;
IMPORT  zzz, wmove, fmove;
EXPORT  mygo;

VAR   ij,man: INTEGER;
   lsq2,nsq2: INTEGER;
   kon1,kon2: INTEGER;
  ifn,ipt,jv: INTEGER;
    kc,nx,ip: INTEGER;
        more: INTEGER;

PROCEDURE mygo(VAR lsq,nsq,kapt,mov,kastle,iprom,keck: INTEGER);
  VAR i,zero: INTEGER;
BEGIN
  zero:=0;
  fmove(jv,lsq,nsq,kon1,mov,kastle,iprom,zero);
  man:=0;      kapt:=0;     keck:=0;
  LOOP
    wmove(man,jv,lsq2,nsq2,kon2,ipt,ifn,kc,nx,ip,more,1);
    IF more<=0       THEN EXIT    END;
    IF kon2=isval[5] THEN keck:=1 END;
  END;
  IF kastle#0 THEN myqn:=0; mykg:=0; RETURN END;
  IF lsq=22 THEN myqn:=0 END;
  IF lsq=29 THEN mykg:=0 END;
  IF lsq=26 THEN myqn:=0; mykg:=0 END;
  IF kon1>=0 THEN RETURN END;

  IF nsq=92 THEN isqn:=0 END;
  IF nsq=99 THEN iskg:=0 END;
  ij:=0; kapt:=1;
  ASSERT(ismen>0);
  FOR i:=1 TO ismen DO
    ij:=ij+1;
    IF ispce[i]=nsq THEN
      ij:=ij-1;
    ELSE
      ispce[ij]:=ispce[i]; istype[ij]:=istype[i]
    END;
  END;
  ismen:=ismen-1
END mygo;

BEGIN
  zzz(ij,man,lsq2,nsq2,kon1,kon2,ifn,ipt,jv,kc,nx,ip,more);
END MYGO;

----------------------------  ISGO  ----------------------------
                            --------

MODULE ISGO;

IMPORT  iskg, isqn, mykg, myqn, myval, isval, mymen, mypce, mytype;
IMPORT  zzz, iscas, bmove, wmove;
EXPORT  isgo;

VAR ij,lsq1,nsq1: INTEGER;
       lsq2,nsq2: INTEGER;
       kon1,kon2: INTEGER;
      m1,jv1,jv2: INTEGER;
      m2,nx1,nx2: INTEGER;
     ifn,ipt,ip2: INTEGER;
         more,kc: INTEGER;

PROCEDURE isgo(VAR lsq,nsq,illeg,kastle,illcas,iprom,keck: INTEGER);
  VAR i: INTEGER;
BEGIN
  keck:=0; illeg:=-1; illcas:=0;
  IF kastle#0 THEN
    iscas(kastle,illcas);
    IF illcas>0 THEN RETURN END;
    isqn:=0; iskg:=0;
  ELSE
    m1:=0;
    REPEAT
      REPEAT
        bmove(m1,jv1,lsq1,nsq1,kon1,ipt,ifn,kc,nx1,iprom,more,0);
        IF more=0 THEN illeg:=0; RETURN END;
        IF more<0 THEN illeg:=1; RETURN END;
      UNTIL (lsq1=lsq) & (nsq1=nsq);
      m2:=0;
      LOOP
        wmove(m2,jv2,lsq2,nsq2,kon2,ipt,ifn,kc,nx2,ip2,more,1);
        IF more<=0 THEN EXIT END;
        IF more>0 THEN
          IF kon2=isval[5] THEN more:=0 END;
        END;
      END;
    UNTIL more<0;
  END;
  m2:=0;
  REPEAT
    bmove(m2,jv2,lsq2,nsq2,kon2,ipt,ifn,kc,nx2,ip2,more,1);
    IF more>0 THEN
      IF kon2=myval[5] THEN keck:=1 END;
    END;
  UNTIL more<=0;
  IF kastle#0 THEN RETURN END;
  IF lsq=92   THEN isqn:=0 END;
  IF lsq=99   THEN iskg:=0 END;
  IF lsq=96   THEN isqn:=0; iskg:=0 END;
  IF kon1<=0  THEN RETURN END;
  IF nsq=22   THEN myqn:=0 END;
  IF nsq=29   THEN mykg:=0 END;
  ij:=0;
  ASSERT(mymen#0);
  FOR i:=1 TO mymen DO
    ij:=ij+1;
    IF mypce[i]=nsq THEN
      ij:=ij-1
    ELSE
      mypce[ij]:=mypce[i]; mytype[ij]:=mytype[i]
    END;
  END;
  mymen:=mymen-1
END isgo;

BEGIN
   zzz(ij,lsq1,nsq1,lsq2,nsq2,kon1,kon2,m1
      ,jv1,jv2,m2,nx1,nx2,ifn,ipt,ip2,more,kc);
END ISGO;

----------------------------  BODY  ----------------------------
                            --------

VAR
    keck: INTEGER;
    move: INTEGER;
   color: INTEGER;
     mov: INTEGER;
    mate: INTEGER;
   lsq,nsq,kapt,kastle,iprom: INTEGER;
   illeg,illcas: INTEGER;

PROCEDURE find_man(val: INTEGER): INTEGER;
  VAR man: INTEGER;
BEGIN
  IF val=0 THEN RETURN 0 END;
  man:=1;
  WHILE (man<=6) & (val#myval[man]) DO INC(man) END;
  IF man<=6 THEN RETURN man END;
  man:=-1;
  WHILE (man>=-6) & (val#isval[-man]) DO DEC(man) END;
  IF man<-6 THEN RETURN 0 END;
  RETURN man
END find_man;

VAR line,col: INTEGER;

PROCEDURE print_board(first,attn: BOOLEAN);

  PROCEDURE delay;
    VAR k: INTEGER;
  BEGIN k:=5000; REPEAT k:=k-1 UNTIL k=0 END delay;

  VAR i,j,p,l,man0,man1: INTEGER;
BEGIN

  IF first THEN
    cb.show_board;
    l:=0;
    FOR p:=21 TO 100 BY 10 DO
      FOR i:=1 TO 8 DO
        man1:=find_man(board[p+i]); cb.put(man1,l,i-1)
      END;
      INC(l);
    END;
    pboard:=board;
    RETURN
  END;

  IF NOT attn THEN
    l:=0;
    FOR p:=21 TO 100 BY 10 DO
      FOR i:=1 TO 8 DO
        IF (board[p+i]#pboard[p+i]) THEN
          man1:=find_man( board[p+i]); cb.put(man1,l,i-1)
        END;
      END;
      INC(l);
    END;
    pboard:=board;
    RETURN
  END;

  l:=0;
  FOR p:=21 TO 100 BY 10 DO
    FOR i:=1 TO 8 DO
      IF (board[p+i]#pboard[p+i]) THEN
        man1:=find_man( board[p+i]);
        man0:=find_man(pboard[p+i]);
        IF man1=0 THEN
          cb.bell;
          FOR j:=0 TO 2 DO
            cb.put(0,l,i-1);    delay;
            cb.put(man0,l,i-1); delay
          END;
          cb.put(0,l,i-1)
        END;
      END;
    END;
    INC(l);
  END;

  l:=0;
  FOR p:=21 TO 100 BY 10 DO
    FOR i:=1 TO 8 DO
      IF (board[p+i]#pboard[p+i]) THEN
        man1:=find_man( board[p+i]);
        man0:=find_man(pboard[p+i]);
        IF man1#0 THEN
          cb.bell;
          FOR j:=0 TO 2 DO
            cb.put(man1,l,i-1); delay;
            cb.put(0   ,l,i-1); delay
          END;
          cb.put(man1,l,i-1); line:=l; col:=i-1
        END
      END
    END;
    INC(l)
  END;
  pboard:=board
END print_board;

PROCEDURE ask;
BEGIN
  IF (matbal+15)<0 THEN
    cb.bell;
    cb.print('Хорошо,я согласна с вашим предложением');
    cb.wait;
    HALT
  END;
  IF (matbal-70)<0 THEN
    cb.bell;
    cb.print('Я отклоняю ваше предложение');
  ELSE
    cb.bell;
    cb.print('Вы должно быть шутник');
  END;
END ask;

PROCEDURE c_move;
  VAR man,l0,l1,c0,c1,i: INTEGER;
BEGIN
  heur(move);
  tree(mov,mate);
  IF keck<=0 THEN
    IF mate>0 THEN
      cb.bell; cb.print("ПАТ"); cb.wait;
      HALT
    END;
  ELSE
    IF mate>0 THEN
      cb.bell; cb.print('Поздравляю! МАТ!'); cb.wait;
      HALT
    END;
  END;

  mygo(lsq,nsq,kapt,mov,kastle,iprom,keck);
  IF    kastle<0 THEN man:=+9; l0:=-1; l1:= 0
  ELSIF kastle>0 THEN man:=+9; l0:= 0; l1:=-1
  ELSE
    l0:=(lsq-21) DIV 10;
    c0:=(lsq-21) MOD 10-1;
    l1:=(nsq-21) DIV 10;
    c1:=(nsq-21) MOD 10-1;
    man:=find_man(pboard[lsq]);
    IF kapt>0 THEN c1:=c1-8 END;
  END;
  cb.fixmove(man,l0,c0,l1,c1);
  print_board(FALSE,TRUE);
  IF (kastle=0) & (iprom>0) THEN
    cb.print("Пешка превратилась в Ферзя");
  END;
  IF keck<=0 THEN
    IF mate<0 THEN cb.bell; cb.print("ПАТ"); cb.wait; HALT END;
  ELSE
    IF mate<0 THEN
      cb.bell; cb.print("Вам МАТ... Благодарю за внимание.");
      cb.wait; HALT
    ELSE
      cb.bell; cb.print('ШАХ')
    END;
  END;
END c_move;


PROCEDURE m_move;

  VAR crs: INTEGER;

  PROCEDURE from;
    VAR ch: CHAR;
  BEGIN
    kastle:=0;
    LOOP
      cb.read(0,line,col,ch);
      CASE ch OF
        |'8': line:=(line+7) MOD 8;
        |'2': line:=(line+9) MOD 8;
        |'4': col :=(col +9) MOD 8;
        |'6': col :=(col +7) MOD 8;
        |'7': line:=(line+7) MOD 8;  col:=(col+9) MOD 8;
        |'1': line:=(line+9) MOD 8;  col:=(col+9) MOD 8;
        |'9': line:=(line+7) MOD 8;  col:=(col+7) MOD 8;
        |'3': line:=(line+9) MOD 8;  col:=(col+7) MOD 8;
        |"o": kastle:=+1; RETURN
        |"O": kastle:=-1; RETURN
        |33c: ask
        |15c: lsq:=line*10+(col+1)+21;
              crs:=find_man(board[lsq]);
              IF crs<0 THEN RETURN END
      ELSE
      END
    END
  END from;

  PROCEDURE to;

    PROCEDURE restore;
      VAR i,x: INTEGER;
    BEGIN
      i:=line*10+(col+1)+21; x:=find_man(board[i]); cb.put(x,line,col)
    END restore;

    VAR x,l,c: INTEGER; ch: CHAR;

  BEGIN
    LOOP
      cb.read(crs,line,col,ch);
      l:=line; c:=col;
      CASE ch OF
        |'8': l:=(line+7) MOD 8;
        |'2': l:=(line+9) MOD 8;
        |'4': c:=(col +9) MOD 8;
        |'6': c:=(col +7) MOD 8;
        |'7': l:=(line+7) MOD 8;  c:=(col+9) MOD 8;
        |'1': l:=(line+9) MOD 8;  c:=(col+9) MOD 8;
        |'9': l:=(line+7) MOD 8;  c:=(col+7) MOD 8;
        |'3': l:=(line+9) MOD 8;  c:=(col+7) MOD 8;
        |33c: ask
        |15c: nsq:=line*10+(col+1)+21;
              x:=find_man(board[nsq]);
              IF x>=0 THEN restore; cb.print(""); RETURN END
      ELSE
      END;
      IF (l#line) OR (c#col) THEN restore END;
      line:=l; col:=c
    END
  END to;

  VAR man,l0,l1,c0,c1: INTEGER;

BEGIN
  from;
  LOOP
    IF kastle=0 THEN to END;
    isgo(lsq,nsq,illeg,kastle,illcas,iprom,keck);

    IF   illcas>0 THEN cb.bell; cb.print('Рокировка невозможна'); from;
    ELSIF illeg=0 THEN cb.bell; cb.print('Ход под ШАХ!\n');       from;
    ELSIF illeg>0 THEN cb.bell; cb.print('Неправильный ход\n');   from;
    ELSIF illeg<0 THEN
      IF   iprom >0 THEN cb.bell; cb.print("Пешка превратилась в Ферзя") END;
      IF   keck  >0 THEN cb.bell; cb.print('ШАХ') END;
      IF    kastle<0 THEN man:=-9; l0:=-1; l1:= 0
      ELSIF kastle>0 THEN man:=-9; l0:= 0; l1:=-1
      ELSE
        l0:=(lsq-21) DIV 10;
        c0:=(lsq-21) MOD 10 - 1;
        l1:=(nsq-21) DIV 10;
        c1:=(nsq-21) MOD 10 - 1;
        man:=find_man(pboard[lsq]);
      END;
      IF pboard[nsq]>0 THEN c1:=c1-8 END;
      cb.fixmove(man,l0,c0,l1,c1);
      print_board(FALSE,FALSE);
      RETURN
    END;
  END;
END m_move;

VAR count: INTEGER;

BEGIN
  INIT;
  mov:=0;
  keck:=0;      iprom:=0;       kastle:=0;
  mate:=0;      illeg:=0;       illcas:=0;

  lev:=0;
  move:=0;
  color:=0;
  line:=1; col:=3;
  print_board(TRUE,FALSE);
  IF color=0 THEN c_move END;
  count:=0;
  LOOP
    m_move;
    c_move;
    count:=count+1;
    IF (mymen+ismen<16) OR (count>20) THEN lev:=1 END;
  END;
END chess.
