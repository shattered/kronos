IMPLEMENTATION MODULE chessBoard; (* Leo 13-Oct-89. (c) KRONOS *)

IMPORT  SYSTEM;
IMPORT  vio: videoGames;
IMPORT   kb: Keyboard;
IMPORT  tim: Time;

TYPE matrix = ARRAY [0..29] OF ARRAY [0..1] OF BITSET;

VAR men: ARRAY [1..6] OF matrix;
 shadow: ARRAY [1..6] OF matrix;

VAR
  ctime: INTEGER;
  mtime: INTEGER;
   last: INTEGER;

CONST
  X0=20; Y0=36; (* left down corner of the board *)
  Y1=266;  (* right upper corner of the moves script *)
  free = 0;
 queen = 1;
  rook = 2;
bishop = 3;
knight = 4;
  king = 5;
  pawn = 6;


PROCEDURE put_mtx(VAL m: matrix; l,mode,x,y: INTEGER);
  VAR i: INTEGER;
BEGIN
  vio.layer(l);
  FOR i:=29 TO 0 BY -1 DO
    vio.bits(mode,x,y,SYSTEM.ADR(m[i]),60);
    INC(y)
  END
END put_mtx;

PROCEDURE show(n: INTEGER; x,y: INTEGER);
BEGIN
  IF n>0 THEN
    put_mtx(men[ABS(n)],1,vio.or, x,y);  put_mtx(men[ABS(n)],0,vio.or ,x,y)
  ELSE
    put_mtx(men[ABS(n)],1,vio.bic,x,y);  put_mtx(men[ABS(n)],0,vio.bic,x,y)
  END;
  vio.layer(0); vio.refresh(x,y,60,30);
  vio.layer(1); vio.refresh(x,y,60,30);
END show;

PROCEDURE show_board;
  VAR i,j: INTEGER;
BEGIN
  vio.layer(1);
  vio.mode:=vio.rep;
  FOR i:=0 TO 7 DO
    vio.print(5,Y0+i*30+9,"%d",8-i);
    vio.print(X0+8*60+7,Y0+i*30+9,"%d",8-i);
    vio.print(X0+i*60+26,Y0+8*30+6,"%c",ORD("H")-i);
    vio.print(X0+i*60+26,Y0-18,"%c",ORD("H")-i);
  END;
END show_board;

PROCEDURE clean(l,c: INTEGER);
  VAR x,y: INTEGER;
BEGIN
  ASSERT(l IN {0..7});
  ASSERT(c IN {0..7});
  x:=c*60+X0; y:=l*30+Y0;
  vio.mode:=vio.bic;
  IF ODD(l+c) THEN vio.layer(0) ELSE vio.layer(1) END;
  vio.rect(x,y,x+59,y+29);
  vio.mode:=vio.rep;
  IF ODD(l+c) THEN vio.layer(1) ELSE vio.layer(0) END;
  vio.rect(x,y,x+59,y+29);
  vio.mode:=vio.or;
  vio.layer(0); vio.frame(x,y,x+59,y+30);
  vio.layer(1); vio.frame(x,y,x+59,y+30);
END clean;

PROCEDURE put(men,l,c: INTEGER);
BEGIN
  ASSERT(l IN {0..7});
  ASSERT(c IN {0..7});
  l:=7-l; c:=7-c;
  clean(l,c);
  IF men=0 THEN RETURN END;
  show(men,X0+c*60,Y0+l*30);
END put;

PROCEDURE print(VAL str: ARRAY OF CHAR; SEQ args: SYSTEM.WORD);
BEGIN
  vio.mode:=vio.rep;
  vio.layer(1);
  vio.print(0,0,"                                                       ");
  vio.layer(0);
  vio.print(0,0,"                                                       ");
  vio.print(0,0,str,args);
  vio.layer(1);
  vio.print(0,0,str,args);
END print;

PROCEDURE wait;
  VAR ch: CHAR;
BEGIN
  kb.read(ch);
END wait;

PROCEDURE bell;
BEGIN vio.bell END bell;

PROCEDURE read(n,l,c: INTEGER; VAR ch: CHAR);

  VAR x,y: INTEGER;
     mode: INTEGER;

  PROCEDURE crs;
    VAR i: INTEGER;
  BEGIN
    vio.mode:=vio.xor;
    x:=c*60+X0; y:=l*30+Y0;
    FOR i:=0 TO 1 DO
      vio.layer(i);
      vio.frame(x+30-24  ,y+15-12,  x+30+24  ,y+15+12  );
      vio.frame(x+30-24+4,y+15-12+2,x+30+24-4,y+15+12-2);
      IF n#0 THEN
        put_mtx(shadow[ABS(n)],i,mode,x,y);
        vio.refresh(x,y,60,30);
      END
    END;
  END crs;

BEGIN
  ASSERT(l IN {0..7});
  ASSERT(c IN {0..7});
  l:=7-l; c:=7-c;
  x:=X0+c*60; y:=Y0+l*30;
  IF n<0 THEN mode:=vio.bic ELSE mode:=vio.or END;
  crs; kb.read(ch); crs;
  CASE ch OF
  |kb.right: ch:='6'
  |kb.left : ch:='4'
  |kb.up   : ch:='8'
  |kb.dw   : ch:='2'
  |kb.end  : ch:='1'
  |kb.pgdw : ch:='3'
  |kb.pgup : ch:='9'
  |kb.home : ch:='7'
  |'5'     : ch:=15c
  ELSE
  END
END read;

VAR MEN: ARRAY [1..6] OF ARRAY [0..3] OF CHAR;
  count: INTEGER;

PROCEDURE fixmove(man,l0,c0,l1,c1: INTEGER);
  VAR x,next,t: INTEGER;     ld: CHAR;
BEGIN
  IF c1<0  THEN c1:=c1+8; ld:=':' ELSE ld:='-' END;
  IF man>0 THEN
    INC(ctime,tim.sys_time(tim.milisec)-last); t:=ctime DIV 1000;
    last:=tim.sys_time(tim.milisec);
    x:=X0+8*60+40
  ELSE
    INC(mtime,tim.sys_time(tim.milisec)-last);
    t:=mtime DIV 1000;
    last:=tim.sys_time(tim.milisec);
    x:=X0+8*60+140
  END;
  vio.mode:=vio.rep;
  vio.layer(1);
  next:=(count+1) MOD 20;
  vio.print(x,Y1-next*vio.char_h,"                       ");
  vio.print(x,Y1- count*vio.char_h   ,"          ");
  vio.print(x,Y1+vio.char_h,"   %2d:%02d ",t DIV 60,t MOD 60);
  IF    l0<0 THEN
    vio.print(x,Y1-count*vio.char_h,"   O-OO");
  ELSIF l1<0 THEN
    vio.print(x,Y1-count*vio.char_h,"   O-O");
  ELSE
    vio.print(x,Y1-count*vio.char_h,"%2s %c%d%c%c%d",MEN[ABS(man)]
               ,ORD("a")+c0,l0+1,ld,ORD("a")+c1,l1+1);
  END;
  IF man<0 THEN count:=next END;
END fixmove;

VAR m: matrix;
    h: matrix;
    c: INTEGER;

PROCEDURE begin;
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO HIGH(m) DO
    m[i][0]:={}; m[i][1]:={};
    h[i][0]:={}; h[i][1]:={}
  END;
  c:=0;
END begin;

PROCEDURE app(VAL s: ARRAY OF CHAR);
  VAR i,j,k: INTEGER;
BEGIN
  i:=0;
  WHILE s[i]#0c DO
    IF s[i]#' ' THEN
      j:=i*2+0; INCL(m[c][j DIV 32],j MOD 32);
      j:=i*2+1; INCL(m[c][j DIV 32],j MOD 32);
      k:=10 + c DIV 2;
      j:=i+4;   INCL(h[k][j DIV 32],j MOD 32);
    END;
    INC(i);
  END;
  INC(c);
END app;

PROCEDURE end(x: INTEGER);
BEGIN
  men[x]   :=m;
  shadow[x]:=h;
END end;

PROCEDURE init;
BEGIN
  begin;
  app("                              ");
  app("                              ");
  app("                              ");
  app("                              ");
  app("                              ");
  app("                              ");
  app("                              ");
  app("                              ");
  app("                              ");
  app("                              ");
  app("              **              ");
  app("             ****             ");
  app("              **              ");
  app("             ****             ");
  app("            ******            ");
  app("           ********           ");
  app("           ********           ");
  app("           ********           ");
  app("            ******            ");
  app("             ****             ");
  app("            ******            ");
  app("           ********           ");
  app("         ************         ");
  app("        **************        ");
  app("       ****************       ");
  app("       ****************       ");
  app("       ****************       ");
  app("                              ");
  app("                              ");
  app("                              ");
  end(pawn);

  begin;
  app("                              ");
  app("                              ");
  app("                              ");
  app("                              ");
  app("                              ");
  app("              **              ");
  app("              **              ");
  app("            ******            ");
  app("       ***    **    ***       ");
  app("     *******  **  *******     ");
  app("    ********* ** *********    ");
  app("   ***********  ***********   ");
  app("   ***********  ***********   ");
  app("   ************************   ");
  app("    **********************    ");
  app("    **********************    ");
  app("     ********************     ");
  app("     ********************     ");
  app("      ******************      ");
  app("      ******************      ");
  app("       ****************       ");
  app("       ****************       ");
  app("        * * * ** * * *        ");
  app("        * * * ** * * *        ");
  app("       ****************       ");
  app("       *              *       ");
  app("        **************        ");
  app("                              ");
  app("                              ");
  app("                              ");
  end(king);

  begin;
  app("                              ");
  app("                              ");
  app("                              ");
  app("                              ");
  app("                              ");
  app("              **              ");
  app("             *  *             ");
  app("        *     **     *        ");
  app("       * *    **    * *       ");
  app("    *   *     **     *   *    ");
  app("   * *  *     **     *  * *   ");
  app("    *    *    **    *    *    ");
  app("     *   *    **    *   *     ");
  app("     *   *   ****   *   *     ");
  app("      *   *  ****  *   *      ");
  app("      *   *  ****  *   *      ");
  app("      *   *  ****  *   *      ");
  app("       *  *  ****  *  *       ");
  app("       ** ** **** ** **       ");
  app("       ** ********** **       ");
  app("       ****************       ");
  app("       ****************       ");
  app("        * * * ** * * *        ");
  app("        * * * ** * * *        ");
  app("       ****************       ");
  app("       *              *       ");
  app("        **************        ");
  app("                              ");
  app("                              ");
  app("                              ");
  end(queen);

  begin;
  app("                              ");
  app("                              ");
  app("                              ");
  app("                              ");
  app("                              ");
  app("              *               ");
  app("             ****             ");
  app("           *******            ");
  app("          **********          ");
  app("         *** ********         ");
  app("         ****** ******        ");
  app("         ****** ******        ");
  app("        ******  ******        ");
  app("       *****    ******        ");
  app("       * *      ******        ");
  app("               ******         ");
  app("              *******         ");
  app("             *******          ");
  app("            *******           ");
  app("           *******            ");
  app("           ******             ");
  app("           ******             ");
  app("           ******             ");
  app("           ******             ");
  app("         **********           ");
  app("        ************          ");
  app("       **************         ");
  app("                              ");
  app("                              ");
  app("                              ");
  end(knight);

  begin;
  app("                              ");
  app("                              ");
  app("                              ");
  app("                              ");
  app("                              ");
  app("      ****  ******  ****      ");
  app("      *   ***    *** * *      ");
  app("       **   *    *   **       ");
  app("        **************        ");
  app("        ** *** **** **        ");
  app("        ** *** **** **        ");
  app("        *            *        ");
  app("        **** **** ****        ");
  app("        **** **** ****        ");
  app("        *            *        ");
  app("        ** ** **** ***        ");
  app("        ** ** **** ***        ");
  app("        *            *        ");
  app("        **** **** ****        ");
  app("        **** **** ****        ");
  app("        *            *        ");
  app("        ** *** **** **        ");
  app("        ** *** **** **        ");
  app("        **************        ");
  app("      ******************      ");
  app("     *                  *     ");
  app("     ********************     ");
  app("                              ");
  app("                              ");
  app("                              ");
  end(rook);

  begin;
  app("                              ");
  app("                              ");
  app("                              ");
  app("                              ");
  app("                              ");
  app("                              ");
  app("                              ");
  app("              **              ");
  app("             ****             ");
  app("              **              ");
  app("             ****             ");
  app("             ****             ");
  app("            ******            ");
  app("           ***  ***           ");
  app("           ***  ***           ");
  app("           *      *           ");
  app("           ***  ***           ");
  app("           ***  ***           ");
  app("            ******            ");
  app("            ******            ");
  app("             ****             ");
  app("             ****             ");
  app("              **              ");
  app("          ***********         ");
  app("        **   *  *   **        ");
  app("       *  ***    ***  *       ");
  app("      ****          ****      ");
  app("                              ");
  app("                              ");
  app("                              ");
  end(bishop);
END init;

BEGIN
  count:=0; ctime:=0; mtime:=0; last:=tim.sys_time(tim.milisec);
  MEN[rook  ]:="Л";
  MEN[bishop]:="С";
  MEN[pawn  ]:="";
  MEN[queen ]:="Ф";
  MEN[king  ]:="Кр";
  MEN[knight]:="К";
  init;
END chessBoard.
