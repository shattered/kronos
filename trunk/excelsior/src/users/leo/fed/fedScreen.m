IMPLEMENTATION MODULE fedScreen; (* nick 29-May-91. (c) KRONOS *)

FROM SYSTEM  IMPORT ADR, ADDRESS,WORD;

IMPORT  cod: defCodes;          IMPORT  err: defErrors;
IMPORT  scr: Screen;            IMPORT  bmg: BMG;
IMPORT  pup: pmPUP;             IMPORT  crs: pmCrs;

----------------------- F u r n i t u r e ----------------------
                       -------------------

PROCEDURE block0(fill,pressed: BOOLEAN; x0,y0,x1,y1: INTEGER);
  VAR t: bmg.TOOL;
BEGIN
 t:=full;
 IF fill THEN
   t.color:=pup.normal; bmg.rect(bmd,t,x0+1,y0+1,x1-1,y1-1)
 END;
 IF pressed THEN t.color:=pup.shadow ELSE t.color:=pup.bright END;
 bmg.line(bmd,t,x0,y0+1,x0,y1);
 bmg.line(bmd,t,x0,y1  ,x1,y1);
 IF pressed THEN t.color:=pup.bright ELSE t.color:=pup.shadow END;
 bmg.line(bmd,t,x1,y0,x1,y1-1);
 bmg.line(bmd,t,x1,y0,x0,y0  );
END block0;

PROCEDURE block(b: bmg.BLOCK; fill,pressed: BOOLEAN);
BEGIN
  block0(fill,pressed,b.x,b.y,b.x+b.w-1,b.y+b.h-1)
END block;

PROCEDURE print(b: bmg.BLOCK; x: INTEGER; f: ARRAY OF CHAR; SEQ a: WORD);
  VAR t: bmg.TOOL;
BEGIN
  INC(b.x); INC(b.y); INC(b.w,2); INC(b.w,2);
  t:=full;            t.color:=pup.black;
  t.clip:=b;          t.back :=pup.normal;
  bmg.print(bmd,t,b.x+x,b.y+1+pup.font^.bline,pup.font,f,a)
END print;

PROCEDURE clear;
  VAR t: bmg.TOOL;
BEGIN
  t:=full;
  t.color:=pup.shadow;
  bmg.rect(bmd,t,wblk.x,wblk.y,wblk.x+wblk.w-1,wblk.y+wblk.h-1);
  t.color:=pup.black;
  bmg.frame(bmd,t,wblk.x,wblk.y,wblk.x+wblk.w-1,wblk.y+wblk.h-1)
END clear;

------------------ C u r s o r   S u p p o r t -----------------
                  -----------------------------

PROCEDURE _inrect(x,y,h,w: INTEGER): BOOLEAN;
CODE
  cod.li1 cod.sub cod.swap
  cod.li1 cod.sub cod.bmg
  cod.bmg_inrect
END _inrect;

PROCEDURE inrect(x,y,w,h: INTEGER): BOOLEAN;
BEGIN
  RETURN _inrect(x,y,h,w)
END inrect;

PROCEDURE inblock(VAL b: bmg.BLOCK): BOOLEAN;
BEGIN
  RETURN _inrect(crs.x-b.x,crs.y-b.y,b.h,b.w)
END inblock;

----------------------------------------------------------------

CONST dp= ARRAY OF INTEGER
-- black  shadow normal light
  {0,0,0, 1,1,1, 2,2,2, 3,3,3,  2,2,2, 2,2,0, 0,2,0, 0,0,2
  ,3,3,3, 3,3,3, 3,3,3, 3,3,3,  3,2,2, 3,3,2, 2,3,2, 2,2,3};

PROCEDURE start_init;
  VAR x,y,i: INTEGER; s: BITSET;
BEGIN
  scr.loophole(scr.bitmap,bmd);
  IF NOT scr.done THEN HALT(scr.error) END;
  WITH scr.state^ DO
    ASSERT((W>= 480)&(H>= 360)&(bpp>=4),err.not_enough);
    ASSERT((W<=1024)&(H<=1024)&(bpp<=8),err.too_large);
  END;
  FOR i:=0 TO HIGH(dp) DIV 3 DO
    WITH scr.state^.pal[i] DO r:=dp[i*3]; g:=dp[i*3+1]; b:=dp[i*3+2] END
  END;
  scr.set_palette(scr.state^.pal,0,16);
  IF NOT scr.done THEN HALT(scr.error) END;
  WITH full DO
    zX:=0; clip.x:=0; clip.w:=scr.state^.W; mode:=bmg.rep; back :={ };
    zY:=0; clip.y:=0; clip.h:=scr.state^.H; mask:={0..3};  color:=mask
  END;
  pup.setplanes({0},{1},{0,1});
  bmg.erase(bmd)
END start_init;

PROCEDURE main_board;
  VAR s: INTEGER;
BEGIN
  s:=pup.sfont^.H+3;
  wblk:=full.clip;
  WITH wblk DO
    block0(TRUE,FALSE,x+1,y+1,x+w-2,y+h-2);
    block0(FALSE,TRUE,x+3,y+3,x+w-4,y+h-4);
  END;
  INC(wblk.x,4); DEC(wblk.w,8);
  INC(wblk.y,4); DEC(wblk.h,8);
  rblk:=wblk;  rblk.h:=s;
  sblk:=rblk;  sblk.y:=sblk.y+s;
  tblk:=rblk;  tblk.y:=wblk.y+wblk.h-s;
  mblk:=tblk;  mblk.y:=tblk.y-s;         tblk.w:=tblk.w-s;
  quit:=tblk;  quit.w:=s;                tblk.x:=tblk.x+s;

  wblk.h:=wblk.h-s*4;
  wblk.y:=wblk.y+s*2;

  block(quit,FALSE,FALSE);  pup.switch(quit,FALSE);
  block(tblk,FALSE,FALSE);  print(tblk,0,version);
  block(mblk,FALSE,FALSE);
  block(rblk,FALSE,FALSE);  block(sblk,FALSE,FALSE);
  clear
END main_board;

BEGIN
  version:="  Font Editor V01.00";
  start_init;
  main_board;
  crs.setcolor(crs.cur,{3});
END fedScreen.
