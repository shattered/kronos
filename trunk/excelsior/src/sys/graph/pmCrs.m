IMPLEMENTATION MODULE pmCrs; (* Leo 16-Apr-91. (c) KRONOS *)

IMPORT       SYSTEM;
IMPORT  cod: defCodes;
IMPORT  err: defErrors;
IMPORT  def: defScreen;
IMPORT  map: defBMG;
IMPORT  low: lowLevel;
IMPORT  cpd: CPD;
IMPORT  key: Keyboard;
IMPORT  scr: Screen;
IMPORT  mem: Heap;

FROM SYSTEM  IMPORT ADR;

TYPE
  WORD    = SYSTEM.WORD;
  ADDRESS = SYSTEM.ADDRESS;
  CURSOR  = RECORD
              size : INTEGER;
              base0: ADDRESS;
              base1: ADDRESS;
              dx,dy: INTEGER;
              color: BITSET;
              clip : def.BLOCK;
            END;

VAR
  bits : INTEGER;
  B    : map.BITMAP;
  T    : def.TOOL;
  curs : ARRAY [-15..+15] OF CURSOR;
  sos  : ADDRESS;
  bump : ADDRESS;
  saved: BOOLEAN;

PROCEDURE _move (a,b,c    : ADDRESS); CODE cod.move END _move;

PROCEDURE bad_parm; BEGIN done:=FALSE; error:=err.bad_parm END bad_parm;

CONST
  mask = ARRAY OF BITSET { {      },
    {00..00},{00..01},{00..02},{00..03},{00..04},{00..05},{00..06},{00..07},
    {00..08},{00..09},{00..10},{00..11},{00..12},{00..13},{00..14},{00..15},
    {00..16},{00..17},{00..18},{00..19},{00..20},{00..21},{00..22},{00..23},
    {00..24},{00..25},{00..26},{00..27},{00..28},{00..29},{00..30},{00..31} };


CONST _show = +1; _clear = -1; _movie = 0;

PROCEDURE moveto(X,Y: INTEGER; action: INTEGER);

  VAR shift: INTEGER;
      sx,sy: INTEGER;
      rx,ry: INTEGER;

  PROCEDURE draw(dir,w,h0,h1,h2: INTEGER; res,sav,b0,b1,L: ADDRESS;
                 wpl,rofs,sofs : INTEGER; color: BITSET);
    VAR l: ADDRESS;
        c: BITSET;
    a0,a1: POINTER TO BITSET;
    p0,p1: BITSET;
    m0,m1: BITSET;
   n,h,ds: INTEGER;
  BEGIN
    ds:=dir*2;
    sofs:=sofs+ORD(sx<0)-ORD(sx+w>T.clip.w);
    rofs:=rofs+ORD(rx<0)-ORD(rx+w>T.clip.w);
    p0:=mask[w]<<shift;
    p1:=mask[shift];
    m0:=p0-p1;
    m1:=p0*p1;
    IF sx<0          THEN m0:=m1; m1:={} END;
    IF sx+w>T.clip.w THEN m1:=m0; m0:={} END;
    h:=h0;
    WHILE h>0 DO
      n:=bits;  l:=L;  c:=color;
      p0:=b0^<<shift; p1:=b1^<<shift;
      REPEAT
        a0:=ADDRESS(l^)+sofs; a1:=ADDRESS(a0)+1;
        _move(sav,a0,2); a0^:=a0^-p0*m0; a1^:=a1^-p0*m1;
        IF c*{0}#{} THEN a0^:=a0^+p1*m0; a1^:=a1^+p1*m1 END;
        INC(sav,ds); INC(l,dir); c:=c>>dir; DEC(n)
      UNTIL n=0;
      INC(sofs,wpl);  INC(b0,dir);  INC(b1,dir);  DEC(h)
    END;
    h:=h1;
    WHILE h>0 DO
      n:=bits; l:=L;     c:=color;
      p0:=b0^<<shift; p1:=b1^<<shift;
      REPEAT
        _move(INTEGER(l^)+rofs,res,2);
        a0:=ADDRESS(l^)+sofs; a1:=ADDRESS(a0)+1;
        _move(sav,a0,2); a0^:=a0^-p0*m0; a1^:=a1^-p0*m1;
        IF c*{0}#{} THEN a0^:=a0^+p1*m0; a1^:=a1^+p1*m1 END;
        INC(sav,ds); INC(res,ds); INC(l,dir); c:=c>>dir; DEC(n)
      UNTIL n=0;
      INC(sofs,wpl);  INC(rofs,wpl);  INC(b0,dir);  INC(b1,dir);  DEC(h)
    END;
    h:=h2;
    WHILE h>0 DO
      n:=bits;  l:=L;
      REPEAT
        _move(INTEGER(l^)+rofs,res,2); INC(res,ds); INC(l,dir); DEC(n)
      UNTIL n=0;
      INC(rofs,wpl);  INC(b0,dir);  INC(b1,dir);  DEC(h)
    END
  END draw;

  VAR wpl: INTEGER;       rofs: INTEGER;
      res: ADDRESS;       sofs: INTEGER;
      sav: ADDRESS;      b0,b1: ADDRESS;
    w,h,H: INTEGER;   h0,h1,h2: INTEGER;

BEGIN
  wpl:=B^.WPL;
  WITH curs[cur] DO
    rx:=x-dx;  sx:=X-dx;  b0:=base0;
    ry:=y-dy;  sy:=Y-dy;  b1:=base1;  w:=size
  END;
  res:=sos;
  sav:=bump;
  H:=w;
  h:=w;
  IF sy<0          THEN INC(H,sy); sy:=0 END;
  IF sy+H>T.clip.h THEN H:=T.clip.h-sy; INC(b0,w-H); INC(b1,w-H) END;
  h:=w;
  IF ry<0          THEN INC(h,ry); ry:=0 END;
  IF ry+h>T.clip.h THEN h:=T.clip.h-ry   END;
  shift:=sx MOD 32;
  sofs :=(B^.H-sy-H)*wpl+sx DIV 32;
  rofs :=(B^.H-ry-h)*wpl+rx DIV 32;
  IF action=_show THEN
    draw(+1,w,H,0,0,res,sav,b0,b1,ADR(B^.layers[0]),+wpl,rofs,sofs,color)
  ELSIF action=_clear THEN
    draw(+1,w,0,0,h,res,sav,b0,b1,ADR(B^.layers[0]),+wpl,rofs,sofs,color)
  ELSIF (y>Y) & (y<Y+w) THEN
    INC(rofs,(h-1)*wpl);  INC(res,h*8-2);  INC(b0,H-1);
    INC(sofs,(H-1)*wpl);  INC(sav,H*8-2);  INC(b1,H-1);
    h1:=sy+H-ry;  h0:=H-h1;  h2:=h-h1;
    draw(-1,w,h0,h1,h2,res,sav,b0,b1,ADR(B^.layers[bits-1]),-wpl,rofs,sofs,color>>3)
  ELSIF (Y>y) & (Y<y+w) THEN
    h1:=ry+h-sy;  h0:=H-h1;  h2:=h-h1;
    draw(+1,w,h0,h1,h2,res,sav,b0,b1,ADR(B^.layers[0]),+wpl,rofs,sofs,color)
  ELSIF y=Y THEN
    draw(+1,w,0,h,0,res,sav,b0,b1,ADR(B^.layers[0]),+wpl,rofs,sofs,color)
  ELSE
    draw(+1,w,H,0,h,res,sav,b0,b1,ADR(B^.layers[0]),+wpl,rofs,sofs,color)
  END;
  x:=X; y:=Y;
  _move(sos,bump,w*8)
END moveto;

PROCEDURE _clip(VAR X,Y: INTEGER);
BEGIN
  WITH clip DO
    IF Y<y THEN Y:=y ELSIF Y>=y+h THEN Y:=y+h-1 END;
    IF X<x THEN X:=x ELSIF X>=x+w THEN X:=x+w-1 END;
  END
END _clip;

PROCEDURE move(X,Y: INTEGER);
BEGIN
  done:=TRUE;
  _clip(X,Y);
  IF (X=x) & (Y=y) THEN RETURN END;
  IF NOT on THEN x:=X; y:=Y ELSE moveto(X,Y,_movie) END
END move;

PROCEDURE setcolor(n: INTEGER; c: BITSET);
BEGIN
  IF (n<-15) OR (n>+15) OR (c={}) THEN bad_parm; RETURN END;
  IF on & (n=cur) THEN moveto(x,y,_clear) END;
  curs[n].color:=c;
  IF n=cur THEN color:=c END;
  IF on & (n=cur) THEN moveto(x,y,_show) END
END setcolor;

PROCEDURE setclip(n: INTEGER; c: def.BLOCK);
BEGIN
  WITH c DO
    IF x<0 THEN INC(w,x); x:=0 END;
    IF y<0 THEN INC(h,y); y:=0 END;
    IF x+w>T.clip.w THEN w:=T.clip.w-x END;
    IF y+h>T.clip.h THEN h:=T.clip.h-y END;
    IF (w<=0) OR (h<=0) THEN bad_parm; RETURN END
  END;
  IF on & (n=cur) THEN moveto(x,y,_clear) END;
  curs[n].clip:=c;
  IF on & (n=cur) THEN _clip(x,y); moveto(x,y,_show) END
END setclip;

PROCEDURE style(n: INTEGER);

  PROCEDURE show;
  BEGIN
    IF on THEN _clip(x,y); moveto(x,y,_show)  END
  END show;

  VAR b,s: ADDRESS; i: INTEGER;

BEGIN
  done:=TRUE;
  IF (n<-15) OR (n>+15) THEN bad_parm; RETURN END;
  IF n=cur          THEN RETURN END;
  IF curs[n].size=0 THEN done:=FALSE; error:=err.undef; RETURN END;
  IF on THEN moveto(x,y,_clear) END;
  IF curs[n].size#curs[cur].size THEN
    i:=curs[n].size*2*bits;
    mem.allocate(s,i);  done:=mem.done;
    IF NOT done THEN error:=mem.error; show; RETURN END;
    mem.allocate(b,i);  done:=mem.done;
    IF NOT done THEN error:=mem.error; show; mem.deallocate(s,i); RETURN END;
    i:=curs[cur].size*2*bits;
    mem.deallocate(sos ,i); sos :=s;
    mem.deallocate(bump,i); bump:=b
  END;
  cur:=n;
  color:=curs[n].color;
  clip :=curs[n].clip;
  show
END style;

PROCEDURE toggle(onscreen: BOOLEAN);
BEGIN
  done:=TRUE;
  IF on=onscreen THEN RETURN END;
  on:=onscreen;
  IF on THEN moveto(x,y,_show) ELSE moveto(x,y,_clear) END
END toggle;

PROCEDURE read(VAR X,Y: INTEGER);
  VAR k,K: BITSET;
  i,dx,dy: INTEGER;
BEGIN
  i:=cpd.ready();
  K:=cpd.state^.keys;  X:=x;  Y:=y;
  IF i>0 THEN
    REPEAT
      cpd.read(dx,dy,k);
      IF k=K THEN INC(X,dx); INC(Y,dy) END
    UNTIL (cpd.ready()<=0) OR (k#K)
  ELSE
    dx:=0; dy:=0;
    LOOP
      IF key.ready()>0 THEN EXIT END;
      cpd.wait(20);
      IF cpd.ready()>0 THEN
        cpd.read(dx,dy,k);
        IF k=K THEN INC(X,dx); INC(Y,dy) END;
        EXIT
      END
    END
  END;
  _clip(X,Y)
END read;

PROCEDURE monitor;
  VAR X,Y: INTEGER;
BEGIN
  read(X,Y); move(X,Y)
END monitor;

PROCEDURE round(b0,b1: ADDRESS; size: INTEGER);
  VAR i,j: INTEGER;
      s,d: POINTER TO BITSET;
BEGIN
  FOR i:=1 TO size-2 DO
    s:=b1+i;  s^:=s^<<1;
    d:=b0+i;  d^:=(s^>>1)+(s^)
  END;
  FOR i:=1 TO size-2 DO
   s:=b1+i;
   d:=b0+i+1; d^:=d^+s^+(s^>>1);
  END;
END round;

PROCEDURE _define(n,Dx,Dy,s: INTEGER; VAL map: ARRAY OF WORD);
  VAR b0,b1: ADDRESS;
BEGIN
  INC(s,2);
  WITH curs[n] DO
    mem.allocate(b0,s); done:=mem.done;
    IF NOT done THEN error:=mem.error; RETURN END;
    mem.allocate(b1,s); done:=mem.done;
    IF NOT done THEN error:=mem.error; mem.deallocate(b0,s); RETURN END;
    low._zero(b0,s);
    low._zero(b1,s);
    IF size>0 THEN
      mem.deallocate(base0,size); mem.deallocate(base1,size)
    END;
    size :=s;
    base0:=b0;
    base1:=b1;
    dx   :=Dx+1;
    dy   :=Dy+1;
    _move(b1+1,ADR(map),s-2);
    round(b0,b1,s)
  END;
  curs[n].color:=color;
  curs[n].clip :=clip;
END _define;

PROCEDURE define(cur,x,y,w: INTEGER; VAL map: ARRAY OF WORD);
BEGIN
  IF (cur<=0)OR(x<0)OR(y<0)OR(w<=0)OR(x>=w)OR(y>=w)OR NOT (w IN {2..30}) THEN
    bad_parm; RETURN
  END;
  _define(cur,x,y,w,map)
END define;

PROCEDURE standards;
  CONST
    _rarrow = ARRAY OF BITSET {
                {0,1,2,3,4,5,6,7              },
                {0,1,2,3,4,5,6                },
                {0,1,2,3,4,5                  },
                {0,1,2,3,4,5                  },
                {0,1,2,3,4,5,6                },
                {0,1,2,3,4,5,6,7              },
                {0,1,    4,5,6,7,8            },
                {0,        5,6,7,8,9          },
                {            6,7,8,9,10       },
                {              7,8,9,10,11    },
                {                8,9,10,11,12 },
                {                  9,10,11    },
                {                    10       } };

    _arrow  = ARRAY OF BITSET {
                {00                                   },
                {  01,02                              },
                {  01,02,03,04                        },
                {     02,03,04,05,06                  },
                {     02,03,04,05,06,07,08            },
                {        03,04,05,06,07,08,09,10      },
                {        03,04,05,06,07,08,09         },
                {           04,05,06,07,08,09         },
                {           04,05,06,07,08,09,10      },
                {              05,06,07,08,09,10,11   },
                {              05,      08,09,10,11,12},
                {                          09,10,11   },
                {                             10      }  };

    _cross  = ARRAY OF BITSET {
                {                  06                  },
                {                  06                  },
                {                  06                  },
                {                  06                  },
                {                                      },
                {                                      },
                {00,01,02,03,      06,      09,10,11,12},
                {                                      },
                {                                      },
                {                  06                  },
                {                  06                  },
                {                  06                  },
                {                  06                  }  };


    _xcross = ARRAY OF BITSET {
                {00,                                 12},
                {   01,                           11   },
                {      02,                     10      },
                {         03,               09         },
                {            04,         08            },
                {                                      },
                {                  06                  },
                {                                      },
                {            04,         08            },
                {         03,               09         },
                {      02,                     10      },
                {   01,                           11   },
                {00,                                 12} };


    _clock  = ARRAY OF BITSET {
                {00,01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16}<<2,
                {      02,                                 14      }<<2,
                {      02,                                 14      }<<2,
                {      02,                                 14      }<<2,
                {      02,03,      06,   08,   10,         14      }<<2,
                {         03,         07,   09,         13         }<<2,
                {         03,                           13         }<<2,
                {            04,                     12            }<<2,
                {               05,      08,      11               }<<2,
                {               05,               11               }<<2,
                {               05,06,         10,11               }<<2,
                {               05,               11               }<<2,
                {               05,      08,      11               }<<2,
                {            04,                     12            }<<2,
                {         03,                           13         }<<2,
                {         03,         07,   09,         13         }<<2,
                {      02,03,      06,   08,   10,         14      }<<2,
                {      02,      05,   07,   09,   11,      14      }<<2,
                {      02,   04,   06,   08,   10,   12,   14      }<<2,
                {      02,03,   05,   07,   09,   11,   13,14      }<<2,
                {00,01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16}<<2
                };


  VAR c: def.BLOCK;

BEGIN
  _define( arrow,0,13,13,_arrow);   setcolor(arrow ,{0   });
  _define(rarrow,0,13,13,_rarrow);  setcolor(rarrow,{0   });
  _define( cross,6, 6,13,_cross);   setcolor(cross ,{0,3 });
  _define(xcross,6, 6,13,_xcross);  setcolor(xcross,{0,3 });
  _define( clock,9,10,21,_clock);   setcolor(clock ,{0..3});
  c.x:=10; c.w:=clip.w-16;
  c.y:=11; c.h:=clip.h-16;          setclip (clock ,c     );
END standards;


VAR i: INTEGER;

BEGIN
  scr.loophole(scr.bitmap,B);
  IF NOT scr.done THEN HALT(scr.error) END;
  bits:=0;
  FOR i:=0 TO HIGH(B^.layers) DO INC(bits,ORD(i IN B^.mask)) END;
  T.clip.x:=0;      T.clip.w:=scr.state^.W;   T.zX:=0;
  T.clip.y:=0;      T.clip.h:=scr.state^.H;   T.zY:=0;
  T.mask:=B^.mask;  T.color :=T.mask;
  T.mode:=def.rep;  T.back  :={};
  done :=TRUE;
  error:=0;         x:=T.clip.w DIV 2;
  cur  :=arrow;     y:=T.clip.h DIV 2;
  clip :=T.clip;    DEC(clip.w,6);
  color:={0};       DEC(clip.h,6);  INC(clip.y,6);
  low.zero(curs);   on:=FALSE;
  standards;
  i:=curs[cur].size*2*bits;
  mem.allocate(sos,i);
  IF NOT mem.done THEN HALT(mem.error) END;
  mem.allocate(bump,i);
  IF NOT mem.done THEN HALT(mem.error) END
END pmCrs.
