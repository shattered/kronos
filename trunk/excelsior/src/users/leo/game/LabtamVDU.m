IMPLEMENTATION MODULE LabtamVDU; (* Leo 27-Jun-88. (c) KRONOS *)

IMPORT  sys: SYSTEM;
IMPORT  mcd: defCodes;
IMPORT  sch: osKernel;
IMPORT  tty: Terminal;
IMPORT  tsk: tskEnv;

PROCEDURE GETM(): BITSET;  CODE mcd.getm END GETM;
PROCEDURE SETM(s: BITSET); CODE mcd.setm END SETM;

PROCEDURE MOVE(t,f: sys.ADDRESS ; s: INTEGER); CODE mcd.move END MOVE;

(* Kadr =  mAdr/4 + 80000h *)
(* free mBuf: 8:0000h      *)

CONST BASE=0F00000h;
      HALF=10000h;

VAR prog: POINTER TO ARRAY [0..0FFFFh] OF CHAR;

PROCEDURE loadVDU(start: sys.ADDRESS );

  VAR co: INTEGER;

  PROCEDURE put(SEQ h: sys.WORD);
    VAR i: INTEGER;
  BEGIN
    FOR i:=0 TO HIGH(h) DO prog^[co]:=CHAR(h[i]); INC(co) END;
  END put;

BEGIN
  prog:=sys.ADDRESS ( (start DIV 4) + BASE );   co:=0;

  ----------------------------------------------------------------

  --- reset:
  put( 0BAh, 000h, 0F2h );              --      mov     dx,0F200
  put( 0B0h, 000h );                    --      mov     al,000h
  put( 0EEh );                          --      out     dx,al
  put( 0BAh, 000h, 0F4h );              --      mov     dx,0F400
  put( 0B0h, 080h );                    --      mov     al,080h
  put( 0EEh );                          --      out     dx,al

  put( 08Ch, 0D3h );                    --      mov     bx,ss

                                        -- $loop:
  put( 0B8h, 000h, 090h );              --      mov     ax,9000h
  put( 08Eh, 0D0h );                    --      mov     ss,ax
  put( 0B8h, 000h, 080h );              --      mov     ax,8000h
  put( 08Eh, 0D8h );                    --      mov     ds,ax
  put( 0B8h, 000h, 010h );              --      mov     ax,1000h
  put( 08Eh, 0C0h );                    --      mov     es,ax
  put( 0FCh );                          --      cld
  put( 036h, 0C6h, 006h, 000h, 000h, 000h );
                                        --      mov     byte ptr ss:00,0

                                        -- $wait:
  put( 036h, 0A0h, 000h, 000h );        --      mov     al,ss:00
  put( 03Ch, 000h );                    --      cmp     al,00
  put( 074h, 0F8h );                    --      je      $wait
  put( 036h, 0C6h, 006h, 000h, 000h, 003h );
                                        --      mov     byte ptr ss:00,3
  put( 03Ch, 0FFh );                    --      cmp     al,0FFh
  put( 074h, 02Eh );                    --      je      $halt
  put( 03Ch, 001h );                    --      cmp     al,01h
  put( 075h, 0EAh );                    --      jne     $wait
  put( 036h, 0C6h, 006h, 000h, 000h, 002h );
                                        --      mov     byte ptr ss:00,2
                                        -- $mover:

  put( 036h, 08Bh, 00Eh, 004h, 000h );
                                        --      mov     cx,ss:0004
  put( 036h, 08Bh, 03Eh, 00Ch, 000h );
                                        --      mov     di,ss:000Ch
  put( 08Bh, 0F7h );                    --      mov     si,di
  put( 0F3h, 0A5h );                    --      repz    movsb

  put( 036h, 083h, 006h, 00Ch, 000h, 064h);
                                        --      add     word ptr ss:12,64h
  put( 036h, 0A1h, 008h, 000h );        --      mov     ax,ss:8
  put( 048h );                          --      dec     ax
  put( 036h, 0A3h, 008h, 000h );        --      mov     word ptr ss:8,ax
  put( 03Dh, 000h, 000h );              --      cmp     ax,0
  put( 075h, 0DEh );                    --      jne     $mover

  put( 0EBh, 0AAh );                    --      jmp     $loop

                                        -- $halt:

  put( 036h, 0C6h, 006h, 000h, 000h, 0BDh );
                                        --      mov     byte ptr ss:00,0BDh
  put( 08Eh, 0D3h );                    --      mov     bx,ss
  put( 0CBh );                          --      ret
  -------------------------------------------------------------------------
END loadVDU;

VAR pBCB: POINTER TO ARRAY [0..3] OF CHAR;
   adr86: sys.ADDRESS ;

PROCEDURE initBCB;
  VAR adr: sys.ADDRESS ;
   F0000h: sys.ADDRESS ;
  CONST channel=8;
BEGIN
  F0000h:=sys.ADDRESS( BASE + 0F0000h DIV 4 );
  adr:=BASE+(0F8010h+channel*4) DIV 4;
  adr:=INTEGER(adr^) MOD HALF;
  ASSERT(adr MOD 4 = 0);
  pBCB:=sys.ADDRESS ( F0000h ) + adr DIV 4;
  adr86:=sys.ADDRESS (pBCB)+1;
END initBCB;

VAR  runned: BOOLEAN;
     autoRe: BOOLEAN;

PROCEDURE runVDU;
  VAR ma: sys.ADDRESS ; time: INTEGER;
BEGIN
  ma:=(sys.ADDRESS (prog)-BASE)*4;
  adr86^:=BITSET( ma MOD HALF ) + BITSET( ( ma DIV HALF) << 28 );
  pBCB^[0]:=1c;
  time:=10000;
  REPEAT DEC(time) UNTIL (time=0) OR ((pBCB^[0]=0c) & (pBCB^[1]=1c));
  pBCB^[0]:=0c;
  IF time#0 THEN runned:=TRUE END;
END runVDU;

PROCEDURE running(): BOOLEAN;
BEGIN RETURN runned END running;

PROCEDURE stopVDU;
  VAR time: INTEGER;
BEGIN
  auto(FALSE);
  control^.flag:=0FFh;
  time:=5000;
  REPEAT DEC(time) UNTIL (time=0) OR (control^.flag=0BDh);
  IF control^.flag=0BDh THEN runned:=FALSE END;
  time:=1000;
  REPEAT DEC(time) UNTIL (time=0) OR (pBCB^[1]=0c);
END stopVDU;

PROCEDURE block(lay,x,y: INTEGER; w,h: INTEGER);
  VAR time: INTEGER;
BEGIN
  IF autoRe THEN RETURN END;
  ASSERT(h>0,4Ah);
  ASSERT(w>0,4Ah);
  ASSERT(lay IN {0..1},4Ah);
  ASSERT((x>=0) & (x+w-1<=799),4Ah);
  ASSERT((y>=0) & (y+h-1<=299),4Ah);
  x:=(x DIV 16)*16;
  lay:=lay*8000h+y*64h+(x DIV 8);
  ASSERT(BITSET(lay)-{0..15}={});
  time:=50000;
  REPEAT DEC(time) UNTIL (control^.flag=0) OR (time=0);
  IF control^.flag#0 THEN RETURN END;
  control^.ofs :=lay;
  control^.len :=(w+31) DIV 16;
  control^.hei :=h;
  control^.flag:=1;
END block;

PROCEDURE ready(): BOOLEAN;
BEGIN RETURN (control^.flag=0) END ready;

PROCEDURE update;
  VAR time: INTEGER;
BEGIN time:=50000;
  IF autoRe THEN SETM(GETM()-{0,1}) END;
  REPEAT DEC(time) UNTIL (control^.flag=0) OR (time=0);
END update;

PROCEDURE refresh;
BEGIN
  IF control^.flag#0 THEN RETURN END;
  control^.ofs :=0;
  control^.len :=32*1024;
  control^.hei :=1;
  control^.flag:=1;
END refresh;

PROCEDURE auto(resh: BOOLEAN);
BEGIN
  IF autoRe & resh THEN RETURN END;
  IF resh THEN
    update; refresh; autoRe:=sch.insert_action(refresh)=0;
  ELSE
    sch.remove_action(refresh);
  END;
  autoRe:=resh;
END auto;

PROCEDURE final;
BEGIN
  IF NOT runned THEN RETURN END;
  auto(FALSE);
  update;
  REPEAT stopVDU UNTIL NOT runned;
  tty.set_pos(24,0); tty.WriteString("" 15c 12c);
  tty.erase(2);
  tty.set_pos(23,0);
  tty.set_cursor(1);
END final;

BEGIN
  runned:=FALSE;
  autoRe:=FALSE;
  initBCB;
  loadVDU(90020h);
  control :=sys.ADDRESS ( (90000h DIV 4) + BASE );
  layer[0]:=sys.ADDRESS (80000h DIV 4)+BASE;
  layer[1]:=sys.ADDRESS (88000h DIV 4)+BASE;
  tsk.final(final);
  tty.erase(2);
  tty.set_cursor(0);
  layer[0]^[0][0]:={};
  layer[1]^[0][0]:={};
  MOVE(sys.ADDRESS(layer[0])+1,layer[0],SIZE(layer[0]^)-1);
  MOVE(sys.ADDRESS(layer[1])+1,layer[1],SIZE(layer[1]^)-1);
  runVDU;
  stopVDU;
  runVDU;
  auto(TRUE);
END LabtamVDU.
