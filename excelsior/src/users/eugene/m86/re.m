MODULE re; (*  04-Mar-91. (c) KRONOS *)

FROM SYSTEM      IMPORT ADDRESS;

IMPORT  bio : BIO;
IMPORT  tty : Terminal;
IMPORT  out : StdIO;
IMPORT  str : Strings;
IMPORT  arg : tskArgs;
IMPORT  mem : Heap;

WITH STORAGE : mem;

TYPE
  size    = (byte,word,double,quadro,size10,size14,size94);
  mem_reg = (f_mem,f_reg);

VAR
  src: ADDRESS;
  cod: DYNARR OF CHAR;
  ip : INTEGER;

PROCEDURE nb(): INTEGER;
BEGIN
  IF ip<=HIGH(cod) THEN INC(ip); RETURN ORD(cod[ip-1]) ELSE RETURN 0 END;
END nb;

PROCEDURE nsb(): INTEGER;
  VAR i: INTEGER;
BEGIN
  i:=nb();
  IF i>=128 THEN RETURN i-256 ELSE RETURN i END;
END nsb;

PROCEDURE nw(): INTEGER;
BEGIN
  RETURN nb()+nb()*256;
END nw;

PROCEDURE reg_mnem(VAR s: ARRAY OF CHAR; n: INTEGER; sz: size);
  CONST
    b_reg = 'alcldlblahchdhbh';
    w_reg = 'axcxdxbxspbpsidi';
BEGIN
  n:=n*2;
  IF sz=byte THEN s[0]:=b_reg[n]; s[1]:=b_reg[n+1];
  ELSE            s[0]:=w_reg[n]; s[1]:=w_reg[n+1];
  END;
  s[2]:=0c;
END reg_mnem;

PROCEDURE access_mode(VAR mm: ARRAY OF CHAR; ofs,sz,cc: INTEGER);
BEGIN
  ofs:=ofs MOD 10000h;
  IF sz#0 THEN
    CASE cc OF
      |0: str.print(mm,'[bx+si+0%$4hh]',ofs);
      |1: str.print(mm,'[bx+di+0%$4hh]',ofs);
      |2: str.print(mm,'[bp+si+0%$4hh]',ofs);
      |3: str.print(mm,'[bp+di+0%$4hh]',ofs);
      |4: str.print(mm,'[si+0%$4hh]'   ,ofs);
      |5: str.print(mm,'[di+0%$4hh]'   ,ofs);
      |6: str.print(mm,'[bp+0%$4hh]'   ,ofs);
      |7: str.print(mm,'[bx+0%$4hh]'   ,ofs);
    END;
  ELSE
    CASE cc OF
      |0: str.print(mm,'[bx+si]');
      |1: str.print(mm,'[bx+di]');
      |2: str.print(mm,'[bp+si]');
      |3: str.print(mm,'[bp+di]');
      |4: str.print(mm,'[si]'   );
      |5: str.print(mm,'[di]'   );
      |6: str.print(mm,'[bp]'   );
      |7: str.print(mm,'[bx]'   );
    END;
  END;
END access_mode;

PROCEDURE app_size(VAR s: ARRAY OF CHAR; ss: ARRAY OF CHAR; sz: size);
BEGIN
  CASE sz OF
    |byte  : str.print(s,'byte_ptr%s',ss);
    |word  : str.print(s,'word_ptr%s',ss);
    |double: str.print(s,'double_ptr%s',ss);
    |quadro: str.print(s,'size8_ptr%s',ss);
    |size10: str.print(s,'size10_ptr%s',ss);
    |size14: str.print(s,'size14_ptr%s',ss);
    |size94: str.print(s,'size94_ptr%s',ss);
  END;
END app_size;

PROCEDURE prm2(VAR s: ARRAY OF CHAR; sz: size; mr: mem_reg);
  VAR rg,mm: ARRAY [0..31] OF CHAR; n,i: INTEGER;
BEGIN
  n:=nb();
  reg_mnem(rg,n DIV 8 MOD 8,sz);
  CASE n DIV 64 OF
    |0: IF n MOD 8 = 6 THEN str.print(mm,'[0%$4hh]',nw());
        ELSE access_mode(mm,0,0,n MOD 8);
        END;
    |1: i:=nb(); IF i>=128 THEN i:=i-256 END; access_mode(mm,i,1,n MOD 8);
    |2: access_mode(mm,nw(),2,n MOD 8);
    |3: reg_mnem(mm,n MOD 8,sz);
  END;
  IF mr=f_mem THEN str.print(s,'%s,%s',mm,rg)
  ELSE str.print(s,'%s,%s',rg,mm)
  END;
END prm2;

PROCEDURE prm1(VAR s: ARRAY OF CHAR; n: INTEGER; sz: size);
  VAR i: INTEGER; ss: ARRAY [0..31] OF CHAR;
BEGIN
  CASE n DIV 64 OF
    |0: IF n MOD 8 = 6 THEN str.print(ss,'[0%$4hh]',nw());
        ELSE access_mode(ss,0,0,n MOD 8);
        END;
    |1: i:=nb(); IF i>=128 THEN i:=i-256 END; access_mode(ss,i,1,n MOD 8);
    |2: access_mode(ss,nw(),2,n MOD 8);
    |3: reg_mnem(s,n MOD 8,sz); RETURN;
  END;
  app_size(s,ss,sz);
END prm1;

PROCEDURE cmd_080(VAR s: ARRAY OF CHAR);
  VAR n: INTEGER; ln: ARRAY [0..31] OF CHAR;
BEGIN
  n:=nb();
  CASE n DIV 8 MOD 8 OF
    |0: prm1(ln,n,byte); str.print(s,'add %s,0%$2hh',ln,nb());
    |1: prm1(ln,n,byte); str.print(s,'or  %s,0%$2hh',ln,nb());
    |2: prm1(ln,n,byte); str.print(s,'adc %s,0%$2hh',ln,nb());
    |3: prm1(ln,n,byte); str.print(s,'sbb %s,0%$2hh',ln,nb());
    |4: prm1(ln,n,byte); str.print(s,'and %s,0%$2hh',ln,nb());
    |5: prm1(ln,n,byte); str.print(s,'sub %s,0%$2hh',ln,nb());
    |6: prm1(ln,n,byte); str.print(s,'xor %s,0%$2hh',ln,nb());
    |7: prm1(ln,n,byte); str.print(s,'cmp %s,0%$2hh',ln,nb());
  END;
END cmd_080;

PROCEDURE cmd_081(VAR s: ARRAY OF CHAR);
  VAR n: INTEGER; ln: ARRAY [0..31] OF CHAR;
BEGIN
  n:=nb();
  CASE n DIV 8 MOD 8 OF
    |0: prm1(ln,n,word); str.print(s,'add %s,0%$4hh',ln,nw());
    |1: prm1(ln,n,word); str.print(s,'or  %s,0%$4hh',ln,nw());
    |2: prm1(ln,n,word); str.print(s,'adc %s,0%$4hh',ln,nw());
    |3: prm1(ln,n,word); str.print(s,'sbb %s,0%$4hh',ln,nw());
    |4: prm1(ln,n,word); str.print(s,'and %s,0%$4hh',ln,nw());
    |5: prm1(ln,n,word); str.print(s,'sub %s,0%$4hh',ln,nw());
    |6: prm1(ln,n,word); str.print(s,'xor %s,0%$4hh',ln,nw());
    |7: prm1(ln,n,word); str.print(s,'cmp %s,0%$4hh',ln,nw());
  END;
END cmd_081;

PROCEDURE cmd_083(VAR s: ARRAY OF CHAR);
  VAR n: INTEGER; ln: ARRAY [0..31] OF CHAR;
BEGIN
  n:=nb();
  CASE n DIV 8 MOD 8 OF
    |0: prm1(ln,n,word); str.print(s,'add %s,0%$4hh',ln,nsb());
    |1: prm1(ln,n,word); str.print(s,'or  %s,0%$4hh',ln,nsb());
    |2: prm1(ln,n,word); str.print(s,'adc %s,0%$4hh',ln,nsb());
    |3: prm1(ln,n,word); str.print(s,'sbb %s,0%$4hh',ln,nsb());
    |4: prm1(ln,n,word); str.print(s,'and %s,0%$4hh',ln,nsb());
    |5: prm1(ln,n,word); str.print(s,'sub %s,0%$4hh',ln,nsb());
    |6: prm1(ln,n,word); str.print(s,'xor %s,0%$4hh',ln,nsb());
    |7: prm1(ln,n,word); str.print(s,'cmp %s,0%$4hh',ln,nsb());
  END;
END cmd_083;

PROCEDURE cmd_08F(VAR s: ARRAY OF CHAR);
  VAR n: INTEGER; ln: ARRAY [0..31] OF CHAR;
BEGIN
  n:=nb();
  CASE n DIV 8 MOD 8 OF
    |0: prm1(ln,n,word); str.print(s,'pop %s',ln);
    |1:
    |2:
    |3:
    |4:
    |5:
    |6:
    |7:
  END;
END cmd_08F;

PROCEDURE cmd_0C0(VAR s: ARRAY OF CHAR);
  VAR n: INTEGER; ln: ARRAY [0..31] OF CHAR;
BEGIN
  n:=nb();
  CASE n DIV 8 MOD 8 OF
    |0: prm1(ln,n,byte); str.print(s,'rol %s,0%$2hh',ln,nb());
    |1: prm1(ln,n,byte); str.print(s,'ror %s,0%$2hh',ln,nb());
    |2: prm1(ln,n,byte); str.print(s,'rcl %s,0%$2hh',ln,nb());
    |3: prm1(ln,n,byte); str.print(s,'rcr %s,0%$2hh',ln,nb());
    |4: prm1(ln,n,byte); str.print(s,'shl %s,0%$2hh',ln,nb());
    |5: prm1(ln,n,byte); str.print(s,'shr %s,0%$2hh',ln,nb());
    |6: prm1(ln,n,byte); str.print(s,'shl %s,0%$2hh',ln,nb());
    |7: prm1(ln,n,byte); str.print(s,'sar %s,0%$2hh',ln,nb());
  END;
END cmd_0C0;

PROCEDURE cmd_0C1(VAR s: ARRAY OF CHAR);
  VAR n: INTEGER; ln: ARRAY [0..31] OF CHAR;
BEGIN
  n:=nb();
  CASE n DIV 8 MOD 8 OF
    |0: prm1(ln,n,word); str.print(s,'rol %s,0%$2hh',ln,nb());
    |1: prm1(ln,n,word); str.print(s,'ror %s,0%$2hh',ln,nb());
    |2: prm1(ln,n,word); str.print(s,'rcl %s,0%$2hh',ln,nb());
    |3: prm1(ln,n,word); str.print(s,'rcr %s,0%$2hh',ln,nb());
    |4: prm1(ln,n,word); str.print(s,'shl %s,0%$2hh',ln,nb());
    |5: prm1(ln,n,word); str.print(s,'shr %s,0%$2hh',ln,nb());
    |6: prm1(ln,n,word); str.print(s,'shl %s,0%$2hh',ln,nb());
    |7: prm1(ln,n,word); str.print(s,'sar %s,0%$2hh',ln,nb());
  END;
END cmd_0C1;

PROCEDURE cmd_0D0(VAR s: ARRAY OF CHAR);
  VAR n: INTEGER; ln: ARRAY [0..31] OF CHAR;
BEGIN
  n:=nb();
  CASE n DIV 8 MOD 8 OF
    |0: prm1(ln,n,byte); str.print(s,'rol %s,1',ln);
    |1: prm1(ln,n,byte); str.print(s,'ror %s,1',ln);
    |2: prm1(ln,n,byte); str.print(s,'rcl %s,1',ln);
    |3: prm1(ln,n,byte); str.print(s,'rcr %s,1',ln);
    |4: prm1(ln,n,byte); str.print(s,'shl %s,1',ln);
    |5: prm1(ln,n,byte); str.print(s,'shr %s,1',ln);
    |6: prm1(ln,n,byte); str.print(s,'shl %s,1',ln);
    |7: prm1(ln,n,byte); str.print(s,'sar %s,1',ln);
  END;
END cmd_0D0;

PROCEDURE cmd_0D1(VAR s: ARRAY OF CHAR);
  VAR n: INTEGER; ln: ARRAY [0..31] OF CHAR;
BEGIN
  n:=nb();
  CASE n DIV 8 MOD 8 OF
    |0: prm1(ln,n,word); str.print(s,'rol %s,1',ln);
    |1: prm1(ln,n,word); str.print(s,'ror %s,1',ln);
    |2: prm1(ln,n,word); str.print(s,'rcl %s,1',ln);
    |3: prm1(ln,n,word); str.print(s,'rcr %s,1',ln);
    |4: prm1(ln,n,word); str.print(s,'shl %s,1',ln);
    |5: prm1(ln,n,word); str.print(s,'shr %s,1',ln);
    |6: prm1(ln,n,word); str.print(s,'shl %s,1',ln);
    |7: prm1(ln,n,word); str.print(s,'sar %s,1',ln);
  END;
END cmd_0D1;

PROCEDURE cmd_0D2(VAR s: ARRAY OF CHAR);
  VAR n: INTEGER; ln: ARRAY [0..31] OF CHAR;
BEGIN
  n:=nb();
  CASE n DIV 8 MOD 8 OF
    |0: prm1(ln,n,byte); str.print(s,'rol %s,cl',ln);
    |1: prm1(ln,n,byte); str.print(s,'ror %s,cl',ln);
    |2: prm1(ln,n,byte); str.print(s,'rcl %s,cl',ln);
    |3: prm1(ln,n,byte); str.print(s,'rcr %s,cl',ln);
    |4: prm1(ln,n,byte); str.print(s,'shl %s,cl',ln);
    |5: prm1(ln,n,byte); str.print(s,'shr %s,cl',ln);
    |6: prm1(ln,n,byte); str.print(s,'shl %s,cl',ln);
    |7: prm1(ln,n,byte); str.print(s,'sar %s,cl',ln);
  END;
END cmd_0D2;

PROCEDURE cmd_0D3(VAR s: ARRAY OF CHAR);
  VAR n: INTEGER; ln: ARRAY [0..31] OF CHAR;
BEGIN
  n:=nb();
  CASE n DIV 8 MOD 8 OF
    |0: prm1(ln,n,word); str.print(s,'rol %s,cl',ln);
    |1: prm1(ln,n,word); str.print(s,'ror %s,cl',ln);
    |2: prm1(ln,n,word); str.print(s,'rcl %s,cl',ln);
    |3: prm1(ln,n,word); str.print(s,'rcr %s,cl',ln);
    |4: prm1(ln,n,word); str.print(s,'shl %s,cl',ln);
    |5: prm1(ln,n,word); str.print(s,'shr %s,cl',ln);
    |6: prm1(ln,n,word); str.print(s,'shl %s,cl',ln);
    |7: prm1(ln,n,word); str.print(s,'sar %s,cl',ln);
  END;
END cmd_0D3;

PROCEDURE cmd_0D4(VAR s: ARRAY OF CHAR);
  VAR n: INTEGER;
BEGIN
  n:=nb();
  CASE n OF
    |00Ah: str.print(s,'aam');
  ELSE str.print(s,'dw 0%$2hD4h',n);
  END;
END cmd_0D4;

PROCEDURE cmd_0D5(VAR s: ARRAY OF CHAR);
  VAR n: INTEGER;
BEGIN
  n:=nb();
  CASE n OF
    |00Ah: str.print(s,'aad');
  ELSE str.print(s,'dw 0%$2hD5h',n);
  END;
END cmd_0D5;

PROCEDURE prm1fpp(VAR s: ARRAY OF CHAR; n: INTEGER; sz: size);
  VAR i: INTEGER; szw,as: ARRAY [0..31] OF CHAR;
BEGIN
  CASE sz OF
    |word  : szw:='word_ptr';
    |double: szw:='doub_ptr';
    |quadro: szw:='quad_ptr';
    |size10: szw:='bt10_ptr';
    |size14: szw:='bt14_ptr';
    |size94: szw:='bt94_ptr';
  END;
  CASE n DIV 64 OF
    |0: IF n MOD 8 = 6 THEN str.print(as,'[0%$4hh]',nw());
        ELSE access_mode(as,0,0,n MOD 8);
        END;
    |1: i:=nb(); IF i>=128 THEN i:=i-256 END; access_mode(as,i,1,n MOD 8);
    |2: access_mode(as,nw(),2,n MOD 8);
    |3: str.print(s,'st(%d)',n MOD 8); RETURN
  END;
  str.print(s,'%s%s',szw,as);
END prm1fpp;

PROCEDURE cmd_0D8(VAR s: ARRAY OF CHAR);
  VAR n: INTEGER; ln: ARRAY [0..31] OF CHAR;
BEGIN
  n:=nb();
  CASE n DIV 8 MOD 8 OF
    |0: prm1fpp(ln,n,double); str.print(s,'fadd  st,%s',ln);
    |1: prm1fpp(ln,n,double); str.print(s,'fmul  st,%s',ln);
    |2: prm1fpp(ln,n,double); str.print(s,'fcom  st,%s',ln);
    |3: prm1fpp(ln,n,double); str.print(s,'fcomp st,%s',ln);
    |4: prm1fpp(ln,n,double); str.print(s,'fsub  st,%s',ln);
    |5: prm1fpp(ln,n,double); str.print(s,'fsubr st,%s',ln);
    |6: prm1fpp(ln,n,double); str.print(s,'fdiv  st,%s',ln);
    |7: prm1fpp(ln,n,double); str.print(s,'fdivr st,%s',ln);
  END;
END cmd_0D8;

PROCEDURE cmd_0D9(VAR s: ARRAY OF CHAR);
  VAR n,m: INTEGER; ln: ARRAY [0..31] OF CHAR;
BEGIN
  n:=nb(); m:=n DIV 64;
  CASE n DIV 8 MOD 8 OF
    |0: prm1fpp(ln,n,double); str.print(s,'fld   %s',ln);
    |1: IF m=3 THEN
          prm1fpp(ln,n,word); str.print(s,'fxch  %s',ln);
        END;
    |2: IF n=0D0h THEN str.print(s,'fnop')
        ELSIF m<3 THEN prm1fpp(ln,n,double); str.print(s,'fst   %s',ln);
        END;
    |3: prm1fpp(ln,n,double); str.print(s,'fstp  %s',ln);
    |4: IF n=0E0h THEN str.print(s,'fchs');
        ELSIF n=0E1h THEN str.print(s,'fabs');
        ELSIF n=0E4h THEN str.print(s,'ftst');
        ELSIF n=0E5h THEN str.print(s,'fxam');
        ELSIF m<3 THEN prm1fpp(ln,n,size14); str.print(s,'fldenv %s',ln);
        END;
    |5: IF n=0E8h THEN str.print(s,'fld1');
        ELSIF n=0E9h THEN str.print(s,'fldl2t');
        ELSIF n=0EAh THEN str.print(s,'fldl2e');
        ELSIF n=0EBh THEN str.print(s,'fldpi');
        ELSIF n=0ECh THEN str.print(s,'fldlg2');
        ELSIF n=0EDh THEN str.print(s,'fldln2');
        ELSIF n=0EEh THEN str.print(s,'fldz');
        ELSIF m<3 THEN prm1fpp(ln,n,word); str.print(s,'fldcw %s',ln);
        END;
    |6: IF n=0F0h THEN str.print(s,'f2xm1');
        ELSIF n=0F1h THEN str.print(s,'fyl2x');
        ELSIF n=0F2h THEN str.print(s,'fptan');
        ELSIF n=0F3h THEN str.print(s,'fpatan');
        ELSIF n=0F4h THEN str.print(s,'fxtract');
        ELSIF n=0F6h THEN str.print(s,'fdecstp');
        ELSIF n=0F7h THEN str.print(s,'fincstp');
        ELSIF m<3 THEN prm1fpp(ln,n,size14); str.print(s,'fstenv %s',ln);
        END;
    |7: IF n=0F8h THEN str.print(s,'fprem');
        ELSIF n=0F9h THEN str.print(s,'fyl2xp1');
        ELSIF n=0FAh THEN str.print(s,'fsqrt');
        ELSIF n=0FCh THEN str.print(s,'frndint');
        ELSIF n=0FDh THEN str.print(s,'fscale');
        ELSIF m<3 THEN prm1fpp(ln,n,word); str.print(s,'fstcw %s',ln);
        END;
  END;
END cmd_0D9;

PROCEDURE cmd_0DA(VAR s: ARRAY OF CHAR);
  VAR n,m: INTEGER; ln: ARRAY [0..31] OF CHAR;
BEGIN
  n:=nb(); m:=n DIV 64;
  IF m=3 THEN RETURN END;
  CASE n DIV 8 MOD 8 OF
    |0: prm1fpp(ln,n,double); str.print(s,'fiadd %s',ln);
    |1: prm1fpp(ln,n,double); str.print(s,'fimul %s',ln);
    |2: prm1fpp(ln,n,double); str.print(s,'ficom %s',ln);
    |3: prm1fpp(ln,n,double); str.print(s,'ficomp %s',ln);
    |4: prm1fpp(ln,n,double); str.print(s,'fisub %s',ln);
    |5: prm1fpp(ln,n,double); str.print(s,'fisubr %s',ln);
    |6: prm1fpp(ln,n,double); str.print(s,'fidiv %s',ln);
    |7: prm1fpp(ln,n,double); str.print(s,'fidivr %s',ln);
  END;
END cmd_0DA;

PROCEDURE cmd_0DB(VAR s: ARRAY OF CHAR);
  VAR n,m: INTEGER; ln: ARRAY [0..31] OF CHAR;
BEGIN
  n:=nb(); m:=n DIV 64;
  CASE n DIV 8 MOD 8 OF
    |0: prm1fpp(ln,n,double); str.print(s,'fild %s',ln);
    |1:
    |2: prm1fpp(ln,n,double); str.print(s,'fist %s',ln);
    |3: prm1fpp(ln,n,double); str.print(s,'fistp %s',ln);
    |4: IF n=0E0h THEN str.print(s,'feni');
        ELSIF n=0E1h THEN str.print(s,'fdisi');
        ELSIF n=0E2h THEN str.print(s,'fclex');
        ELSIF n=0E3h THEN str.print(s,'finit');
        END;
    |5: prm1fpp(ln,n,size10); str.print(s,'fld %s',ln);
    |6:
    |7: prm1fpp(ln,n,size10); str.print(s,'fstp %s',ln);
  END;
END cmd_0DB;

PROCEDURE cmd_0DD(VAR s: ARRAY OF CHAR);
  VAR n,m: INTEGER; ln: ARRAY [0..31] OF CHAR;
BEGIN
  n:=nb(); m:=n DIV 64;
  IF m=3 THEN RETURN END;
  CASE n DIV 8 MOD 8 OF
    |0: prm1fpp(ln,n,quadro); str.print(s,'fld  %s',ln);
    |1:
    |2: prm1fpp(ln,n,quadro); str.print(s,'fst    %s',ln);
    |3: prm1fpp(ln,n,quadro); str.print(s,'fstp   %s',ln);
    |4: prm1fpp(ln,n,size94); str.print(s,'frstor %s',ln);
    |5:
    |6: prm1fpp(ln,n,size94); str.print(s,'fsave  %s',ln);
    |7: prm1fpp(ln,n,word); str.print(s,'fstsw  %s',ln);
  END;
END cmd_0DD;

PROCEDURE cmd_0DE(VAR s: ARRAY OF CHAR);
  VAR n,m: INTEGER; ln: ARRAY [0..31] OF CHAR;
BEGIN
  n:=nb(); m:=n DIV 64;
  IF m=3 THEN
    CASE n DIV 8 MOD 8 OF
      |0: prm1fpp(ln,n,word); str.print(s,'faddp  %s,st',ln);
      |1: prm1fpp(ln,n,word); str.print(s,'fmulp  %s,st',ln);
      |2:
      |3: IF n=0D9h THEN str.print(s,'fcompp') END;
      |4: prm1fpp(ln,n,word); str.print(s,'fsubrp %s,st',ln);
      |5: prm1fpp(ln,n,word); str.print(s,'fsubp  %s,st',ln);
      |6: prm1fpp(ln,n,word); str.print(s,'fdivrp %s,st',ln);
      |7: prm1fpp(ln,n,word); str.print(s,'fdivp  %s,st',ln);
    END;
  ELSE
    CASE n DIV 8 MOD 8 OF
      |0: prm1fpp(ln,n,word); str.print(s,'fiadd %s',ln);
      |1: prm1fpp(ln,n,word); str.print(s,'fimul %s',ln);
      |2: prm1fpp(ln,n,word); str.print(s,'ficom %s',ln);
      |3: prm1fpp(ln,n,word); str.print(s,'ficomp %s',ln);
      |4: prm1fpp(ln,n,word); str.print(s,'fisubr %s',ln);
      |5: prm1fpp(ln,n,word); str.print(s,'fisub %s',ln);
      |6: prm1fpp(ln,n,word); str.print(s,'fidivr %s',ln);
      |7: prm1fpp(ln,n,word); str.print(s,'fidiv %s',ln);
    END;
  END;
END cmd_0DE;

PROCEDURE cmd_0FF(VAR s: ARRAY OF CHAR);
  VAR n: INTEGER; ln: ARRAY [0..31] OF CHAR;
BEGIN
  n:=nb();
  CASE n DIV 8 MOD 8 OF
    |0: prm1(ln,n,word); str.print(s,'inc %s',ln);
    |1: prm1(ln,n,word); str.print(s,'dec %s',ln);
    |2: prm1(ln,n,word); str.print(s,'call [%s]',ln);
    |3: prm1(ln,n,word); str.print(s,'call [*][%s]',ln);
    |4: prm1(ln,n,word); str.print(s,'jmp  [%s]',ln);
    |5: prm1(ln,n,word); str.print(s,'jmpl [%s]',ln);
    |6: prm1(ln,n,word); str.print(s,'push %s',ln);
    |7:
  END;
END cmd_0FF;

PROCEDURE cmd_0FE(VAR s: ARRAY OF CHAR);
  VAR n: INTEGER; ln: ARRAY [0..31] OF CHAR;
BEGIN
  n:=nb();
  CASE n DIV 8 MOD 8 OF
    |0: prm1(ln,n,byte); str.print(s,'inc %s',ln);
    |1: prm1(ln,n,byte); str.print(s,'dec %s',ln);
    |2:
    |3:
    |4:
    |5:
    |6:
    |7:
  END;
END cmd_0FE;

PROCEDURE cmd_0F6(VAR s: ARRAY OF CHAR);
  VAR n: INTEGER; ln: ARRAY [0..31] OF CHAR;
BEGIN
  n:=nb();
  CASE n DIV 8 MOD 8 OF
    |0: prm1(ln,n,byte); str.print(s,'test %s,0%$2hh',ln,nb());
    |1:
    |2: prm1(ln,n,byte); str.print(s,'not %s',ln);
    |3: prm1(ln,n,byte); str.print(s,'neg %s',ln);
    |4: prm1(ln,n,byte); str.print(s,'mul al,%s',ln);
    |5: prm1(ln,n,byte); str.print(s,'imul al,%s',ln);
    |6: prm1(ln,n,byte); str.print(s,'div al,%s',ln);
    |7: prm1(ln,n,byte); str.print(s,'idiv al,%s',ln);
  END;
END cmd_0F6;

PROCEDURE cmd_0F7(VAR s: ARRAY OF CHAR);
  VAR n: INTEGER; ln: ARRAY [0..31] OF CHAR;
BEGIN
  n:=nb();
  CASE n DIV 8 MOD 8 OF
    |0: prm1(ln,n,word); str.print(s,'test %s,0%$4hh',ln,nw());
    |1:
    |2: prm1(ln,n,word); str.print(s,'not %s',ln);
    |3: prm1(ln,n,word); str.print(s,'neg %s',ln);
    |4: prm1(ln,n,word); str.print(s,'mul ax,%s',ln);
    |5: prm1(ln,n,word); str.print(s,'imul ax,%s',ln);
    |6: prm1(ln,n,word); str.print(s,'div ax,%s',ln);
    |7: prm1(ln,n,word); str.print(s,'idiv ax,%s',ln);
  END;
END cmd_0F7;

PROCEDURE cmd_0C6(VAR s: ARRAY OF CHAR);
  VAR n: INTEGER; ln: ARRAY [0..31] OF CHAR;
BEGIN
  n:=nb();
  CASE n DIV 8 MOD 8 OF
    |0: prm1(ln,n,byte); str.print(s,'mov %s,0%$2hh',ln,nb());
    |1:
    |2:
    |3:
    |4:
    |5:
    |6:
    |7:
  END;
END cmd_0C6;

PROCEDURE cmd_0C7(VAR s: ARRAY OF CHAR);
  VAR n: INTEGER; ln: ARRAY [0..31] OF CHAR;
BEGIN
  n:=nb();
  CASE n DIV 8 MOD 8 OF
    |0: prm1(ln,n,word); str.print(s,'mov %s,0%$4hh',ln,nw());
    |1:
    |2:
    |3:
    |4:
    |5:
    |6:
    |7:
  END;
END cmd_0C7;

PROCEDURE cmd_08C(VAR s: ARRAY OF CHAR);
  VAR n: INTEGER; ln: ARRAY [0..31] OF CHAR;
BEGIN
  n:=nb();
  CASE n DIV 8 MOD 8 OF
    |0: prm1(ln,n,word); str.print(s,'mov %s,es',ln);
    |1: prm1(ln,n,word); str.print(s,'mov %s,cs',ln);
    |2: prm1(ln,n,word); str.print(s,'mov %s,ss',ln);
    |3: prm1(ln,n,word); str.print(s,'mov %s,ds',ln);
    |4:
    |5:
    |6:
    |7:
  END;
END cmd_08C;

PROCEDURE cmd_08E(VAR s: ARRAY OF CHAR);
  VAR n: INTEGER; ln: ARRAY [0..31] OF CHAR;
BEGIN
  n:=nb();
  CASE n DIV 8 MOD 8 OF
    |0: prm1(ln,n,word); str.print(s,'mov es,%s',ln);
    |1: prm1(ln,n,word); str.print(s,'mov cs,%s',ln);
    |2: prm1(ln,n,word); str.print(s,'mov ss,%s',ln);
    |3: prm1(ln,n,word); str.print(s,'mov ds,%s',ln);
    |4:
    |5:
    |6:
    |7:
  END;
END cmd_08E;

PROCEDURE vis_cmd(cmd: INTEGER; VAR s: ARRAY OF CHAR);
  VAR ln: ARRAY [0..31] OF CHAR; i,j: INTEGER;
BEGIN
  str.print(s,'??? 0%$2hh',cmd);
  CASE cmd OF
    |037h: str.print(s,'aaa');
    |0D5h: cmd_0D5(s);
    |0D4h: cmd_0D4(s);
    |03Fh: str.print(s,'aas');
    |012h: prm2(ln,byte,f_reg); str.print(s,'adc %s',ln);
    |010h: prm2(ln,byte,f_mem); str.print(s,'adc %s',ln);
    |013h: prm2(ln,word,f_reg); str.print(s,'adc %s',ln);
    |011h: prm2(ln,word,f_mem); str.print(s,'adc %s',ln);
    |014h: str.print(s,'adc al,0%$2hh',nb());
    |015h: str.print(s,'adc ax,0%$4hh',nw());
    |01Ah: prm2(ln,byte,f_reg); str.print(s,'sbb %s',ln);
    |018h: prm2(ln,byte,f_mem); str.print(s,'sbb %s',ln);
    |01Bh: prm2(ln,word,f_reg); str.print(s,'sbb %s',ln);
    |019h: prm2(ln,word,f_mem); str.print(s,'sbb %s',ln);
    |01Ch: str.print(s,'sbb al,0%$2hh',nb());
    |01Dh: str.print(s,'sbb ax,0%$4hh',nw());
    |080h: cmd_080(s);
    |081h: cmd_081(s);
    |083h: cmd_083(s);
    |002h: prm2(ln,byte,f_reg); str.print(s,'add %s',ln);
    |000h: prm2(ln,byte,f_mem); str.print(s,'add %s',ln);
    |003h: prm2(ln,word,f_reg); str.print(s,'add %s',ln);
    |001h: prm2(ln,word,f_mem); str.print(s,'add %s',ln);
    |004h: str.print(s,'add al,0%$2hh',nb());
    |005h: str.print(s,'add ax,0%$4hh',nw());
    |02Ah: prm2(ln,byte,f_reg); str.print(s,'sub %s',ln);
    |028h: prm2(ln,byte,f_mem); str.print(s,'sub %s',ln);
    |02Bh: prm2(ln,word,f_reg); str.print(s,'sub %s',ln);
    |029h: prm2(ln,word,f_mem); str.print(s,'sub %s',ln);
    |02Ch: str.print(s,'sub al,0%$2hh',nb());
    |02Dh: str.print(s,'sub ax,0%$4hh',nw());
    |022h: prm2(ln,byte,f_reg); str.print(s,'and %s',ln);
    |020h: prm2(ln,byte,f_mem); str.print(s,'and %s',ln);
    |023h: prm2(ln,word,f_reg); str.print(s,'and %s',ln);
    |021h: prm2(ln,word,f_mem); str.print(s,'and %s',ln);
    |024h: str.print(s,'and al,0%$2hh',nb());
    |025h: str.print(s,'and ax,0%$4hh',nw());
    |032h: prm2(ln,byte,f_reg); str.print(s,'xor %s',ln);
    |030h: prm2(ln,byte,f_mem); str.print(s,'xor %s',ln);
    |033h: prm2(ln,word,f_reg); str.print(s,'xor %s',ln);
    |031h: prm2(ln,word,f_mem); str.print(s,'xor %s',ln);
    |034h: str.print(s,'xor al,0%$2hh',nb());
    |035h: str.print(s,'xor ax,0%$4hh',nw());
    |0E8h: i:=nw(); str.print(s,'call 0%$4hh',(ip+i) MOD 10000h);
    |0FFh: cmd_0FF(s);
    |09Ah: i:=nw(); j:=nw(); str.print(s,'call 0%$4hh[0%$4hh]',j,i);
    |0EAh: i:=nw(); j:=nw(); str.print(s,'jmp 0%$4hh[0%$4hh]',j,i);
    |098h: str.print(s,'cbw');
    |0F8h: str.print(s,'clc');
    |0FCh: str.print(s,'cld');
    |0FAh: str.print(s,'cli');
    |0F5h: str.print(s,'cmc');
    |03Ah: prm2(ln,byte,f_reg); str.print(s,'cmp %s',ln);
    |038h: prm2(ln,byte,f_mem); str.print(s,'cmp %s',ln);
    |03Bh: prm2(ln,word,f_reg); str.print(s,'cmp %s',ln);
    |039h: prm2(ln,word,f_mem); str.print(s,'cmp %s',ln);
    |03Ch: str.print(s,'cmp al,0%$2hh',nb());
    |03Dh: str.print(s,'cmp ax,0%$4hh',nw());
    |099h: str.print(s,'cwd');
    |027h: str.print(s,'daa');
    |02Fh: str.print(s,'das');
    |048h..04Fh: reg_mnem(ln,cmd MOD 8,word); str.print(s,'dec %s',ln);
    |0FEh: cmd_0FE(s);
    |0F6h: cmd_0F6(s);
    |0F7h: cmd_0F7(s);
    |0F4h: str.print(s,'hlt');
    |0E4h: str.print(s,'in al,0%$2hh',nb());
    |0E5h: str.print(s,'in ax,0%$2hh',nb());
    |0E6h: str.print(s,'out al,0%$2hh',nb());
    |0E7h: str.print(s,'out ax,0%$2hh',nb());
    |0ECh: str.print(s,'in al,[dx]');
    |0EDh: str.print(s,'in ax,[dx]');
    |0EEh: str.print(s,'out al,[dx]');
    |0EFh: str.print(s,'out ax,[dx]');
    |040h..047h: reg_mnem(ln,cmd MOD 8,word); str.print(s,'inc %s',ln);
    |0CCh: str.print(s,'int 3');
    |0CDh: str.print(s,'int 0%$2hh',nb());
    |0CEh: str.print(s,'into 4');
    |0CFh: str.print(s,'iret');
    |070h: str.print(s,'jo  0%$4hh',(nsb()+ip) MOD 10000h);
    |071h: str.print(s,'jno 0%$4hh',(nsb()+ip) MOD 10000h);
    |072h: str.print(s,'jc  0%$4hh',(nsb()+ip) MOD 10000h);
    |073h: str.print(s,'jnc 0%$4hh',(nsb()+ip) MOD 10000h);
    |074h: str.print(s,'jz  0%$4hh',(nsb()+ip) MOD 10000h);
    |075h: str.print(s,'jnz 0%$4hh',(nsb()+ip) MOD 10000h);
    |076h: str.print(s,'jbe 0%$4hh',(nsb()+ip) MOD 10000h);
    |077h: str.print(s,'ja  0%$4hh',(nsb()+ip) MOD 10000h);
    |078h: str.print(s,'js  0%$4hh',(nsb()+ip) MOD 10000h);
    |079h: str.print(s,'jns 0%$4hh',(nsb()+ip) MOD 10000h);
    |07Ah: str.print(s,'jp  0%$4hh',(nsb()+ip) MOD 10000h);
    |07Bh: str.print(s,'jnp 0%$4hh',(nsb()+ip) MOD 10000h);
    |07Ch: str.print(s,'jl  0%$4hh',(nsb()+ip) MOD 10000h);
    |07Dh: str.print(s,'jge 0%$4hh',(nsb()+ip) MOD 10000h);
    |07Eh: str.print(s,'jle 0%$4hh',(nsb()+ip) MOD 10000h);
    |07Fh: str.print(s,'jg  0%$4hh',(nsb()+ip) MOD 10000h);
    |0E9h: str.print(s,'jmp 0%$4hh',(nw()+ip) MOD 10000h);
    |0EBh: str.print(s,'jmp 0%$4hh',(nsb()+ip) MOD 10000h);
    |0E3h: str.print(s,'jcxz 0%$4hh',(nsb()+ip) MOD 10000h);
    |09Fh: str.print(s,'lahf');
    |0C4h: prm2(ln,word,f_reg); str.print(s,'les %s',ln);
    |0C5h: prm2(ln,word,f_reg); str.print(s,'lds %s',ln);
    |08Dh: prm2(ln,word,f_reg); str.print(s,'lea %s',ln);
    |0F0h: str.print(s,'lock');
    |0E0h: str.print(s,'loopnz 0%$4hh',(nsb()+ip) MOD 10000h);
    |0E1h: str.print(s,'loopz 0%$4hh', (nsb()+ip) MOD 10000h);
    |0E2h: str.print(s,'loop 0%$4hh',  (nsb()+ip) MOD 10000h);
    |08Ah: prm2(ln,byte,f_reg); str.print(s,'mov %s',ln);
    |088h: prm2(ln,byte,f_mem); str.print(s,'mov %s',ln);
    |08Bh: prm2(ln,word,f_reg); str.print(s,'mov %s',ln);
    |089h: prm2(ln,word,f_mem); str.print(s,'mov %s',ln);
    |0A0h: str.print(s,'mov al,[0%$4hh]',nw());
    |0A1h: str.print(s,'mov ax,[0%$4hh]',nw());
    |0A2h: str.print(s,'mov [0%$4hh],al',nw());
    |0A3h: str.print(s,'mov [0%$4hh],ax',nw());
    |0B0h..0B7h: reg_mnem(ln,cmd MOD 8,byte); str.print(s,'mov %s,0%$2hh',ln,nb());
    |0B8h..0BFh: reg_mnem(ln,cmd MOD 8,word); str.print(s,'mov %s,0%$4hh',ln,nw());
    |0C6h: cmd_0C6(s);
    |0C7h: cmd_0C7(s);
    |08Ch: cmd_08C(s);
    |08Eh: cmd_08E(s);
    |08Fh: cmd_08F(s);
    |090h: str.print(s,'nop');
    |00Ah: prm2(ln,byte,f_reg); str.print(s,'or  %s',ln);
    |008h: prm2(ln,byte,f_mem); str.print(s,'or  %s',ln);
    |00Bh: prm2(ln,word,f_reg); str.print(s,'or  %s',ln);
    |009h: prm2(ln,word,f_mem); str.print(s,'or  %s',ln);
    |00Ch: str.print(s,'or  al,0%$2hh',nb());
    |00Dh: str.print(s,'or  ax,0%$4hh',nw());
    |058h..05Fh: reg_mnem(ln,cmd MOD 8,word); str.print(s,'pop  %s',ln);
    |050h..057h: reg_mnem(ln,cmd MOD 8,word); str.print(s,'push %s',ln);
    |007h: str.print(s,'pop  es');
    |017h: str.print(s,'pop  ss');
    |01Fh: str.print(s,'pop  ds');
    |006h: str.print(s,'push es');
    |00Eh: str.print(s,'push cs');
    |016h: str.print(s,'push ss');
    |01Eh: str.print(s,'push ds');
    |09Dh: str.print(s,'popf');
    |09Ch: str.print(s,'pushf');
    |0F3h: str.print(s,'repz');
    |0F2h: str.print(s,'repnz');
    |0C3h: str.print(s,'ret');
    |0CBh: str.print(s,'ret seg');
    |0C2h: str.print(s,'ret 0%$4hh',nw());
    |0CAh: str.print(s,'ret seg,0%$4hh',nw());
    |09Eh: s:='sahf';
    |0F9h: s:='stc';
    |0FDh: s:='std';
    |0FBh: s:='sti';
    |0A6h: s:='cmps';
    |0A7h: s:='cmps word';
    |0A4h: s:='movs';
    |0A5h: s:='movs word';
    |0AEh: s:='scas';
    |0AFh: s:='scas word';
    |0ACh: s:='lods';
    |0ADh: s:='lods word';
    |0AAh: s:='stos';
    |0ABh: s:='stos word';
    |09Bh: s:='wait';
    |0D7h: s:='xlat';
    |084h: prm2(ln,byte,f_reg); str.print(s,'test %s',ln);
    |085h: prm2(ln,word,f_reg); str.print(s,'test %s',ln);
    |0A8h: str.print(s,'test al,0%$2hh',nb());
    |0A9h: str.print(s,'test ax,0%$2hh',nw());
    |086h: prm2(ln,byte,f_reg); str.print(s,'xchg %s',ln);
    |087h: prm2(ln,word,f_reg); str.print(s,'xchg %s',ln);
    |091h..097h: reg_mnem(ln,cmd MOD 8,word); str.print(s,'xchg ax,%s',ln);
    |062h: prm2(ln,word,f_reg); str.print(s,'bound %s',ln);
    |0C8h: i:=nw(); j:=nb(); str.print(s,'enter 0%$4hh,0%$2hh',i,j);
    |0C9h: s:='leave';
    |061h: s:='popa';
    |060h: s:='pusha';
    |06Ah: str.print(s,'push 0%$4hh',nsb());
    |068h: str.print(s,'push 0%$4hh',nw());
    |026h: str.print(s,'seg  es');
    |036h: str.print(s,'seg  ss');
    |02Eh: str.print(s,'seg  cs');
    |03Eh: str.print(s,'seg  ds');
    |0D0h: cmd_0D0(s);
    |0D1h: cmd_0D1(s);
    |0D2h: cmd_0D2(s);
    |0D3h: cmd_0D3(s);
    |0C0h: cmd_0C0(s);
    |0C1h: cmd_0C1(s);
    |0D8h: cmd_0D8(s);
    |0D9h: cmd_0D9(s);
    |0DAh: cmd_0DA(s);
    |0DBh: cmd_0DB(s);
    |0DDh: cmd_0DD(s);
    |0DEh: cmd_0DE(s);
  ELSE
  END;
END vis_cmd;

PROCEDURE vis_code;
--    code :
--                       bytes
-- info                  64
-- <глобалы>             info.glob_size
-- <код>                 info.code_size
-- <мультиглобалы>       info.mglo_no * 8
-- <таблица процедур>    info.proc_no * 4
-- <таблица вн.процедур> info.link_no * 2
-- <таблица модулей>

VAR
  info  : POINTER TO RECORD
    version  : INTEGER;   -- версия кодофайла
    def_time : INTEGER;   -- время компиляции definition
    imp_time : INTEGER;   -- время компиляции implementation
    reserv_03: INTEGER;
    ------------------
    glob_size: INTEGER;   -- размер глобалов
    code_size: INTEGER;   -- размер кода
    mglo_no  : INTEGER;   -- количество мультиглобалов
    exts_no  : INTEGER;   -- количество внешних модулей
    ------------------
    min_stack: INTEGER;   -- мин. стек для глоб. мультизначений
    add_stack: INTEGER;   -- дополнительный стек
    link_no  : INTEGER;
    proc_no  : INTEGER;
    ------------------
    reserv_0C: INTEGER;
    reserv_0D: INTEGER;
    reserv_0E: INTEGER;
    reserv_0F: INTEGER;
  END;
  VAR
    i,j: INTEGER;
    p  : POINTER TO ARRAY [0..1FFFFh] OF CHAR;
    ln1: ARRAY [0..63] OF CHAR;
    ln2: ARRAY [0..63] OF CHAR;
BEGIN
  info:=src; p:=src;
  out.print('global area     : %4d bytes\n',info^.glob_size);
  IF arg.flag('-','g') THEN
    j:=64;
    FOR i:=0 TO info^.glob_size-1 DO
      IF i MOD 16 = 0 THEN out.print('%$8h:',i) END;
      out.print(' %$2h',p^[j]); INC(j);
      IF i MOD 16 = 15 THEN out.print('\n') END;
    END;
    out.print('\n');
  END;
  out.print('indirect globals: %4d\n',info^.mglo_no);
  IF arg.flag('-','m') THEN
    out.print('   offset   size\n');
    j:=64+info^.glob_size+info^.code_size;
    FOR i:=0 TO info^.mglo_no-1 DO
      out.print('  %6$hh  %5d\n',
        ORD(p^[j+0])+ORD(p^[j+1])*100h+ORD(p^[j+2])*10000h,
        ORD(p^[j+4])+ORD(p^[j+5])*100h+ORD(p^[j+6])*10000h);
      INC(j,8);
    END;
  END;
  out.print('procedures      : %4d\n',info^.proc_no);
  IF arg.flag('-','p') THEN
    j:=64+info^.glob_size+info^.code_size+info^.mglo_no*8;
    FOR i:=0 TO info^.proc_no-1 DO
      out.print('proc %3d: %$4hh\n',i,ORD(p^[j])+ORD(p^[j+1])*100h); INC(j,4);
    END;
  END;
  out.print('code segnemt    : %4d bytes\n',info^.code_size);
  IF NOT arg.flag('-','c') THEN
    NEW(cod,info^.code_size);
    j:=BYTES(info^)+info^.glob_size;
    FOR i:=0 TO HIGH(cod) DO cod[i]:=p^[i+j] END;
    ip:=0;
    WHILE ip<=HIGH(cod) DO
      i:=ip;
      vis_cmd(nb(),ln1);
      out.print('0%$4hh:',i);
      ln2:='';
      FOR j:=i TO ip-1 DO str.append(ln2,' %$2h',cod[j]) END;
      out.print('%-15s  %s\n',ln2,ln1);
    END;
  END;
END vis_code;

PROCEDURE chk_io(VAL nm: ARRAY OF CHAR);
BEGIN
  IF bio.done THEN RETURN END;
  tty.perror(bio.error,'IO error, file "%s", %%s.\n',nm);
  HALT(bio.error);
END chk_io;

VAR f: bio.FILE; nm: ARRAY [0..63] OF CHAR;

BEGIN
  NEW(cod);
  IF LEN(arg.words)#1 THEN
    tty.print('re <code file name> [-gpmc]\n');
    tty.print('  -g  print global data\n');
    tty.print('  -p  print procedure table\n');
    tty.print('  -m  print table of indirect globals\n');
    tty.print('  -c  inhibit printing of code\n');
    HALT
  END;
  str.print(nm,'%s.obj',arg.words[0]);
  bio.open(f,nm,'r'); chk_io(nm);
  mem.ALLOCATE(src,(bio.eof(f)+3) DIV 4);
  bio.read(f,src,bio.eof(f)); chk_io(nm);
  bio.close(f); chk_io(nm);
  vis_code;
END re.
