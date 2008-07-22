MODULE as80; (*  25-Dec-90. (c) KRONOS *)

IMPORT  bio : BIO;
IMPORT  out : StdIO;
IMPORT  tty : Terminal;
IMPORT  arg : tskArgs;
IMPORT  str : Strings;

FROM SYSTEM      IMPORT ADR;

VAR
  mem: ARRAY [0..0FFFFh] OF CHAR;
  sta: ARRAY [0..0FFFFh] OF CHAR;
  lbl: ARRAY [0..0FFFFh] OF CHAR;
  cpu: INTEGER;

CONST
  undef = 0c;
  code  = 1c;
  data  = 2c;
  z80   = 0;
  i8080 = 1;
  i8085 = 2;

PROCEDURE cmd_len(pos: INTEGER): INTEGER;
  CONST len = ARRAY OF INTEGER {
    1,3,1,1,1,1,2,1,1,1,1,1,1,1,2,1, -- 0
    2,3,1,1,1,1,2,1,2,1,1,1,1,1,2,1, -- 1
    2,3,3,1,1,1,2,1,2,1,3,1,1,1,2,1, -- 2
    2,3,3,1,1,1,2,1,2,1,3,1,1,1,2,1, -- 3
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, -- 4
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, -- 5
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, -- 6
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, -- 7
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, -- 8
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, -- 9
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, -- A
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, -- B
    1,1,3,3,3,1,2,1,1,1,3,2,3,3,2,1, -- C
    1,1,3,2,3,1,2,1,1,1,3,2,3,4,2,1, -- D
    1,1,3,1,3,1,2,1,1,1,3,1,3,4,2,1, -- E
    1,1,3,1,3,1,2,1,1,1,3,1,3,4,2,1  -- F
  };
  VAR i: INTEGER;
BEGIN
  i:=INTEGER(mem[pos]);
  IF (cpu#z80) & ( (i=18h) OR (i=30h) ) THEN RETURN 1 END;
  i:=len[i];
  IF i#4 THEN RETURN i END;
  IF INTEGER(mem[pos])=0DDh THEN
    CASE INTEGER(mem[pos+1]) OF
      |009h: RETURN 2;
      |019h: RETURN 2;
      |021h: RETURN 4;
      |022h: RETURN 4;
      |023h: RETURN 2;
      |029h: RETURN 2;
      |02Ah: RETURN 4;
      |02Bh: RETURN 2;
    ELSE RETURN 0;
    END;
  ELSIF INTEGER(mem[pos])=0EDh THEN RETURN 0;
  ELSE RETURN 0;
  END;
END cmd_len;

PROCEDURE mark_code(fr: INTEGER);
  VAR i: INTEGER;
BEGIN
  LOOP
    IF sta[fr]#data THEN RETURN END;
    CASE INTEGER(mem[fr]) OF
      |0C3h:
        sta[fr]:=code; fr:=INTEGER(mem[fr+1])+INTEGER(mem[fr+2])*256;
        lbl[fr]:=code;
      |0C2h,0CAh,0D2h,0DAh,0E2h,0EAh,0F2h,0FAh,
       0CDh,0C4h,0CCh,0D4h,0DCh,0E4h,0ECh,0F4h,0FCh:
        sta[fr]:=code; i:=INTEGER(mem[fr+1])+INTEGER(mem[fr+2])*256;
        INC(fr,3); mark_code(fr); fr:=i; lbl[fr]:=code;
      |018h:
        IF cpu=z80 THEN
          sta[fr]:=code; i:=INTEGER(mem[fr+1]);
          IF 7 IN BITSET(i) THEN i:=INTEGER(BITSET(i)+{7..31}) END;
          fr:=fr+2+i; lbl[fr]:=code;
        ELSE
          INC(fr);
        END;
      |038h,030h,028h,020h,010h:
        IF cpu=z80 THEN
          sta[fr]:=code; i:=INTEGER(mem[fr+1]);
          IF 7 IN BITSET(i) THEN i:=INTEGER(BITSET(i)+{7..31}) END;
          fr:=fr+2; mark_code(fr);
          fr:=fr+i; lbl[fr]:=code;
        ELSE
          INC(fr);
        END;
      |0E9h,0C9h:
        sta[fr]:=code; RETURN
    ELSE
      IF (INTEGER(mem[fr])=0DDh) & (INTEGER(mem[fr+1])=0E9h) THEN
        sta[fr]:=code; RETURN
      ELSIF (INTEGER(mem[fr])=0FDh) & (INTEGER(mem[fr+1])=0E9h) THEN
        sta[fr]:=code; RETURN
      END;
      i:=cmd_len(fr);
      IF i=0 THEN
        out.print('; warning: illegal command, address %$4h\n',fr);
      ELSE
        sta[fr]:=code; INC(fr,i);
      END;
    END;
  END;
END mark_code;

PROCEDURE vis_mem;
  VAR
    pos  : INTEGER;
  PROCEDURE nxt(): INTEGER;
  BEGIN
    INC(pos); RETURN INTEGER(mem[pos-1]);
  END nxt;
  PROCEDURE e2(): INTEGER;
    VAR i: INTEGER;
  BEGIN
    i:=nxt();
    IF 7 IN BITSET(i) THEN i:=INTEGER(BITSET(i)+{7..31}) END;
    RETURN i;
  END e2;
  VAR i: INTEGER;
BEGIN
  pos:=0;
  WHILE pos<=HIGH(mem) DO
    IF sta[pos]=undef THEN
      REPEAT INC(pos) UNTIL (pos>HIGH(mem)) OR (sta[pos]#undef);
      IF pos<=HIGH(mem) THEN out.print('        .ORG %$4hh\n',pos) END;
    ELSIF sta[pos]=code THEN
      IF lbl[pos]=undef THEN
        out.print('        ')
      ELSE
        out.print('L%$4h:  ',pos)
      END;
      CASE nxt() OF
        |000h: out.print('NOP\n');
        |001h: out.print('LD  BC,#0%$4hh\n',nxt()+nxt()*256);
        |002h: out.print('LD  (BC),A\n');
        |003h: out.print('INC BC\n');
        |004h: out.print('INC B\n');
        |005h: out.print('DEC B\n');
        |006h: out.print('LD  B,#0%$2hh\n',nxt());
        |007h: out.print('RLCA\n');
        |008h: out.print('EX  AF,AF"\n');
        |009h: out.print('ADD HL,BC\n');
        |00Ah: out.print('LD  A,(BC)\n');
        |00Bh: out.print('DEC BC\n');
        |00Ch: out.print('INC C\n');
        |00Dh: out.print('DEC C\n');
        |00Eh: out.print('LD  C,#0%$2hh\n',nxt());
        |00Fh: out.print('RRCA\n');
        |010h: out.print('DJNZ L%$4h\n',pos+e2());
        |011h: out.print('LD  DE,#0%$4hh\n',nxt()+nxt()*256);
        |012h: out.print('LD  (DE),A\n');
        |013h: out.print('INC DE\n');
        |014h: out.print('INC D\n');
        |015h: out.print('DEC D\n');
        |016h: out.print('LD  D,#0%$2hh\n',nxt());
        |017h: out.print('RLA\n');
        |018h:
          IF cpu=z80 THEN
            out.print('JR  L%$4h\n',pos+e2());
          ELSE
            out.print('.BYTE 018h\n');
          END;
        |019h: out.print('ADD HL,DE\n');
        |01Ah: out.print('LD  A,(DE)\n');
        |01Bh: out.print('DEC DE\n');
        |01Ch: out.print('INC E\n');
        |01Dh: out.print('DEC E\n');
        |01Eh: out.print('LD  E,#0%$2hh\n',nxt());
        |01Fh: out.print('RRA\n');
        |020h: out.print('JR  NZ,L%$4h\n',pos+e2());
        |021h: out.print('LD  HL,#0%$4hh\n',nxt()+nxt()*256);
        |022h: out.print('LD  (%$4hh),HL\n',nxt()+nxt()*256);
        |023h: out.print('INC HL\n');
        |024h: out.print('INC H\n');
        |025h: out.print('DEC H\n');
        |026h: out.print('LD  H,#0%$2hh\n',nxt());
        |027h: out.print('DAA\n');
        |028h: out.print('JR  Z,L%$4h\n',pos+e2());
        |029h: out.print('ADD HL,HL\n');
        |02Ah: out.print('LD  (HL),(%$4hh)\n',nxt()+nxt()*256);
        |02Bh: out.print('DEC HL\n');
        |02Ch: out.print('INC L\n');
        |02Dh: out.print('DEC L\n');
        |02Eh: out.print('LD  L,#0%$2hh\n',nxt());
        |02Fh: out.print('CPL\n');
        |030h:
          IF cpu=z80 THEN
            out.print('JR  NC,L%$4h\n',pos+e2());
          ELSE
            out.print('.BYTE 030h\n');
          END;
        |031h: out.print('LD  SP,#0%$4hh\n',nxt()+nxt()*256);
        |032h: out.print('LD  (%$4hh),A\n',nxt()+nxt()*256);
        |033h: out.print('INC SP\n');
        |034h: out.print('INC (HL)\n');
        |035h: out.print('DEC (HL)\n');
        |036h: out.print('LD  (HL),#0%$2hh\n',nxt());
        |037h: out.print('SCF\n');
        |038h: out.print('JR  C,L%$4h\n',pos+e2());
        |039h: out.print('ADD HL,SP\n');
        |03Ah: out.print('LD  A,(%$4hh)\n',nxt()+nxt()*256);
        |03Bh: out.print('DEC SP\n');
        |03Ch: out.print('INC A\n');
        |03Dh: out.print('DEC A\n');
        |03Eh: out.print('LD  A,#0%$2hh\n',nxt());
        |03Fh: out.print('CCF\n');
        |040h: out.print('LD  B,B\n');
        |041h: out.print('LD  B,C\n');
        |042h: out.print('LD  B,D\n');
        |043h: out.print('LD  B,E\n');
        |044h: out.print('LD  B,H\n');
        |045h: out.print('LD  B,L\n');
        |046h: out.print('LD  B,(HL)\n');
        |047h: out.print('LD  B,A\n');
        |048h: out.print('LD  C,B\n');
        |049h: out.print('LD  C,C\n');
        |04Ah: out.print('LD  C,D\n');
        |04Bh: out.print('LD  C,E\n');
        |04Ch: out.print('LD  C,H\n');
        |04Dh: out.print('LD  C,L\n');
        |04Eh: out.print('LD  C,(HL)\n');
        |04Fh: out.print('LD  C,A\n');
        |050h: out.print('LD  D,B\n');
        |051h: out.print('LD  D,C\n');
        |052h: out.print('LD  D,D\n');
        |053h: out.print('LD  D,E\n');
        |054h: out.print('LD  D,H\n');
        |055h: out.print('LD  D,L\n');
        |056h: out.print('LD  D,(HL)\n');
        |057h: out.print('LD  D,A\n');
        |058h: out.print('LD  E,B\n');
        |059h: out.print('LD  E,C\n');
        |05Ah: out.print('LD  E,D\n');
        |05Bh: out.print('LD  E,E\n');
        |05Ch: out.print('LD  E,H\n');
        |05Dh: out.print('LD  E,L\n');
        |05Eh: out.print('LD  E,(HL)\n');
        |05Fh: out.print('LD  E,A\n');
        |060h: out.print('LD  H,B\n');
        |061h: out.print('LD  H,C\n');
        |062h: out.print('LD  H,D\n');
        |063h: out.print('LD  H,E\n');
        |064h: out.print('LD  H,H\n');
        |065h: out.print('LD  H,L\n');
        |066h: out.print('LD  H,(HL)\n');
        |067h: out.print('LD  H,A\n');
        |068h: out.print('LD  L,B\n');
        |069h: out.print('LD  L,C\n');
        |06Ah: out.print('LD  L,D\n');
        |06Bh: out.print('LD  L,E\n');
        |06Ch: out.print('LD  L,H\n');
        |06Dh: out.print('LD  L,L\n');
        |06Eh: out.print('LD  L,(HL)\n');
        |06Fh: out.print('LD  L,A\n');
        |070h: out.print('LD  (HL),B\n');
        |071h: out.print('LD  (HL),C\n');
        |072h: out.print('LD  (HL),D\n');
        |073h: out.print('LD  (HL),E\n');
        |074h: out.print('LD  (HL),H\n');
        |075h: out.print('LD  (HL),L\n');
        |076h: out.print('HALT\n');
        |077h: out.print('LD  (HL),A\n');
        |078h: out.print('LD  A,B\n');
        |079h: out.print('LD  A,C\n');
        |07Ah: out.print('LD  A,D\n');
        |07Bh: out.print('LD  A,E\n');
        |07Ch: out.print('LD  A,H\n');
        |07Dh: out.print('LD  A,L\n');
        |07Eh: out.print('LD  A,(HL)\n');
        |07Fh: out.print('LD  A,A\n');
        |080h: out.print('ADD B\n');
        |081h: out.print('ADD C\n');
        |082h: out.print('ADD D\n');
        |083h: out.print('ADD E\n');
        |084h: out.print('ADD H\n');
        |085h: out.print('ADD L\n');
        |086h: out.print('ADD (HL)\n');
        |087h: out.print('ADD A\n');
        |088h: out.print('ADC B\n');
        |089h: out.print('ADC C\n');
        |08Ah: out.print('ADC D\n');
        |08Bh: out.print('ADC E\n');
        |08Ch: out.print('ADC H\n');
        |08Dh: out.print('ADC L\n');
        |08Eh: out.print('ADC (HL)\n');
        |08Fh: out.print('ADC A\n');
        |090h: out.print('SUB B\n');
        |091h: out.print('SUB C\n');
        |092h: out.print('SUB D\n');
        |093h: out.print('SUB E\n');
        |094h: out.print('SUB H\n');
        |095h: out.print('SUB L\n');
        |096h: out.print('SUB (HL)\n');
        |097h: out.print('SUB A\n');
        |098h: out.print('SBC B\n');
        |099h: out.print('SBC C\n');
        |09Ah: out.print('SBC D\n');
        |09Bh: out.print('SBC E\n');
        |09Ch: out.print('SBC H\n');
        |09Dh: out.print('SBC L\n');
        |09Eh: out.print('SBC (HL)\n');
        |09Fh: out.print('SBC A\n');
        |0A0h: out.print('AND B\n');
        |0A1h: out.print('AND C\n');
        |0A2h: out.print('AND D\n');
        |0A3h: out.print('AND E\n');
        |0A4h: out.print('AND H\n');
        |0A5h: out.print('AND L\n');
        |0A6h: out.print('AND (HL)\n');
        |0A7h: out.print('AND A\n');
        |0A8h: out.print('XOR B\n');
        |0A9h: out.print('XOR C\n');
        |0AAh: out.print('XOR D\n');
        |0ABh: out.print('XOR E\n');
        |0ACh: out.print('XOR H\n');
        |0ADh: out.print('XOR L\n');
        |0AEh: out.print('XOR (HL)\n');
        |0AFh: out.print('XOR A\n');
        |0B0h: out.print('OR  B\n');
        |0B1h: out.print('OR  C\n');
        |0B2h: out.print('OR  D\n');
        |0B3h: out.print('OR  E\n');
        |0B4h: out.print('OR  H\n');
        |0B5h: out.print('OR  L\n');
        |0B6h: out.print('OR  (HL)\n');
        |0B7h: out.print('OR  A\n');
        |0B8h: out.print('COM B\n');
        |0B9h: out.print('COM C\n');
        |0BAh: out.print('COM D\n');
        |0BBh: out.print('COM E\n');
        |0BCh: out.print('COM H\n');
        |0BDh: out.print('COM L\n');
        |0BEh: out.print('COM (HL)\n');
        |0BFh: out.print('COM A\n');
        |0C0h: out.print('RET NZ\n');
        |0C1h: out.print('POP BC\n');
        |0C2h: out.print('JP NZ,L%$4h\n',nxt()+nxt()*256);
        |0C3h: out.print('JP L%$4h\n',nxt()+nxt()*256);
        |0C4h: out.print('CALL NZ,L%$4h\n',nxt()+nxt()*256);
        |0C5h: out.print('PUSH BC\n');
        |0C6h: out.print('ADD #0%$2hh\n',nxt());
        |0C7h: out.print('RST 0\n');
        |0C8h: out.print('RET Z\n');
        |0C9h: out.print('RET\n');
        |0CAh: out.print('JP Z,L%$4h\n',nxt()+nxt()*256);
        |0CBh: out.print('.BYTE 0CBh\n');
        |0CCh: out.print('CALL Z,L%$4h\n',nxt()+nxt()*256);
        |0CDh: out.print('CALL L%$4h\n',nxt()+nxt()*256);
        |0CEh: out.print('ADC #0%$2hh\n',nxt());
        |0CFh: out.print('RST 8\n');
        |0D0h: out.print('RET NC\n');
        |0D1h: out.print('POP DE\n');
        |0D2h: out.print('JP NC,L%$4h\n',nxt()+nxt()*256);
        |0D3h: out.print('OUT (%$2hh),A\n',nxt());
        |0D4h: out.print('CALL NC,L%$4h\n',nxt()+nxt()*256);
        |0D5h: out.print('PUSH DE\n');
        |0D6h: out.print('SUB #0%$2hh\n',nxt());
        |0D7h: out.print('RST 10h\n');
        |0D8h: out.print('RET C\n');
        |0D9h: out.print('EXX\n');
        |0DAh: out.print('JP C,L%$4h\n',nxt()+nxt()*256);
        |0DBh: out.print('IN A,(%$2hh)\n',nxt());
        |0DCh: out.print('CALL C,L%$4h\n',nxt()+nxt()*256);
        |0DDh: out.print('.BYTE 0DDh\n');
        |0DEh: out.print('SBC #0%$2hh\n',nxt());
        |0DFh: out.print('RST 20h\n');
        |0E0h: out.print('RET PO\n');
        |0E1h: out.print('POP HL\n');
        |0E2h: out.print('JP PO,L%$4h\n',nxt()+nxt()*256);
        |0E3h: out.print('EX (SP),HL\n');
        |0E4h: out.print('CALL PO,L%$4h\n',nxt()+nxt()*256);
        |0E5h: out.print('PUSH HL\n');
        |0E6h: out.print('AND #0%$2hh\n',nxt());
        |0E7h: out.print('RST 20h\n');
        |0E8h: out.print('RET PE\n');
        |0E9h: out.print('JP (HL)\n');
        |0EAh: out.print('JP PE,L%$4h\n',nxt()+nxt()*256);
        |0EBh: out.print('EX DE,HL\n');
        |0ECh: out.print('CALL PE,L%$4h\n',nxt()+nxt()*256);
        |0EDh: out.print('.BYTE 0DDh\n');
        |0EEh: out.print('XOR #0%$2hh\n',nxt());
        |0EFh: out.print('RST 28h\n');
        |0F0h: out.print('RET P\n');
        |0F1h: out.print('POP AF\n');
        |0F2h: out.print('JP P,L%$4h\n',nxt()+nxt()*256);
        |0F3h: out.print('DI\n');
        |0F4h: out.print('CALL P,L%$4h\n',nxt()+nxt()*256);
        |0F5h: out.print('PUSH AF\n');
        |0F6h: out.print('OR  #0%$2hh\n',nxt());
        |0F7h: out.print('RST 30h\n');
        |0F8h: out.print('RET M\n');
        |0F9h: out.print('LD  SP,HL\n');
        |0FAh: out.print('JP M,L%$4h\n',nxt()+nxt()*256);
        |0FBh: out.print('EI\n');
        |0FCh: out.print('CALL M,L%$4h\n',nxt()+nxt()*256);
        |0FDh: out.print('.BYTE 0FDh\n');
        |0FEh: out.print('COM #0%$2hh\n',nxt());
        |0FFh: out.print('RST 38h\n');
      END;
    ELSIF sta[pos]=data THEN
      out.print('        .BYTE 0%$2hh',nxt());
      i:=1;
      WHILE (i<8) & (sta[pos]=data) DO
        out.print(',0%$2hh',nxt()); INC(i);
      END;
      out.print('\n');
    END;
  END;
END vis_mem;

PROCEDURE help;
BEGIN
  out.print('as80 <file name> { <program entry address> }\n');
  out.print('   z80 assembler.\n');
  HALT;
END help;

PROCEDURE io_chk;
BEGIN
  IF bio.done THEN RETURN END;
  tty.perror(bio.error,'File "%s", %%s.\n',arg.words[0]); HALT(1);
END io_chk;

VAR
  i,e,p: INTEGER;
  ok   : BOOLEAN;
  inp  : bio.FILE;

BEGIN
  tty.print('z80 assembler, v 25-Dec-90\n');
  FOR i:=0 TO HIGH(mem) DO mem[i]:=377c; sta[i]:=undef END;
  lbl:=sta;
  cpu:=i8080;
  IF HIGH(arg.words)<0 THEN help; HALT END;
  bio.open(inp,arg.words[0],'r'); io_chk;
  IF NOT arg.number('eof',e) THEN e:=bio.eof(inp) END;
  IF e>BYTES(mem) THEN e:=BYTES(mem) END;
  IF e<0 THEN e:=0 END;
IF e<=3000h THEN
  bio.read(inp,ADR(mem),e); io_chk;
  FOR i:=0 TO e-1 DO sta[i]:=data END;
ELSE
bio.read(inp,ADR(mem),3000h); io_chk;
FOR i:=0 TO 2FFFh DO sta[i]:=data END;
bio.read(inp,ADR(mem)+2000h,e-3000h); io_chk;
FOR i:=8000h TO 4FFFh+e DO sta[i]:=data END;
END;

  bio.close(inp); io_chk;
  IF HIGH(arg.words)>0 THEN
    FOR i:=1 TO HIGH(arg.words) DO
      p:=0; str.iscan(e,arg.words[i],p,ok);
      IF ok & (arg.words[i][p]=0c) & (e>=0) & (e<=HIGH(mem)) THEN
        mark_code(e);
      ELSE
        tty.print('Invalid number: "%s".\n',arg.words[i]); HALT(1);
      END;
    END;
  ELSE
    mark_code(0);
  END;
  vis_mem;
END as80.
