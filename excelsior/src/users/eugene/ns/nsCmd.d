DEFINITION MODULE nsCmd; (* Sem 30-Nov-90. (c) KRONOS *)

FROM SYSTEM      IMPORT WORD;

IMPORT sym : nsSym;

TYPE
  cop       = (absf,addf,cmpf,divf,movf,mulf,negf,subf,            -- fpp
               add,addc,addr,and,bic,cmp,mov,or,sub,subc,tbit,xor, -- int
               abs,addp,ash,cbit,cbiti,sbit,sbiti,                 -- int 04Eh
               com,ibit,lsh,neg,not,rot,subp,                      -- int 04Eh
               dei,div,mei,mod,mul,quo,rem,                        -- int 0CEh
               ffs);                                               -- int 06Eh

  cpu_reg   = (UPSR,о1,о2,о3,о4,о5,о6,о7,FP,SP,SB,оB,оC,PSR,INTBASE,MD);
  mmu_reg   = (BPR0,BPR1,п2,п3,PF0,PF1,п6,п7,SC,п9,MSR,BCNT,PTB0,PTB1,пE,EIA);
  condition = (EQ,NE,CS,CC,HI,LS,GT,LE,FS,FC,LO,HS,LT,GE);

VAR
  code : DYNARR OF CHAR;
  cnt  : INTEGER;

PROCEDURE new_code(n: INTEGER): INTEGER;

PROCEDURE put (n: WORD);
PROCEDURE put2(n: WORD);
PROCEDURE disp(n: INTEGER);

PROCEDURE md(a: sym.access; pos: INTEGER): BITSET;
PROCEDURE isz(n: INTEGER): BITSET;
PROCEDURE put_disp(a: sym.access; sz: INTEGER);
PROCEDURE put_index(a: sym.access);

PROCEDURE cmd (c: cop;  x,y: sym.access; sz: INTEGER);

PROCEDURE enter  (rg: BITSET; loc_sz: INTEGER);
PROCEDURE exit   (rg: BITSET);
PROCEDURE restore(rg: BITSET);
PROCEDURE save   (rg: BITSET);
PROCEDURE ret    (disp: INTEGER);
PROCEDURE rxp    (disp: INTEGER);
PROCEDURE case   (src: sym.access; sz: INTEGER);

PROCEDURE index  (length,index: sym.access; accum,sz: INTEGER);
PROCEDURE check  (bounds,src: sym.access; dest,sz: INTEGER);
PROCEDURE flag;
PROCEDURE svc;

PROCEDURE floor(x,y: sym.access; xsz,ysz: INTEGER);
PROCEDURE round(x,y: sym.access; xsz,ysz: INTEGER);
PROCEDURE trunc(x,y: sym.access; xsz,ysz: INTEGER);
PROCEDURE movif(x,y: sym.access; xsz,ysz: INTEGER);
PROCEDURE movfl(x,y: sym.access);
PROCEDURE movlf(x,y: sym.access);

PROCEDURE movm (x,y: sym.access; sz,no: INTEGER);
PROCEDURE cmpm (x,y: sym.access; sz,no: INTEGER);

PROCEDURE movs (sz: INTEGER; back,while,until: BOOLEAN);
PROCEDURE movst(back,while,until: BOOLEAN);
PROCEDURE cmps (sz: INTEGER; back,while,until: BOOLEAN);
PROCEDURE cmpst(back,while,until: BOOLEAN);
PROCEDURE skps (sz: INTEGER; back,while,until: BOOLEAN);
PROCEDURE skpst(back,while,until: BOOLEAN);

PROCEDURE movx (x,y: sym.access; xsz,ysz: INTEGER);
PROCEDURE movz (x,y: sym.access; xsz,ysz: INTEGER);
PROCEDURE ext  (base,dest: sym.access; sz,offset,length: INTEGER);
PROCEDURE ins  (src,base : sym.access; sz,offset,length: INTEGER);
PROCEDURE exts (base,dest: sym.access; sz,offset,length: INTEGER);
PROCEDURE inss (src,base : sym.access; sz,offset,length: INTEGER);

PROCEDURE lfsr  (x: sym.access);
PROCEDURE sfsr  (x: sym.access);
PROCEDURE lpr   (x: sym.access; rg: cpu_reg; sz: INTEGER);
PROCEDURE spr   (x: sym.access; rg: cpu_reg; sz: INTEGER);
PROCEDURE lmr   (x: sym.access; rg: mmu_reg);
PROCEDURE smr   (x: sym.access; rg: mmu_reg);
PROCEDURE bispsr(x: sym.access; sz: INTEGER);
PROCEDURE bicpsr(x: sym.access; sz: INTEGER);

PROCEDURE adjsp(x: sym.access; sz: INTEGER);
PROCEDURE s    (cc: condition; y: sym.access; sz: INTEGER);

PROCEDURE cxpd(x: sym.access);
PROCEDURE jump(x: sym.access);
PROCEDURE jsr (x: sym.access);
PROCEDURE acb (inc: INTEGER; index: sym.access; dest,sz: INTEGER);

END nsCmd.
