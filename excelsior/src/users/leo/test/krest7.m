MODULE krest7; (*$+10000 Leo  07-Dec-87. (c) KRONOS *)
               (*        Leo  25-Jan-01.            *)


(*****************  KRONOS  TESTs  *********************)
(*                                                     *)
(*      UNTESTED COMMANDS for 20-May-88:               *)
(*                                                     *)
(* TEST1: LXA, SSWU                                    *)
(* TEST4: SHL, SHR                                     *)
(* TEST5: FADD, FSUB, FMUL, FDIV, FCMP, FABS, FNEG     *)
(*        FFCT                                         *)
(* TEST6: CX, LEA, LEW, SEW                            *)
(* TEST7: CPCOP, PCOP                                  *)
(*                                                     *)
(*****************  KRONOS  TESTs  *********************)

(*$T-  онтроль индексов !                     *)
(*$R-  онтроль отрезков !                     *)

IMPORT  defCodes, SYSTEM;
IMPORT  cod: defCodes;
FROM defCodes  IMPORT  setm, getm, sgw3, quit, store, lodt, lib,
                      jfsc, li6, activ, add;
FROM SYSTEM     IMPORT  ADDRESS;

--IMPORT krestDebug; CONST Debug=TRUE;
--                   CONST Debug=FALSE;
CONST Debug = TRUE;

CONST QUOT=TRUE;
--CONST QUOT=FALSE;

CONST consolVM  = TRUE;
CONST consolI6  = FALSE;
CONST consolIgd = FALSE;

CONST CPU=07 ;   (* 2 -- for KRONOS P2.2   6 -- for KRONOS P2.6 *)

VAR       w02,w03,w04,w05,w06,w07,w08,w09,w0A,w0B,w0C,w0D,w0E,w0F: INTEGER;
  w10,w11,w12,w13,w14,w15,w16,w17,w18,w19,w1A,w1B,w1C,w1D,w1E,w1F: INTEGER;
  w20,w21,w22,w23,w24,w25,w26,w27,w28,w29,w2A,w2B,w2C,w2D,w2E,w2F: INTEGER;
  w30,w31,w32,w33,w34,w35,w36,w37,w38,w39,w3A,w3B,w3C,w3D,w3E,w3F: INTEGER;
  w40,w41,w42,w43,w44,w45,w46,w47,w48,w49,w4A,w4B,w4C,w4D,w4E,w4F: INTEGER;
  w50,w51,w52,w53,w54,w55,w56,w57,w58,w59,w5A,w5B,w5C,w5D,w5E,w5F: INTEGER;
  w60,w61,w62,w63,w64,w65,w66,w67,w68,w69,w6A,w6B,w6C,w6D,w6E,w6F: INTEGER;
  w70,w71,w72,w73,w74,w75,w76,w77,w78,w79,w7A,w7B,w7C,w7D,w7E,w7F: INTEGER;
  w80,w81,w82,w83,w84,w85,w86,w87,w88,w89,w8A,w8B,w8C,w8D,w8E,w8F: INTEGER;
  w90,w91,w92,w93,w94,w95,w96,w97,w98,w99,w9A,w9B,w9C,w9D,w9E,w9F: INTEGER;

--              GLOBAL AREA LAYOUT:

-- WORD  7                        
-- WORD  8..0D                    
-- WORD  E                      образец 55555555 } после выполнения
-- WORD  F                      образец AAAAAAAA }    tst003
--
-- WORD  4                        для TEST0
-- WORD  5                      CPU
--
-- WORD  2                       
--                              9091h -- Qbus INP, OUT test
--
-- WORD  3
--                    BAD0   неправильный номер процессора
--                    BAD1   неправильная команда ACTIV
--                    BAD2   неправильная команда ENTR
--                    BAD3   неожиданно T-регистр <> 0
--
--              BYTE 1            (для ошибок с одинаковым кодом)
--              BYTE 0            (младший байт)
--
--                      01    нарушена последовательность тестов
--                      02    ошибка чтения/записи в  стек
--                      03    неправильная нумерация в  стеке
--                      04    ошибка движения 0 или 1 через верхушку стека
--                            или ROL, ROR
--
--                      4C    переполнение/исчерпание  стека
--                           (проверяется  исполнения каждого теста)
--
--                      0F    ошибка команд  LI0..LI0F
--                      10    ошибка команды LIB
--                      11    ошибка команды LID
--                      12    ошибка команды LIW
--
--                      14    ошибка команды LLA
--                      15    ошибка команды LGA
--
--                      18    не исполнился JFLC
--                      19    не исполнился JFL
--                      1A    не исполнился JFSC
--                      1B    не исполнился JFS
--                      1C    не исполнился JBLC
--                      1D    не исполнился JBL
--                      1E    не исполнился JBSC
--                      1F    не исполнился JBS
--
--                      23    ошибка чтения/нумерации LSW
--                      24    ошибка чтения данных LLW4
--                      30    ошибка чтения/записи или нумерации LLW, SLW
--                      31    ошибка чтения/записи или нумерации LGW, SGW
--                      33    ошибка записи/нумерации SSW
--                      34    ошибка запмси данных SLW4
--                      3F    ошибка номеров SLW4..SLW0F или LLW4..LLW0F
--                      40    ошибка LXB или SXB
--                      41    ошибка LXW или SXW
--                      42    ошибка чтения данных LGW2..LGW0F
--                      52    ошибка записи данных LGW2
--                      5F    ошибка номеров SGW2..SGW0F или LGW2..LGW0F
--                    60..6F  ошибка чтения/нумерации LSW0..LSW0F
--                    70..7F  ошибка записи/нумерации SSW0..SSW0F
--
--                      8E    ошибка ROL
--                      8F    ошибка ROR
--                      90    ошибка INP для Qbus
--                      91    ошибка OUT для Qbus
--
--                      A0    ошибка в команде LSS
--                      A1    ошибка в команде LEQ
--                      A2    ошибка в команде GTR
--                      A3    ошибка в команде GEQ
--                      A4    ошибка в команде EQU
--                      A5    ошибка в команде NEQ
--                      A6    ошибка в команде ABS
--                      A7    ошибка в команде NEG
--
--                      AE    ошибка в команде NOTS
--
--                      B0    ошибка в команде DECS
--                      B2    ошибка в команде STORE или LODFV
--                      BE    ошибка в команде ORJP
--                      BF    ошибка в команде ANDJP
--                      C8    ошибка в команде ALLOC
--                      CA    ошибка выполнения RTN
--                      CF    ошибка CL
--
--                      D0    ошибка при вызове loc1..locF неправильные S или L
--                      D1    ошибка при вызове loc1
--                      D2    ошибка при вызове loc2
--
--                      E4    ошибка INC1 или ADD
--                      E5    ошибка DEC1 или SUB
--                      E6    ошибка INC  или ADD
--                      E7    ошибка DEC  или SUB
--
--
----------------------------------------------------------------
--                                           --
----------------------------------------------------------------

PROCEDURE STOP(why: INTEGER);
CODE sgw3 quit END STOP;

PROCEDURE check4C;
CODE
  store lodt jfsc 04    -- если глубина  стека не равна 0
   lib 4Ch sgw3 quit    -- 
END check4C;

PROCEDURE SETM(m: BITSET); CODE setm END SETM;
PROCEDURE GETM(): BITSET ; CODE getm END GETM;

PROCEDURE TregADR(): ADDRESS; CODE activ li6 add END TregADR;


----------------------------------------------------------------

MODULE TEST0;

IMPORT CPU, w04, Debug, check4C;
FROM defCodes   IMPORT  li0, llw, lxb, lsw0, li1, lgw, lxw, lsw1,
                        li2, lew, lgw2, lsw2, li3, lsw, lgw3, lsw3,
                        li4, llw4, lgw4, lsw4, li5, llw5, lgw5, lsw5,
                        li6, llw6, lgw6, lsw6, li7, llw7, lgw7, lsw7,
                        li8, llw8, lgw8, lsw8, li9, llw9, lgw9, lsw9,
                        li0A, llw0A, lgw0A, lsw0A, li0B, llw0B, lgw0B, lsw0B,
                        li0C, llw0C, lgw0C, lsw0C, li0D, llw0D, lgw0D, lsw0D,
                        li0E, llw0E, lgw0E, lsw0E, li0F, llw0F, lgw0F, lsw0F,
                        lib, slw, sxb, ssw0, lid, sgw, sxw, ssw1,
                        liw, sew, sgw2, ssw2, lin, ssw, sgw3, ssw3,
                        lla, slw4, sgw4, ssw4, lga, slw5, sgw5, ssw5,
                        lsa, slw6, sgw6, ssw6, lea, slw7, sgw7, ssw7,
                        jflc, slw8, sgw8, ssw8, jfl, slw9, sgw9, ssw9,
                        jfsc, slw0A, sgw0A, ssw0A, jfs, slw0B, sgw0B, ssw0B,
                        jblc, slw0C, sgw0C, ssw0C, jbl, slw0D, sgw0D, ssw0D,
                        jbsc, slw0E, sgw0E, ssw0E, jbs, slw0F, sgw0F, ssw0F,
                        reset, lss, move, incl, quit, leq, chknil, excl,
                        getm, gtr, lsta,     setm, geq, comp,
                        trap, equ, gb, inc1, tra, neq, gb1, dec1,
                        tr, abs, chk, inc, idle, neg, chkz, dec,
                        add, or, alloc, stot, sub, and, entr, lodt,
                        mul, xor, rtn, lxa, div, bic, nop, lpc,
                        shl, in, cx, bbu, shr, bit, ci, bbp,
                        rol, not, cf, bblt, ror, mod, cl,
                        io0, decs, cl0, swap, io1, drop, cl1, lpa,
                        io2, lodfv, cl2, lpw, io3, store, cl3, spw,
                        io4, stofv, cl4, sswu, copt, cl5, rchk,
                        cpcop, cl6, rchkz, pcop, cl7, cm,
                        fadd, for1, cl8, fsub, for2, cl9, bmg,
                        fmul, entc, cl0A, activ, fdiv, xit, cl0B, usr,
                        fcmp, addpc, cl0C, sys, fabs, jump, cl0D, nii,
                        fneg, orjp, cl0E, dot, ffct, andjp, cl0F, invld;

EXPORT loc1, loc2, loc3, loc4, loc5, loc6, loc7, loc8
     , loc9, locA, locB, locC, locD, locE, locF, loc10;

-- роцедура loc1..F должны быть текстуально первыми процедурами
-- и иметь номера в процедурной табличке 1..F соответственно

PROCEDURE loc1(): INTEGER;
-- global word 8 должен быть равен L
-- global word 9 должен быть равен S
-- возвращает свой номер
-- все эти замечания относятся и к следующим 15 процедурам
  PROCEDURE cod;
  CODE
    lgw8 lla 0     equ jfsc 08
    lgw9 li0 alloc equ jfsc 02 li1 rtn
    lib 0D0h sgw3 quit         li1 rtn
  END cod;
BEGIN cod END loc1;

PROCEDURE loc2(): INTEGER;
  PROCEDURE cod;
  CODE
    lgw8 lla 0     equ jfsc 08
    lgw9 li0 alloc equ jfsc 02 li2 rtn
    lib 0D0h sgw3 quit         li2 rtn
  END cod;
BEGIN cod END loc2;

PROCEDURE loc3(): INTEGER;
  PROCEDURE cod;
  CODE
    lgw8 lla 0     equ jfsc 08
    lgw9 li0 alloc equ jfsc 02 li3 rtn
    lib 0D0h sgw3 quit         li3 rtn
  END cod;
BEGIN cod END loc3;

PROCEDURE loc4(): INTEGER;
  PROCEDURE cod;
  CODE
    lgw8 lla 0     equ jfsc 08
    lgw9 li0 alloc equ jfsc 02 li4 rtn
    lib 0D0h sgw3 quit         li4 rtn
  END cod;
BEGIN cod END loc4;

PROCEDURE loc5(): INTEGER;
  PROCEDURE cod;
  CODE
    lgw8 lla 0     equ jfsc 08
    lgw9 li0 alloc equ jfsc 02 li5 rtn
    lib 0D0h sgw3 quit         li5 rtn
  END cod;
BEGIN cod END loc5;

PROCEDURE loc6(): INTEGER;
  PROCEDURE cod;
  CODE
    lgw8 lla 0     equ jfsc 08
    lgw9 li0 alloc equ jfsc 02 li6 rtn
    lib 0D0h sgw3 quit         li6 rtn
  END cod;
BEGIN cod END loc6;

PROCEDURE loc7(): INTEGER;
  PROCEDURE cod;
  CODE
    lgw8 lla 0     equ jfsc 08
    lgw9 li0 alloc equ jfsc 02 li7 rtn
    lib 0D0h sgw3 quit         li7 rtn
  END cod;
BEGIN cod END loc7;

PROCEDURE loc8(): INTEGER;
  PROCEDURE cod;
  CODE
    lgw8 lla 0     equ jfsc 08
    lgw9 li0 alloc equ jfsc 02 li8 rtn
    lib 0D0h sgw3 quit         li8 rtn
  END cod;
BEGIN cod END loc8;

PROCEDURE loc9(): INTEGER;
  PROCEDURE cod;
  CODE
    lgw8 lla 0     equ jfsc 08
    lgw9 li0 alloc equ jfsc 02 li9 rtn
    lib 0D0h sgw3 quit         li9 rtn
  END cod;
BEGIN cod END loc9;

PROCEDURE locA(): INTEGER;
  PROCEDURE cod;
  CODE
    lgw8 lla 0     equ jfsc 08
    lgw9 li0 alloc equ jfsc 02 li0A rtn
    lib 0D0h sgw3 quit         li0A rtn
  END cod;
BEGIN cod END locA;

PROCEDURE locB(): INTEGER;
  PROCEDURE cod;
  CODE
    lgw8 lla 0     equ jfsc 08
    lgw9 li0 alloc equ jfsc 02 li0B rtn
    lib 0D0h sgw3 quit         li0B rtn
  END cod;
BEGIN cod END locB;

PROCEDURE locC(): INTEGER;
  PROCEDURE cod;
  CODE
    lgw8 lla 0     equ jfsc 08
    lgw9 li0 alloc equ jfsc 02 li0C rtn
    lib 0D0h sgw3 quit         li0C rtn
  END cod;
BEGIN cod END locC;

PROCEDURE locD(): INTEGER;
  PROCEDURE cod;
  CODE
    lgw8 lla 0     equ jfsc 08
    lgw9 li0 alloc equ jfsc 02 li0D rtn
    lib 0D0h sgw3 quit         li0D rtn
  END cod;
BEGIN cod END locD;

PROCEDURE locE(): INTEGER;
  PROCEDURE cod;
  CODE
    lgw8 lla 0     equ jfsc 08
    lgw9 li0 alloc equ jfsc 02 li0E rtn
    lib 0D0h sgw3 quit         li0E rtn
  END cod;
BEGIN cod END locE;

PROCEDURE locF(): INTEGER;
  PROCEDURE cod;
  CODE
    lgw8 lla 0     equ jfsc 08
    lgw9 li0 alloc equ jfsc 02 li0F rtn
    lib 0D0h sgw3 quit         li0F rtn
  END cod;
BEGIN cod END locF;

PROCEDURE loc10(): INTEGER;
  PROCEDURE cod;
  CODE
    lgw8 lla 0     equ jfsc 09
    lgw9 li0 alloc equ jfsc 03 lib 10h rtn
    lib 0D0h sgw3 quit         lib 10h rtn
  END cod;
BEGIN cod END loc10;


----------------------------------------------------------------

PROCEDURE enterTEST0;
CODE
  sys 00 lib CPU neq jfsc 07                    -- еправильный тип процессора
    lid 0D0h 0BAh sgw3 quit jbs 07              --  0BAD0h, дальнейшая
                                                -- работа !
  getm copt sgw7 3 bic setm                     -- disable ipts
  1 0 tra                                       -- update registers
  0 lsw0 sgw8                                   -- global 8 = P register
  0 lsw0 lsw4 li1 sub sgw9                      -- global 9 = S register
  lgw7 setm                                     -- restore mask

  activ lgw8 neq jfsc 05                        -- неправильная команда activ
    lid 0D1h 0BAh sgw3 quit                     --  0BAD1h

  entr 252 -- 256 локалов для 0'левой процедуры

  0 alloc lgw9 lib 252 add neq jfsc 05          -- неправильная команда entr
    lid 0D2h 0BAh sgw3 quit                     --  0BAD2h

END enterTEST0;

PROCEDURE tst000; --         ACTIV
CODE
  00h sgw2   --   0
  00h sgw3   --    0
END tst000;

PROCEDURE tst001;       --   
--
--      JFLC,JFL,JFSC,JFS,JBLC,JBL,JBSC,JBS,ORJP,ANDJP,NOT
--       : 18 19 1A 1B 1E 1F BE BF AE
--       : 18 19 1A 1B 1E 1F BE BF AE 01
--
CODE
  lga 02 inc1                      -- увеличить номер теста
  lgw2 01h neq jfsc 05h            -- нарушена последовательность тестов
   lid 01h 01h sgw3 quit           -- занесение причины,  0101

  jfs 04h                          -- безусловный переход вперед
   lib 1Bh sgw3 quit               --  001B, переход не выполнился

  jfl 04h 00h                      -- длинный безусловный переход вперед
   lib 19h sgw3 quit               --  0019, переход не выполнился

  li0 jfsc 05                      -- условный переход вперед по FALSE
   lid 1Ah 01h sgw3 quit           --  01A1, переход не выполнился

  li1 jfsc 02                      -- условный переход вперед по TRUE
   jfs 05                          -- не должен выполняться для 01
    lid 1Ah 02h sgw3 quit          --  021A, неожиданный переход

  li0 jflc 05h 00h                 -- условный переход вперед по FALSE
   lid 18h 01h sgw3 quit           --  0118, переход не выполнился

  li1 jflc 02h 00h                 -- условный переход вперед по TRUE
   jfs 05                          -- не должен выполняться для 01
    lid 18h 02h sgw3 quit          --  0218, неожиданный переход

  jfs 02                           --
   jfs 06                          -- сюда должна пыгнуть следующая команда
   jbs 04                          -- безусловный переход назад
    lib 1Fh sgw3 quit              --  001F, переход не произошел

  jfs 02                           --
   jfs 07                          -- сюда должна пыгнуть следующая команда
   jbl 05h 00h                     -- безусловный переход назад длинный
    lib 1Dh sgw3 quit              --  001D, переход не произошел

  jfs 02                           --
   jfs 08                          -- сюда должна пыгнуть следующая команда
   li0 jbsc 05                     -- условный переход назад по FALSE
    lid 1Eh 01h sgw3 quit          --  011E, переход не произошел

  jfs 02                           --
   jfs 09                          -- сюда должна пыгнуть следующая команда
   00 jblc 06h 00h                 -- условный переход назад длинный по FALSE
    lid 1Ch 01h sgw3 quit          --  011C, переход не произошел

  jfs 05                           --
   lib 1Eh 02h sgw3 quit           --  021E, неожиданный переход
  li1 jbsc 08                      -- условный переход назад по TRUE

  jfs 05                           --
   lid 1Ch 02h sgw3 quit           --  021C, неожиданный переход
  li1 jblc 09h 00h                 -- условный переход назад длинный по TRUE

  li0 orjp 02                      --  переход по FALSE
    jfs 05                         -- должен попасть сюда
   lid 0BEh 01h sgw3 quit          --  01BE, неожиданный переход
  store lodt li0 neq jfsc 05
   lid 0BFh 01h sgw3 quit          --  04BE, стек не пуст!

  li1 orjp 07                      --  переход по TRUE
   lid 0BEh 02h sgw3 quit          --  02BE, переход не исполнился
   jfs 09
  li1 neq jfsc 05                  -- на стеке должно быть 00000001
   lid 0BEh 03h sgw3 quit          --  03BE, неправильное знач. на стеке

  li1 andjp 02                     --  переход по TRUE
    jfs 05                         -- должен попасть сюда
   lid 0BFh 01h sgw3 quit          --  01BF, неожиданный переход
  store lodt li0 neq jfsc 05
   lid 0BFh 01h sgw3 quit          --  04BF, стек не пуст!


  li0 andjp 07                     --  переход по FALSE
   lid 0BFh 02h sgw3 quit          --  02BF, переход не исполнился
   jfs 09
  li0 neq jfsc 05                  -- на стеке должно быть 00000000
   lid 0BFh 03h sgw3 quit          --  03BF, неправильное знач. на стеке

  li0 not li1 neq jfsc 05          -- NOT FALSE m.b. = TRUE
   lid 0AEh 01h sgw3 quit          --  01AE

  li1 not li0 neq jfsc 05          -- NOT TRUE  m.b. = FALSE
   lid 0AEh 02h sgw3 quit          --  02AE

  li1 neg not li0 neq jfsc 05      -- NOT TRUE  m.b. = FALSE
   lid 0AEh 03h sgw3 quit          --  03AE

END tst001;

PROCEDURE tst002;       --  
--
--      COPT,DROP,LI0,LI1,...,LI0E,LI0F,LIB,LID,LIW
--       : 00h,01h,02h,...,0Fh,10h,11h,12h
--       : 0Fh,                10h,11h,12h,02h
--
CODE
  lga 02 inc1                      -- увеличить номер теста
  lgw2 02h neq jfsc 05h            -- нарушена последовательность тестов
   lid 01h 02h sgw3 quit           -- занесение причины,  0201

  li0C copt
  li0C neq jfsc 05h                -- неправильный COPT
   lid 0B5h 01h sgw3 quit          -- занесение причины,  01B5

  li8 drop
  li0C neq jfsc 05h                -- неправильный DROP
   lid 0B1h 01h sgw3 quit          -- занесение причины,  01B1

  li0   li1  add li2  add li3  add li4  add     -- 0 + 1 + 2 + 3 + 4
        li5  add li6  add li7  add li8  add     --   + 5 + 6 + 7 + 8
        li9  add li0A add li0B add li0C add     --   + 9 + A + B + C
        li0D add li0E add li0F add lib  78h     --   + D + E + F = 78h
  neq jfsc 05
    lid 0Fh 01h sgw3 quit                       -- сумма # 120,  010F
  li0 neg li0 neq jfsc 05                       -- NEG(0) = 0 ?
    lid 0Fh 02h sgw3 quit                       --  020F

  li0 lib 00h neq jfsc 04                       -- загрузка байта 00
    lib 10h sgw3 quit                           -- не работает LIB,  0010
  li0F 4 rol li0F or lib 0FFh neq jfsc 05       -- загрузка байта FF
    lid 10h 0FFh sgw3 quit                      -- не работает LIB,  FF10

  li0 lid 00h 00h neq jfsc 04h                  -- загрузка 2-х байтов 0000
    lib 11h sgw3 quit                           -- не работает LID,  0011
  lib 0FFh 8 rol lib 0FFh or lid 0FFh 0FFh neq  -- загрузка 2-х байтов FFFF
  jfsc 05                                       -- не работает LID
    lid 11h 0FFh sgw3 quit                      --  FF11

  li0 liw 00h 00h 00h 00h neq jfsc 04h          -- загрузка слова 00000000
    lib 12h sgw3 quit                           -- не работает LIW,  0012
  li1 neg liw 0FFh 0FFh 0FFh 0FFh neq jfsc 05   -- загрузка слова FFFFFFFF
    lid 12h 0FFh sgw3 quit                      -- не работает LIW,  FF12

END tst002;

PROCEDURE tst003;       --     
--
--       : 02h
--
CODE
  lga 02 inc1                      -- увеличить номер теста
  lgw2 03h neq jfsc 05h            -- нарушена последовательность тестов
   lid 01h 03h sgw3 quit           -- занесение причины,  0301

  lib 10h sgw8                                  --  создаем образец
  li1                                           --  010101010101...01
    li2 rol li1 or lgw8 li1 sub copt sgw8 not jbsc 0Ch
  sgw8                                          --  запоминаем в global word 08
  lgw8 liw 55h 55h 55h 55h neq jfsc 05          --  правильно ли создан?
    lid 02h 55h sgw3 quit                       --  ошибка порождения образца
                                                --   5502

  lgw8  copt  sgw0F                             --  global word F = 55555555
  li1 neg xor sgw0E                             --  global word E = AAAAAAAA

  li8 li8 rol sgw7                              --  счетчик циклов

  lgw8                                          --  размножим образец на
  copt copt copt copt copt copt                 --  стеке 7 раз
  or   or   or   or   or   or                   --  и сложим с собой
  lgw8 neq jfsc 05                              --  лишние биты 1 в стеке!
    lid 02h 11h sgw3 quit                       --   1102

  lgw8                                          --  размножим образец на
  copt copt copt copt copt copt                 --  стеке 7 раз
  and  and  and  and  and  and                  --  и умножим на себя
  lgw8 neq jfsc 04h                             --  пропали биты 1 в стеке!
    lib 02h sgw3 quit                           --   0002

  lgw8 li1 neg xor sgw8                         --  55555555 -> AAAAAAAA

  lgw7 li1 sub copt sgw7 not jbsc 38h           -- счетчик = 0 ?

END tst003;

PROCEDURE tst004;       --    
--
--       каждое слово стека пишутся номера его полуслов.
--      осле записи всех номеров стек читается и номера сравниваются.
--       : 03h
--
--
CODE
  lga 02 inc1                      -- увеличить номер теста
  lgw2 04h neq jfsc 05h            -- нарушена последовательность тестов
   lid 01h 04h sgw3 quit           -- занесение причины,  0401


  li2 li8 rol sgw7                              --  счетчик циклов

  liw 01h 00h 02h 00h                           -- 0 и 1 полуслова
  liw 04h 00h 08h 00h                           -- 2 и 3 полуслова
  liw 10h 00h 20h 00h                           -- 4 и 5 полуслова
  liw 40h 00h 80h 00h                           -- 6 и 7 полуслова
  liw 00h 01h 00h 02h                           -- 8 и 9 полуслова
  liw 00h 04h 00h 08h                           -- A и B полуслова
  liw 00h 10h 00h 20h                           -- B и C полуслова
        sgw8    sgw9    sgw0A
        sgw0B   sgw0C   sgw0D
  liw 01h 00h 02h 00h equ jfsc 56               -- 0 и 1 полуслова
  liw 00h 10h 00h 20h lgw8  equ jfsc 47         -- B и C полуслова
  liw 00h 04h 00h 08h lgw9  equ jfsc 38         -- A и B полуслова
  liw 00h 01h 00h 02h lgw0A equ jfsc 29         -- 8 и 9 полуслова
  liw 40h 00h 80h 00h lgw0B equ jfsc 20         -- 6 и 7 полуслова
  liw 10h 00h 20h 00h lgw0C equ jfsc 11         -- 4 и 5 полуслова
  liw 04h 00h 08h 00h lgw0D equ jfsc 02         -- 2 и 3 полуслова
  jfs 04h
    lib 03h sgw3 quit                           --  0003

  lgw7 li1 sub copt sgw7 not jbsc 117           -- счетчик = 0 ?

END tst004;

PROCEDURE tst005;       --        0,1   
CODE
  lga 02 inc1                      -- увеличить номер теста
  lgw2 05h neq jfsc 05h            -- нарушена последовательность тестов
   lid 01h 05h sgw3 quit           -- занесение причины,  0501

  lib 20h sgw7                                  -- счетчик

  li1                                           -- проверка движения 1
  lgw7 rol                                      -- влево  на  gw7
  lgw7 ror                                      -- вправо на  gw7
  li1 neq jfsc 05                               -- ошибка движения 1
    lid 04h 11h sgw3 quit                       -- или ROL,ROR;  1104

  li1 copt neg xor copt                         -- проверка движения 0
  lgw7 rol                                      -- влево  на  gw7
  lgw7 ror                                      -- вправо на  gw7
  neq jfsc 04h                                  -- ошибка движения 0
    lib 04 sgw3 quit                            -- или ROL,ROR;  0004

  lgw7 li1 sub copt sgw7 not jbsc 38            -- счетчик = 0 ?

END tst005;

PROCEDURE tst006;       --       LLW, SLW, LLA, LGA, LPA
--
--      LLW4,SLW4,LLA,LGA,LPA
--       : 14h,15h,24h,34h,0F1
--
CODE
  lga 02 inc1                      -- увеличить номер теста
  lgw2 06h neq jfsc 05h            -- нарушена последовательность тестов
   lid 01h 06h sgw3 quit           -- занесение причины,  0601

  getm copt sgw7 3 bic setm                     -- disable ipts
  activ sgw0A lga 0Ah copt tra                  -- update registers
  activ  lsw1 sgw8                              -- global 8 = L
  activ  lsw0 sgw9                              -- global 9 = G
  lgw7 setm                                     -- restore mask

  lla  00h lgw8 equ jfsc 26                     -- проверка LLA 00
  lla 0FFh lgw8 lsa 0FFh equ jfsc 18            -- проверка LLA FF
  lla 055h lgw8 lsa 055h equ jfsc 10            -- проверка LLA 55
  lla 0AAh lgw8 lsa 0AAh equ jfsc 02            -- проверка LLA AA
  jfs 04
   lib 14h sgw3 quit

  lga  00h lgw9 equ jfsc 26                     -- проверка LGA 00
  lga 0FFh lgw9 lsa 0FFh equ jfsc 18            -- проверка LGA FF
  lga 055h lgw9 lsa 055h equ jfsc 10            -- проверка LGA 55
  lga 0AAh lgw9 lsa 0AAh equ jfsc 02            -- проверка LGA AA
  jfs 04
   lib 15h sgw3 quit

  lpa  00h lgw8 li1 sub equ jfsc 29             -- проверка LPA 00
  lpa 0FEh lgw8 lib 0FFh sub equ jfsc 20        -- проверка LPA FF
  lpa 054h lgw8 lib 055h sub equ jfsc 11        -- проверка LPA 55
  lpa 0A9h lgw8 lib 0AAh sub equ jfsc 02        -- проверка LPA AA
  jfs 04
   lib 0F1h sgw3 quit

  li1 neg sgw0D         -- globals  D,E,F = FFFFFFFF, 55555555, AAAAAAAA

  lgw0F slw4 lla 04 lsw0 lgw0F equ jfsc 29
  lgw0E slw4 lla 04 lsw0 lgw0E equ jfsc 20
  lgw0D slw4 lla 04 lsw0 lgw0D equ jfsc 11
  li0   slw4 lla 04 lsw0 li0   equ jfsc 02
  jfs 04
   lib 34h sgw3 quit

  lla 04 lgw0F ssw0 llw4 lgw0F equ jfsc 29
  lla 04 lgw0E ssw0 llw4 lgw0E equ jfsc 20
  lla 04 lgw0D ssw0 llw4 lgw0D equ jfsc 11
  lla 04  li0  ssw0 llw4  li0  equ jfsc 02
  jfs 04
   lib 24h sgw3 quit
END tst006;

PROCEDURE tst007;       --        LLW4..F,SLW4..F
--
--       каждое локальное слово пишется и читается образец с установленным
--      в позиции равной номеру локального слова битом.
--       : 3Fh
--
CODE
  lga 02 inc1                      -- увеличить номер теста
  lgw2 07h neq jfsc 05h            -- нарушена последовательность тестов
   lid 01h 07h sgw3 quit           -- занесение причины,  0701

  lid 10h 00h slw4
  lid 20h 00h slw5
  lid 30h 00h slw6
  lid 40h 00h slw7
  lid 80h 00h slw8
  lid 00h 01h slw9
  lid 00h 02h slw0A
  lid 00h 04h slw0B
  lid 00h 08h slw0C
  lid 00h 10h slw0D
  lid 00h 20h slw0E
  lid 00h 40h slw0F

  lid 10h 00h llw4   equ jfsc 79
  lid 20h 00h llw5   equ jfsc 72
  lid 30h 00h llw6   equ jfsc 65
  lid 40h 00h llw7   equ jfsc 58
  lid 80h 00h llw8   equ jfsc 51
  lid 00h 01h llw9   equ jfsc 44
  lid 00h 02h llw0A  equ jfsc 37
  lid 00h 04h llw0B  equ jfsc 30
  lid 00h 08h llw0C  equ jfsc 23
  lid 00h 10h llw0D  equ jfsc 16
  lid 00h 20h llw0E  equ jfsc 09
  lid 00h 40h llw0F  equ jfsc 02
  jfs 04
   lib 3Fh sgw3 quit
END tst007;

PROCEDURE tst008;       --        LLW 10..FF,SLW 10..FF
--
--       некоторые локальное слово пишется и читается
--      номер этого локального слова.
--       : 30h
--
CODE
  lga 02 inc1                      -- увеличить номер теста
  lgw2 08h neq jfsc 05h            -- нарушена последовательность тестов
   lid 01h 08h sgw3 quit           -- занесение причины,  0801

  lib  10h slw  10h
  lib  20h slw  20h
  lib  40h slw  40h
  lib  80h slw  80h
  lib 0F0h slw 0F0h
  lib 0F1h slw 0F1h
  lib 0F2h slw 0F2h
  lib 0F4h slw 0F4h
  lib 0F8h slw 0F8h
  lib 0FFh slw 0FFh

  lib  10h llw  10h  equ jfsc 65
  lib  20h llw  20h  equ jfsc 58
  lib  40h llw  40h  equ jfsc 51
  lib  80h llw  80h  equ jfsc 44
  lib 0F0h llw 0F0h  equ jfsc 37
  lib 0F1h llw 0F1h  equ jfsc 30
  lib 0F2h llw 0F2h  equ jfsc 23
  lib 0F4h llw 0F4h  equ jfsc 16
  lib 0F8h llw 0F8h  equ jfsc 09
  lib 0FFh llw 0FFh  equ jfsc 02
  jfs 04
   lib 30h sgw3 quit
END tst008;

PROCEDURE tst009;       --       LGW2, SGW2,
--
--      LGW8,SGW8
--       : 14h,15h,24h,34h
--
CODE
  lga 02 inc1                      -- увеличить номер теста
  lgw2 09h neq jfsc 05h            -- нарушена последовательность тестов
   lid 01h 09h sgw3 quit           -- занесение причины,  0901

  li1 neg sgw0D         -- globals  D,E,F = FFFFFFFF, 55555555, AAAAAAAA

  lgw0F sgw8 lga 08 lsw0 lgw0F equ jfsc 29
  lgw0E sgw8 lga 08 lsw0 lgw0E equ jfsc 20
  lgw0D sgw8 lga 08 lsw0 lgw0D equ jfsc 11
  li0   sgw8 lga 08 lsw0 li0   equ jfsc 02
  jfs 04
   lib 52h sgw3 quit

  lga 08 lgw0F ssw0 lgw8 lgw0F equ jfsc 29
  lga 08 lgw0E ssw0 lgw8 lgw0E equ jfsc 20
  lga 08 lgw0D ssw0 lgw8 lgw0D equ jfsc 11
  lga 08  li0  ssw0 lgw8  li0  equ jfsc 02
  jfs 04
   lib 42h sgw3 quit

  lgw0F lgw0E or lgw0D neq jfsc 04
   lib 42h sgw3 quit
END tst009;


PROCEDURE tst00A;       --        LGW4..F,SGW4..F
--
--       каждое глобальное слово пишется и читается образец с установленным
--      в позиции равной номеру локального слова битом.
--       : 5Fh
--
CODE
  lga 02 inc1                      -- увеличить номер теста
  lgw2 0Ah neq jfsc 05h            -- нарушена последовательность тестов
   lid 01h 0Ah sgw3 quit           -- занесение причины,  0A01

  lgw0F slw0F                           -- сохранение значений глобалов
  lgw0E slw0E                           -- 2,4,E,F
  lgw2  slw4
  lgw4  slw6

  lid 04h 00h sgw2
  lid 08h 00h sgw3
  lid 10h 00h sgw4
  lid 20h 00h sgw5
  lid 30h 00h sgw6
  lid 40h 00h sgw7
  lid 80h 00h sgw8
  lid 00h 01h sgw9
  lid 00h 02h sgw0A
  lid 00h 04h sgw0B
  lid 00h 08h sgw0C
  lid 00h 10h sgw0D
  lid 00h 20h sgw0E
  lid 00h 40h sgw0F

  lid 04h 00h lgw2   equ jfsc 93
  lid 08h 00h lgw3   equ jfsc 86
  lid 10h 00h lgw4   equ jfsc 79
  lid 20h 00h lgw5   equ jfsc 72
  lid 30h 00h lgw6   equ jfsc 65
  lid 40h 00h lgw7   equ jfsc 58
  lid 80h 00h lgw8   equ jfsc 51
  lid 00h 01h lgw9   equ jfsc 44
  lid 00h 02h lgw0A  equ jfsc 37
  lid 00h 04h lgw0B  equ jfsc 30
  lid 00h 08h lgw0C  equ jfsc 23
  lid 00h 10h lgw0D  equ jfsc 16
  lid 00h 20h lgw0E  equ jfsc 09
  lid 00h 40h lgw0F  equ jfsc 02
  jfs 04
   lib 3Fh sgw3 quit

  llw0F sgw0F                           -- востановление значений глобалов
  llw0E sgw0E                           -- 2,4,E,F
  llw4  sgw2
  llw6  sgw4
END tst00A;

PROCEDURE tst00B;       --        LGW 10..FF,SGW 10..FF
--
--       некоторые глобальное слово пишется и читается
--      номер этого глобального слова.
--       : 31h
--
CODE
  lga 02 inc1                      -- увеличить номер теста
  lgw2 0Bh neq jfsc 05h            -- нарушена последовательность тестов
   lid 01h 0Bh sgw3 quit           -- занесение причины,  0B01

  lib  10h sgw  10h
  lib  20h sgw  20h
  lib  40h sgw  40h
  lib  80h sgw  80h
  lib 081h sgw 081h
  lib 082h sgw 082h
  lib 084h sgw 084h
  lib 088h sgw 088h
  lib 08Fh sgw 08Fh

  lib  10h lgw  10h  equ jfsc 65
  lib  20h lgw  20h  equ jfsc 58
  lib  40h lgw  40h  equ jfsc 51
  lib  80h lgw  80h  equ jfsc 44
  lib 081h lgw 081h  equ jfsc 30
  lib 082h lgw 082h  equ jfsc 23
  lib 084h lgw 084h  equ jfsc 16
  lib 088h lgw 088h  equ jfsc 09
  lib 08Fh lgw 08Fh  equ jfsc 02
  jfs 04
   lib 30h sgw3 quit
END tst00B;

PROCEDURE tst00C;       --  ALLOC,DECS,CL01..CL0F,RTN
--
--       : 0C8,0B0,0CA,0D0,0D1..0DF
--
CODE
  lga 02 inc1                      -- увеличить номер теста
  lgw2 0Ch neq jfsc 05h            -- нарушена последовательность тестов
   lid 01h 0Ch sgw3 quit           -- занесение причины,  0C01

  getm copt sgw7 3 bic setm                     -- disable ipts
  activ sgw0A lga 0Ah copt tra                  -- update registers
  activ lsw4 li1 sub sgw9                       -- global 9 = S
  lgw7 setm                                     -- restore mask

  0 alloc lgw9 neq jfsc 04                      -- 0 ALLOC должен выдавть S
   lib 0C8h sgw3 quit
  li0F alloc lgw9 neq jfsc 04                   -- 0F ALLOC должен выдавть S
   lib 0C8h sgw3 quit
  0 alloc lgw9 li0F add neq jfsc 04             -- и увеличивать S на 0F
   lib 0C8h sgw3 quit
  li0F decs 0 alloc lgw9 neq jfsc 04            -- 0F DECS должен уменьшать S
   lib 0B0h sgw3 quit                           -- на 0F

  lgw9 sgw8         -- L регистр вызванной процедуры равен S регистру текущей
  lgw9 li4 add sgw9 -- S регистр вызванной процедуры L+4

  cl1  li1 neq jfsc 04                          -- не вызвали, или промахнулись
   lib 0D1h sgw3 quit

  0 alloc lgw8 neq jfsc 05 lid 0CAh 1 sgw3 quit -- RTN не востановил S

  cl2  li2 neq jfsc 04                          -- не вызвали, или промахнулись
   lib 0D2h sgw3 quit

  cl3  li3 neq jfsc 04                          -- не вызвали, или промахнулись
   lib 0D3h sgw3 quit

  cl4  li4 neq jfsc 04                          -- не вызвали, или промахнулись
   lib 0D4h sgw3 quit

  cl5  li5 neq jfsc 04                          -- не вызвали, или промахнулись
   lib 0D5h sgw3 quit

  cl6  li6 neq jfsc 04                          -- не вызвали, или промахнулись
   lib 0D6h sgw3 quit

  cl7  li7 neq jfsc 04                          -- не вызвали, или промахнулись
   lib 0D7h sgw3 quit

  cl8  li8 neq jfsc 04                          -- не вызвали, или промахнулись
   lib 0D8h sgw3 quit

  cl9  li9 neq jfsc 04                          -- не вызвали, или промахнулись
   lib 0D9h sgw3 quit

  cl0A li0A neq jfsc 04                         -- не вызвали, или промахнулись
   lib 0DAh sgw3 quit

  cl0B li0B neq jfsc 04                         -- не вызвали, или промахнулись
   lib 0DBh sgw3 quit

  cl0C li0C neq jfsc 04                         -- не вызвали, или промахнулись
   lib 0DCh sgw3 quit

  cl0D li0D neq jfsc 04                         -- не вызвали, или промахнулись
   lib 0DDh sgw3 quit

  cl0E li0E neq jfsc 04                         -- не вызвали, или промахнулись
   lib 0DEh sgw3 quit

  cl0F li0F neq jfsc 04                         -- не вызвали, или промахнулись
   lib 0DFh sgw3 quit

END tst00C;

PROCEDURE tst00D;       --  CL
--
--       : 0CF
--
CODE
  lga 02 inc1                      -- увеличить номер теста
  lgw2 0Dh neq jfsc 05h            -- нарушена последовательность тестов
   lid 01h 0Dh sgw3 quit           -- занесение причины,  0D01

  0 alloc sgw8      -- L регистр вызванной процедуры равен S регистру текущей
  lgw8 li4 add sgw9 -- S регистр вызванной процедуры L+4

  cl 01h li1 neq jfsc 05                        -- не вызвали, или промахнулись
   lid 0CFh 01h sgw3 quit

  cl 02h li2 neq jfsc 05                        -- не вызвали, или промахнулись
   lid 0CFh 02h sgw3 quit

  cl 03h li3 neq jfsc 05                        -- не вызвали, или промахнулись
   lid 0CFh 03h sgw3 quit

  cl 04h li4 neq jfsc 05                        -- не вызвали, или промахнулись
   lid 0CFh 04h sgw3 quit

  cl 05h li5 neq jfsc 05                        -- не вызвали, или промахнулись
   lid 0CFh 05h  sgw3 quit

  cl 06h li6 neq jfsc 05                        -- не вызвали, или промахнулись
   lid 0CFh 06h  sgw3 quit

  cl 07h li7 neq jfsc 05                        -- не вызвали, или промахнулись
   lid 0CFh 07h  sgw3 quit

  cl 08h li8 neq jfsc 05                        -- не вызвали, или промахнулись
   lid 0CFh 08h  sgw3 quit

  cl 09h li9 neq jfsc 05                        -- не вызвали, или промахнулись
   lid 0CFh 09h  sgw3 quit

  cl 0Ah li0A neq jfsc 05                       -- не вызвали, или промахнулись
   lid 0CFh 0Ah   sgw3 quit

  cl 0Bh li0B neq jfsc 05                       -- не вызвали, или промахнулись
   lid 0CFh 0Bh   sgw3 quit

  cl 0Ch li0C neq jfsc 05                       -- не вызвали, или промахнулись
   lid 0CFh 0Ch   sgw3 quit

  cl 0Dh li0D neq jfsc 05                       -- не вызвали, или промахнулись
   lid 0CFh 0Dh   sgw3 quit

  cl 0Eh li0E neq jfsc 05                       -- не вызвали, или промахнулись
   lid 0CFh 0Eh   sgw3 quit

  cl 0Fh li0F neq jfsc 05                       -- не вызвали, или промахнулись
   lid 0CFh 0Fh   sgw3 quit

  cl 10h lib 10h neq jfsc 05                    -- не вызвали, или промахнулись
   lib 0CFh 10h  sgw3 quit

  0 alloc lgw8 neq jfsc 05 lid 0CAh 2 sgw3 quit -- RTN не востановил S

END tst00D;

PROCEDURE tst00E;       --  STOT, LODT, SWAP, STORE, LODFV
--
--       : B2h
--
CODE
  lga 02 inc1                      -- увеличить номер теста
  lgw2 0Eh neq jfsc 05h            -- нарушена последовательность тестов
   lid 01h 0Eh sgw3 quit           -- занесение причины,  0E01


  lgw0F stot lodt
  lgw0F neq jfsc 05                -- не работают STOT или LODT
   lid 0E8h 0E9h sgw3 quit         -- занесение причины,  E9E8

  lgw0E stot lodt
  lgw0E neq jfsc 05                -- не работают STOT или LODT
   lid 0E8h 0E9h sgw3 quit         -- занесение причины,  E9E8

  lgw0E lgw0F swap
  lgw0E neq jfsc 05                --  работает SWAP
    lid 01h 0F0h sgw3 quit         -- занесение причины,  01F0
  lgw0F neq jfsc 05                --  работает SWAP
    lid 02h 0F0h sgw3 quit         -- занесение причины,  02F0

  lgw0F sgw8                                    --  global word F = 55555555

  li8 li2 rol sgw7                              --  счетчик циклов

  lgw8                                          --  размножим образец на
  copt copt copt copt copt                      --  стеке 6 раз
  store lgw8 lodfv
  or   or   or   or   or  or                    --  и сложим с собой
  lgw8 neq jfsc 05                              --  лишние биты 1 в стеке!
    lid 0B2h 01h sgw3 quit                       --   01B2

  lgw8                                          --  размножим образец на
  copt copt copt copt copt                      --  стеке 7 раз
  store lgw8 lodfv
  and  and  and  and  and  and                  --  и умножим на себя
  lgw8 neq jfsc 04h                             --  пропали биты 1 в стеке!
    lib 0B2h sgw3 quit                          --   00B2

  lgw8 li1 neg xor sgw8                         --  55555555 -> AAAAAAAA

  lgw7 li1 sub copt sgw7 not jbsc 3Ch           -- счетчик = 0 ?

END tst00E;

PROCEDURE tst00F;       --   STORE,LODFV
--
--       каждое слово стека пишутся номера его полуслов.
--      осле записи всех номеров STORE и LODFV
--      стек читается и номера сравниваются.
--       : B2h
--
--
CODE
  lga 02 inc1                      -- увеличить номер теста
  lgw2 0Fh neq jfsc 05h            -- нарушена последовательность тестов
   lid 01h 0Fh sgw3 quit           -- занесение причины,  0F01

  li8 li2 rol sgw7                              --  счетчик циклов


  liw 01h 00h 02h 00h                           -- 0 и 1 полуслова
  store
  liw 04h 00h 08h 00h                           -- 2 и 3 полуслова
  lodfv store
  liw 10h 00h 20h 00h                           -- 4 и 5 полуслова
  lodfv store
  liw 40h 00h 80h 00h                           -- 6 и 7 полуслова
  lodfv store
  liw 00h 01h 00h 02h                           -- 8 и 9 полуслова
  lodfv store
  liw 00h 04h 00h 08h                           -- A и B полуслова
  lodfv store
  liw 00h 10h 00h 20h                           -- B и C полуслова
  lodfv
        sgw8    sgw9    sgw0A
        sgw0B   sgw0C   sgw0D
  liw 01h 00h 02h 00h equ jfsc 56               -- 0 и 1 полуслова
  liw 00h 10h 00h 20h lgw8  equ jfsc 47         -- B и C полуслова
  liw 00h 04h 00h 08h lgw9  equ jfsc 38         -- A и B полуслова
  liw 00h 01h 00h 02h lgw0A equ jfsc 29         -- 8 и 9 полуслова
  liw 40h 00h 80h 00h lgw0B equ jfsc 20         -- 6 и 7 полуслова
  liw 10h 00h 20h 00h lgw0C equ jfsc 11         -- 4 и 5 полуслова
  liw 04h 00h 08h 00h lgw0D equ jfsc 02         -- 2 и 3 полуслова
  jfs 05h
    lid 0B2h 02 sgw3 quit                       --  02B2

  lgw7 li1 sub copt sgw7 not jbsc 130           -- счетчик = 0 ?

END tst00F;

PROCEDURE tst010;       --   STOFV,LODFV
--
--       каждое слово стека пишутся номера его полуслов.
--      осле записи всех номеров STOFV и LODFV
--      стек читается и номера сравниваются.
--       : B4h
--
--
CODE
  lga 02 inc1                      -- увеличить номер теста
  lgw2 lib 10h neq jfsc 05h        -- нарушена последовательность тестов
   lid 01h 10h sgw3 quit           -- занесение причины,  1001

  liw 01h 00h 02h 00h                           -- 0 и 1 полуслова
  liw 04h 00h 08h 00h                           -- 2 и 3 полуслова
  liw 10h 00h 20h 00h                           -- 4 и 5 полуслова
  liw 40h 00h 80h 00h                           -- 6 и 7 полуслова
  liw 00h 01h 00h 02h                           -- 8 и 9 полуслова
  liw 00h 04h 00h 08h                           -- A и B полуслова
  liw 00h 10h 00h 20h                           -- B и C полуслова
  stofv lodt lodfv
        sgw8    sgw9    sgw0A
        sgw0B   sgw0C   sgw0D
  liw 01h 00h 02h 00h equ jfsc 56               -- 0 и 1 полуслова
  liw 04h 00h 08h 00h lgw0D equ jfsc 47         -- B и C полуслова
  liw 10h 00h 20h 00h lgw0C equ jfsc 38         -- A и B полуслова
  liw 40h 00h 80h 00h lgw0B equ jfsc 29         -- 8 и 9 полуслова
  liw 00h 01h 00h 02h lgw0A equ jfsc 20         -- 6 и 7 полуслова
  liw 00h 04h 00h 08h lgw9  equ jfsc 11         -- 4 и 5 полуслова
  liw 00h 10h 00h 20h lgw8  equ jfsc 02         -- 2 и 3 полуслова
  jfs 05h
    lid 0B4h 00 sgw3 quit                       --  00B4

END tst010;


PROCEDURE enableQUIT;
CODE getm 4 or setm END enableQUIT;

BEGIN
  IF Debug THEN enableQUIT END;

  enterTEST0;
  w04:=1; w04:=w04<<4;
  REPEAT
    tst000; check4C;
    tst001; check4C;
    tst002; check4C;
    tst003; check4C;
    tst004; check4C;
    tst005; check4C;
    tst006; check4C;
    tst007; check4C;
    tst008; check4C;
    tst009; check4C;
    tst00A; check4C;
    tst00B; check4C;
    tst00C; check4C;
    tst00D; check4C;
    tst00E; check4C;
    tst00F; check4C;
    tst010; check4C;
    w04:=w04-1;
  UNTIL w04=0;

END TEST0;

----------------------------------------------------------------

MODULE TEST1;

IMPORT CPU, w02, w03, w04, check4C, STOP;
FROM defCodes  IMPORT  li0, llw, lxb, lsw0, li1, lgw, lxw, lsw1,
                        li2, lew, lgw2, lsw2, li3, lsw, lgw3, lsw3,
                        li4, llw4, lgw4, lsw4, li5, llw5, lgw5, lsw5,
                        li6, llw6, lgw6, lsw6, li7, llw7, lgw7, lsw7,
                        li8, llw8, lgw8, lsw8, li9, llw9, lgw9, lsw9,
                        li0A, llw0A, lgw0A, lsw0A, li0B, llw0B, lgw0B, lsw0B,
                        li0C, llw0C, lgw0C, lsw0C, li0D, llw0D, lgw0D, lsw0D,
                        li0E, llw0E, lgw0E, lsw0E, li0F, llw0F, lgw0F, lsw0F,
                        lib, slw, sxb, ssw0, lid, sgw, sxw, ssw1,
                        liw, sew, sgw2, ssw2, lin, ssw, sgw3, ssw3,
                        lla, slw4, sgw4, ssw4, lga, slw5, sgw5, ssw5,
                        lsa, slw6, sgw6, ssw6, lea, slw7, sgw7, ssw7,
                        jflc, slw8, sgw8, ssw8, jfl, slw9, sgw9, ssw9,
                        jfsc, slw0A, sgw0A, ssw0A, jfs, slw0B, sgw0B, ssw0B,
                        jblc, slw0C, sgw0C, ssw0C, jbl, slw0D, sgw0D, ssw0D,
                        jbsc, slw0E, sgw0E, ssw0E, jbs, slw0F, sgw0F, ssw0F,
                        reset, lss, move, incl, quit, leq, chknil, excl,
                        getm, gtr, lsta,      setm, geq, comp,
                        trap, equ, gb, inc1, tra, neq, gb1, dec1,
                        tr, abs, chk, inc, idle, neg, chkz, dec,
                        add, or, alloc, stot, sub, and, entr, lodt,
                        mul, xor, rtn, lxa, div, bic, nop, lpc,
                        shl, in, cx, bbu, shr, bit, ci, bbp,
                        rol, not, cf, bblt, ror, mod, cl,
                        io0, decs, cl0, swap, io1, drop, cl1, lpa,
                        io2, lodfv, cl2, lpw, io3, store, cl3, spw,
                        io4, stofv, cl4, sswu, copt, cl5, rchk,
                        cpcop, cl6, rchkz, pcop, cl7, cm,
                        fadd, for1, cl8, fsub, for2, cl9, bmg,
                        fmul, entc, cl0A, activ, fdiv, xit, cl0B, usr,
                        fcmp, addpc, cl0C, sys, fabs, jump, cl0D, nii,
                        fneg, orjp, cl0E, dot, ffct, andjp, cl0F, invld;
FROM SYSTEM     IMPORT  ADDRESS, ADR;


PROCEDURE tst100;
BEGIN
  w02:=100h;
  w03:=0;
END tst100;

PROCEDURE tst101;  --    LXB, SXB, LXW, SXW
--
--       : 40,41
--
  VAR i: CHAR;
      b: ARRAY [0c..MAX(CHAR)] OF CHAR;
      w: ARRAY [0c..MAX(CHAR)] OF INTEGER;
BEGIN
  INC(w02);                             -- нарушение последовательности
  IF w02#101h THEN STOP(10101h) END;    -- тестов.  10101

  i:=HIGH(b);
  WHILE (i>=0c) DO b[i]:=i; DEC(i) END;
  i:=HIGH(b);
  WHILE (i>=0c) DO
    IF b[i]#i THEN STOP(40h) END;       -- не работают LXB или SXB
    DEC(i);                             --  40
  END;

  i:=HIGH(w);
  WHILE (i>=0c) DO w[i]:=i<<(ORD(i) MOD 32); DEC(i) END;
  i:=HIGH(w);
  WHILE (i>=0c) DO
    IF w[i]#i<<(ORD(i) MOD 32) THEN     -- не работают LXW или SXW
      STOP(41h)                         --  41
    END;
    DEC(i);
  END;

END tst101;

PROCEDURE tst102;       --  OR,AND,XOR,BIC
--
--       : A8,A9,AA,AB
--
  VAR s0,s1,s2,s3: BITSET;
BEGIN
  INC(w02);                             -- нарушение последовательности
  IF w02#102h THEN STOP(10201h) END;    -- тестов.  10201

  s0:={};
  s1:={00,02,04,06,08,10,12,14,16,18,20,22,24,26,28,30};   -- 55555555
  s2:={01,03,05,07,09,11,13,15,17,19,21,23,25,27,29,31};   -- AAAAAAAA
  s3:={0..31};

--   OR код 0A8

  IF s0+s0#s0 THEN STOP(0000000A8h) END;
  IF s0+s1#s1 THEN STOP(0005555A8h) END;
  IF s0+s2#s2 THEN STOP(000AAAAA8h) END;
  IF s0+s3#s3 THEN STOP(000FFFFA8h) END;

  IF s1+s0#s1 THEN STOP(0550055A8h) END;
  IF s1+s1#s1 THEN STOP(0555555A8h) END;
  IF s1+s2#s3 THEN STOP(055AAFFA8h) END;
  IF s1+s3#s3 THEN STOP(055FFFFA8h) END;

  IF s2+s0#s2 THEN STOP(0AA00AAA8h) END;
  IF s2+s1#s3 THEN STOP(0AA55FFA8h) END;
  IF s2+s2#s2 THEN STOP(0AAAAAAA8h) END;
  IF s2+s3#s3 THEN STOP(0AAFFFFA8h) END;

  IF s3+s0#s3 THEN STOP(0FF00FFA8h) END;
  IF s3+s1#s3 THEN STOP(0FF55FFA8h) END;
  IF s3+s2#s3 THEN STOP(0FFAAFFA8h) END;
  IF s3+s3#s3 THEN STOP(0FFFFFFA8h) END;

--  AND код 0A9

  IF s0*s0#s0 THEN STOP(0000000A9h) END;
  IF s0*s1#s0 THEN STOP(0005500A9h) END;
  IF s0*s2#s0 THEN STOP(000AA00A9h) END;
  IF s0*s3#s0 THEN STOP(000FF00A9h) END;

  IF s1*s0#s0 THEN STOP(0550000A9h) END;
  IF s1*s1#s1 THEN STOP(0555555A9h) END;
  IF s1*s2#s0 THEN STOP(055AA00A9h) END;
  IF s1*s3#s1 THEN STOP(055FF55A9h) END;

  IF s2*s0#s0 THEN STOP(0AA0000A9h) END;
  IF s2*s1#s0 THEN STOP(0AA5500A9h) END;
  IF s2*s2#s2 THEN STOP(0AAAAAAA9h) END;
  IF s2*s3#s2 THEN STOP(0AAFFAAA9h) END;

  IF s3*s0#s0 THEN STOP(0FF0000A9h) END;
  IF s3*s1#s1 THEN STOP(0FF5555A9h) END;
  IF s3*s2#s2 THEN STOP(0FFAAAAA9h) END;
  IF s3*s3#s3 THEN STOP(0FFFFFFA9h) END;

--  XOR код 0AA

  IF s0/s0#s0 THEN STOP(0000000AAh) END;
  IF s0/s1#s1 THEN STOP(0005555AAh) END;
  IF s0/s2#s2 THEN STOP(000AAAAAAh) END;
  IF s0/s3#s3 THEN STOP(000FFFFAAh) END;

  IF s1/s0#s1 THEN STOP(0550055AAh) END;
  IF s1/s1#s0 THEN STOP(0555500AAh) END;
  IF s1/s2#s3 THEN STOP(055AAFFAAh) END;
  IF s1/s3#s2 THEN STOP(055FFAAAAh) END;

  IF s2/s0#s2 THEN STOP(0AA00AAAAh) END;
  IF s2/s1#s3 THEN STOP(0AA55FFAAh) END;
  IF s2/s2#s0 THEN STOP(0AAAA00AAh) END;
  IF s2/s3#s1 THEN STOP(0AAFF55AAh) END;

  IF s3/s0#s3 THEN STOP(0FF00FFAAh) END;
  IF s3/s1#s2 THEN STOP(0FF55AAAAh) END;
  IF s3/s2#s1 THEN STOP(0FFAA55AAh) END;
  IF s3/s3#s0 THEN STOP(0FFFF00AAh) END;

--  BIC код 0AB

  IF s0-s0#s0 THEN STOP(0000000ABh) END;
  IF s0-s1#s0 THEN STOP(0005500ABh) END;
  IF s0-s2#s0 THEN STOP(000AA00ABh) END;
  IF s0-s3#s0 THEN STOP(000FF00ABh) END;

  IF s1-s0#s1 THEN STOP(0550055ABh) END;
  IF s1-s1#s0 THEN STOP(0555500ABh) END;
  IF s1-s2#s1 THEN STOP(055AA55ABh) END;
  IF s1-s3#s0 THEN STOP(055FF00ABh) END;

  IF s2-s0#s2 THEN STOP(0AA00AAABh) END;
  IF s2-s1#s2 THEN STOP(0AA55AAABh) END;
  IF s2-s2#s0 THEN STOP(0AAAA00ABh) END;
  IF s2-s3#s0 THEN STOP(0AAFF00ABh) END;

  IF s3-s0#s3 THEN STOP(0FF00FFABh) END;
  IF s3-s1#s2 THEN STOP(0FF55AAABh) END;
  IF s3-s2#s1 THEN STOP(0FFAA55ABh) END;
  IF s3-s3#s0 THEN STOP(0FFFF00ABh) END;

END tst102;

PROCEDURE tst103;  --    ROL, ROR, IN, BIT
--
--       : 8E,8F,AC,AD
--
  VAR shift,i,l,r: INTEGER;
      L0,L1,R0,R1: BITSET;
  CONST all={0..31};
BEGIN
  INC(w02);                             -- нарушение последовательности
  IF w02#103h THEN STOP(10301h) END;    -- тестов.  10301
  shift:=31;
  REPEAT
    L1:={0}; R1:=L1;    L0:=all-{0}; R0:=L0;
    l := 0;  r := 0;
    i:=32;
    REPEAT
      L1:=L1<<shift; l:=l+shift;
      L0:=L0<<shift; IF l>31 THEN l:=l-32 END;
      R1:=R1>>shift; r:=r-shift;
      R0:=R0>>shift; IF r<00 THEN r:=r+32 END;
      IF L1#{l}    THEN STOP(008Eh) END; --  при сдвигах 0 или 1
      IF R1#{r}    THEN STOP(008Fh) END; -- влево или вправо
      IF L0/L1#all THEN STOP(118Eh) END;
      IF R0/R1#all THEN STOP(118Fh) END;
      IF NOT (l IN L1 ) THEN STOP(0ACh) END; -- не работает IN
      IF NOT (l IN {l}) THEN STOP(0ADh) END; -- не работает BIT
      i:=i-1;
    UNTIL i=0;
    shift:=shift-1;
  UNTIL shift=0;
END tst103;

PROCEDURE tst104;  --    LSW, SSW, LSW0..0F, SSW0..0F
--
--       : 23,33,60..6F,70..7F
--
  VAR record:
      RECORD
        f00,f01,f02,f03
       ,f04,f05,f06,f07
       ,f08,f09,f0A,f0B
       ,f0C,f0D,f0E,f0F: INTEGER;
        more: ARRAY [10h..0FFh] OF INTEGER;
      END;

  VAR i: INTEGER;
      p: INTEGER;
      a: ADDRESS;

BEGIN
  INC(w02);                             -- нарушение последовательности
  IF w02#104h THEN STOP(10401h) END;    -- тестов.  10401
  p:=1;
  WITH record DO
    f00:=p; p:=p<<1;    f01:=p; p:=p<<1;
    f02:=p; p:=p<<1;    f03:=p; p:=p<<1;
    f04:=p; p:=p<<1;    f05:=p; p:=p<<1;
    f06:=p; p:=p<<1;    f07:=p; p:=p<<1;
    f08:=p; p:=p<<1;    f09:=p; p:=p<<1;
    f0A:=p; p:=p<<1;    f0B:=p; p:=p<<1;
    f0C:=p; p:=p<<1;    f0D:=p; p:=p<<1;
    f0E:=p; p:=p<<1;    f0F:=p; p:=p<<1;
    more[010h]:=010h;
    more[020h]:=020h;
    more[040h]:=040h;
    more[080h]:=080h;
    more[0F0h]:=0F0h;
    more[0F1h]:=0F1h;
    more[0F2h]:=0F2h;
    more[0F4h]:=0F4h;
    more[0F8h]:=0F8h;
    more[0FFh]:=0FFh;
  END;
  a:=ADR(record);
  i:=0; p:=1;
  REPEAT
    IF a^#p THEN STOP(70h+i) END;
    INC(a); p:=p<<1;
    i:=i+1;
  UNTIL i=10h;
  a:=ADR(record.more[010h]);   IF a^#010h THEN STOP(01033h) END;
  a:=ADR(record.more[020h]);   IF a^#020h THEN STOP(02033h) END;
  a:=ADR(record.more[040h]);   IF a^#040h THEN STOP(04033h) END;
  a:=ADR(record.more[080h]);   IF a^#080h THEN STOP(08033h) END;
  a:=ADR(record.more[0F0h]);   IF a^#0F0h THEN STOP(0F033h) END;
  a:=ADR(record.more[0F1h]);   IF a^#0F1h THEN STOP(0F133h) END;
  a:=ADR(record.more[0F2h]);   IF a^#0F2h THEN STOP(0F233h) END;
  a:=ADR(record.more[0F4h]);   IF a^#0F4h THEN STOP(0F433h) END;
  a:=ADR(record.more[0F8h]);   IF a^#0F8h THEN STOP(0F833h) END;
  a:=ADR(record.more[0FFh]);   IF a^#0FFh THEN STOP(0FF33h) END;
  p:=1;
  WITH record DO
    IF f00#p THEN STOP(60h) END;  p:=p<<1;
    IF f01#p THEN STOP(61h) END;  p:=p<<1;
    IF f02#p THEN STOP(62h) END;  p:=p<<1;
    IF f03#p THEN STOP(63h) END;  p:=p<<1;
    IF f04#p THEN STOP(64h) END;  p:=p<<1;
    IF f05#p THEN STOP(65h) END;  p:=p<<1;
    IF f06#p THEN STOP(66h) END;  p:=p<<1;
    IF f07#p THEN STOP(67h) END;  p:=p<<1;
    IF f08#p THEN STOP(68h) END;  p:=p<<1;
    IF f09#p THEN STOP(69h) END;  p:=p<<1;
    IF f0A#p THEN STOP(6Ah) END;  p:=p<<1;
    IF f0B#p THEN STOP(6Bh) END;  p:=p<<1;
    IF f0C#p THEN STOP(6Ch) END;  p:=p<<1;
    IF f0D#p THEN STOP(6Dh) END;  p:=p<<1;
    IF f0E#p THEN STOP(6Eh) END;  p:=p<<1;
    IF f0F#p THEN STOP(6Fh) END;  p:=p<<1;
    IF more[010h]#010h THEN STOP(01023h) END;
    IF more[020h]#020h THEN STOP(02023h) END;
    IF more[040h]#040h THEN STOP(04023h) END;
    IF more[080h]#080h THEN STOP(08023h) END;
    IF more[0F0h]#0F0h THEN STOP(0F023h) END;
    IF more[0F1h]#0F1h THEN STOP(0F123h) END;
    IF more[0F2h]#0F2h THEN STOP(0F223h) END;
    IF more[0F4h]#0F4h THEN STOP(0F423h) END;
    IF more[0F8h]#0F8h THEN STOP(0F823h) END;
    IF more[0FFh]#0FFh THEN STOP(0FF23h) END;
  END;
END tst104;

PROCEDURE tst105;  --    INC, DEC, INC1, DEC1
--
--       : E4,E5,E6,E7
--
  VAR i,j,n,one: INTEGER; s: BITSET;
BEGIN
  INC(w02);                             -- нарушение последовательности
  IF w02#105h THEN STOP(10501h) END;    -- тестов.  10501
  one:=1;
  -- роверка функционирования INC1
  i:=000000000h; j:=000000000h; INC(i); j:=j+1;
  IF  (i#j)  OR  (i#000000001h) THEN STOP(0111111E4h) END;
  i:=0AAAAAAAAh; j:=0AAAAAAAAh; INC(i); j:=j+1;
  IF  (i#j)  OR  (i#0AAAAAAABh) THEN STOP(0AAAAAAE4h) END;
  i:=055555555h; j:=055555555h; INC(i); j:=j+1;
  IF  (i#j)  OR  (i#055555556h) THEN STOP(0555555E4h) END;

  -- роверка функционирования DEC1
  i:=000000000h; j:=000000000h; DEC(i); j:=j-1;
  IF  (i#j)  OR  (i#0FFFFFFFFh) THEN STOP(0111111E5h) END;
  i:=0AAAAAAAAh; j:=0AAAAAAAAh; DEC(i); j:=j-1;
  IF  (i#j)  OR  (i#0AAAAAAA9h) THEN STOP(0AAAAAAE5h) END;
  i:=055555555h; j:=055555555h; DEC(i); j:=j-1;
  IF  (i#j)  OR  (i#055555554h) THEN STOP(0555555E5h) END;

  --    

  n:=29;
  REPEAT
    s:={}; i:=n;
    REPEAT s:=s+{i}; i:=i-1 UNTIL i<0;  -- s={0..n}

    -- для команд INC1 и ADD:
    i:=INTEGER(s);   j:=i;
    INC(i); j:=j+1;
    IF  (i#j)  OR  (i#INTEGER({n+1})) THEN STOP(0E4h+INTEGER(n<<8)) END;

    -- для команд DEC1 и SUB:
    i:=INTEGER({n+1}); j:=i;
    DEC(i);  j:=j-1;
    IF  (i#j)  OR  (i#INTEGER(s))     THEN STOP(0E5h+INTEGER(n<<8)) END;

    -- для команд INC  и ADD:
    i:=INTEGER(s);   j:=i;
    INC(i,one); j:=j+one;
    IF  (i#j)  OR  (i#INTEGER({n+1})) THEN STOP(0E4h+INTEGER(n<<8)) END;

    -- для команд DEC  и SUB:
    i:=INTEGER({n+1}); j:=i;
    DEC(i,one);  j:=j-one;
    IF  (i#j)  OR  (i#INTEGER(s))     THEN STOP(0E7h+INTEGER(n<<8)) END;

    n:=n-1;
  UNTIL n=0;

END tst105;

PROCEDURE tst106;  --    LSTA
--
--       : E4,E5,E6,E7
--
  PROCEDURE strings(): ADDRESS; CODE lgw 1 END strings;

  PROCEDURE lsta0000(): INTEGER; CODE lsta  00   00h END lsta0000;
  PROCEDURE lsta00AA(): INTEGER; CODE lsta 0AAh  00h END lsta00AA;
  PROCEDURE lsta00FF(): INTEGER; CODE lsta 0FFh  00h END lsta00FF;
  PROCEDURE lsta5500(): INTEGER; CODE lsta 000h 055h END lsta5500;
  PROCEDURE lstaFF00(): INTEGER; CODE lsta 000h 0FFh END lstaFF00;
  PROCEDURE lstaAAAA(): INTEGER; CODE lsta 0AAh 0AAh END lstaAAAA;
  PROCEDURE lstaFFFF(): INTEGER; CODE lsta 0FFh 0FFh END lstaFFFF;

  VAR str: ADDRESS;

BEGIN
  INC(w02);                             -- нарушение последовательности
  IF w02#106h THEN STOP(10601h) END;    -- тестов.  10601
  str:=strings();
  IF lsta0000() # str + 00000h THEN STOP( 0000C2h) END;
  IF lsta00AA() # str + 000AAh THEN STOP( 00AAC2h) END;
  IF lsta00FF() # str + 000FFh THEN STOP( 00FFC2h) END;
  IF lsta5500() # str + 05500h THEN STOP( 5500C2h) END;
  IF lstaFF00() # str + 0FF00h THEN STOP(0FF00C2h) END;
  IF lstaAAAA() # str + 0AAAAh THEN STOP(0AAAAC2h) END;
  IF lstaFFFF() # str + 0FFFFh THEN STOP(0FFFFC2h) END;
END tst106;

BEGIN
  w04:=1; w04:=w04<<4;
  REPEAT
    tst100; check4C;
    tst101; check4C;
    tst102; check4C;
    tst103; check4C;
    tst104; check4C;
    tst105; check4C;
    tst106; check4C;
    w04:=w04-1;
  UNTIL w04=0;
END TEST1;

----------------------------------------------------------------

MODULE Labtam8086; (* Leo 04-Mar-88. (c) KRONOS;   ONLY for KRONOS-P2.5 *)

                IMPORT  CPU, check4C;
FROM SYSTEM     IMPORT  ADDRESS;
FROM defCodes  IMPORT  copt, jbsc, drop;

EXPORT QUALIFIED puts;

CONST NL=36c; CR=15c; LF=12c; SI=17c; SO=16c;


VAR NewLine: BOOLEAN;     EndString: BOOLEAN;
    SISO   : BOOLEAN;     SelectIn : BOOLEAN;


CONST BASE=80000h;
      HALF=10000h;
      trb=93h;                  (* test & reset byte *)


VAR put_in : INTEGER;           MASK16_31: BITSET; (* :={16..31} *)
    put_end: INTEGER;           MASK00_15: BITSET; (* :={00..15} *)
    put_beg: INTEGER;
    put_out: INTEGER;


        BCB: POINTER TO ARRAY [0..0FFFFh] OF CHAR;
       wBCB: POINTER TO ARRAY [0..0FFFFh] OF INTEGER;
     F0000h: POINTER TO ARRAY [0..0FFFFh] OF CHAR;
     inFLAG: INTEGER; (* byte pointers *)
    outFLAG: INTEGER; (* relative 0    *)

PROCEDURE InitBCB(channel: INTEGER);
  VAR adr: ADDRESS;
BEGIN
  adr:=BASE+(0F8010h+channel*4) DIV 4;
  adr:=INTEGER(adr^) MOD HALF;
  ASSERT(adr MOD 4 = 0);
  BCB:=ADDRESS( F0000h ) + adr DIV 4; wBCB:=ADDRESS(BCB);
  outFLAG:=( INTEGER(BCB)*4 )+12;
   inFLAG:=( INTEGER(BCB)*4 )+00;
  NewLine:=TRUE;        EndString:=TRUE;
  SISO   :=TRUE;        SelectIn :=TRUE;

  put_in :=INTEGER(wBCB^[3]>>16) MOD HALF; (* 14,15 bytes BCB *)
  put_end:=INTEGER(wBCB^[5]>>16) MOD HALF; (* 22,23 bytes BCB *)
  put_beg:=        wBCB^[5] MOD HALF;      (* 20,21 bytes BCB *)

END InitBCB;

PROCEDURE wait(flag: INTEGER); CODE copt trb jbsc 04 drop END wait;

PROCEDURE puts(VAL s: ARRAY OF CHAR; VAR Pos,Len: INTEGER);

  VAR pos,len,new: INTEGER; ch,nextch: CHAR;

  PROCEDURE newline(): BOOLEAN;
    VAR next: INTEGER;
  BEGIN
    IF  new=put_end THEN next:=put_beg ELSE next:=new+1 END;
    IF next=put_out THEN RETURN TRUE   END;
    F0000h^[put_in]:=CR; put_in:=new;
    new:=next;           ch:=LF;
    RETURN FALSE
  END newline;

  PROCEDURE SelectInOut(): BOOLEAN;
    VAR next: INTEGER;
  BEGIN
    IF  new=put_end THEN next:=put_beg ELSE next:=new+1 END;
    IF next=put_out THEN RETURN TRUE   END;
    SelectIn:=(ch<=177c);
    IF SelectIn THEN F0000h^[put_in]:=SI ELSE F0000h^[put_in]:=SO END;
    put_in:=new;    new:=next;
    RETURN FALSE
  END SelectInOut;

BEGIN
  pos:=Pos;     len:=Len;
  wait(outFLAG);
    put_out:=INTEGER( BITSET(wBCB^[4])*MASK00_15 );
  BCB^[12]:=1c;
  nextch:=s[pos];
  LOOP
    ch:=nextch;
    IF      len=0          THEN         EXIT END;
    IF EndString & (ch=0c) THEN len:=0; EXIT END;
    IF put_in=put_end      THEN new:=put_beg ELSE new:=put_in+1 END;
    IF new=put_out         THEN         EXIT END;
    IF NewLine & (ch=NL) THEN
      IF newline() THEN EXIT END;
    ELSIF SISO THEN
      IF (ch<=177c)#SelectIn THEN
        IF SelectInOut() THEN EXIT END;
      END;
      ch:=CHAR(BITSET(ch)*{0..6});
    END;
    INC(pos);  DEC(len);        nextch:=s[pos];
    F0000h^[put_in]:=ch;        put_in:=new;
  END;
  Pos:=pos;     Len:=len;
  wait(outFLAG);
  wBCB^[3]:=INTEGER( BITSET(wBCB^[3])-MASK16_31+BITSET(put_in<<16)+{0} );
END puts;

BEGIN
  IF CPU=5 THEN
    MASK16_31:={16..31};
    MASK00_15:={00..15};
    F0000h:=ADDRESS( BASE + 0F0000h DIV 4 );
    InitBCB(9);
    check4C;
  END;
END Labtam8086;

----------------------------------------------------------------

MODULE Output;

                IMPORT  CPU, SETM, GETM, STOP, w02, check4C;
                IMPORT  Labtam8086, consolI6, consolIgd, consolVM, Debug;
FROM SYSTEM     IMPORT  ADDRESS, ADR, WORD;
FROM defCodes  IMPORT  inp, out, drop;
IMPORT  cod: defCodes;

EXPORT WriteString, Show, Write, WriteLn, WriteInt, WriteHex;

------------  Q bus -----------------

CONST csr=177564b DIV 2;
      dtr=177566b DIV 2;

PROCEDURE QIN(reg: INTEGER): BITSET;
CODE inp END QIN;

PROCEDURE QOUT(reg: INTEGER; ch: CHAR);
CODE out END QOUT;

PROCEDURE tstQbus;
  VAR i: INTEGER; ei: BITSET;
BEGIN
  w02:=09091h;
  ei:=GETM();
  SETM(ei-{0,1});
  i:=10000;
  REPEAT DEC(i) UNTIL (i=0) OR ({7}*BITSET(QIN(csr))#{});
  IF i=0 THEN STOP(0190h) END;
  QOUT(csr,CHAR({6}));
  IF {6}*BITSET(QIN(csr))#{6} THEN STOP(0191h) END;
  QOUT(csr,0c);
  IF {6}*BITSET(QIN(csr))={6} THEN STOP(0291h) END;
  i:=10000;
  REPEAT DEC(i) UNTIL (i=0) OR ({7}*BITSET(QIN(csr))#{});
  IF i=0 THEN STOP(0290h) END;
  QOUT(dtr,15c);
  i:=10000;
  REPEAT DEC(i) UNTIL (i=0) OR ({7}*BITSET(QIN(csr))#{});
  IF i=0 THEN STOP(0391h) END;
  check4C;
  SETM(ei);
END tstQbus;

VAR SI: BOOLEAN;

PROCEDURE QUIT; CODE 81h END QUIT;

PROCEDURE DOT(i: INTEGER); CODE 0FEh END DOT;

PROCEDURE writeQ(ch: CHAR);
BEGIN
--DOT(INTEGER(ch)); QUIT;
  IF (ch>=40c) & ((ch<177c)#SI) THEN
    SI:=(ch<177c);
    IF SI THEN writeQ(17c) ELSE writeQ(16c) END;
  END;
  REPEAT UNTIL 7 IN BITSET(QIN(csr));
  QOUT(dtr,ch);
END writeQ;

------------  M bus -----------------

PROCEDURE writeMstr(VAL s: ARRAY OF CHAR);
  VAR i,l: INTEGER;
BEGIN i:=0; l:=HIGH(s)+1;
  REPEAT Labtam8086.puts(s,i,l) UNTIL (l=0);
END writeMstr;

------------  IGD   -----------------

CONST max_l=50; (* maximum screen sizes *)
      max_c=80;

VAR lines: INTEGER;  (* screen sizes *)
  columns: INTEGER;

TYPE line=ARRAY [0..max_c-1] OF INTEGER;
   screen=ARRAY [0..max_l-1] OF line;

VAR wl,wc: INTEGER;  (* write position  *)

CONST rep=0; or=1; xor=2; bic=3;

CONST SYS_FNT_font = ARRAY OF CHAR {
 006c, 007c, 000c, 000c,
 000c, 000c, 000c, 000c, 000c, 000c, 000c,
 000c, 000c, 000c, 000c, 000c, 000c, 000c,
 000c, 000c, 000c, 000c, 000c, 000c, 000c,
 000c, 000c, 000c, 000c, 000c, 000c, 000c,
 000c, 000c, 000c, 000c, 000c, 000c, 000c,
 000c, 000c, 000c, 000c, 000c, 000c, 000c,
 000c, 000c, 000c, 000c, 000c, 000c, 000c,
 000c, 000c, 000c, 000c, 000c, 000c, 000c,
 000c, 000c, 000c, 000c, 000c, 000c, 000c,
 000c, 000c, 000c, 000c, 000c, 000c, 000c,
 000c, 000c, 000c, 000c, 000c, 000c, 000c,
 000c, 000c, 000c, 000c, 000c, 000c, 000c,
 000c, 000c, 000c, 000c, 000c, 000c, 000c,
 000c, 000c, 000c, 000c, 000c, 000c, 000c,
 000c, 000c, 000c, 000c, 000c, 000c, 000c,
 000c, 000c, 000c, 000c, 000c, 000c, 000c,
 000c, 000c, 000c, 000c, 000c, 000c, 000c,
 000c, 000c, 000c, 000c, 000c, 000c, 000c,
 000c, 000c, 000c, 000c, 000c, 000c, 000c,
 000c, 000c, 000c, 000c, 000c, 000c, 000c,
 000c, 000c, 000c, 000c, 000c, 000c, 000c,
 000c, 000c, 000c, 000c, 000c, 000c, 000c,
 000c, 000c, 000c, 000c, 000c, 000c, 000c,
 000c, 000c, 000c, 000c, 000c, 000c, 000c,
 000c, 000c, 000c, 000c, 000c, 000c, 000c,
 000c, 000c, 000c, 000c, 000c, 000c, 000c,
 000c, 000c, 000c, 000c, 000c, 000c, 000c,
 000c, 000c, 000c, 000c, 000c, 000c, 000c,
 000c, 000c, 000c, 000c, 000c, 000c, 000c,
 000c, 000c, 000c, 000c, 000c, 000c, 000c,
 000c, 000c, 000c, 000c, 000c, 000c, 000c,
 000c, 000c, 000c, 000c, 000c, 000c, 000c,
 000c, 000c, 000c, 000c, 000c, 000c, 000c,
 000c, 004c, 004c, 004c, 000c, 004c, 000c,
 000c, 012c, 012c, 000c, 000c, 000c, 000c,
 000c, 012c, 037c, 012c, 037c, 012c, 000c,
 000c, 016c, 005c, 016c, 024c, 016c, 000c,
 000c, 021c, 010c, 004c, 002c, 021c, 000c,
 000c, 002c, 005c, 026c, 011c, 026c, 000c,
 000c, 010c, 004c, 000c, 000c, 000c, 000c,
 000c, 010c, 004c, 004c, 004c, 010c, 000c,
 000c, 002c, 004c, 004c, 004c, 002c, 000c,
 000c, 000c, 012c, 004c, 012c, 000c, 000c,
 000c, 004c, 004c, 037c, 004c, 004c, 000c,
 000c, 000c, 000c, 000c, 000c, 010c, 004c,
 000c, 000c, 000c, 037c, 000c, 000c, 000c,
 000c, 000c, 000c, 000c, 000c, 010c, 010c,
 000c, 020c, 010c, 004c, 002c, 001c, 000c,
 000c, 016c, 021c, 025c, 021c, 016c, 000c,
 000c, 004c, 006c, 004c, 004c, 016c, 000c,
 000c, 017c, 020c, 016c, 001c, 037c, 000c,
 000c, 037c, 020c, 016c, 020c, 017c, 000c,
 000c, 012c, 011c, 037c, 010c, 034c, 000c,
 000c, 037c, 001c, 017c, 020c, 017c, 000c,
 000c, 016c, 001c, 017c, 021c, 016c, 000c,
 000c, 037c, 020c, 010c, 004c, 004c, 000c,
 000c, 016c, 021c, 016c, 021c, 016c, 000c,
 000c, 016c, 021c, 036c, 020c, 016c, 000c,
 000c, 000c, 004c, 000c, 004c, 000c, 000c,
 000c, 000c, 010c, 000c, 010c, 004c, 000c,
 000c, 010c, 004c, 002c, 004c, 010c, 000c,
 000c, 000c, 037c, 000c, 037c, 000c, 000c,
 000c, 002c, 004c, 010c, 004c, 002c, 000c,
 000c, 016c, 021c, 010c, 000c, 004c, 000c,
 000c, 016c, 025c, 035c, 001c, 036c, 000c,
 000c, 016c, 021c, 037c, 021c, 021c, 000c,
 000c, 017c, 021c, 017c, 021c, 017c, 000c,
 000c, 036c, 001c, 001c, 001c, 036c, 000c,
 000c, 017c, 021c, 021c, 021c, 017c, 000c,
 000c, 037c, 001c, 017c, 001c, 037c, 000c,
 000c, 037c, 001c, 017c, 001c, 001c, 000c,
 000c, 016c, 001c, 035c, 021c, 016c, 000c,
 000c, 021c, 021c, 037c, 021c, 021c, 000c,
 000c, 016c, 004c, 004c, 004c, 016c, 000c,
 000c, 034c, 010c, 010c, 012c, 004c, 000c,
 000c, 021c, 011c, 007c, 011c, 021c, 000c,
 000c, 001c, 001c, 001c, 001c, 037c, 000c,
 000c, 021c, 033c, 025c, 021c, 021c, 000c,
 000c, 021c, 023c, 025c, 031c, 021c, 000c,
 000c, 016c, 021c, 021c, 021c, 016c, 000c,
 000c, 017c, 021c, 017c, 001c, 001c, 000c,
 000c, 016c, 021c, 021c, 011c, 026c, 020c,
 000c, 017c, 021c, 017c, 011c, 021c, 000c,
 000c, 016c, 001c, 016c, 020c, 016c, 000c,
 000c, 037c, 004c, 004c, 004c, 004c, 000c,
 000c, 021c, 021c, 021c, 021c, 016c, 000c,
 000c, 021c, 021c, 021c, 012c, 004c, 000c,
 000c, 021c, 021c, 025c, 025c, 012c, 000c,
 000c, 021c, 012c, 004c, 012c, 021c, 000c,
 000c, 021c, 012c, 004c, 004c, 004c, 000c,
 000c, 037c, 010c, 004c, 002c, 037c, 000c,
 000c, 016c, 002c, 002c, 002c, 016c, 000c,
 000c, 001c, 002c, 004c, 010c, 020c, 000c,
 000c, 016c, 010c, 010c, 010c, 016c, 000c,
 000c, 004c, 012c, 021c, 000c, 000c, 000c,
 000c, 000c, 000c, 000c, 000c, 037c, 000c,
 000c, 004c, 010c, 000c, 000c, 000c, 000c,
 000c, 016c, 021c, 037c, 021c, 021c, 000c,
 000c, 017c, 021c, 017c, 021c, 017c, 000c,
 000c, 036c, 001c, 001c, 001c, 036c, 000c,
 000c, 017c, 021c, 021c, 021c, 017c, 000c,
 000c, 037c, 001c, 017c, 001c, 037c, 000c,
 000c, 037c, 001c, 017c, 001c, 001c, 000c,
 000c, 016c, 001c, 035c, 021c, 016c, 000c,
 000c, 021c, 021c, 037c, 021c, 021c, 000c,
 000c, 016c, 004c, 004c, 004c, 016c, 000c,
 000c, 034c, 010c, 010c, 012c, 004c, 000c,
 000c, 021c, 011c, 007c, 011c, 021c, 000c,
 000c, 001c, 001c, 001c, 001c, 037c, 000c,
 000c, 021c, 033c, 025c, 021c, 021c, 000c,
 000c, 021c, 023c, 025c, 031c, 021c, 000c,
 000c, 016c, 021c, 021c, 021c, 016c, 000c,
 000c, 017c, 021c, 017c, 001c, 001c, 000c,
 000c, 016c, 021c, 021c, 011c, 026c, 020c,
 000c, 017c, 021c, 017c, 011c, 021c, 000c,
 000c, 016c, 001c, 016c, 020c, 016c, 000c,
 000c, 037c, 004c, 004c, 004c, 004c, 000c,
 000c, 021c, 021c, 021c, 021c, 016c, 000c,
 000c, 021c, 021c, 021c, 012c, 004c, 000c,
 000c, 021c, 021c, 025c, 025c, 012c, 000c,
 000c, 021c, 012c, 004c, 012c, 021c, 000c,
 000c, 021c, 012c, 004c, 004c, 004c, 000c,
 000c, 037c, 010c, 004c, 002c, 037c, 000c,
 000c, 010c, 004c, 006c, 004c, 010c, 000c,
 000c, 010c, 010c, 010c, 010c, 010c, 000c,
 000c, 002c, 004c, 014c, 004c, 002c, 000c,
 000c, 002c, 025c, 010c, 000c, 000c, 000c,
 077c, 077c, 077c, 077c, 077c, 077c, 077c };

TYPE Font = RECORD
              w,h : INTEGER;
              base: ADDRESS;
            END;

VAR font: Font;

TYPE BMD = RECORD
             w,h: INTEGER;
             wpl: INTEGER;
            base: ADDRESS;
            patt: BITSET;
           END;

VAR bmd, car_bmd: BMD;

VAR  shift: ADDRESS;
    pallet: ADDRESS;
      back: ADDRESS;

---------------------  HARDWARE INTERFACE  ---------------------
                     ----------------------

PROCEDURE bblt(to: ADDRESS;   to_ofs: INTEGER;
             from: ADDRESS; from_ofs: INTEGER; nobits: INTEGER);
CODE cod.bblt END bblt;

PROCEDURE bbltg(mode: INTEGER;
                to: ADDRESS;   to_ofs: INTEGER;
              from: ADDRESS; from_ofs: INTEGER; nobits: INTEGER);
CODE 0F9h 02h END bbltg;

PROCEDURE move(dest,sou: ADDRESS; size: INTEGER);
CODE cod.move END move;

TYPE Line = ARRAY [0..15] OF BITSET;

VAR ln00: Line;

PROCEDURE init_hw;
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO HIGH(ln00) DO ln00[i]:={} END;
END init_hw;

-------------------------------------------------------------

PROCEDURE init_font;
  PROCEDURE const_adr(VAL s: ARRAY OF CHAR): ADDRESS;
  CODE drop END const_adr;

  VAR    i: INTEGER;
         p: POINTER TO ARRAY [0..3] OF CHAR;

BEGIN
  p:=const_adr(SYS_FNT_font);
  font.w:=INTEGER(p^[0]);
  font.h:=INTEGER(p^[1]);
  font.base:=ADDRESS(ADDRESS(p)+1);
END init_font;

-------------------------------------------------------------

VAR
   pos_x: ARRAY [0..max_c-1] OF INTEGER;
   pos_y: ARRAY [0..max_l-1] OF INTEGER;

PROCEDURE show_carret;
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO font.h-1 DO
     bbltg(xor,car_bmd.base+(pos_y[wl]+i)*bmd.wpl,pos_x[wc]
                             ,font.base,font.h*ORD(177c)*8+i*8,font.w);
  END;
END show_carret;

PROCEDURE set_car(l,c: INTEGER);
BEGIN
  IF l<0 THEN l:=0 ELSIF l>=max_l THEN l:=max_l-1 END;
  IF c<0 THEN c:=0 ELSIF c>=max_c THEN c:=max_c-1 END;
  IF (l=wl) & (c=wc) THEN RETURN END;
  wl:=l;  wc:=c;
END set_car;

PROCEDURE erase_eol; FORWARD;

PROCEDURE dl(no: INTEGER);  (* dellete lines *)

  VAR l,y,h,i,n,v: INTEGER;
        adr0,adr1: ADDRESS;

BEGIN
  l:=wl;
  IF no<=0 THEN no:=1 ELSIF no>max_l-l THEN no:=max_l-l END;
  y:=pos_y[l];     h:=(max_l-l)*font.h;
  n:=bmd.wpl;
  adr0:=bmd.base+n*y;
  adr1:=bmd.base+n*(y+font.h*no);
  v:=(max_l-l-no)*font.h*n;
  move(adr0,adr1,v); INC(adr0,v);
  move(adr0,ADR(ln00),n); adr1:=adr0+n;
  move(adr1,adr0,n*(no*font.h-1));
END dl;

PROCEDURE il(no: INTEGER);  (* insert  lines *)

  VAR l,y,h,i,n,v: INTEGER;
        adr0,adr1: ADDRESS;

BEGIN l:=wl;
  IF no<=0 THEN no:=1 ELSIF no>max_l-l THEN no:=max_l-l END;
  n:=bmd.wpl;
  y:=pos_y[l];
  h:=(max_l-l)*font.h;
  adr0:=bmd.base+n*(font.h-1+pos_y[max_l-1]);
  adr1:=bmd.base+n*(font.h-1+pos_y[max_l-1-no]);
  v:=font.h*(max_l-(l+no))-1;
  FOR i:=0 TO v DO move(adr0,adr1,n); DEC(adr0,n); DEC(adr1,n) END;
  v:=font.h*no-1;
  FOR i:=0 TO v DO move(adr0,ADR(ln00),n); DEC(adr0,n) END;
END il;

PROCEDURE scroll_up(no: INTEGER);
  VAR sav_l,sav_c: INTEGER;
BEGIN
  sav_l:=wl; sav_c:=wc;
  set_car(0,0);
  dl(no);
  set_car(sav_l,sav_c);
END scroll_up;

PROCEDURE scroll_dw(no: INTEGER);
  VAR sav_l,sav_c: INTEGER;
BEGIN
  sav_l:=wl; sav_c:=wc;
  set_car(0,0);
  il(no);
  set_car(sav_l,sav_c);
END scroll_dw;

PROCEDURE erase_eos;
  VAR i: INTEGER;
      l: INTEGER;
    v,h: INTEGER;
      y: INTEGER;
    ptr: POINTER TO line;
    adr: ADDRESS;
  space: INTEGER;
BEGIN
  erase_eol;
  l:=wl+1;
  IF l>max_l-1 THEN RETURN END;
  y:=pos_y[l]; h:=(max_l-l)*font.h;
  adr:=bmd.wpl*y+bmd.base;
  v:=(max_l-l)*font.h-1;
  FOR i:=0 TO v DO move(adr,ADR(ln00),bmd.wpl); INC(adr,bmd.wpl) END;
END erase_eos;

PROCEDURE erase_eol;
  VAR i: INTEGER;
    x,y: INTEGER;
   bits: INTEGER;
    adr: ADDRESS;
BEGIN
  x:=pos_x[wc]; y:=pos_y[wl];
  adr:=bmd.wpl*y+bmd.base;  bits:=pos_x[max_c-1]-x+font.w;
  FOR i:=0 TO font.h-1 DO bblt(adr,x,ADR(ln00),0,bits); INC(adr,bmd.wpl) END;
END erase_eol;

----------------------------  INIT  ----------------------------
                            --------

PROCEDURE mark_pos;
  VAR i: INTEGER;
    x,y: INTEGER;
BEGIN
  x:=0; i:=0;
  WHILE (x<bmd.w) & (i<max_c) DO pos_x[i]:=x; INC(i); INC(x,font.w) END;
  columns:=i;
  WHILE i<max_c DO pos_x[i]:=0; INC(i) END;
  y:=0; i:=0;
  WHILE (y<bmd.h) & (i<max_l) DO pos_y[i]:=y; INC(i); INC(y,font.h) END;
  lines:=i;
  WHILE i<max_l DO pos_y[i]:=0; INC(i) END;
END mark_pos;

PROCEDURE init_scr;
  VAR l,c: INTEGER;
BEGIN
  wl:=0; wc:=0;
  show_carret;
END init_scr;

PROCEDURE init_palet;
  VAR adr: ADDRESS;
  VAR i: INTEGER;
BEGIN
  adr:=pallet;
  REPEAT UNTIL BITSET(back^)*{0}={};
  REPEAT UNTIL BITSET(back^)*{0}#{};           (*F0F*)
  FOR i:=0 TO 3 DO adr^:=0FFFh; INC(adr); adr^:=07EFh; INC(adr);
    adr^:=0000h; INC(adr); adr^:=0FFFh; INC(adr);
  END;
END init_palet;

PROCEDURE init_bmd;
  VAR i: INTEGER;
BEGIN
  bmd.w:=480;           bmd.wpl :=16;
  bmd.h:=360;           bmd.base:=1F8000h;
  car_bmd.w:=480;           car_bmd.wpl :=16;
  car_bmd.h:=360;           car_bmd.base:=1F8000h+512*16;
  shift:=ADDRESS(1F0000h);      pallet:=ADDRESS(1F0010h);
  shift^:=0;                      back:=ADDRESS(1F0020h);
  init_palet;
  bmd.base^:=0;
  move(bmd.base+1,bmd.base,128*256-1);
END init_bmd;

PROCEDURE putc(wc,wl: INTEGER; ch: CHAR);
  VAR i: INTEGER;
BEGIN
   ch:=CHAR(BITSET(ch)*{0..6});
   FOR i:=0 TO font.h-1 DO
     bbltg(rep,bmd.base+(pos_y[wl]+i)*bmd.wpl,pos_x[wc]
              ,font.base,(font.h*ORD(ch))*8+i*8,font.w);
   END;
END putc;

PROCEDURE writeIgd(VAL str: ARRAY OF CHAR; len: INTEGER);

  VAR i: INTEGER; c: BOOLEAN;

  PROCEDURE lf;
  BEGIN
    IF wl<max_l-1 THEN set_car(wl+1,wc)
    ELSE scroll_up(1); set_car(wl,wc)
    END;
  END lf;

  VAR ch: CHAR;

BEGIN
  show_carret;
  i:=0;
  IF len>HIGH(str) THEN len:=HIGH(str)+1 END;
  (*$T-$W$Z*)
  WHILE (i<len) & (str[i]#0c) DO ch:=str[i];
    IF ch=15c THEN
      set_car(wl,0);
    ELSIF ch=12c THEN
      lf
    ELSIF ch=36c THEN
      set_car(wl,0); lf
    ELSE
      putc(wc,wl,ch);
      wc:=wc+1;
      IF wc>=max_c THEN set_car(wl,0); lf END;
    END;
    i:=i+1;
  END;
  show_carret;
END writeIgd;

PROCEDURE initIgd;
  VAR i: INTEGER; c: CHAR;
BEGIN
  init_bmd;
  init_hw;
  init_font;
  mark_pos;
  init_scr;
END initIgd;

------------  I6    -----------------

CONST
  x1  = {0};            bits5 = {};             odd     = {4};
  x16 = {1};            bits6 = {2};            even    = {4,5};
  x64 = {0,1};          bits7 = {3};            stop1   = {6};
                        bits8 = {2,3};          stop1_5 = {7};
                                                stop2   = {6,7};
DEFAULT = x16 + stop2 + bits8 ;

(*CONTROL*)
  TxEN    = 0;          -- TRANSMIT ENABLE
  DTRY    = 1;          -- DATA TERMINAL READY (used to enable RECEIVE ipt)
  RxEN    = 2;          -- RECEIVE ENABLE
  SBRK    = 3;          -- SEND BREAK
  ER      = 4;          -- ERROR RESET (not used yet)
  RTS     = 5;          -- READY TO SEND (used to enable TRANSMIT ipt)
  IR      = 6;          -- INTERNAL RESET
  EH      = 7;          -- ENTER HUNT MODE (enable search for Sync characters)

  Ript = DTRY;
  Tipt = RTS;

(*STATUS*)
  DSR     =  7;         -- DATA SEND READY (used to indicate connection)
  SINDET  =  6;         --
  FE      =  5;         -- FRAMING  ERROR (stop bit lost)
  OE      =  4;         -- DATA     OVERRUN
  PE      =  3;         -- PARITY   ERROR
  TxE     =  2;         -- TRANSMIT EMPTY
  RxRDY   =  1;         -- RECEIVE  READY
  TxRDY   =  0;         -- TRANSMIT READY

VAR CSR: POINTER TO BITSET; DTR: POINTER TO CHAR;

PROCEDURE writeI6(Ch: CHAR);
BEGIN
  REPEAT UNTIL TxRDY IN CSR^; DTR^:=Ch;
END writeI6;

-------------------------------------

PROCEDURE writeVM(Ch: CHAR);
CODE
   93h
END writeVM;


PROCEDURE WriteString(VAL s: ARRAY OF CHAR);
  VAR i: INTEGER;
BEGIN
  IF consolIgd THEN
    writeIgd(s,HIGH(s));
  ELSIF consolVM THEN
    i:=0;
    WHILE (i<=HIGH(s)) & (s[i]#0c) DO writeVM(s[i]); INC(i) END;
  ELSIF consolI6 THEN
    i:=0;
    WHILE (i<=HIGH(s)) & (s[i]#0c) DO writeI6(s[i]); INC(i) END;
  ELSIF CPU IN {2,6} THEN
    i:=0;
    WHILE (i<=HIGH(s)) & (s[i]#0c) DO writeQ(s[i]); INC(i) END;
  ELSIF CPU=5 THEN
    writeMstr(s)
  ELSE
  END;
END WriteString;

PROCEDURE Write(ch: CHAR);
  VAR s: ARRAY [0..3] OF CHAR;
BEGIN s[0]:=ch; s[1]:=0c; WriteString(s) END Write;

PROCEDURE WriteLn; BEGIN WriteString("" 15c 12c) END WriteLn;

PROCEDURE Show(VAL s: ARRAY OF CHAR);
BEGIN WriteString(s); WriteLn END Show;

PROCEDURE WriteInt0(i: INTEGER; n: INTEGER);
  VAR d,j: INTEGER;
BEGIN
  d:=i MOD 10; i:=i DIV 10; DEC(n);
  IF i=0 THEN
    WHILE n>0 DO Write(' '); DEC(n) END
  ELSE WriteInt0(i,n)
  END;
  Write(CHAR(ORD('0')+d));
END WriteInt0;

PROCEDURE WriteInt(w: WORD; n: INTEGER);
  VAR i: INTEGER;
BEGIN i:=w;
  IF n<=0 THEN n:=1 END;
  IF i=MIN(INTEGER) THEN WriteString("-2147483648"); RETURN END;
  IF i<0 THEN Write('-'); DEC(n); i:=ABS(i) END;
  WriteInt0(i,n)
END WriteInt;

PROCEDURE WriteHex(w: WORD; n: INTEGER);
  VAR i,d: INTEGER;
BEGIN
  IF n<=0 THEN n:=8 END;
  WHILE n>8 DO Write(' '); n:=n-1 END;
  i:=8-n;
  WHILE i>0 DO w:=w<<4; i:=i-1 END;
  i:=1;
  WHILE i<=n DO w:=w<<4; d:=INTEGER(BITSET(w)*{0..3});
    IF d>9 THEN INC(d,ORD('A')-10) ELSE INC(d,ORD('0')) END;
    Write(CHAR(d)); INC(i);
  END;
END WriteHex;

BEGIN
  IF consolIgd THEN
    initIgd;
  ELSIF consolI6 THEN
    DTR:=ADDRESS(8200F0h);
    CSR:=ADDRESS(DTR)+1;
    IF NOT Debug THEN
      CSR^:={};
      CSR^:={};
      CSR^:={};
      CSR^:={};
      CSR^:={IR};
      CSR^:=DEFAULT;
      CSR^:={TxEN,RxEN};
    END;
  END;
  IF (CPU#5) & NOT consolI6 & NOT consolIgd THEN tstQbus END;
  SI:=TRUE; Write(17c);
  WriteLn;
  Show("               KREST v0.0.0  hacked by Leopold /1988 March 14/");
  WriteLn;
  Show("                                 (c)  KRONOS");
  WriteLn;
  Show("                     ---   preliminary test finished ---");
  WriteLn;
END Output;

----------------------------------------------------------------

MODULE Visir;

                IMPORT  WriteString, Show, WriteHex, WriteLn, CPU;

EXPORT TEST, ERROR, ORDER;

PROCEDURE TEST(n: INTEGER; VAL msg: ARRAY OF CHAR);
BEGIN
  WriteString("TEST "); WriteHex(n,3);
  WriteString(" -- ");  WriteString(msg); WriteLn;
END TEST;

PROCEDURE ERROR(VAL s: ARRAY OF CHAR); BEGIN Show(s) END ERROR;

CONST ORDER="Illegal order of tests";

PROCEDURE delay;
  VAR i: INTEGER;
BEGIN
  IF CPU#2 THEN i:=80000h ELSE i:=20000h END;
  REPEAT i:=i-1 UNTIL i=0;
END delay;

BEGIN
  TEST(001h,"JFLC, JFL, JFSC, JFS, JBLC, JBL, JBSC, JBS, ORJP, ANDJP, NOT");
  TEST(002h,"COPT, DROP, LI0..LI0F, LIB, LID, LIW");
  TEST(003h,"STACK bits");
  TEST(004h,"STACK positions");
  TEST(005h,"STACK top");
  TEST(006h,"LLW4..0F, SLW4..0F, LLA, LGA, LPA");
  TEST(007h,"LLW4..0F, SLW4..0F positions");
  TEST(008h,"LLW 10..FF, SLW 10..FF positions");
  TEST(009h,"LGW2..0F SGW2..0F");
  TEST(00Ah,"LGW2..0F SGW2..0F positions");
  TEST(00Bh,"LGW 10..FF, SGW 10..FF  positions");
  TEST(00Ch,"ALLOC, DECS, CL01..CL0F, RTN");
  TEST(00Dh,"CL");
  TEST(00Eh,"STOT, LODT, SWAP, STORE, LODFV");
  TEST(00Fh,"STORE, LODFV whole stack");
  TEST(010h,"STOFV, LODFV whole stack");
  WriteLn;
  delay;
  TEST(101h,"LXB, SXB, LXW, SXW");
  TEST(102h,"OR, AND, XOR, BIC");
  TEST(103h,"ROL, ROR, IN, BIT");
  TEST(104h,"LSW, SSW, LSW0..0F, SSW0..0F");
  TEST(105h,"INC, DEC, INC1, DEC1");
  TEST(106h,"LSTA");
  WriteLn;
  TEST(0FFFh,"QIN, QOUT");
  WriteLn;
END Visir;

----------------------------------------------------------------

MODULE TEST2;


                IMPORT  CPU, w02, w03, w04, check4C, QUOT;
                IMPORT  TregADR, GETM, SETM, Debug;
                IMPORT  WriteHex, WriteInt, WriteLn, WriteString,
                        Show, Write;
                IMPORT  TEST, ERROR, STOP, ORDER;
                IMPORT  loc1,loc2,loc4,loc8,locF,loc10;
FROM defCodes  IMPORT  li0, llw, lxb, lsw0, li1, lgw, lxw, lsw1,
                        li2, lew, lgw2, lsw2, li3, lsw, lgw3, lsw3,
                        li4, llw4, lgw4, lsw4, li5, llw5, lgw5, lsw5,
                        li6, llw6, lgw6, lsw6, li7, llw7, lgw7, lsw7,
                        li8, llw8, lgw8, lsw8, li9, llw9, lgw9, lsw9,
                        li0A, llw0A, lgw0A, lsw0A, li0B, llw0B, lgw0B, lsw0B,
                        li0C, llw0C, lgw0C, lsw0C, li0D, llw0D, lgw0D, lsw0D,
                        li0E, llw0E, lgw0E, lsw0E, li0F, llw0F, lgw0F, lsw0F,
                        lib, slw, sxb, ssw0, lid, sgw, sxw, ssw1,
                        liw, sew, sgw2, ssw2, lin, ssw, sgw3, ssw3,
                        lla, slw4, sgw4, ssw4, lga, slw5, sgw5, ssw5,
                        lsa, slw6, sgw6, ssw6, lea, slw7, sgw7, ssw7,
                        jflc, slw8, sgw8, ssw8, jfl, slw9, sgw9, ssw9,
                        jfsc, slw0A, sgw0A, ssw0A, jfs, slw0B, sgw0B, ssw0B,
                        jblc, slw0C, sgw0C, ssw0C, jbl, slw0D, sgw0D, ssw0D,
                        jbsc, slw0E, sgw0E, ssw0E, jbs, slw0F, sgw0F, ssw0F,
                        reset, lss, move, incl, quit, leq, chknil, excl,
                        getm, gtr, lsta,      setm, geq, comp,
                        trap, equ, gb, inc1, tra, neq, gb1, dec1,
                        tr, abs, chk, inc, idle, neg, chkz, dec,
                        add, or, alloc, stot, sub, and, entr, lodt,
                        mul, xor, rtn, lxa, div, bic, nop, lpc,
                        shl, in, cx, bbu, shr, bit, ci, bbp,
                        rol, not, cf, bblt, ror, mod, cl,
                        io0, decs, cl0, swap, io1, drop, cl1, lpa,
                        io2, lodfv, cl2, lpw, io3, store, cl3, spw,
                        io4, stofv, cl4, sswu, copt, cl5, rchk,
                        cpcop, cl6, rchkz, pcop, cl7, cm,
                        fadd, for1, cl8, fsub, for2, cl9, bmg,
                        fmul, entc, cl0A, activ, fdiv, xit, cl0B, usr,
                        fcmp, addpc, cl0C, sys, fabs, jump, cl0D, nii,
                        fneg, orjp, cl0E, dot, ffct, andjp, cl0F, invld;
FROM SYSTEM     IMPORT  ADDRESS, ADR;

PROCEDURE tst200;
BEGIN
  w02:=200h;
  w03:=0;
END tst200;

PROCEDURE lla3(): ADDRESS;
CODE lla 03 END lla3;

PROCEDURE returnchangemask(newmask: BITSET);
  VAR w3,w2: ADDRESS;
BEGIN
  w3:=lla3(); w2:=w3-1;
  w3^:=newmask;
  w2^:=BITSET(w2^)+{30};
  RETURN
END returnchangemask;

PROCEDURE testmask;
  VAR m,mn,x: BITSET; w2,w3: ADDRESS;
BEGIN
  m:=GETM();
  w3:=lla3();  w3^:=0;
  w2:=w3-1;
  mn:=m-{1,31}+{30,2};
  IF CPU=2 THEN mn:=mn+BITSET(mn>>16) END;
  SETM(m);
  IF 30 IN BITSET(w2^) THEN
    SETM(mn);   x:=w3^;   SETM(m);
    IF x # m THEN
      ERROR("SETM: Mask not saved in 3-rd local word"); STOP(83h);
    END;
    returnchangemask(mn); x:=GETM(); SETM(m);
    IF (CPU=2) & (x*{0..15,31}#mn*{0..15,31})
    OR (CPU#2) & (x # mn) THEN
      ERROR("RTN: Not restore saved mask"); STOP(83CAh);
    END;
  END;
  SETM(mn); x:=GETM(); SETM(m);
  IF (CPU=2) & (x*{0..15,31}#mn*{0..15,31})
    OR (CPU#2) & (x # mn) THEN
    ERROR("SETM/GETM: Illegal mask"); STOP(8382h);
  END;
END testmask;

PROCEDURE tst201;  --    GETM, SETM
--
--       :  82,83,CA
--
  VAR co: INTEGER;
BEGIN
  TEST(201h,"GETM, SETM"); INC(w02);
  IF w02#201h THEN ERROR(ORDER); STOP(20101h) END;
  IF CPU=2 THEN co:=1024 ELSE co:=4096 END;
  REPEAT
    testmask;
    co:=co-1;
  UNTIL co=0;
END tst201;

PROCEDURE testTreg(VAL msg: ARRAY OF CHAR; cause: INTEGER);
  VAR trap: ADDRESS;
BEGIN
  trap:=TregADR();
  IF trap^#0 THEN
    WriteLn; WriteString(msg); WriteString("Unexcpected interrupt: ");
    WriteHex(trap^,8); WriteLn; STOP(cause); trap^:=0;
  END;
END testTreg;

PROCEDURE tst202;  --    ADD, SUB
--
--       :  88,89
--
  VAR m: BITSET;
   trap: ADDRESS;
    i,j: INTEGER;
    d,n: INTEGER;
     co: INTEGER;
  CONST maxint=MAX(INTEGER);
        minint=MIN(INTEGER);
BEGIN
  TEST(202h,"ADD, SUB"); INC(w02);
  IF w02#202h THEN ERROR(ORDER); STOP(20201h) END;
  trap:=TregADR(); m:=GETM(); trap^:=0;
  IF CPU=2 THEN co:=64 ELSE co:=256 END;
  REPEAT
    SETM(m-{31});
    testTreg("",0BAD3h);
    n:=2;
    WHILE n>0 DO
      IF n MOD 2 # 0 THEN i:= maxint    DIV 2; d:=i-1;
      ELSE                i:=(minint+1) DIV 2; d:=i+1
      END;
      WHILE d>0 DO
        WHILE i>0 DO
          j:=i-(-d-1);
          IF i+d#j-1 THEN
            WriteLn; WriteString("ADD/SUB: ");
            WriteHex(i,8); Write('+');
            WriteHex(d,8); Write('#');
            WriteHex(j,8); Show("-1"); STOP(0188h);
          END;
          i:=i DIV 2;
        END;
        d:=d DIV 2;
      END;
      n:=n-1;
    END;
    testTreg("ADD/SUB point 0: ",8889h);
    i:=maxint;       i:=i+1;
    IF trap^=41h THEN
      trap^:=0;
    ELSIF CPU <> 7 THEN -- 07 does not serve overflow ipt
      WriteLn; WriteString("ADD: Not raised interrupt 41h on overflow: ");
      WriteHex(maxint,8); Show("+00000001");
      STOP(0288h);
    END;
    testTreg("ADD point 3: ",0388h);
    i:=minint;       i:=i-1;
    IF trap^=41h THEN trap^:=0;
    ELSIF CPU <> 7 THEN
      WriteLn; WriteString("SUB: Not raised interrupt 41h on overflow: ");
      WriteHex(minint,8); Show("-00000001");
      STOP(0189h);
    END;
    testTreg("SUB point 2: ",0289h);
    trap^:=0;
    SETM(m);
    co:=co-1;
  UNTIL co=0;
END tst202;

PROCEDURE errorICMP(VAL msg: ARRAY OF CHAR; lo,hi: INTEGER;
                    VAL  op: ARRAY OF CHAR; res: BOOLEAN);
BEGIN
  WriteLn; WriteString(msg);
  WriteHex(lo,8); WriteString(op); WriteHex(hi,8);
  IF res THEN Show("=TRUE") ELSE Show("=FALSE") END;
END errorICMP;

PROCEDURE tstICMP(lo,hi: INTEGER);
  VAR b: BOOLEAN; i,j: INTEGER;
BEGIN j:=0;
  b:=(lo< hi); i:=j;
  IF NOT b THEN errorICMP("LSS:",lo,hi,"<" ,b); STOP(0A0h) END;
  b:=(lo<=hi); i:=j;
  IF NOT b THEN errorICMP("LEQ:",lo,hi,"<=",b); STOP(0A1h) END;
  b:=(lo> hi); i:=j;
  IF     b THEN errorICMP("GTR:",lo,hi,">" ,b); STOP(0A2h) END;
  b:=(lo>=hi); i:=j;
  IF     b THEN errorICMP("GEQ:",lo,hi,">=",b); STOP(0A3h) END;
  b:=(lo =hi); i:=j;
  IF     b THEN errorICMP("EQU:",lo,hi,"=" ,b); STOP(0A4h) END;
  b:=(lo #hi); i:=j;
  IF NOT b THEN errorICMP("NEQ:",lo,hi,"#" ,b); STOP(0A5h) END;
END tstICMP;

PROCEDURE tst203;  --    LSS, LEQ, GTR, GEQ, EQU, NEQ
--
--       :  A0,A1,A2,A3,A4,A5
--
  VAR m: BITSET;
    h,l: INTEGER;
    i,j: INTEGER;
     co: INTEGER;
      b: BOOLEAN;
   trap: ADDRESS;

  CONST maxint=MAX(INTEGER);
        minint=MIN(INTEGER);
BEGIN
  TEST(203h,"LSS, LEQ, GTR, GEQ, EQU, NEQ"); INC(w02);
  IF w02#203h THEN ERROR(ORDER); STOP(20301h) END;
  m:=GETM();
  trap:=TregADR();
  IF CPU=2 THEN co:=5 ELSE co:=20 END;
  REPEAT
    j:=0;
    SETM(m-{31});
    testTreg("",0BAD3h);
    tstICMP( 0,+1);
    tstICMP(-1, 0);
    tstICMP(-1,+1);
    testTreg("LSS..NEQ: ",0A0A5h);
    tstICMP(minint,     0);
    tstICMP(minint,    +1);
    tstICMP(minint,    -1);
    tstICMP(     0,maxint);
    tstICMP(    -1,maxint);
    tstICMP(    +1,maxint);
    tstICMP(minint,maxint);
    testTreg("LSS..NEQ: ",0FFA0A5h);
    h:=maxint;
    WHILE h>0 DO
      l:=minint+1;
      WHILE l<0 DO
        tstICMP(l,h);
        b:=(l=l); i:=j;
        IF NOT b THEN errorICMP("EQU:",l,l,"=",b); STOP(1A4h) END;
        b:=(h#h); i:=j;
        IF     b THEN errorICMP("NEQ:",h,h,"#",b); STOP(1A5h) END;
        l:=-(ABS(l) DIV 2);
      END;
      h:=h DIV 2;
    END;
    trap^:=0;
    co:=co-1;
  UNTIL co=0;
  SETM(m);
END tst203;

PROCEDURE tst204;  --    ABS, NEG
--
--       :  A6,A7
--
  VAR m: BITSET;
   trap: ADDRESS;
  o,i,j: INTEGER;
     co: INTEGER;

  CONST maxint=MAX(INTEGER);
        minint=MIN(INTEGER);
BEGIN
  TEST(204h,"ABS, NEG"); INC(w02);
  IF w02#204h THEN ERROR(ORDER); STOP(20401h) END;
  m:=GETM();
  IF CPU=2 THEN co:=50 ELSE co:=200 END;
  REPEAT
    j:=0;
    trap:=TregADR();
    SETM(m-{31});
    testTreg("",0BAD3h);
    i:=minint+1; o:=0;
    LOOP
      IF ABS(i)#o-i THEN
        WriteLn; WriteString("ABS: ABS("); WriteHex(i,8);
        WriteString(")#00000000-"); WriteHex(i,8); WriteLn;
        STOP(01A6h);
      END;
      IF i=-1 THEN EXIT END;
      i:=INTEGER(BITSET(i)+{0})>>1;
    END;
    testTreg("ABS: ",02A6h);
    i:=maxint;
    LOOP
      IF ABS(i)#i THEN
        WriteLn; WriteString("ABS: ABS("); WriteHex(i,8);
        WriteString(")#"); WriteHex(i,8); WriteLn;
        STOP(03A6h);
      END;
      IF i=0 THEN EXIT END;
      i:=INTEGER(BITSET(i)-{0})>>1;
    END;
    testTreg("ABS: ",04A6h);
    i:=minint; i:=ABS(i);
    IF trap^=41h THEN
      trap^:=0
    ELSIF CPU <> 7 THEN
      WriteLn; WriteString("ABS: Not raised interrupt 41h on overflow: ABS(");
      WriteHex(minint,8); Show(")");
      STOP(05A6h);
    END;
    testTreg("ABS: ",06A6h);
    i:=maxint; o:=0;
    LOOP
      j:=-i;
      IF o-i#j THEN
        WriteLn; WriteString("NEG: -"); WriteHex(i,8);
        WriteString("#00000000-"); WriteHex(i,8); WriteLn;
        STOP(01A7h);
      END;
      j:=-j;
      IF j#i THEN
        WriteLn; WriteString("NEG: -"); WriteHex(j,8);
        WriteString("#"); WriteHex(i,8); WriteLn;
        STOP(02A7h);
      END;
      IF i=0 THEN EXIT END;
      i:=i DIV 2;
    END;
    testTreg("NEG: ",03A7h);
    i:=minint; i:=-i;
    IF trap^=41h THEN trap^:=0
    ELSIF CPU <> 7 THEN
      WriteLn; WriteString("NEG: Not raised interrupt 41h on overflow: -");
      WriteHex(minint,8); WriteLn;
      STOP(04A7h);
    END;
    testTreg("NEG: ",05A7h);
    co:=co-1;
  UNTIL co=0;
  trap^:=0;
  SETM(m);
END tst204;

PROCEDURE tst205;

  CONST err1="Illegal base after LPC";
        err2="Illegal proc.no. after LPC";

        n1  = BITSET(01h);
        n2  = BITSET(02h);
        n4  = BITSET(04h);
        n8  = BITSET(08h);
        nF  = BITSET(0Fh);
        n10 = BITSET(10h);


  VAR p: PROCEDURE(): INTEGER;
     co: INTEGER;
      a: ADDRESS;
    dft: ADDRESS;
   sdft: BITSET;
   save: INTEGER;

  PROCEDURE G(): ADDRESS; CODE lga 00 END G;

BEGIN
  TEST(205h,"LPC"); INC(w02);
  IF w02#205h THEN ERROR(ORDER); STOP(20501h) END;
  (* create DFT for LPC *)
  a:=G()-1;     save:=a^;
  a^:=ADR(dft); dft:=G();

  sdft:=BITSET(ADR(dft));
  IF CPU=2 THEN co:=1024 ELSE co:=4*1024 END;
  REPEAT
    p:=loc1;
    IF BITSET(p)*{0..23}#sdft  THEN ERROR(err1); STOP(001EBh) END;
    IF BITSET(p<<8)*{0..7}#n1  THEN ERROR(err2); STOP(0F1EBh) END;
    p:=loc2;
    IF BITSET(p)*{0..23}#sdft  THEN ERROR(err1); STOP(002EBh) END;
    IF BITSET(p<<8)*{0..7}#n2  THEN ERROR(err2); STOP(0F2EBh) END;
    p:=loc4;
    IF BITSET(p)*{0..23}#sdft  THEN ERROR(err1); STOP(004EBh) END;
    IF BITSET(p<<8)*{0..7}#n4  THEN ERROR(err2); STOP(0F4EBh) END;
    p:=loc8;
    IF BITSET(p)*{0..23}#sdft  THEN ERROR(err1); STOP(008EBh) END;
    IF BITSET(p<<8)*{0..7}#n8  THEN ERROR(err2); STOP(0F8EBh) END;
    p:=locF;
    IF BITSET(p)*{0..23}#sdft  THEN ERROR(err1); STOP(00FEBh) END;
    IF BITSET(p<<8)*{0..7}#nF  THEN ERROR(err2); STOP(0FFEBh) END;
    p:=loc10;
    IF BITSET(p)*{0..23}#sdft  THEN ERROR(err1); STOP(010EBh) END;
    IF BITSET(p<<8)*{0..7}#n10 THEN ERROR(err2); STOP(0F0EBh) END;
    co:=co-1;
  UNTIL co=0;
  a:=G()-1;     a^:=save;
END tst205;

PROCEDURE tst206;
  VAR t: ADDRESS;
    i,j: INTEGER;
  s1,s2: BITSET;
      m: BITSET;
BEGIN
  TEST(206h,"INCL, EXCL"); INC(w02);
  IF w02#206h THEN ERROR(ORDER); STOP(20601h) END;
  t:=TregADR();
  IF CPU=2 THEN j:=64 ELSE j:=255 END;
  REPEAT
    s1:={0}<<(j MOD 32); s2:={0}<<(j MOD 32);
    i:=0;
    REPEAT
      INCL(s1,i); testTreg("INCL",1E0h); s2:=s2+{i};
      IF s1#s2 THEN ERROR("Illegal INCL"); STOP(3E0h) END;
      i:=i+1;
    UNTIL i=32;
    i:=0;
    REPEAT
      EXCL(s1,i); testTreg("EXCL",2E1h); s2:=s2-{i};
      IF s1#s2 THEN ERROR("Illegal EXCL"); STOP(4E1h) END;
      i:=i+1;
    UNTIL i=32;
    j:=j-1;
  UNTIL j=0;
END tst206;

PROCEDURE tstPW(): INTEGER;

  PROCEDURE tst(): INTEGER;  --        LPW 10..FF,SPW 10..FF
  --
  --       некоторые слова параметров пишутся и читаются
  --      номера этих слов.
  --       : 0F2h
  --
  CODE
    lib  10h spw  10h
    lib  20h spw  20h
    lib  40h spw  40h
    lib  80h spw  80h
    lib 0F0h spw 0F0h
    lib 0F1h spw 0F1h
    lib 0F2h spw 0F2h
    lib 0F4h spw 0F4h
    lib 0F8h spw 0F8h
    lib 0FFh spw 0FFh

    lib  10h lpw  10h  equ jfsc 65
    lib  20h lpw  20h  equ jfsc 58
    lib  40h lpw  40h  equ jfsc 51
    lib  80h lpw  80h  equ jfsc 44
    lib 0F0h lpw 0F0h  equ jfsc 37
    lib 0F1h lpw 0F1h  equ jfsc 30
    lib 0F2h lpw 0F2h  equ jfsc 23
    lib 0F4h lpw 0F4h  equ jfsc 16
    lib 0F8h lpw 0F8h  equ jfsc 09
    lib 0FFh lpw 0FFh  equ jfsc 02
    jfs 02
     1 rtn
     0 rtn
  END tst;

BEGIN
  RETURN tst();
END tstPW;

PROCEDURE tst207;
  VAR co: INTEGER;
     wsp: ARRAY [0..255] OF INTEGER; (* for parm words in tstPW *)
BEGIN
  TEST(207h,"LPW, SPW"); INC(w02);
  IF w02#207h THEN ERROR(ORDER); STOP(20701h) END;
  IF CPU=2 THEN co:=63 ELSE co:=255 END;
  REPEAT
    IF tstPW()#0 THEN ERROR("Illegal LPW or SPW"); STOP(0F2h) END;
    co:=co-1;
  UNTIL co=0;
END tst207;

VAR rem: INTEGER;

PROCEDURE imul(x,y: INTEGER): INTEGER;
  VAR z: INTEGER;
BEGIN
  IF y<0 THEN x:=-x; y:=-y END;
  z:=0;
  WHILE y#0 DO
    IF 0 IN BITSET(y) THEN z:=z+x END;
    y:=INTEGER(BITSET(y)-{0})>>1;
    x:=INTEGER(BITSET(x<<1)-{0});
  END;
  RETURN z;
END imul;

PROCEDURE idiv(x,y: INTEGER): INTEGER;
  VAR z,bt: INTEGER;
BEGIN
  z:=0; bt:=1;
  WHILE ABS(x)>ABS(y) DO
    bt:=bt<<1; y:=INTEGER(BITSET(y<<1)-{0});
  END;
  LOOP
    IF (x>=0) = (y>=0) THEN
      IF y<0 THEN
        IF x<=y THEN x:=x-y; z:=z+bt END;
      ELSE
        IF x>=y THEN x:=x-y; z:=z+bt END;
      END;
    ELSE
      IF y<0 THEN
        IF x>0 THEN x:=x+y; z:=z-bt END;
      ELSE
        IF x<0 THEN x:=x+y; z:=z-bt END;
      END;
    END;
    IF bt=1 THEN EXIT END;
    bt:=bt>>1;
    IF y>0 THEN
      y:=INTEGER(BITSET(y)-{0})>>1;
    ELSE
      y:=INTEGER(BITSET(y)+{0})>>1;
    END;
  END;
  rem:=x;
  RETURN z;
END idiv;

PROCEDURE iquot(x,y: INTEGER): INTEGER;
  VAR z,bt: INTEGER;
BEGIN
  z:=0; bt:=1;
  WHILE ABS(x)>ABS(y) DO
    bt:=bt<<1; y:=INTEGER(BITSET(y<<1)-{0});
  END;
  LOOP
    IF x<=-ABS(y) THEN
      IF y<0 THEN x:=x-y; z:=z+bt ELSE x:=x+y; z:=z-bt END;
    ELSIF x>=ABS(y) THEN
      IF y>0 THEN x:=x-y; z:=z+bt ELSE x:=x+y; z:=z-bt END;
    END;
    IF bt=1 THEN EXIT END;
    bt:=bt>>1;
    IF y>0 THEN
      y:=INTEGER(BITSET(y)-{0})>>1;
    ELSE
      y:=INTEGER(BITSET(y)+{0})>>1;
    END;
  END;
  rem:=x;
  RETURN z;
END iquot;

PROCEDURE imod(x,y: INTEGER): INTEGER;
BEGIN
  x:=idiv(x,y); RETURN rem;
END imod;

PROCEDURE irem(x,y: INTEGER): INTEGER;
BEGIN
  x:=iquot(x,y); RETURN rem;
END irem;

PROCEDURE test_one(x,y: INTEGER);
  PROCEDURE div_c(x,y: INTEGER): INTEGER; CODE 08Bh END div_c;
  PROCEDURE quo_c(x,y: INTEGER): INTEGER; CODE 0E3h 01h END quo_c;
  PROCEDURE mod_c(x,y: INTEGER): INTEGER; CODE 0AFh END mod_c;
  PROCEDURE rem_c(x,y: INTEGER): INTEGER; CODE 0E3h 03h END rem_c;
BEGIN
  testTreg("",0BAD3h);
  IF x*y # imul(x,y) THEN
    WriteInt(x,0); WriteString(' MUL '); WriteInt(y,0);
    WriteString(' = '); WriteInt(x*y,0);
    WriteString(', must be '); WriteInt(imul(x,y),0);
    WriteLn; STOP(20811h);
  END;
  IF NOT QUOT THEN
    IF (y#0) & (x DIV y # iquot(x,y)) THEN
      WriteInt(x,0); WriteString(' DIV '); WriteInt(y,0);
      WriteString(' = '); WriteInt(x DIV y,0);
      WriteString(', must be '); WriteInt(iquot(x,y),0);
      WriteLn; STOP(20812h);
    END;
    IF (y#0) & (x MOD y # irem(x,y)) THEN
      WriteInt(x,0); WriteString(' MOD '); WriteInt(y,0);
      WriteString(' = '); WriteInt(x MOD y,0);
      WriteString(', must be '); WriteInt(irem(x,y),0);
      WriteLn; STOP(20813h);
    END;
  ELSE
    IF (y#0) & (div_c(x,y)#idiv(x,y)) THEN
      WriteInt(x,0); WriteString(' DIV '); WriteInt(y,0);
      WriteString(' = '); WriteInt(div_c(x,y),0);
      WriteString(', must be '); WriteInt(idiv(x,y),0);
      WriteLn; STOP(20814h);
    END;
    IF (y#0) & (mod_c(x,y)#imod(x,y)) THEN
      WriteInt(x,0); WriteString(' MOD '); WriteInt(y,0);
      WriteString(' = '); WriteInt(mod_c(x,y),0);
      WriteString(', must be '); WriteInt(imod(x,y),0);
      WriteLn; STOP(20815h);
    END;
    IF (y#0) & (quo_c(x,y)#iquot(x,y)) THEN
      WriteInt(x,0); WriteString(' QUOT '); WriteInt(y,0);
      WriteString(' = '); WriteInt(quo_c(x,y),0);
      WriteString(', must be '); WriteInt(iquot(x,y),0);
      WriteLn; STOP(20816h);
    END;
    IF (y#0) & (rem_c(x,y)#irem(x,y)) THEN
      WriteInt(x,0); WriteString(' REM '); WriteInt(y,0);
      WriteString(' = '); WriteInt(rem_c(x,y),0);
      WriteString(', must be '); WriteInt(irem(x,y),0);
      WriteLn; STOP(20817h);
    END;
  END;
  testTreg("MUL/DIV point 0: ",8889h);
END test_one;

PROCEDURE tst208;  --    MUL, DIV
--
--
  VAR m: BITSET;
   trap: ADDRESS;
    i,j: INTEGER;
    d,n: INTEGER;
     co: INTEGER;
  CONST maxint=MAX(INTEGER);
        minint=MIN(INTEGER);
BEGIN
  TEST(208h,"MUL, DIV, QUOT"); INC(w02);
  IF w02#208h THEN ERROR(ORDER); STOP(20801h) END;
  trap:=TregADR(); m:=GETM();
  IF CPU=2 THEN co:=20 ELSE co:=80 END;
  REPEAT
    SETM(m-{31});
    test_one(0,0);
    test_one(0,3);
    test_one(3,3);
    test_one(3,0);
    test_one(0,-3);
    test_one(-3,-3);
    test_one(-3,0);
    test_one(3,-3);
    test_one(-3,3);
    test_one(334,13);
    test_one(777,7);
    test_one(7,777);
    test_one(334,-13);
    test_one(777,-7);
    test_one(7,-777);
    test_one(-334,-13);
    test_one(-777,-7);
    test_one(-7,-777);
    test_one(-334,13);
    test_one(-777,7);
    test_one(-7,777);
    SETM(m);
    co:=co-1;
  UNTIL co=0;
END tst208;

BEGIN
  tst200; check4C;
  tst201; check4C;
  tst202; check4C;
  tst203; check4C;
  tst204; check4C;
  tst205; check4C;
  tst206; check4C;
  tst207; check4C;
  tst208; check4C;
  WriteLn;
END TEST2;

MODULE TEST3; (* Traps *)

                IMPORT  CPU, w02, w03, w04, GETM, SETM;
                IMPORT  TregADR, Debug, check4C;
                IMPORT  Show, WriteString, Write, WriteInt, WriteLn;
                IMPORT  ERROR, TEST, STOP, ORDER;
FROM SYSTEM     IMPORT  ADR, ADDRESS, WORD;
FROM defCodes  IMPORT  li0, llw, lxb, lsw0, li1, lgw, lxw, lsw1,
                        li2, lew, lgw2, lsw2, li3, lsw, lgw3, lsw3,
                        li4, llw4, lgw4, lsw4, li5, llw5, lgw5, lsw5,
                        li6, llw6, lgw6, lsw6, li7, llw7, lgw7, lsw7,
                        li8, llw8, lgw8, lsw8, li9, llw9, lgw9, lsw9,
                        li0A, llw0A, lgw0A, lsw0A, li0B, llw0B, lgw0B, lsw0B,
                        li0C, llw0C, lgw0C, lsw0C, li0D, llw0D, lgw0D, lsw0D,
                        li0E, llw0E, lgw0E, lsw0E, li0F, llw0F, lgw0F, lsw0F,
                        lib, slw, sxb, ssw0, lid, sgw, sxw, ssw1,
                        liw, sew, sgw2, ssw2, lin, ssw, sgw3, ssw3,
                        lla, slw4, sgw4, ssw4, lga, slw5, sgw5, ssw5,
                        lsa, slw6, sgw6, ssw6, lea, slw7, sgw7, ssw7,
                        jflc, slw8, sgw8, ssw8, jfl, slw9, sgw9, ssw9,
                        jfsc, slw0A, sgw0A, ssw0A, jfs, slw0B, sgw0B, ssw0B,
                        jblc, slw0C, sgw0C, ssw0C, jbl, slw0D, sgw0D, ssw0D,
                        jbsc, slw0E, sgw0E, ssw0E, jbs, slw0F, sgw0F, ssw0F,
                        reset, lss, move, incl, quit, leq, chknil, excl,
                        getm, gtr, lsta,      setm, geq, comp,
                        trap, equ, gb, inc1, tra, neq, gb1, dec1,
                        tr, abs, chk, inc, idle, neg, chkz, dec,
                        add, or, alloc, stot, sub, and, entr, lodt,
                        mul, xor, rtn, lxa, div, bic, nop, lpc,
                        shl, in, cx, bbu, shr, bit, ci, bbp,
                        rol, not, cf, bblt, ror, mod, cl,
                        io0, decs, cl0, swap, io1, drop, cl1, lpa,
                        io2, lodfv, cl2, lpw, io3, store, cl3, spw,
                        io4, stofv, cl4, sswu, copt, cl5, rchk,
                        cpcop, cl6, rchkz, pcop, cl7, cm,
                        fadd, for1, cl8, fsub, for2, cl9, bmg,
                        fmul, entc, cl0A, activ, fdiv, xit, cl0B, usr,
                        fcmp, addpc, cl0C, sys, fabs, jump, cl0D, nii,
                        fneg, orjp, cl0E, dot, ffct, andjp, cl0F, invld;

EXPORT MonitorTrap, Happend?;

PROCEDURE tst300;
BEGIN
  w02:=300h;
  w03:=0;
END tst300;

TYPE Process = POINTER TO
       RECORD
         G: ADDRESS;
         L: ADDRESS;
        PC: INTEGER;
         M: BITSET;
         S: ADDRESS;
         H: ADDRESS;
         T: INTEGER;
       END;

PROCEDURE NEWPROCESS(proc: PROC; a: ADDRESS; sz: INTEGER; VAR p: Process);
  VAR pr: Process; n,pc: INTEGER;
BEGIN
  pr:=a;
  n :=INTEGER( BITSET(proc<<8)*{0..7} );
  a :=INTEGER( BITSET(proc)*{0..23});
  pr^.G :=a^;
  a :=ADDRESS(INTEGER(pr^.G^)+n);
  pr^.PC:=a^;
  pr^.M :=GETM();
  pr^.L :=ADDRESS(pr)+10h;
  pr^.S :=ADDRESS(pr)+15h;
  a     :=pr^.S-1; a^:=0;
  pr^.H :=ADDRESS(pr)+sz-1;
  pr^.T :=0;
  p:=Process(pr);
END NEWPROCESS;

PROCEDURE TRANSFER(VAR from,to: Process); CODE tra END TRANSFER;

PROCEDURE TRAP(n: INTEGER); CODE trap END TRAP;

VAR no, sv0, sv1: INTEGER;

PROCEDURE OnTrap(n: INTEGER; VAR driver, ipted: Process);
  VAR a: ADDRESS; ei: BITSET;
BEGIN
  ei:=GETM();
  SETM(ei-{0,1});
  no :=n;       a :=n*2;
  sv0:=a^;      a^:=driver;     INC(a);
  sv1:=a^;      a^:=ADR(ipted);
  SETM(ei);
END OnTrap;

PROCEDURE Restore;
  VAR a: ADDRESS; ei: BITSET;
BEGIN
  ei:=GETM(); SETM(ei-{0,1});
  a:=no*2; a^:=sv0; INC(a); a^:=sv1;
  SETM(ei);
END Restore;

VAR
 ipted: Process;
  moni: Process;
  haps: INTEGER;

PROCEDURE monitor;
BEGIN
  LOOP
    haps:=ipted^.T;
    TRANSFER(moni,ipted);
  END;
END monitor;

PROCEDURE MonitorTrap(n: INTEGER; VAR wsp: ARRAY OF WORD);
  VAR ei: BITSET;
BEGIN
  ei:=GETM(); SETM(ei-{0,1});
  NEWPROCESS(monitor,ADR(wsp),SIZE(wsp),moni);
  IF n>3Fh THEN n:=3Fh END;
  OnTrap(n,moni,ipted);
  SETM(ei);
END MonitorTrap;

PROCEDURE Happend?(): INTEGER;
  VAR i: INTEGER;
BEGIN
  Restore;
  IF haps<0 THEN RETURN -1 END;
  i:=haps; haps:=-1;
  RETURN i
END Happend?;

VAR coT: INTEGER;
    coI: INTEGER;
 tester: Process;
   adrT: ADDRESS;

PROCEDURE trapFFtester;
BEGIN
  LOOP
    coI:=coI-1;
    IF coT#coI THEN
      ERROR("Unexpected soft interrupt"); STOP(0284h)
    END;
    IF adrT^#0FFh THEN
      ERROR("Illegal trapno in T register"); STOP(0384h)
    END;
    TRANSFER(tester,ipted);
  END;
END trapFFtester;

PROCEDURE tst301;
  VAR m: BITSET;
     co: INTEGER;
    wsp: ARRAY [0..255] OF INTEGER;
BEGIN
  TEST(301h,"soft TRAP"); INC(w02);
  IF w02#301h THEN ERROR(ORDER); STOP(30101h) END;
  m:=GETM();
  SETM(m+{31}-{0,1});
  NEWPROCESS(trapFFtester,ADR(wsp),SIZE(wsp),tester);
  OnTrap(3Fh,tester,ipted);
  adrT:=TregADR();
  IF CPU=2 THEN co:=4 ELSE co:=16 END;
  REPEAT
    coT:=255; coI:=255;
    REPEAT
      coT:=coT-1;
      TRAP(0FFh);
      IF coT#coI THEN
        ERROR("Lost soft interrupt raised by TRAP"); STOP(0184h)
      END;
    UNTIL coT=0;
    co:=co-1;
  UNTIL co=0;
  SETM(m);
  Restore;
END tst301;

VAR time: INTEGER;

PROCEDURE clock;
BEGIN
  LOOP
    INC(time);
    TRANSFER(tester,ipted);
  END;
END clock;


PROCEDURE tst302;
  VAR m: BITSET;
     co: INTEGER;
   Twsp: ARRAY [0..255] OF INTEGER;
BEGIN
  TEST(302h,"CLOCK"); INC(w02);
  IF w02#302h THEN ERROR(ORDER); STOP(30201h) END;
  m:=GETM();
  SETM(m-{0,1});
  NEWPROCESS(clock,ADR(Twsp),SIZE(Twsp),tester);
  OnTrap(01h,tester,ipted);
  SETM(m+{1});
  IF CPU=2 THEN SETM(GETM()+{0}) END;
  time:=0;
  REPEAT
    co:=50*1024;
    REPEAT DEC(co) UNTIL co=0;  -- about second
    IF time=0 THEN
      ERROR("Lost TIMER interrupts"); STOP(0284h); INC(time,100);
    END;
    WriteString("timer: "); WriteInt(time,0); Write(15c);
  UNTIL time>5*50;
  WriteString("              " 15c);
  SETM(m);
  Restore;
END tst302;

BEGIN haps:=-1;
  tst300; check4C;
  tst301; check4C;
  tst302; check4C;
  WriteLn;
END TEST3; (* Traps *)


MODULE TEST4;

                IMPORT  CPU, w02, w03, w04, GETM, SETM, QUOT;
                IMPORT  TregADR, Debug, check4C;
                IMPORT  Show, WriteString, Write, WriteInt, WriteLn, WriteHex;
                IMPORT  ERROR, TEST, STOP, ORDER;
                IMPORT  MonitorTrap, Happend?;
FROM SYSTEM     IMPORT  ADR, ADDRESS;
FROM defCodes  IMPORT  li0, llw, lxb, lsw0, li1, lgw, lxw, lsw1,
                        li2, lew, lgw2, lsw2, li3, lsw, lgw3, lsw3,
                        li4, llw4, lgw4, lsw4, li5, llw5, lgw5, lsw5,
                        li6, llw6, lgw6, lsw6, li7, llw7, lgw7, lsw7,
                        li8, llw8, lgw8, lsw8, li9, llw9, lgw9, lsw9,
                        li0A, llw0A, lgw0A, lsw0A, li0B, llw0B, lgw0B, lsw0B,
                        li0C, llw0C, lgw0C, lsw0C, li0D, llw0D, lgw0D, lsw0D,
                        li0E, llw0E, lgw0E, lsw0E, li0F, llw0F, lgw0F, lsw0F,
                        lib, slw, sxb, ssw0, lid, sgw, sxw, ssw1,
                        liw, sew, sgw2, ssw2, lin, ssw, sgw3, ssw3,
                        lla, slw4, sgw4, ssw4, lga, slw5, sgw5, ssw5,
                        lsa, slw6, sgw6, ssw6, lea, slw7, sgw7, ssw7,
                        jflc, slw8, sgw8, ssw8, jfl, slw9, sgw9, ssw9,
                        jfsc, slw0A, sgw0A, ssw0A, jfs, slw0B, sgw0B, ssw0B,
                        jblc, slw0C, sgw0C, ssw0C, jbl, slw0D, sgw0D, ssw0D,
                        jbsc, slw0E, sgw0E, ssw0E, jbs, slw0F, sgw0F, ssw0F,
                        reset, lss, move, incl, quit, leq, chknil, excl,
                        getm, gtr, lsta,       setm, geq, comp,
                        trap, equ, gb, inc1, tra, neq, gb1, dec1,
                        tr, abs, chk, inc, idle, neg, chkz, dec,
                        add, or, alloc, stot, sub, and, entr, lodt,
                        mul, xor, rtn, lxa, div, bic, nop, lpc,
                        shl, in, cx, bbu, shr, bit, ci, bbp,
                        rol, not, cf, bblt, ror, mod, cl,
                        io0, decs, cl0, swap, io1, drop, cl1, lpa,
                        io2, lodfv, cl2, lpw, io3, store, cl3, spw,
                        io4, stofv, cl4, sswu, copt, cl5, rchk,
                        cpcop, cl6, rchkz, pcop, cl7, cm,
                        fadd, for1, cl8, fsub, for2, cl9, bmg,
                        fmul, entc, cl0A, activ, fdiv, xit, cl0B, usr,
                        fcmp, addpc, cl0C, sys, fabs, jump, cl0D, nii,
                        fneg, orjp, cl0E, dot, ffct, andjp, cl0F, invld;

PROCEDURE tst400;
BEGIN
  w02:=400h;
  w03:=0;
END tst400;

PROCEDURE ClearStack;
CODE store lodt decs END ClearStack;

PROCEDURE tst401;
  VAR wsp: ARRAY [0..127] OF INTEGER;
       co: INTEGER;
        s: BITSET;
       ei: BITSET;
BEGIN
  TEST(401h,"INCL, EXCL traps 4A"); INC(w02);
  IF w02#401h THEN ERROR(ORDER); STOP(40101h) END;
  RETURN ;
  (*
  ei:=GETM();
  SETM(ei+{31}-{0,1});
  co:=256;
  REPEAT
    MonitorTrap(4Ah,wsp); INCL(s,-1);
    IF Happend?()#4Ah THEN ERROR("INCL trap 4Ah not raised"); STOP(5E0h) END;
    ClearStack;
    MonitorTrap(4Ah,wsp); INCL(s,32);
    IF Happend?()#4Ah THEN ERROR("INCL trap 4Ah not raised"); STOP(6E0h) END;
    ClearStack;
    MonitorTrap(4Ah,wsp); EXCL(s,-1);
    IF Happend?()#4Ah THEN ERROR("EXCL trap 4Ah not raised"); STOP(7E1h) END;
    ClearStack;
    MonitorTrap(4Ah,wsp); EXCL(s,32);
    IF Happend?()#4Ah THEN ERROR("EXCL trap 4Ah not raised"); STOP(8E1h) END;
    ClearStack;
    co:=co-1;
  UNTIL co=0;
  SETM(ei);
  *)
END tst401;

PROCEDURE tst402;
  VAR wsp: ARRAY [0..127] OF INTEGER;
       co: INTEGER;
       ei: BITSET;

  PROCEDURE CHKZ(i,hi: INTEGER);    CODE chkz END CHKZ;
  PROCEDURE CHK (i,lo,hi: INTEGER); CODE chk  END CHK;

BEGIN
  TEST(402h,"CHK, CHKZ"); INC(w02);
  IF w02#402h THEN ERROR(ORDER); STOP(40201h) END;
  ei:=GETM(); SETM(ei+{31}-{0,1});
  co:=255;
  REPEAT
    MonitorTrap(4Ah,wsp); CHKZ(co,256);
    IF Happend?()>0h THEN
      ERROR("CHKZ unexpected trap 4Ah raised"); STOP(1C7h);
      ClearStack;
    END;
    MonitorTrap(4Ah,wsp); CHK(co,0,256);
    IF Happend?()>0h THEN
      ERROR("CHK unexpected trap 4Ah raised"); STOP(2C6h);
      ClearStack;
    END;

    MonitorTrap(4Ah,wsp); CHKZ(256,co);
    IF Happend?()#4Ah THEN
      ERROR("CHKZ trap 4Ah not raised"); STOP(3C7h)
    END;
    ClearStack;
    MonitorTrap(4Ah,wsp); CHK(256,0,co);
    IF Happend?()#4Ah THEN
      ERROR("CHK  trap 4Ah not raised"); STOP(4C6h)
    END;
    ClearStack;
    co:=co-1;
  UNTIL co=0;
  SETM(ei);
END tst402;

PROCEDURE tst403;
  VAR co: INTEGER;
   s1,s2: BITSET;
     geq: BOOLEAN;
     leq: BOOLEAN;
   i0,i1: INTEGER;
   i2,i3: INTEGER;
BEGIN
  TEST(403h,"SLEQ, SGEQ"); INC(w02);
  IF w02#403h THEN ERROR(ORDER); STOP(40301h) END;
  IF CPU=2 THEN co:=256 ELSE co:=1024 END;
  REPEAT
    i0:=0;
    REPEAT i1:=0;
      REPEAT i2:=0;
        REPEAT i3:=0;
          REPEAT
            s1:={};
            IF i0#0 THEN s1:=s1+{0,8,16,24,31} END;
            IF i1#0 THEN s1:=s1+{1,9,17,25}    END;
            IF i2#0 THEN s1:=s1+{2,6,18,27}    END;
            IF i3#0 THEN s1:=s1+{3,5,19,29}    END;
            IF ODD(co) THEN s2:=s1; s1:=s2/{0..31} ELSE s2:=s1/{0..31} END;
            geq:=((s1-s2)#{}) & ((s2-s1)={}) OR (s1=s2);
            leq:=((s2-s1)#{}) & ((s1-s2)={}) OR (s1=s2);
            IF (s1>=s2)#geq THEN ERROR("Illegal SGEQ"); STOP(0E3h) END;
            IF (s1<=s2)#leq THEN ERROR("Illegal SLEQ"); STOP(0E2h) END;
            INC(i3);
          UNTIL i3>1;
          INC(i2);
        UNTIL i2>1;
        INC(i1);
      UNTIL i1>1;
      INC(i0);
    UNTIL i0>1;
    co:=co-1;
  UNTIL co=0;
END tst403;

PROCEDURE tst404;
  VAR i: INTEGER;
  s1,s2: ARRAY [0..7] OF CHAR;
  r1,r2: BOOLEAN;
  c1,c2: BOOLEAN;
  x,max: INTEGER;
    ch1: CHAR;
    ch2: CHAR;

  PROCEDURE COMP(s1,s2: ADDRESS); CODE comp not swap not END COMP;
  PROCEDURE pop(): BOOLEAN;       CODE END pop;

BEGIN
  TEST(404h,"COMP (slow)"); INC(w02);
  IF w02#404h THEN ERROR(ORDER); STOP(40401h) END;

  i:=0; max:=0;
  REPEAT s1[i]:=0c; INC(i) UNTIL i=8;
  REPEAT
    i:=0;
    REPEAT s2[i]:=0c; INC(i) UNTIL i=8;
    REPEAT

      COMP(ADR(s1),ADR(s2)); r1:=pop(); r2:=pop();
      i:=0; ch1:=s1[0]; ch2:=s2[0];
      WHILE (ch1=ch2) & (ch1#0c) & (ch2#0c) DO
        INC(i); ch1:=s1[i]; ch2:=s2[i]
      END;
      c1:=NOT BOOLEAN(s1[i]);     c2:=NOT BOOLEAN(s2[i]);
--    WriteString("c1="); WriteInt(c1,0); WriteLn;
--    WriteString("r1="); WriteInt(r1,0); WriteLn;
--    WriteString("c2="); WriteInt(c2,0); WriteLn;
--    WriteString("r2="); WriteInt(r2,0); WriteLn;
      IF (c1#r1) OR (c2#r2) THEN ERROR("Illegal COMP"); STOP(0C3h) END;

      i:=0;
      REPEAT
        x:=(ORD(s2[i])+1) MOD 3; s2[i]:=CHAR(x); INC(i)
      UNTIL (x#0);
    UNTIL s2[7]#0c;

    i:=0; IF max>3 THEN i:=4 END;
    REPEAT
      s1[i]:=CHAR( (ORD(s1[i])+1) MOD 3 ); INC(i)
    UNTIL (s1[i-1]#0c);
    IF i>max THEN
      IF max=3 THEN s1:="" 1c 2c 1c 2c END;
      max:=i; WriteInt(max,0); Write(15c)
    END;
  UNTIL s1[7]#0c;
  Write(" "); Write(15c);
END tst404;

PROCEDURE tst405;

  PROCEDURE p(i: INTEGER): INTEGER;
  CODE copt 8 rol copt 8 rol copt 8 rol or or or END p;

  PROCEDURE MOVE(t,f: ADDRESS; size: INTEGER); CODE move END MOVE;

  VAR s,d: ARRAY [0..63] OF INTEGER;
    l,i,j: INTEGER;
     co,n: INTEGER;

BEGIN
  TEST(405h,"MOVE"); INC(w02);
  IF w02#405h THEN ERROR(ORDER); STOP(40501h) END;
  IF CPU=2 THEN co:=64 ELSE co:=256 END;
  REPEAT
    i:=0;
    REPEAT s[i]:=p(i); d[i]:=0; INC(i) UNTIL i>HIGH(s);
    n:=co MOD 16 + 1; l:=HIGH(s)+1-n*2;
    MOVE(ADR(d[n]),ADR(s[n]),l);
    i:=n; j:=l;
    REPEAT
      IF d[i]#s[i] THEN ERROR("Illegal MOVE"); STOP(1C0h) END; INC(i); DEC(j)
    UNTIL j=0;
    i:=n-1;
    REPEAT
      IF d[i]#0 THEN ERROR("Illegal MOVE"); STOP(2C0h) END; DEC(i);
    UNTIL i<0;
    i:=n+l;
    REPEAT
      IF d[i]#0 THEN ERROR("Illegal MOVE"); STOP(3C0h) END; INC(i);
    UNTIL i>HIGH(s);
    DEC(co);
  UNTIL co=0;
END tst405;


BEGIN
  tst400; check4C;
  IF NOT QUOT THEN tst401; check4C ELSE INC(w02) END;
  tst402; check4C;
  IF NOT QUOT THEN tst403; check4C ELSE INC(w02) END;
  tst404; check4C;
  tst405; check4C;
  WriteLn;
END TEST4;

MODULE TEST9; (* Control *)

                IMPORT  CPU, w02, w03, w04;
                IMPORT  TregADR, Debug, check4C;
                IMPORT  Show, WriteString, Write, WriteInt, WriteLn;
                IMPORT  ERROR, TEST, STOP, ORDER;
FROM SYSTEM     IMPORT  ADR, ADDRESS;
FROM defCodes  IMPORT  li0, llw, lxb, lsw0, li1, lgw, lxw, lsw1,
                        li2, lew, lgw2, lsw2, li3, lsw, lgw3, lsw3,
                        li4, llw4, lgw4, lsw4, li5, llw5, lgw5, lsw5,
                        li6, llw6, lgw6, lsw6, li7, llw7, lgw7, lsw7,
                        li8, llw8, lgw8, lsw8, li9, llw9, lgw9, lsw9,
                        li0A, llw0A, lgw0A, lsw0A, li0B, llw0B, lgw0B, lsw0B,
                        li0C, llw0C, lgw0C, lsw0C, li0D, llw0D, lgw0D, lsw0D,
                        li0E, llw0E, lgw0E, lsw0E, li0F, llw0F, lgw0F, lsw0F,
                        lib, slw, sxb, ssw0, lid, sgw, sxw, ssw1,
                        liw, sew, sgw2, ssw2, lin, ssw, sgw3, ssw3,
                        lla, slw4, sgw4, ssw4, lga, slw5, sgw5, ssw5,
                        lsa, slw6, sgw6, ssw6, lea, slw7, sgw7, ssw7,
                        jflc, slw8, sgw8, ssw8, jfl, slw9, sgw9, ssw9,
                        jfsc, slw0A, sgw0A, ssw0A, jfs, slw0B, sgw0B, ssw0B,
                        jblc, slw0C, sgw0C, ssw0C, jbl, slw0D, sgw0D, ssw0D,
                        jbsc, slw0E, sgw0E, ssw0E, jbs, slw0F, sgw0F, ssw0F,
                        reset, lss, move, incl, quit, leq, chknil, excl,
                        getm, gtr, lsta,       setm, geq, comp,
                        trap, equ, gb, inc1, tra, neq, gb1, dec1,
                        tr, abs, chk, inc, idle, neg, chkz, dec,
                        add, or, alloc, stot, sub, and, entr, lodt,
                        mul, xor, rtn, lxa, div, bic, nop, lpc,
                        shl, in, cx, bbu, shr, bit, ci, bbp,
                        rol, not, cf, bblt, ror, mod, cl,
                        io0, decs, cl0, swap, io1, drop, cl1, lpa,
                        io2, lodfv, cl2, lpw, io3, store, cl3, spw,
                        io4, stofv, cl4, sswu, copt, cl5, rchk,
                        cpcop, cl6, rchkz, pcop, cl7, cm,
                        fadd, for1, cl8, fsub, for2, cl9, bmg,
                        fmul, entc, cl0A, activ, fdiv, xit, cl0B, usr,
                        fcmp, addpc, cl0C, sys, fabs, jump, cl0D, nii,
                        fneg, orjp, cl0E, dot, ffct, andjp, cl0F, invld;

PROCEDURE tst900;
BEGIN
  w02:=900h;
  w03:=0;
END tst900;

PROCEDURE tst901;

  CONST gb_ci="GB, GB1 or CI error";

  VAR loc0: INTEGER;

  PROCEDURE inter1(n: INTEGER);

    VAR loc1: INTEGER;

    PROCEDURE inter2(n: INTEGER);

      VAR loc2: INTEGER;

      PROCEDURE inter3(n: INTEGER);
      BEGIN
        IF loc0#n-2 THEN ERROR(gb_ci); STOP(30CDh) END;
        IF loc1#n-1 THEN ERROR(gb_ci); STOP(31CDh) END;
        IF loc2#n   THEN ERROR(gb_ci); STOP(32CDh) END;
      END inter3;

    BEGIN
      IF loc0#n-1 THEN ERROR(gb_ci); STOP(20CDh) END;
      IF loc1#n   THEN ERROR(gb_ci); STOP(21CDh) END;
      loc2:=n+1;
      inter3(n+1);
    END inter2;

  BEGIN
    IF loc0#n THEN ERROR(gb_ci); STOP(1CDh) END;
    loc1:=n+1;
    inter2(n+1);
  END inter1;

BEGIN
  TEST(901h,"CI, GB, GB1"); INC(w02);
  IF w02#901h THEN ERROR(ORDER); STOP(90101h) END;
  loc0:=0;
  REPEAT inter1(loc0); loc0:=loc0+13 UNTIL loc0>0FFFFh;
END tst901;

PROCEDURE tst902;
  CONST err1="Illegal FOR1 or FOR2";
        err2="Illegal FOR2";

  VAR i,j: INTEGER;
      k,l: INTEGER;
BEGIN
  TEST(902h,"FOR1, FOR2"); INC(w02);
  IF w02#902h THEN ERROR(ORDER); STOP(90201h) END;
  j:=0;
  FOR i:=0 TO 1023 DO
    IF i#j THEN ERROR(err1); STOP(1B8h) END;
    IF j>1023 THEN ERROR(err2); STOP(2B9h) END;
    INC(j);
  END;
  j:=1023;
  FOR i:=1023 TO 0 BY -1 DO
    IF i#j THEN ERROR(err1); STOP(3B8h) END;
    IF j<0 THEN ERROR(err2); STOP(4B9h) END;
    DEC(j);
  END;

  j:=-1024;
  FOR i:=-1024 TO 1023 DO
    IF i#j THEN ERROR(err1); STOP(5B8h) END;
    IF j>1023 THEN ERROR(err2); STOP(6B9h) END;
    INC(j);
  END;
  j:=1023;
  FOR i:=1023 TO -1024 BY -1 DO
    IF i#j THEN ERROR(err1); STOP(7B8h) END;
    IF j<-1024 THEN ERROR(err2); STOP(8B9h) END;
    DEC(j);
  END;

  j:=-1024;
  FOR i:=-1024 TO 1023 BY 127 DO
    IF i#j THEN ERROR(err1); STOP(9B8h) END;
    IF j>1023 THEN ERROR(err2); STOP(0AB9h) END;
    INC(j,127);
  END;
  j:=1023;
  FOR i:=1023 TO -1024 BY -128 DO
    IF i#j THEN ERROR(err1); STOP(0BB8h) END;
    IF j<-1024 THEN ERROR(err2); STOP(0CB9h) END;
    DEC(j,128);
  END;

  j:=0;
  FOR i:=0 TO 1023 DO
    IF i#j THEN ERROR(err1); STOP(0DB8h) END;
    IF j>1023 THEN ERROR(err2); STOP(0EB9h) END;
    INC(j);
    k:=0;
    FOR l:=0 TO 127 DO
      IF l#k THEN ERROR(err1); STOP(0FB8h) END;
      IF l>127 THEN ERROR(err2); STOP(10B9h) END;
      INC(k);
    END;
  END;

END tst902;

PROCEDURE tst903;

  VAR i,co: INTEGER;

  PROCEDURE l(n: INTEGER);  CODE END l;
  PROCEDURE top(): INTEGER; CODE END top;

BEGIN
  TEST(903h,"ENTC, XIT"); INC(w02);
  IF w02#903h THEN ERROR(ORDER); STOP(90301h) END;
  co:=255;
  REPEAT
    i:=0;
    REPEAT
      CASE i OF
          |000h: l(000h)  |001h: l(001h)  |002h: l(002h)  |003h: l(003h)
          |004h: l(004h)  |005h: l(005h)  |006h: l(006h)  |007h: l(007h)
          |008h: l(008h)  |009h: l(009h)  |00Ah: l(00Ah)  |00Bh: l(00Bh)
          |00Ch: l(00Ch)  |00Dh: l(00Dh)  |00Eh: l(00Eh)  |00Fh: l(00Fh)
  
          |010h: l(010h)  |011h: l(011h)  |012h: l(012h)  |013h: l(013h)
          |014h: l(014h)  |015h: l(015h)  |016h: l(016h)  |017h: l(017h)
          |018h: l(018h)  |019h: l(019h)  |01Ah: l(01Ah)  |01Bh: l(01Bh)
          |01Ch: l(01Ch)  |01Dh: l(01Dh)  |01Eh: l(01Eh)  |01Fh: l(01Fh)
  
          |020h: l(020h)  |021h: l(021h)  |022h: l(022h)  |023h: l(023h)
          |024h: l(024h)  |025h: l(025h)  |026h: l(026h)  |027h: l(027h)
          |028h: l(028h)  |029h: l(029h)  |02Ah: l(02Ah)  |02Bh: l(02Bh)
          |02Ch: l(02Ch)  |02Dh: l(02Dh)  |02Eh: l(02Eh)  |02Fh: l(02Fh)
  
          |030h: l(030h)  |031h: l(031h)  |032h: l(032h)  |033h: l(033h)
          |034h: l(034h)  |035h: l(035h)  |036h: l(036h)  |037h: l(037h)
          |038h: l(038h)  |039h: l(039h)  |03Ah: l(03Ah)  |03Bh: l(03Bh)
          |03Ch: l(03Ch)  |03Dh: l(03Dh)  |03Eh: l(03Eh)  |03Fh: l(03Fh)
  
          |040h: l(040h)  |041h: l(041h)  |042h: l(042h)  |043h: l(043h)
          |044h: l(044h)  |045h: l(045h)  |046h: l(046h)  |047h: l(047h)
          |048h: l(048h)  |049h: l(049h)  |04Ah: l(04Ah)  |04Bh: l(04Bh)
          |04Ch: l(04Ch)  |04Dh: l(04Dh)  |04Eh: l(04Eh)  |04Fh: l(04Fh)

          |050h: l(050h)  |051h: l(051h)  |052h: l(052h)  |053h: l(053h)
          |054h: l(054h)  |055h: l(055h)  |056h: l(056h)  |057h: l(057h)
          |058h: l(058h)  |059h: l(059h)  |05Ah: l(05Ah)  |05Bh: l(05Bh)
          |05Ch: l(05Ch)  |05Dh: l(05Dh)  |05Eh: l(05Eh)  |05Fh: l(05Fh)
  
          |060h: l(060h)  |061h: l(061h)  |062h: l(062h)  |063h: l(063h)
          |064h: l(064h)  |065h: l(065h)  |066h: l(066h)  |067h: l(067h)
          |068h: l(068h)  |069h: l(069h)  |06Ah: l(06Ah)  |06Bh: l(06Bh)
          |06Ch: l(06Ch)  |06Dh: l(06Dh)  |06Eh: l(06Eh)  |06Fh: l(06Fh)
  
          |070h: l(070h)  |071h: l(071h)  |072h: l(072h)  |073h: l(073h)
          |074h: l(074h)  |075h: l(075h)  |076h: l(076h)  |077h: l(077h)
          |078h: l(078h)  |079h: l(079h)  |07Ah: l(07Ah)  |07Bh: l(07Bh)
          |07Ch: l(07Ch)  |07Dh: l(07Dh)  |07Eh: l(07Eh)  |07Fh: l(07Fh)
  
          |080h: l(080h)  |081h: l(081h)  |082h: l(082h)  |083h: l(083h)
          |084h: l(084h)  |085h: l(085h)  |086h: l(086h)  |087h: l(087h)
          |088h: l(088h)  |089h: l(089h)  |08Ah: l(08Ah)  |08Bh: l(08Bh)
          |08Ch: l(08Ch)  |08Dh: l(08Dh)  |08Eh: l(08Eh)  |08Fh: l(08Fh)
  
          |090h: l(090h)  |091h: l(091h)  |092h: l(092h)  |093h: l(093h)
          |094h: l(094h)  |095h: l(095h)  |096h: l(096h)  |097h: l(097h)
          |098h: l(098h)  |099h: l(099h)  |09Ah: l(09Ah)  |09Bh: l(09Bh)
          |09Ch: l(09Ch)  |09Dh: l(09Dh)  |09Eh: l(09Eh)  |09Fh: l(09Fh)

          |0A0h: l(0A0h)  |0A1h: l(0A1h)  |0A2h: l(0A2h)  |0A3h: l(0A3h)
          |0A4h: l(0A4h)  |0A5h: l(0A5h)  |0A6h: l(0A6h)  |0A7h: l(0A7h)
          |0A8h: l(0A8h)  |0A9h: l(0A9h)  |0AAh: l(0AAh)  |0ABh: l(0ABh)
          |0ACh: l(0ACh)  |0ADh: l(0ADh)  |0AEh: l(0AEh)  |0AFh: l(0AFh)
  
          |0B0h: l(0B0h)  |0B1h: l(0B1h)  |0B2h: l(0B2h)  |0B3h: l(0B3h)
          |0B4h: l(0B4h)  |0B5h: l(0B5h)  |0B6h: l(0B6h)  |0B7h: l(0B7h)
          |0B8h: l(0B8h)  |0B9h: l(0B9h)  |0BAh: l(0BAh)  |0BBh: l(0BBh)
          |0BCh: l(0BCh)  |0BDh: l(0BDh)  |0BEh: l(0BEh)  |0BFh: l(0BFh)
  
          |0C0h: l(0C0h)  |0C1h: l(0C1h)  |0C2h: l(0C2h)  |0C3h: l(0C3h)
          |0C4h: l(0C4h)  |0C5h: l(0C5h)  |0C6h: l(0C6h)  |0C7h: l(0C7h)
          |0C8h: l(0C8h)  |0C9h: l(0C9h)  |0CAh: l(0CAh)  |0CBh: l(0CBh)
          |0CCh: l(0CCh)  |0CDh: l(0CDh)  |0CEh: l(0CEh)  |0CFh: l(0CFh)
  
          |0D0h: l(0D0h)  |0D1h: l(0D1h)  |0D2h: l(0D2h)  |0D3h: l(0D3h)
          |0D4h: l(0D4h)  |0D5h: l(0D5h)  |0D6h: l(0D6h)  |0D7h: l(0D7h)
          |0D8h: l(0D8h)  |0D9h: l(0D9h)  |0DAh: l(0DAh)  |0DBh: l(0DBh)
          |0DCh: l(0DCh)  |0DDh: l(0DDh)  |0DEh: l(0DEh)  |0DFh: l(0DFh)

          |0E0h: l(0E0h)  |0E1h: l(0E1h)  |0E2h: l(0E2h)  |0E3h: l(0E3h)
          |0E4h: l(0E4h)  |0E5h: l(0E5h)  |0E6h: l(0E6h)  |0E7h: l(0E7h)
          |0E8h: l(0E8h)  |0E9h: l(0E9h)  |0EAh: l(0EAh)  |0EBh: l(0EBh)
          |0ECh: l(0ECh)  |0EDh: l(0EDh)  |0EEh: l(0EEh)  |0EFh: l(0EFh)
  
          |0F0h: l(0F0h)  |0F1h: l(0F1h)  |0F2h: l(0F2h)  |0F3h: l(0F3h)
          |0F4h: l(0F4h)  |0F5h: l(0F5h)  |0F6h: l(0F6h)  |0F7h: l(0F7h)
          |0F8h: l(0F8h)  |0F9h: l(0F9h)  |0FAh: l(0FAh)  |0FBh: l(0FBh)
          |0FCh: l(0FCh)  |0FDh: l(0FDh)  |0FEh: l(0FEh)  |0FFh: l(0FFh)
      ELSE l(100h)
      END;
      IF top()#i THEN ERROR("Illegal ENTC or XIT"); STOP(0BAh) END;
      i:=i+1;
    UNTIL i>100h;
    co:=co-1;
  UNTIL co=0;
END tst903;

VAR coFP: INTEGER;

PROCEDURE proc; BEGIN INC(coFP) END proc;

PROCEDURE tst904;
  VAR p: PROC;
   i,co: INTEGER;
      a: ADDRESS;
    dft: ADDRESS;
   save: INTEGER;

  PROCEDURE G(): ADDRESS; CODE lga 00 END G;

BEGIN
  TEST(904h,"CF"); INC(w02);
  IF w02#904h THEN ERROR(ORDER); STOP(90301h) END;
  (* create DFT for LPC *)
  a:=G()-1;     save:=a^;
  a^:=ADR(dft); dft:=G();

  co:=255; p:=proc;
  REPEAT
    i:=0; coFP:=0;
    REPEAT
      p; INC(i);
      IF i#coFP THEN ERROR("Illegal CF"); STOP(0CEh) END;
    UNTIL i>0FFh;
    co:=co-1;
  UNTIL co=0;
  a:=G()-1;     a^:=save;
END tst904;

BEGIN
  tst900; check4C;
  tst901; check4C;
  tst902; check4C;
  tst903; check4C;
  tst904; check4C;
  WriteLn;
END TEST9;

PROCEDURE MakeLoop5_6;
CODE cod.lgw 00 cod.lsw0 cod.jump END MakeLoop5_6;

PROCEDURE MakeLoop2;
CODE cod.lgw 00 cod.lsw0 cod.stot cod.xit END MakeLoop2;

BEGIN
  Show("KREST FINISHED");
  IF Debug THEN HALT END;
  IF CPU IN {5,6,7} THEN
    MakeLoop5_6
  ELSE
    MakeLoop2
  END;
END krest7.
