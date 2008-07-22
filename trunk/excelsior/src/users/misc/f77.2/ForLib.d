DEFINITION MODULE ForLib; (* 05-Apr-89. (c) KRONOS *)

FROM SYSTEM  IMPORT ADDRESS, WORD;

TYPE ptostr= POINTER TO
             RECORD
               base:ADDRESS;
               offs:INTEGER;
               len :INTEGER;
             END;

VAR chtop :INTEGER;
    chbase:ADDRESS;

PROCEDURE Stop(cs:INTEGER; wd:WORD);                  -- 1
PROCEDURE Pause(cs:INTEGER; wd:WORD);                 -- 2
PROCEDURE ChIndex (str1,str2:ptostr):INTEGER;         -- 3
PROCEDURE FormDesc(VAR Darray:ARRAY OF INTEGER);      -- 4
PROCEDURE StrMove(str:ptostr);                        -- 5
PROCEDURE ChMove;                                     -- 6
PROCEDURE ChFree(top:INTEGER);                        -- 7
PROCEDURE ChComp(str1,str2:ptostr);                   -- 8
PROCEDURE ChCmp;                                      -- 9
PROCEDURE ChAssign(str1,str2:ptostr);                 -- 10

CONST pi = 3.1415926535898;

PROCEDURE integer(x: REAL): INTEGER;                  -- 11
PROCEDURE sqrt (x: REAL): REAL;                       -- 12
PROCEDURE exp  (x: REAL): REAL;                       -- 13
PROCEDURE ln   (x: REAL): REAL;                       -- 14
PROCEDURE sin  (x: REAL): REAL;                       -- 15
PROCEDURE cos  (x: REAL): REAL;                       -- 16
PROCEDURE atan (arg: REAL): REAL;                     -- 17
PROCEDURE atan2(arg1, arg2: REAL): REAL;              -- 18
PROCEDURE acos (x:REAL):REAL;                         -- 19
PROCEDURE asin (x:REAL):REAL;                         -- 20
PROCEDURE tan  (x:REAL):REAL;                         -- 21
PROCEDURE cosh (x:REAL):REAL;                         -- 22
PROCEDURE sinh (x:REAL):REAL;                         -- 23
PROCEDURE tanh (x:REAL):REAL;                         -- 24
PROCEDURE log10(x:REAL):REAL;                         -- 25

-- Complex functions --

PROCEDURE Cxabs (re,im:REAL):REAL;                    -- 26
PROCEDURE Cxsqrt(re,im:REAL);                         -- 27
PROCEDURE Cxlog (re,im:REAL);                         -- 28
PROCEDURE Cxexp (re,im:REAL);                         -- 29
PROCEDURE Cxsin (re,im:REAL);                         -- 30
PROCEDURE Cxcos (re,im:REAL);                         -- 31
PROCEDURE CxMul;                                      -- 32
PROCEDURE CxDiv;                                      -- 33

PROCEDURE Powerii;                                    -- 34
PROCEDURE Powerri;                                    -- 35
PROCEDURE Powerdi;                                    -- 36
PROCEDURE Powerci;                                    -- 37
PROCEDURE Powerrr;                                    -- 38
PROCEDURE Powerdd;                                    -- 39
PROCEDURE Powercc;                                    -- 40

-- temp proc --

PROCEDURE powri(r:REAL; i:INTEGER):REAL;              -- 41
PROCEDURE powrr(r,rn:REAL):REAL;                      -- 42

-- Min Max --

PROCEDURE Max0(i,j:INTEGER):INTEGER;                  -- 43
PROCEDURE Amax1(x,y:REAL):REAL;                       -- 44

PROCEDURE Min0(i,j:INTEGER):INTEGER;                  -- 45
PROCEDURE Amin1(x,y:REAL):REAL;                       -- 46
--  ttttt --
PROCEDURE Missing;                                    -- 47
--  ttttt --
PROCEDURE Dim(a,b:REAL):REAL;                         -- 48
PROCEDURE iDim(a,b:INTEGER):INTEGER;                  -- 49
PROCEDURE AMod(a,b:REAL):REAL;                        -- 50
PROCEDURE iSign(a,b:INTEGER):INTEGER;                 -- 51
PROCEDURE Sign(a,b:REAL):REAL;                        -- 52

END ForLib.
