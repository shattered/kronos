MODULE for;

FROM StdIO  IMPORT Show , print;
FROM Args   IMPORT ScanFlags, Flag?, TakeWord;
FROM fcScan IMPORT InitZeroScan, InitScan, NewLine, ErrorCo,
                   BuffIsEmpty, KeyON, Error ;
FROM fcObj  IMPORT VisIdTable;
FROM fcHeap IMPORT ReleaseHeap;
FROM fcDcl  IMPORT declare, ProcNo, Loffs, Mapoffs,Maplength,
                   Toffs, ModIdno, self, unit, ProcId ;
FROM fcStm  IMPORT Statement, InitStm, InitLabels, EndUnit,
                   FinishCheck;
FROM objFile IMPORT CreateObj, CloseObj, pEndproc, EOF, pTag;
FROM fcGen   IMPORT InitPool, pSpool,InitGen;
FROM FileNames  IMPORT LastPart,ChangeExt;

VAR  fn,sou,last: ARRAY [0..79] OF CHAR;
     errors:INTEGER;

PROCEDURE Fpstart;
BEGIN
  InitZeroScan(sou);
  InitGen(last);
  InitPool(last);
  CreateObj(last);
  errors:=0; ProcNo:=1;
END Fpstart;

PROCEDURE Fpfinish;
BEGIN
--   IF errors=0 THEN
     pSpool; pTag(EOF);
     CloseObj;
--   END;
END Fpfinish;

PROCEDURE UnitParser;
  VAR templen:INTEGER;
BEGIN
  InitScan;
  InitLabels; InitStm;
  declare;
 -- IF KeyON("t") THEN VisIdTable; END;
  IF Flag?("t") THEN VisIdTable; END;
  LOOP
    Statement;
    IF EndUnit THEN
      templen:=Toffs-Mapoffs-Maplength;
-- print('Locals= %d Maplength= %d tempvars= %d \n',
--     Loffs,Maplength,templen);
      pEndproc(ProcId,Maplength,Loffs,templen); EXIT;
    END;
    IF self=blockdata  THEN Error(44); END;
    NewLine;
  END;
  FinishCheck; errors:=errors+ErrorCo;
  IF ErrorCo=0 THEN
    print("Нет ошибок \n");
  ELSE
    Show("*********************");
    print("Число ошибок %d\n",ErrorCo);
  END;
IF Flag?("t") THEN VisIdTable; END;
-- IF KeyON("t") THEN VisIdTable; END;
END UnitParser;

PROCEDURE Parser;
BEGIN
  Fpstart;
  REPEAT
    UnitParser;
    ReleaseHeap;
  UNTIL BuffIsEmpty;
  Fpfinish;
END Parser;

BEGIN
   ScanFlags;
   LOOP
     TakeWord(sou);
     IF sou[0]=0c THEN EXIT END;
     fn:=sou; LastPart(fn,last);
--   print(' source= %s  first= %s last= %s \n', sou,fn,last);
     Parser;
   END;

END for.
