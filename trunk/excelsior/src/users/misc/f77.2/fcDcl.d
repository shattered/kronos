DEFINITION MODULE fcDcl;

FROM SYSTEM    IMPORT  ADDRESS, WORD;
FROM fcObj     IMPORT  Info, Idname, Types;

TYPE unit=(main, blockdata, function, subroutine, subrA);
     LHD = RECORD
             lo,hi,d:INTEGER;
           END;
     ALHD = ARRAY [0..6] OF LHD;

TYPE PEquiStr =POINTER TO EquiStr;
     PEquiObj =POINTER TO EquiObj;
     EquiStr  = RECORD
                  list:PEquiObj;
                  next:PEquiStr;
                END;
     EquiObj  = RECORD
                  name:INTEGER;
                  dim :INTEGER;
                  index:ADDRESS;
                  pos:INTEGER;
                  next:PEquiObj;
                END;

     PBlock = POINTER TO Block;
     Block  = RECORD
               name: Idname;   (* name of COMMON block
                                  name=BLANK! for blank block *)
               da : INTEGER;   (* number of block  *)
               FP : INTEGER;   (* idno first element  *)
               LP : INTEGER;   (* idno last  element  *)
               len: INTEGER;   (* length of block     *)
               next: PBlock;
              END;

     PDataStr = POINTER TO DataStr;
     PDataNames=POINTER TO DataNames;
     PDataConst=POINTER TO DataConst;

     DataStr = RECORD
                 next: PDataStr;
                 names:PDataNames;
                 consts:PDataConst;
               END;
     DataNames = RECORD
                   next: PDataNames;
                   idno : INTEGER;
                   nel  : INTEGER;
                   offset:INTEGER;
                 END;
     DataConst = RECORD
                   next: PDataConst;
                   nrep: INTEGER;
                   tp  : Types;
                   val : WORD;
                   val1: WORD;
                 END;

VAR PEquiv: PEquiStr;
    PlBlock:PBlock;
    DataList: PDataStr;

VAR ProcId: Idname;    (* Имя процедуры                  *)
    self:   unit;      (* Тип текущей единицы компиляции *)
    ParNo:  INTEGER;   (* Число параметров Func, Subr;   *)
    ModIdno:INTEGER;   (* idno для Func и Subr *)
    ProcNo: INTEGER;   (* Номер процедуры                *)
    procno: INTEGER;   (* Номер текущей процедуры        *)
    Loffs:  INTEGER;   (* Смещение в локальной области   *)
    Mapoffs:INTEGER;   (* Смещение до карты адресов      *)
  Maplength:INTEGER;   (* Длина карты адресов            *)
    Toffs:  INTEGER;   (* Смещение в области врем. перем.*)

PROCEDURE size(VAR I:Info):INTEGER;
PROCEDURE AllocVar(VAR I:Info);
PROCEDURE AllocExt(VAR I:Info);
PROCEDURE AllocIntr(VAR I:Info);
PROCEDURE declare;
(* Разбор всех описаний *)
PROCEDURE Entry;
(*
PROCEDURE InitDcl;
*)
END fcDcl.
