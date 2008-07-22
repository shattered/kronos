DEFINITION MODULE mxObj; (* Ned 06-Nov-87. (c) KRONOS *)

TYPE
  TypePtr   = POINTER TO TypeVal;
  ObjPtr    = POINTER TO Obj;
  ParmPtr   = POINTER TO Parameter;
  header_ptr= POINTER TO header_rec;

TYPE -- определение представления типовых значений

  TypeMode  = (  invtype
               , int, bool, char, bitset, real, addr, word
               , enum, rang, ptr
               , hidden, proctype, functype, settype
               , arr, farr, rec, dynarr
              );
  Types     = SET OF TypeMode;

  TypeVal   = RECORD
                ref : INTEGER; -- номер в симфайле
                obj : ObjPtr;   (*  *)
                next: TypePtr;
                base: TypePtr;
                size: INTEGER;  (* max for rang *)
                CASE mode: TypeMode OF
                  | rang  : min : INTEGER;
                  | ptr   : tid : INTEGER; -- tid>0 for fwdptr
                  | arr   : inx : TypePtr;
                  | rec   : head: header_ptr;
                  | dynarr: desc: TypePtr;
                  | enum  : list: ObjPtr;
                  | proctype,functype: plist: ParmPtr;
                (* ELSE nothing *)
                END;
              END;

TYPE -- определение представление заголовков областей действия --

  header_rec  = RECORD
                self  : ObjPtr;
                locals: ObjPtr;
                ctime : INTEGER;
                extno : INTEGER;
                bound : TypePtr;  -- граница описания "своих" типов
                next  : header_ptr;
              END;

CONST              -- признаки объектов --
  varpar    =  0;  -- var   parameter
  seqpar    =  1;  -- seq   parameter
  RO        =  2;  -- read only for var & parameter
  forward   =  3;  -- forward declaration
  qualified =  4;  -- qualified export
  code_proc =  5;  --
  penetrate =  6;  -- penetrate visibile object
  exported  =  7;  -- объект указан в списке экспорта
  duplicate =  8;  -- дубль: импортирован или экспортирован
  complete  =  9;  -- полностью прочитанный внешний модуль
  undefined = 10;  -- недоопределенный объект

  private    = 16;  -- private generation tags [16..31]

TYPE        -- определение представления объектов --
  Mode      = (inv, cons, econs, vari, field, proc, std_proc, modul, typ);
  Modes     = SET OF Mode;

  Obj       = RECORD
                id   : INTEGER;
                host : header_ptr; -- object scope
                next : ObjPtr;     -- in host list
                sake : ObjPtr;     -- next with same id (namesake)
                tags : BITSET;
                type : TypePtr;
                scope: INTEGER;    -- extno or level
                addr : INTEGER;
                CASE mode: Mode OF
                  | econs     : list: ObjPtr;
                  | proc,modul: head: header_ptr;
                  | std_proc  : no  : INTEGER;
                END;
              END;

TYPE       -- определение представления параметра --
  Parameter = RECORD
                  id: INTEGER;
                type: TypePtr;
                kind: BITSET;     -- {} для параметра по значению
                addr: INTEGER;
                next: ParmPtr;
              END;

CONST
   StandTypes = Types{invtype,int,bool,char,bitset,real,addr,word};
  ScalarTypes = Types{invtype,int,bool,char,addr,enum,rang};
  SimpleTypes = Types{MIN(TypeMode)..MAX(TypeMode)}-Types{arr,farr,rec,dynarr};
         INTs = Types{int,addr};
         PTRs = Types{addr,word,ptr};
         SETs = Types{bitset,settype};
         ARRs = Types{arr,farr,dynarr};
         DYNs = Types{farr,dynarr};
        PROCs = Types{proctype,functype};

VAL
  CurBlock : ObjPtr;  -- CurProc OR CurModule
  SuperProc: ObjPtr;
  Ilg      : ObjPtr;  -- illegal object
  Any      : TypePtr; -- illegal type
  extNo    : INTEGER; -- число внешних

VAR
  intp, charp, boolp, procp, realp, bitsetp, stringp: TypePtr;
  addrp, wordp: TypePtr;

---------------------  СОЗДАНИЕ ОБЪЕКТОВ  ---------------------
                     ---------------------

PROCEDURE NewObj (VAR o: ObjPtr; m: Mode);

PROCEDURE NewType(VAR t: TypePtr; m: TypeMode);
PROCEDURE RemType(VAR t: TypePtr);

---------------------------  БЛОКИ  ---------------------------
                           ---------

PROCEDURE EnterBlock(block: ObjPtr);
(* Вход в блок (процедуру или модуль).
   Выполняется после описания имени процедуры и модуля.
*)

PROCEDURE ExitProc;
(* Выход из процедуры. Уничтожает локальные объекты. *)

TYPE Conflict = PROCEDURE ((*out:*) ObjPtr, (*in:*) ObjPtr);

PROCEDURE ExitModule(c: Conflict);
(* Выход из модуля.
   Вызывает процедуру разрешения конфликта, если
   снаружи виден экспортируемый объект.
   При этом экспорт не выполняется.
*)

PROCEDURE locals(): ObjPtr;
(* Возврашает список локалов текущего контекста *)

-------------------------  ВИДИМОСТЬ  -------------------------
                         -------------

PROCEDURE Dcl(id: INTEGER; obj: ObjPtr);
(* Объявлет объект видимым, в случае повторного описания *)
(* сообщает об ошибке и устанавливает вид объекта -inv-  *)

PROCEDURE DclInScope(scope: header_ptr; id: INTEGER; obj: ObjPtr);
(* Объявляет объект видимым в области scope (в модуле или записи) *)

PROCEDURE undoDcl(o: ObjPtr);
(* Убирает объект из списка тезок. Объект становится невидим. *)

PROCEDURE Vis?(id: INTEGER; VAR obj: ObjPtr): BOOLEAN;
(* Видим ли объект? *)

PROCEDURE Vis(id: INTEGER): ObjPtr;
(* Ищет видимый объект; если не находит ругается *)
(* и создает его заменитель с видом -inv-        *)

PROCEDURE VisInScope?(scope: header_ptr; id: INTEGER; VAR ob: ObjPtr): BOOLEAN;
(* Ищет объект с указанным именем в контексте -scope-. *)

PROCEDURE VisInScope(scope: header_ptr; id: INTEGER): ObjPtr;
(* Соотносится с  -VisInScope?- как -Vis- и -Vis?-.  *)
(* В случае ошибки включает invalid объект в список. *)

PROCEDURE VisOut(id: INTEGER): ObjPtr;
(* Ищет объект в объемлющем контексте. *)

------------------------ ИМПОРТ/ЭКСПОРТ ------------------------
-- При   транспорте   (импорте/экспорте)  перечислимого  типа --
-- транспортирует все его константы.                          --
----------------------------------------------------------------

PROCEDURE Import(o: ObjPtr; id: INTEGER);
(* Импорт объекта в текущий контекст под именем id *)

PROCEDURE FromImport(mdl: ObjPtr; id: INTEGER);
(* Импорт объекта с именем -id- из модуля -mdl-
   в текущий контекст.
*)

PROCEDURE Export(id: INTEGER);
(* Экспорт объекта с именем -id- из текущего контекста
   в объемлющий контекст.
*)

-----------------------  ВНЕШНИЕ МОДУЛИ  ----------------------
                       ------------------

PROCEDURE lookupModule(id: INTEGER; VAR mdl: ObjPtr);
(* Ищет модуль в списке внешних, если не находит,
   то создает новый и заносит в него очередной внешний номер.
   Сообщает об ошибке, если найденный объект не модуль и
   возвращает Ilg.
*)

TYPE iterProc = PROCEDURE ((*mdl:*) ObjPtr);

PROCEDURE iterModules(ip: iterProc);
(* Итератор внешних модулей *)

-----------------------  АТРИБУТЫ ТИПОВ  ----------------------
                       ------------------

PROCEDURE chkType(o: ObjPtr);
(* проверка на тип с руганью *)

PROCEDURE Scalar?(t: TypePtr): BOOLEAN;
PROCEDURE Simple?(t: TypePtr): BOOLEAN;
(* Проверки на скалярный и однословный тип *)

PROCEDURE chkScalar(VAR t: TypePtr);
PROCEDURE chkSimple(VAR t: TypePtr);
(* Проверки на скалярный и однословный тип с руганью *)

PROCEDURE Char?(t: TypePtr): BOOLEAN;
(* TRUE - если тип литерный или отрезок литерного типа *)

PROCEDURE LoHi(t: TypePtr; VAR lo,hi: INTEGER);
(* Возвращает верхнию и нижнюю границу скалярного типа *)

PROCEDURE tsize(t: TypePtr): INTEGER;
(* Размер типа *)

---------------------  КОНСТРУКТОРЫ ТИПОВ  --------------------
                     ----------------------

PROCEDURE set_hidden(hid,type: TypePtr);
(* Определяет реализацию скрытого типа *)

PROCEDURE MakeArr(inx,elem: TypePtr): TypePtr;
PROCEDURE MakeDynArr(elem: TypePtr; dim: INTEGER): TypePtr;
PROCEDURE MakeSet(base: TypePtr): TypePtr;
PROCEDURE MakeFlx(elem: TypePtr): TypePtr;
PROCEDURE MakePtr(base: TypePtr): TypePtr;
PROCEDURE MakeFwdPtr(id: INTEGER): TypePtr;
PROCEDURE MakeRange(base: TypePtr; lo,hi: INTEGER): TypePtr;
PROCEDURE MakeHidden(size: INTEGER): TypePtr;

PROCEDURE MakeEnum(): TypePtr;
PROCEDURE AppEnum(t: TypePtr; val: INTEGER): ObjPtr;
(* MakeEnum { AppEnum } *)

PROCEDURE MakeRec(): TypePtr;
PROCEDURE AppField(rec: TypePtr; id: INTEGER; f: TypePtr; ofs: INTEGER);
(* MakeRec { AppField } *)

PROCEDURE MakeProcType(): TypePtr;
PROCEDURE AppParm(proc: TypePtr; id: INTEGER; t: TypePtr; kind: BITSET): ParmPtr;
PROCEDURE ProcType(proc: TypePtr);
PROCEDURE FuncType(proc,res: TypePtr);
(*
   MakeProcType; { AppParm } ( ProcType | FuncType )
*)

--------------------  СОВМЕСТИМОСТЬ ТИПОВ  --------------------
                    -----------------------

PROCEDURE TypeCmp(t1,t2: TypePtr);
PROCEDURE AsgCmp (t1,t2: TypePtr); -- t1:=t2
PROCEDURE ProcCmp(t1,t2: TypePtr);
(* Проверяют совместимость типов. *)

PROCEDURE ProcCmp?(t1,t2: TypePtr): INTEGER;
(* Не ругается, а выдает номер ошибки, или 0 если OK *)

-----------------------  ИНИЦИАЛИЗАЦИЯ  -----------------------
                       -----------------

PROCEDURE Ini;
PROCEDURE Exi;

END mxObj.
