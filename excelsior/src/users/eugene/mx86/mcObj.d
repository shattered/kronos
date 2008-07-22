DEFINITION MODULE mcObj; (* Ned 06-Nov-87. (c) KRONOS *)

IMPORT   pc: pcTab;
IMPORT  ers: coErrors;

TYPE
  type_ptr   = POINTER TO type_rec;
  obj_ptr    = POINTER TO obj_rec;
  header_ptr = POINTER TO header_rec;

TYPE -- определение представления типовых значений

  TypeMode  = (  invtype
               , int, uint, bool, char, bitset, real, addr, word
               , enum, range, newrange, ptr
               , hidden, proctype, functype, settype
               , arr, farr, rec, dynarr, dyndesc
              );
  Types     = SET OF TypeMode;

  type_rec  = RECORD
                ref : INTEGER; -- номер в симфайле
                obj : obj_ptr;   (*  *)
                base: type_ptr;
                next: type_ptr;
                gen : pc.ref;
                CASE mode: TypeMode OF
                  | ptr   : tid : INTEGER; -- tid>0 for fwdptr
                  | arr   : inx : type_ptr;
                  | rec   : head: header_ptr;
                  | dynarr: desc: type_ptr;
                  | enum  : list: obj_ptr;
                  | proctype,functype:
                           plist: obj_ptr;
                (* ELSE nothing *)
                END;
              END;

TYPE -- определение представление заголовков областей действия --

  header_rec = RECORD
                 self  : obj_ptr;
                 locals: obj_ptr;
                 ctime : INTEGER;
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

TYPE        -- определение представления объектов --
  Mode      = ( inv
               ,cons
               ,econs
               ,vari
               ,field
               ,proc
               ,std_proc
               ,module
               ,type
               ,param
               ,expr            -- for parser
              );
  Modes     = SET OF Mode;

  obj_rec   = RECORD
                id   : INTEGER;
                host : header_ptr;  -- object scope
                next : obj_ptr;     -- in host list
                sake : obj_ptr;     -- next with same id (namesake)
                tags : BITSET;
                type : type_ptr;
                no   : INTEGER;     -- for proc, std_proc, module
                gen  : pc.ref;
                CASE mode: Mode OF
                  | econs      : list: obj_ptr;
                  | proc,module: head: header_ptr;
                END;
              END;

CONST
  SCALARs = Types{invtype,int,uint,bool,char,addr,enum,range,newrange};
  SIMPLEs = Types{MIN(TypeMode)..MAX(TypeMode)}-Types{arr,farr,rec,dynarr};
     INTs = Types{int,uint,addr};
     SETs = Types{bitset,settype};
     ARRs = Types{arr,farr,dynarr};
     DYNs = Types{farr,dynarr};
    PROCs = Types{proctype,functype};

VAL
  CurBlock : obj_ptr;  -- CurProc OR CurModule
  SuperProc: obj_ptr;
  Ilg      : obj_ptr;  -- illegal object
  Any      : type_ptr; -- illegal type
  externals: header_ptr;

VAR
  intp, uintp, charp, boolp, procp, realp, bitsetp, stringp: type_ptr;
  addrp, wordp: type_ptr;

---------------------  СОЗДАНИЕ ОБЪЕКТОВ  ---------------------
                     ---------------------

PROCEDURE new_obj(VAR o: obj_ptr; m: Mode);
PROCEDURE new_type(VAR t: type_ptr; m: TypeMode);

---------------------------  БЛОКИ  ---------------------------
                           ---------

PROCEDURE EnterBlock(block: obj_ptr);
(* Вход в блок (процедуру или модуль).
   Выполняется после описания имени процедуры и модуля.
*)

PROCEDURE ExitProc;
(* Выход из процедуры. Уничтожает локальные объекты. *)

TYPE Conflict = PROCEDURE ((*out:*) obj_ptr, (*in:*) obj_ptr);

PROCEDURE ExitModule(c: Conflict);
(* Выход из модуля.
   Вызывает процедуру разрешения конфликта, если
   снаружи виден экспортируемый объект.
   При этом экспорт не выполняется.
*)

-------------------------  ВИДИМОСТЬ  -------------------------
                         -------------

PROCEDURE dcl(id: INTEGER; obj: obj_ptr);
(* Объявлет объект видимым, в случае повторного описания *)
(* сообщает об ошибке и устанавливает вид объекта -inv-  *)

PROCEDURE dcl_in(scope: header_ptr; id: INTEGER; obj: obj_ptr);
(* Объявляет объект видимым в области scope (в модуле или записи) *)

PROCEDURE undo_dcl(o: obj_ptr);
(* Убирает объект из списка тезок. Объект становится невидим. *)

PROCEDURE vis?(id: INTEGER; VAR obj: obj_ptr): BOOLEAN;
(* Видим ли объект? *)

PROCEDURE vis(id: INTEGER): obj_ptr;
(* Ищет видимый объект; если не находит ругается *)
(* и создает его заменитель с видом -inv-        *)

PROCEDURE vis_in?(scope: header_ptr; id: INTEGER; VAR ob: obj_ptr): BOOLEAN;
PROCEDURE vis_in (scope: header_ptr; id: INTEGER): obj_ptr;

PROCEDURE vis_out(id: INTEGER): obj_ptr;
(* Ищет объект в объемлющем контексте. *)

------------------------ ИМПОРТ/ЭКСПОРТ ------------------------
-- При   транспорте   (импорте/экспорте)  перечислимого  типа --
-- транспортирует все его константы.                          --
----------------------------------------------------------------

PROCEDURE Import(o: obj_ptr; id: INTEGER);
(* Импорт объекта в текущий контекст под именем id *)

PROCEDURE FromImport(mdl: obj_ptr; id: INTEGER);
(* Импорт объекта с именем -id- из модуля -mdl-
   в текущий контекст.
*)

PROCEDURE Export(id: INTEGER);
(* Экспорт объекта с именем -id- из текущего контекста
   в объемлющий контекст.
*)

-----------------------  ВНЕШНИЕ МОДУЛИ  ----------------------
                       ------------------

PROCEDURE lookupModule(id: INTEGER; VAR mdl: obj_ptr): BOOLEAN;
(* Ищет модуль в списке внешних, если не находит,
   то создает новый и заносит в него очередной внешний номер.
   Сообщает об ошибке, если найденный объект не модуль и
   возвращает Ilg.
   TRUE, если новый.
*)

-----------------------  АТРИБУТЫ ТИПОВ  ----------------------
                       ------------------

PROCEDURE chkType(o: obj_ptr);
(* проверка на тип с руганью *)

PROCEDURE chkScalar(VAR t: type_ptr);
(* Проверка на скалярный тип с руганью *)

---------------------  КОНСТРУКТОРЫ ТИПОВ  --------------------
                     ----------------------

PROCEDURE set_hidden(hid,type: type_ptr);
(* Определяет реализацию скрытого типа *)

PROCEDURE MakeArr(inx,elem: type_ptr): type_ptr;
PROCEDURE MakeDynArr(elem: type_ptr; dim: INTEGER): type_ptr;
PROCEDURE MakeSet(base: type_ptr): type_ptr;
PROCEDURE MakeFlx(elem: type_ptr): type_ptr;
PROCEDURE MakePtr(base: type_ptr): type_ptr;
PROCEDURE MakeFwdPtr(id: INTEGER): type_ptr;
PROCEDURE MakeRange(base: type_ptr; mode: TypeMode): type_ptr;
PROCEDURE MakeHidden(size: INTEGER): type_ptr;

PROCEDURE MakeEnum(): type_ptr;
PROCEDURE AppEnum(t: type_ptr): obj_ptr;
(* MakeEnum { AppEnum } *)

PROCEDURE MakeRec(): type_ptr;
PROCEDURE AppField(r: type_ptr; id: INTEGER; f: type_ptr): obj_ptr;
(* MakeRec { AppField } *)

PROCEDURE MakeProcType(): type_ptr;
PROCEDURE AppParm(p: type_ptr; id: INTEGER; t: type_ptr; k: BITSET): obj_ptr;
PROCEDURE ProcType(proc: type_ptr);
PROCEDURE FuncType(proc,res: type_ptr);
(*
   MakeProcType; { AppParm } ( ProcType | FuncType )
*)

-----------------------  ИНИЦИАЛИЗАЦИЯ  -----------------------
                       -----------------

PROCEDURE Ini;
PROCEDURE Exi;

END mcObj.
