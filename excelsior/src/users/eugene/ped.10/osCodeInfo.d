DEFINITION MODULE osCodeInfo; (* Leo 01-Feb-88. (c) KRONOS *)

FROM SYSTEM    IMPORT   ADDRESS, WORD;

(*
        Модуль    призван    скрыть    от    разработчиков
   компиляторов,  загрузчиков  и комплексаторов конкретное
   устройство  кодофайла.
        В  его терминах кодофайл - это непрерывная область
   памяти,  указатель на которую и является значением типа
   "CodePtr".  Чтение  и  запись  этой  области  памяти на
   внешние  носители  не  входит  в  задачи этого модуля и
   обеспечивается внешним образом.
        В    начале    кодофайла   располагается   область
   атрибутов. Расположение остальных должно быть такое:

         <атрибуты> <строковый пул> <код>  <внешние связи>

        Первой  строкой  в  <строковом пуле> ДОЛЖНО лежать
   имя   модуля,  завершенное  хотя  бы  одним  байтом  со
   значением 0c.
        Версия  кодофайла имеет внутрисистемное значение и
   изменяется  только  в  случае  существенных изменений в
   модуле CodePtr с целью проконтролировать перекомпиляцию
   всех   кодофайлов.   (Загрузчик   отказывается  грузить
   кодофайлы с устаревшей версией).

*)


TYPE CodePtr = ADDRESS;

     String32 = ARRAY [0..31] OF CHAR;
     String4  = ARRAY [0.. 3] OF CHAR;
     StrPtr   = POINTER TO String32;

     Info = RECORD
              version: [0..255];  (* версия кодофайла                   *)
                 name: StrPtr;    (* указатель на имя                   *)
             compiler: String4;   (* имя компилятора                    *)
              defTime: INTEGER;   (* время компиляции definition        *)
              impTime: INTEGER;   (* время компиляции implementation    *)
               noproc: [0..255];  (* число процедур                     *)
                noglo: [0..255];  (* число глобалов ( + GW0,GW1)        *)
                noext: [0..255];  (* число внешних  ( + self)           *)
               strOfs: INTEGER;   (* смещение до строкового пула        *)
              codeOfs: INTEGER;   (* смещение до кода                   *)
               extOfs: INTEGER;   (* смещение до внешних имен           *)
                 size: INTEGER;   (* размер кодофайла в словах    !!!   *)
               stack0: INTEGER;   (* мин. стек для глоб. мультизначений *)
               addstk: INTEGER;   (* дополнительный стек                *)
              nextOfs: INTEGER;   (* ссылка на следующий кодофайл !!    *)
           END;

           (*       Все смещения в словах, относительно CodePtr         *)
           (* !!!   Размер кодофайла НЕ ХРАНИТСЯ а выставляется на      *)
           (*       стадии загрузки!                                    *)
           (* !!    Если nextOfs>0 нам попался пакет кодофайлов!        *)

PROCEDURE currentVersion(): INTEGER;
(* актуальная на данный момент версия кодофала *)

PROCEDURE version(code: CodePtr): INTEGER;
(*               Возвращает версию кодофайла               *)

PROCEDURE getInfo(code: CodePtr; VAR info: Info);
(*            Распаковывает атрибуты кодофайла.            *)

PROCEDURE putInfo(code: CodePtr; VAR info: Info);
(*             Упаковывает атрибуты кодофайла.             *)

PROCEDURE default(VAR info: Info);
(* выдает расписанное по умолчанию Info *)

(* ------------------------------------------------------- *)
(* Полезные    процедуры   используемые   при   работе   с *)
(* загруженным модулем:                                    *)
(* ------------------------------------------------------- *)

PROCEDURE Code(G: ADDRESS): ADDRESS;
(* По  глобальной  области  модуля выдает указатель на его *)
(* кодовый сегмент.                                        *)

PROCEDURE getSize(C: ADDRESS): INTEGER;
PROCEDURE putSize(C: ADDRESS; size: INTEGER);
(* выставление/опрос размера кодофайла                     *)

PROCEDURE ModuleName(G: ADDRESS): StrPtr;
(*           По адресу входа выдает имя модуля.            *)

PROCEDURE noExternals(code: CodePtr): INTEGER;

PROCEDURE noGlobals(code: CodePtr): INTEGER;

PROCEDURE defTime(code: CodePtr): INTEGER;

PROCEDURE impTime(code: CodePtr): INTEGER;

PROCEDURE FirstExternal(code: CodePtr; VAR name: StrPtr; VAR deftime: INTEGER);
(* Устанавливает  -name-  на  имя  первого (с максимальным *)
(* номером)  внешнего модуля. Возвращает в -deftime- время *)
(* компиляции его определения.                             *)

PROCEDURE NextExternal(code: CodePtr; VAR name: StrPtr; VAR deftime: INTEGER);
(* Устанавливает -name- на имя следуюшего внешнего модуля. *)
(* Возвращает    в    -deftime-   время   компиляции   его *)
(* определения.                                            *)

PROCEDURE Entry(G: ADDRESS): ADDRESS;
(* По  глобальной  области модуля выдает адрес его входа в *)
(* DFT.                                                    *)

PROCEDURE ProcTableAddress(code: CodePtr): ADDRESS;
(*            Выдает адрес процедурной таблицы.            *)

PROCEDURE StringsAddress(code: CodePtr): ADDRESS;
(*            Выдает адрес сторокового пула.               *)

PROCEDURE CodeName(code: CodePtr): StrPtr;
(*            Выдает имя кодофайла.                        *)

END osCodeInfo.
