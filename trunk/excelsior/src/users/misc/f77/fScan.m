IMPLEMENTATION MODULE fScan; (* Max *)

FROM SYSTEM    IMPORT   WORD, ADR, ADDRESS;
FROM KRONOS    IMPORT   MOVE, DROP;
FROM Args      IMPORT   TakeWord, ScanFlags, Flag?;
FROM fGen      IMPORT   codfile;
FROM FileNames IMPORT   ChangeExt;
FROM CONSTS    IMPORT   Delta, MaxReal;
IMPORT StdIO, fHeap;

CONST eol=0c; InSize=255;

TYPE Name = ARRAY [0..19] OF CHAR;
     FortranStr=ARRAY [0..InSize] OF CHAR;

VAR In: FortranStr;               (* Входная строка              *)
    No: INTEGER;                 (* ее счетчик                  *)
    ch: CHAR;                     (* Текущая литера              *)
    FilNm: Filename;              (* Имя входного потока         *)
    S: StdIO.Stream;              (* поток                       *)
    Mark,Mark1:INTEGER;          (* Номер маркированной позиции *)
    Go:BOOLEAN;                   (* Go=FALSE, если ch уже взята *)
    GoMark,GoMark1:BOOLEAN;       (* Go в момент маркирования    *)
    Line:INTEGER;                (* номер текущей строки        *)
    worktype: Work;               (* Режим работы                *)
    let?,dig?:ARRAY [0c..377c] OF CHAR;  (*IF BOOLEAN(let?[ch])..*)
    CALL:Idname;
    Debugg:BOOLEAN;
(* ------------------ H E A P --------------------- *)
MODULE HeapSupport;

FROM fHeap IMPORT Allocate, SetAbort, DeAllocate;
(*main*)  IMPORT ADDRESS, WORD, Fault;

EXPORT  Give, Free, InitHeap, ReleaseHeap;

CONST Wsz=6; CurSz=100;

VAR heap:ADDRESS;
    First:ARRAY [0..Wsz] OF WORD;
    Hfree,Sz:INTEGER;
    OS: ARRAY [0..99] OF ADDRESS; (* адреса взятых сегментов *)
    ns:INTEGER; (* счетчик OS *)

PROCEDURE Give(VAR p:ADDRESS; sz:INTEGER);
BEGIN
  IF First[sz]#NIL THEN
    (*$T-*) p:=First[sz]; First[sz]:=p^; (*$T+*)
    RETURN ;
  END;
  IF (Sz-Hfree) < sz THEN Sz:=CurSz;
    Allocate(heap,Sz); IF heap=NIL THEN Fault(13); END;
    OS[ns]:=heap; INC(ns);
    Hfree:=0;
  END;
  INTEGER(p):=INTEGER(heap)+Hfree; INC(Hfree,sz);
END Give;

PROCEDURE Free(VAR p:ADDRESS; sz:INTEGER);
BEGIN p^:=First[sz]; (*$T-*) First[sz]:=p; (*$T+*) p:=NIL; END Free;

PROCEDURE InitHeap;
  VAR i: INTEGER;
BEGIN Hfree:=0; SetAbort(FALSE); Sz:=CurSz;
  Allocate(heap,Sz); IF heap=NIL THEN Fault(13); END;
  OS[0]:=heap; ns:=1;
  FOR i:=1 TO Wsz DO First[i]:=NIL; END
END InitHeap;

PROCEDURE ReleaseHeap;
  VAR i: INTEGER;
BEGIN Sz:=CurSz;
  FOR i:=0 TO ns-1 DO DeAllocate(OS[i],Sz); END;
END ReleaseHeap;

END HeapSupport;
(* ------------- E N D   H E A P ---------------------- *)
(* ------------- I D E N T T A B L E ------------------ *)
MODULE IdentTable;

IMPORT StdIO;
IMPORT Default, Idname, eol, Fault, Symbol, NoIds,
       ADDRESS, MOVE, ADR, Mode, Types, Give, Free,
       GetType, SetType, SetMode, GetMode;
EXPORT IdStr, StrId, DelId, Find, InitId, WhatSy?, VisIdTable;

VAR I:ARRAY [0..NoIds] OF ADDRESS;
    Isymb: ARRAY [0..NoIds] OF CHAR;
    Ip: POINTER TO Idname;

VAR Col,Sum,Len:INTEGER; C:CHAR;

PROCEDURE Hash(VAR IdName:Idname);
BEGIN
  Sum:=0; Len:=0;
  REPEAT  C:=IdName[Len];
    Sum := INTEGER(BITSET(Sum)/BITSET(C<<Len));
    INC(Len);
  UNTIL C = eol;
  Sum := INTEGER(BITSET(Sum)*{0..8});
END Hash;

PROCEDURE StrId(VAR IdName:Idname):INTEGER;
VAR R:INTEGER;
BEGIN (*$T-*)
  Hash(IdName); Len:=(Len+3) DIV 4; R:=13;
  LOOP
    IF I[Sum]=NIL THEN
      Give(I[Sum],Len); C:=Default[IdName[0]];
      SetType(Sum,Types(ORD(C)));
      MOVE(I[Sum],ADR(IdName),Len); RETURN Sum;
    END;
    Ip:=I[Sum];
    IF IdName=Ip^ THEN RETURN Sum; END;
    Sum := (Sum+R) MOD NoIds;  R := (R+Sum) MOD NoIds;
    INC(Col);
  END; (*$T+*)
END StrId;

PROCEDURE Find(VAR IdName:Idname):INTEGER;
  VAR R:INTEGER;
BEGIN (*$T-*)
  Hash(IdName); R:=13;
  LOOP
    IF I[Sum]=NIL THEN RETURN NoIds;
    ELSE Ip:=I[Sum];
      IF IdName=Ip^ THEN RETURN Sum; END;
      Sum:=(Sum+R) MOD NoIds;  R := (R+Sum) MOD NoIds;
    END;
  END; (*$T+*)
END Find;

PROCEDURE IdStr(d:INTEGER;VAR IdName:Idname);
BEGIN
  IF I[d]=NIL THEN IdName[0]:=177c; IdName[1]:=eol; RETURN; END;
  Ip:=I[d]; Len:=0;
  WHILE Ip^[Len]#eol DO INC(Len); END;
  INC(Len); Len:=(Len+3) DIV 4;
  MOVE(ADR(IdName),I[d],Len);
END IdStr;

PROCEDURE DelId(d:INTEGER);
BEGIN
  IF Symbol(ORD(Isymb[d]))=invKW THEN
    Ip:=I[d]; Len:=0;
    WHILE Ip^[Len]#eol DO INC(Len); END;
    INC(Len); Len:=(Len+3) DIV 4;
    Free(I[d],Len);
  END;
END DelId;

PROCEDURE MakeKW(KW:Idname;s:Symbol);
  VAR i: INTEGER;
BEGIN
  i:=StrId(KW); Isymb[i]:=CHAR(ORD(s));
END MakeKW;

PROCEDURE WhatSy?(d:INTEGER):Symbol;
BEGIN
  RETURN Symbol(ORD(Isymb[d]));
END WhatSy?;

PROCEDURE InitId;
VAR h: INTEGER;
BEGIN Col:=0;
  FOR h:=0 TO HIGH(I) DO
    I[h]:=NIL; Isymb[h]:=CHAR(ORD(invKW));
  END;
  MakeKW('DO'       ,do   );   MakeKW('GOTO'         ,goto );
  MakeKW('IF'       ,if   );   MakeKW('CONTINUE'     ,cnt  );
  MakeKW('THEN'     ,then );   MakeKW('ELSE'         ,else );
  MakeKW('ELSEIF'   ,elsif);   MakeKW('ENDIF'        ,endif);
  MakeKW('END'      ,end  );   MakeKW('RETURN'       ,rtn  );
  MakeKW('STOP'     ,stp  );   MakeKW('REAL'         ,real );
  MakeKW('INTEGER'  ,int  );   MakeKW('LOGICAL'      ,log  );
  MakeKW('WRITE'    ,wr   );   MakeKW('READ'         ,rd   );
  MakeKW('PRINT'    ,pr   );   MakeKW('FORMAT'       ,frm  );
  MakeKW('DIMENSION',dim  );   MakeKW('LT'           ,lt   );
  MakeKW('LE'       ,le   );   MakeKW('EQ'           ,eq   );
  MakeKW('NE'       ,ne   );   MakeKW('OR'           ,or   );
  MakeKW('GE'       ,ge   );   MakeKW('GT'           ,gt   );
  MakeKW('AND'      ,and  );   MakeKW('NOT'          ,not  );
  MakeKW('SUBROUTINE',subr);   MakeKW('FUNCTION'     ,func );
  MakeKW('IMPLICIT' ,impl );   MakeKW('COMMON'       ,comm );
  MakeKW('DATA'     ,data );   MakeKW('CHARACTER'    ,char );
  MakeKW('PARAMETER',param);   MakeKW('CALL'         ,call );
  MakeKW('FALSE'    ,false);   MakeKW('TRUE'         ,true );
  MakeKW('FORWARD'  ,fwrd );   MakeKW('PROGRAM'      ,prog );
  MakeKW('EXTERNAL' ,ext  );   MakeKW('ASSIGN'       ,asgn );
  MakeKW('EQUIVALENCE',equi);
END InitId;

PROCEDURE VisIdTable;
  VAR IdName:Idname; h:INTEGER;
BEGIN
  FOR h:=0 TO HIGH(I) DO
  IF GetMode(h)#Empty THEN
    IdStr(h,IdName); StdIO.print("%8d %s",h,IdName);
    CASE GetType(h) OF
     Int  : StdIO.WriteString(" Int  ");
    |Real : StdIO.WriteString(" Real ");
    |Logic: StdIO.WriteString(" Logic");
    |Char : StdIO.WriteString(" Char ");
    |Undef: StdIO.WriteString(" Undef");
    END;
    CASE GetMode(h) OF
     Var    : StdIO.WriteString(" Var");
    |Func   : StdIO.WriteString(" Func");
    |Subr   : StdIO.WriteString(" Subr");
    |LocFunc: StdIO.WriteString(" LocFunc");
    |Const  : StdIO.WriteString(" Const");
    |Array  : StdIO.WriteString(" Array");
    |LocVar : StdIO.WriteString(" LocVar");
    END;
    IF Isymb[h]#CHAR(ORD(invKW)) THEN StdIO.WriteString(" --KW-- "); END;
    StdIO.WriteLn;
  END;
  END; (* for-cycle *)
  StdIO.print('Number of Collisions %d\n',Col);
END VisIdTable;

END IdentTable;
(* ---------------E N D   I D E N T T A B L E --------------- *)
(* --------------- O B J E C T S ---------------------------- *)

MODULE Objects;

IMPORT Mode, Types, Give, Free, NoIds, IdStr, Idname,
       Default, Info, ADDRESS, WORD, Fault, Symbol;
EXPORT SetType, GetType, SetMode, GetMode, InitObjects,
       GenObj, Unpack, DelObj, SaveO, RestoreO, SetDefaultTp;

TYPE P =POINTER TO so;
     so=RECORD
          idn:INTEGER;
          tp:Types;
          md:Mode;
          pnt:ADDRESS;
          next:P
        END;

VAR Itype: ARRAY [0..NoIds] OF CHAR;
    Imode: ARRAY [0..NoIds] OF CHAR;
    Object:ARRAY [0..NoIds] OF ADDRESS; (*указатели на упакованные об'екты*)
    SO:P;

PROCEDURE SetMode(d:INTEGER;M:Mode);
BEGIN Imode[d]:=CHAR(ORD(M)); END SetMode;

PROCEDURE GetMode(d:INTEGER):Mode;
BEGIN RETURN Mode(ORD(Imode[d])); END GetMode;

PROCEDURE SetType(d:INTEGER;T:Types);
BEGIN Itype[d]:=CHAR(ORD(T)); END SetType;

PROCEDURE GetType(d:INTEGER):Types;
BEGIN RETURN Types(ORD(Itype[d])); END GetType;

PROCEDURE SetDefaultTp(d:INTEGER);
  VAR IdName:Idname; c:CHAR;
BEGIN
  IdStr(d,IdName); c:=IdName[0];
  SetType(d,Types(ORD(Default[c])));
END SetDefaultTp;

PROCEDURE InitObjects;
VAR h: INTEGER; C:CHAR;
BEGIN
  SO:=NIL;
  FOR h:=0 TO HIGH(Itype) DO
    Object[h]:=NIL; Itype[h]:=CHAR(ORD(Undef));
    Imode[h]:=CHAR(ORD(Empty));
  END;
  FOR C:=0c TO 377c DO Default[C]:=CHAR(ORD(Real)); END;
  FOR C:="I" TO "N" DO Default[C]:=CHAR(ORD(Int));   END;
END InitObjects;

PROCEDURE GenObj(I:Info);
  VAR p,p1:ADDRESS; w:WORD;
BEGIN
  CASE GetMode(I.name) OF
    Var      : w:=I.offset; IF I.param THEN INCL(BITSET(w),31); END;
               Give(p,1); p^:=w;
   |Func,Subr: w:=I.offset; Give(p,2); p^:=w;
               INTEGER(p1):=INTEGER(p)+1; w:=I.parlist; p1^:=w;
   |LocFunc  : w:=BITSET(I.offset)+BITSET(I.dimension>>8);
               Give(p,2); p^:=w; INTEGER(p1):=INTEGER(p)+1;
               w:=I.partypes; p1^:=w;
   |Const    : Give(p,1); p^:=I.offset;
   |Array    : w:=I.offset; IF I.param THEN INCL(BITSET(w),31); END;
               Give(p,3); p^:=w;  INTEGER(p1):=INTEGER(p)+1;
               w:=I.desc; p1^:=w; INTEGER(p1):=INTEGER(p)+2;
               w:=BITSET(I.abs)+BITSET(I.dimension>>4); p1^:=w;
   |Empty    : Fault(14);
   |LocVar   : w:=I.offset; Give(p,1); p^:=w;
  END;
  Object[I.name]:=p;
END GenObj;

PROCEDURE Unpack(VAR I:Info);
  VAR p:ADDRESS; w:WORD; d:INTEGER;
BEGIN
  d:=I.name; p:=Object[d];
  CASE GetMode(d) OF
    Var      : w:=p^; I.param:=(31 IN BITSET(w));
               I.offset:=INTEGER(BITSET(w)*{0..30});
   |Func,Subr: I.offset:=p^; INTEGER(p):=INTEGER(p)+1; w:=p^;
               I.parlist:=ADDRESS(w);
   |LocFunc  : w:=p^; I.offset:=INTEGER(BITSET(w)*{0..15});
               I.dimension:=INTEGER(BITSET(w<<8)*{0..7});
               INTEGER(p):=INTEGER(p)+1; w:=p^;
               I.partypes:=ADDRESS(w);
   |Const    : I.offset:=p^;
   |Array    : w:=p^; I.param:=(31 IN BITSET(w));
               I.offset:=INTEGER(BITSET(w)*{0..30});
               INTEGER(p):=INTEGER(p)+1; w:=p^;
               I.desc:=ADDRESS(w);
               INTEGER(p):=INTEGER(p)+1; w:=p^;
               I.dimension:=INTEGER(BITSET(w<<4)*{0..3});
               I.abs:=INTEGER(BITSET(w)*{0..27});
   |Empty    : Fault(15);
   |LocVar   : w:=p^; I.offset:=INTEGER(w);
  END;
END Unpack;

PROCEDURE DelObj(d:INTEGER);
  VAR m:Mode;
BEGIN
  IF Object[d]=NIL THEN Fault(16); END;
  m:=Mode(ORD(Imode[d])); SetMode(d,Empty);
  CASE m OF
    Var,Const,LocVar :Free(Object[d],1);
   |LocFunc,Subr,Func:Free(Object[d],2);
   |Array            :Free(Object[d],3);
  END;
END DelObj;

PROCEDURE RestoreO;
  VAR p,p1:P; idn:INTEGER;
BEGIN
  p:=SO; SO:=NIL;
  WHILE p#NIL DO
    idn:=p^.idn;
    DelObj(idn);
    SetType(idn,p^.tp);
    SetMode(idn,p^.md);
    Object[idn]:=p^.pnt;
    p1:=p; p:=p^.next;
    Free(p1,TSIZE(so));
  END;
END RestoreO;

PROCEDURE SaveO(i:INTEGER);
  VAR p:P;
BEGIN
  Give(p,TSIZE(so));
  p^.idn :=i;
  p^.tp  :=GetType(i);
  p^.md  :=GetMode(i);
  p^.pnt :=Object[i];
  p^.next:=SO; SO:=p;
END SaveO;

END Objects;
(* --------------- E N D  O B J E C T S --------------------- *)

VAR St:Idname; (* для визуализации *)

(* --------------------- E R R O R S ------------------------ *)
MODULE ERRORS;

(*FROM Main*) IMPORT St, IdStr, Idname, In, No, eol, ErrorCo,
                     Symbol, idno, WhatSy?, GetSy, sy, Ival, Line;
FROM StdIO    IMPORT Show, WriteString, Write, WriteLn, print;

EXPORT Error, Fault, VisSy, ErrorId, Expected, Warning, FAULT;

CONST ErrorLimit=6;
VAR E:ARRAY [0..79] OF CHAR;

PROCEDURE ShowErrorLine;
  VAR i: INTEGER; Limit:INTEGER;
BEGIN
  print("*%d*",Line);
  IF In[No-1]=eol THEN  DEC(No); END;
  IF No<=70 THEN Limit:=No-1 ELSE Limit:=70; END;
  FOR i:=0 TO Limit DO Write(In[i]); END; Write(177c);
  IF Limit#70 THEN i:=No;
    WHILE (i<70) AND (In[i]#eol) DO Write(In[i]); INC(i); END;
  END;
  WriteLn;
  IF ErrorCo>=ErrorLimit THEN
    Show("**********************************");
    Show("  Слишком много ошибок."); HALT;
  END;
END ShowErrorLine;

PROCEDURE Warning(n:INTEGER);
  VAR N:Idname;
BEGIN
  WriteString("*** Внимание ! ");
  CASE n OF
   00: Show("Параметров при вызове меньше,чем описано");
  |01: Show("Переменная впервые появляется");
       WriteString("не в  левой части оператора присваивания:");
       IdStr(idno,N); Show(N);
  ELSE Show("invalid WORNING"); END;
  ShowErrorLine;
END Warning;

PROCEDURE Error(n:INTEGER);
BEGIN INC(ErrorCo);
  CASE n OF
   00: E:="Незнакомая литера:"
  |01: E:="Строковая константа не заключена в кавычки:"
  |02: E:="Ожидалось отношение либо логическая операция(константа)"
  |03: E:="Ожидалась точка:"
  |04: E:="Неправильная метка"
  |05: E:="Слишком длинный идентификатор"
  |06: E:="В поле метки не цифра:"
  |07: E:="Тип выражения должен быть логический"
  |08: Show("Передача управления внутрь тела DO-цикла,");
       E:="IF - ,ELSEIF - ,ELSE-блоков запрещена."
  |09: E:="Дважды встречалась одна метка"
  |10: E:="Метка в разделе описаний"
  |11: E:="Неправильный оператор"
  |12: E:="Недопустимая операция"
  |13: E:="Несовместимы типы операндов"
  |14: E:="Ожидалась переменная"
  |15: E:="Неправильное выражение"
  |16: E:="Ожидался конец строки"
  |17: E:="Тип выражения не совместим с типом переменной"
  |18: E:="Недопустимо в логическом IF"
  |19: E:="Неверный тип выражения"
  |20: E:="Попытка распечaтать пустой об'ект"
  |21: E:="Нарушен порядок описаний"
  |22: E:="Ожидалось имя типа"
  |23: E:="Неверный диапазон"
  |24: E:="Ожидалoсь константное выражение"
  |25: E:="Ожидался идентификатор"
  |26: E:="Тип выражения не совместим с типом параметра"
  |27: E:="Тип выражения должен быть целый"
  |28: E:="Размерность массива не может быть больше трех"
  |29: E:="Ошибка в описаниях"
  |30: E:="Параметр не может быть константой"
  |31: E:="Не стоит извращаться"
  |32: E:="Нарушена размерность массива"
  |33: E:="Выход за границы массива"
  |34: E:="Цикл не может заканчиваться таким оператором"
  |35: E:="У функции-формулы допускается не более 6 параметров"
  |36: E:="Тип формулы не совместим с типом функции"
  |37: E:="Функция не может быть своим параметром"
  |38: E:="Два одинаковых параметра"
  |39: E:="Функция не может быть рекурсивной"
  |40: E:="Неправильное число параметров"
  |41: E:="Неверное ключевое слово"
  |42: E:="Упростите выражение, не хватает стека"
  |43: E:="В строке продолжения в 6-ой позиции ожидался не '0'"
  |44: E:="Недопустимо в главном модуле"
  |45: E:="Слишком много параметров-выражений"
  |46: E:="Не могу использоваь параметр подпрограммы в этом качестве"
  |47: E:="Ожидалось FUNCTION или SUBROUTINE"
  |48: E:="В разделе FORWARD ожидалось DIMENSION, описание типа или END"
  |49: E:="Подпрограмма не может быть своим параметром"
  |50: E:="Подпрограмма SUBROUTINE не моджет обладать типом"
  |51: E:="Описание FORWARD не должно встречаться внутри модуля"
  |52: E:="Типом может обладать только подпрограмма-функция"
  |53: E:="Ожидалось имя переменной или массива"
  |54: E:="Параметров у подпрограммы больше, чем описано"
  |55: E:="Вместо переменной подставлен массив"
  |56: E:="Вместо массива подставлена переменная"
  |57: E:="Ожидалось имя массива"
  |58: E:="Не найдена метка формата"
  |59: E:="FORMAT без метки"
  |60: E:="Ожидалась строка"
  ELSE E:="Незнакомая Error, комп,"
  END;
  Show(E); ShowErrorLine;
END Error;

PROCEDURE ErrorId(n:INTEGER);
  VAR IdName:Idname;
BEGIN INC(ErrorCo);
  CASE n OF
   00: E:="Не может обладать значением:";
  |01: E:="Повторно об'явлен об'ект: ";
  |03: E:="Нет открывающего оператора IF или ELSE IF для";
  |04: E:="Нельзя передавать управление на оператор";
  |05: E:="Это не функция:";
  |06: E:="Тип параметра не соответствует типу переменной";
  |07: E:="Подпрограмма уже была описана:";
  |08: E:="Не является параметром:";
  |09: E:="Неописанная подпрограмма:";
  |10: E:="Не может быть подпрограммой-функцией:";
  ELSE E:="Незнакомая ErrorId, комп,";
  END;
  WriteString(E); IdStr(idno,IdName); Write(40c); Show(IdName);
  ShowErrorLine;
END ErrorId;

PROCEDURE FAULT;
BEGIN
  Show("Фатально, компиляция прервана");
  HALT;
END FAULT;

PROCEDURE Fault(n:INTEGER);
BEGIN
  CASE n OF
    00: E:="Не хватает буфера для генерируемого кода";
   |01: E:="Функция не может быть массивом";
   |02: E:="Функция не может быть параметром";
   |03: E:="Подпрограмма встречается второй раз";
   |04: E:="Ожидалось имя подпрограммы";
   |05: E:="Пустой файл"; Show(E); HALT;
   |06: E:="Слишком много строк-продолжений";
   |07: E:="Не закрыто тело DO-цикла";
   |08: E:="Не закрыто тело структурного IF-оператора";
   |09: E:="Недопустимы два главных модуля";
   |10: E:="Пустой стек;комп.";
   |11: E:="Должен быть хотя бы один оператор";
   |12: E:="Неожиданный конец исходного текста.";
   |13: E:="Нет памяти для кучи; комп.";
   |14: E:="Попытка создать Empty-об'ект; комп.";
   |15: E:="Попытка распаковать Empty-об'ект; комп.";
   |16: E:="Попытка удалить пустой об'ект; комп.";
   |17: E:="НЕ РЕАЛИЗОВАНО";
  ELSE  E:="Неверный параметр Fault; комп";
  END; (* case *) Show(E);
  ShowErrorLine; FAULT;
END Fault;

PROCEDURE VisSy(sy: Symbol; VAR S:Idname);
BEGIN
  CASE sy OF
   lpar:        S:= "'('";
  |rpar:        S:= "')'";          |coma:      S:= "','";
  |eqv:         S:= "'='";          |plus:      S:= "'+'";
  |minus:       S:= "'-'";          |slash:     S:= "'/'";
  |times:       S:= "'*'";          |power:     S:= "'**'";
  ELSE WriteString(' Что-то не то в VisSy ');
  END; (* case *)
END VisSy;

PROCEDURE Expected(Sy:Symbol);
BEGIN
  VisSy(Sy,St); INC(ErrorCo);
  print("ожидалось %s\n", St);
  ShowErrorLine;
END Expected;

END ERRORS;
(* --------------- E N D   E R R O R S ---------------------- *)

PROCEDURE Getch;
BEGIN  (* Пробелы игнорируются *)
  ch:=In[No]; INC(No);
  WHILE ch=40c DO ch:=In[No]; INC(No); END;
END Getch;

PROCEDURE GetKW;
  VAR i: INTEGER;    (* Возврашает sy для KW *)
      KW:Idname;
BEGIN (* Первую ch уже взяли *)
  KW[0]:=ch; i:=1; Getch; Go:=FALSE;
  WHILE BOOLEAN(let?[ch]) DO
    IF (i=4) THEN KW[4]:=eol;
      IF KW=CALL THEN sy:=call; RETURN; END;
    END;
    IF i=15 THEN Error(5); KW[15]:=eol;
      StdIO.Show(KW); sy:=invKW; RETURN;
    END;
    KW[i]:=ch; INC(i); Getch;
  END;
  KW[i]:=eol; i:=Find(KW); sy:=WhatSy?(i);
END GetKW;

PROCEDURE GetIdent;
  VAR Id:Idname; i,j: INTEGER;
BEGIN
   CASE worktype OF
    kw : GetKW; IF sy=invKW THEN Error(41); END;
   |ass: (* GetId(Id); *)
         Id[0]:=ch; i:=1; Getch;
         WHILE BOOLEAN(dig?[ch]) OR BOOLEAN(let?[ch]) DO
           IF i=15 THEN Error(5); Id[15]:=eol;
             StdIO.Show(Id); sy:=invKW; RETURN;
           END;
           Id[i]:=ch; INC(i); Getch;
         END;
         Id[i]:=eol; Go:=FALSE; idno:=StrId(Id); sy:=id;
   |dcl: (*declaration*)
         Id[0]:=ch; i:=1; Id[1]:=eol; sy:=WhatSy?(Find(Id));
         WHILE ORD(sy)>ORD(dim) DO Getch;
           IF (ch=eol) OR (i=11) THEN sy:=invKW; RETURN; END;
           Id[i]:=ch; INC(i); Id[i]:=eol;
           j:=Find(Id); sy:=WhatSy?(j);
         END;
   END (* case *);
END GetIdent;

PROCEDURE GetString;
  VAR i:INTEGER;
BEGIN i:=0;
  REPEAT
    ch:=In[No]; INC(No);
    IF ch=eol THEN Error(1);RETURN; END;
    Sval[i]:=ch; INC(i);
  UNTIL ch="'";
  Sval[i-1]:=eol; sy:=stringconst;
END GetString;

PROCEDURE Fraction(v: INTEGER);
  VAR r,f: REAL;
     e,sg: INTEGER;
BEGIN
  IF ch='.' THEN Getch END;
  sy:=realconst; Ival:=INTEGER(1.); r:=FLOAT(v);
  f:=1.;
  WHILE ORD(ch)-ORD('0') IN {0..9} DO f:=f/10.;
    IF f>=Delta THEN
      r:=r + FLOAT(ORD(ch)-ORD('0'))*f;
    ELSE Error(61)
    END; Getch
  END;
  IF CAP(ch)='E' THEN Getch; sg:=+1;
    IF ch='-' THEN sg:=-1; Getch ELSIF ch='+' THEN Getch END;
    e:=0;
    WHILE ORD(ch)-ORD('0') IN {0..9} DO
      IF e<255 THEN e:=e*10+ORD(ch)-ORD('0') END;
      Getch;
    END;
    IF e>40 THEN Error(61); RETURN END;
    WHILE e>0 DO
      IF sg>0 THEN
        IF r>MaxReal/10. THEN Error(61); RETURN END;
        r:=r*10.
      ELSE
        IF r<Delta THEN Error(61); RETURN END;
        r:=r/10.
      END;  DEC(e)
    END;
  END;
  Ival:=INTEGER(r);
  Rval:=r;
END Fraction;

PROCEDURE GetConst;
  VAR d: INTEGER;
BEGIN Ival:=0;
  WHILE BOOLEAN(dig?[ch]) DO
    d:=ORD(ch)-ORD('0');
    IF Ival >= (MAX(INTEGER)-d) DIV 10 THEN Error(61)
    ELSE
      Ival:=Ival*10+d;
    END; Getch;
  END; sy:=const;
  IF (ch='.') OR (CAP(ch)='E') THEN Fraction(Ival) END;
  Go:=FALSE;
(* realconst is not produced *)
END GetConst;

PROCEDURE GetSy;
  VAR Id:Idname; i: INTEGER;
BEGIN IF Go THEN Getch; ELSE Go:=TRUE; END;
  CASE ch OF
   eol: sy:=EOL;
  |'(': sy:=lpar; |',': sy:=coma;  |'+': sy:=plus; |'/': sy:=slash;
  |')': sy:=rpar; |'-': sy:=minus; |':': sy:=col;  |'=': sy:=eqv;
  |"'"     : GetString;
  |'0'..'9': GetConst;
  |'.': Getch; GetKW;
        IF (ORD(sy)<ORD(lt)) OR (sy=invKW) THEN Error(2);
        ELSE Go:=TRUE; IF ch#'.' THEN Error(3); END;
        END;
  |'*': Getch; IF ch='*' THEN sy:=power; ELSE sy:=times; Go:=FALSE; END;
  ELSE   IF NOT (BOOLEAN(let?[ch])) THEN Error(0); sy:=invKW; RETURN;
         ELSE GetIdent; END;
  END; (* c a s e *)
END GetSy;

PROCEDURE Digit?(ch:CHAR):BOOLEAN;
BEGIN RETURN (ORD(ch)-ORD('0')) IN {0..9} END Digit?;

PROCEDURE Letter?(ch:CHAR):BOOLEAN;
BEGIN
  RETURN ((ORD(ch)>=ORD('A')) AND (ORD(ch)<=ORD('Z'))) OR (* Capital Latin *)
         ((ORD(ch)>=ORD('a')) AND (ORD(ch)<=ORD('z'))) OR (* Bold Latin    *)
         ((ORD(ch)>=300b    ) AND (ORD(ch)<=ORD('Ч'))) OR (* Capital Russ. *)
         ((ORD(ch)>=ORD('ю')) AND (ORD(ch)<=ORD('ч')));   (* Bold Russian  *)
END Letter?;

PROCEDURE GetLetter(VAR c:CHAR):BOOLEAN; (* for implicit *)
BEGIN
  IF Go THEN Getch; ELSE Go:=TRUE; END;
  IF BOOLEAN(let?[ch]) THEN c:=ch; RETURN TRUE;
  ELSE Error(0); RETURN FALSE; END;
END GetLetter;

PROCEDURE MarkPos;
BEGIN Mark:=No; GoMark:=Go; END MarkPos;

PROCEDURE BackUp;
BEGIN No:=Mark; ch:=In[No-1]; Go:=GoMark; END BackUp;

PROCEDURE MarkPos1;
BEGIN Mark1:=No; GoMark1:=Go; END MarkPos1;

PROCEDURE BackUp1;
BEGIN No:=Mark1; ch:=In[No-1]; Go:=GoMark1; END BackUp1;

VAR Buff:ARRAY [0..79] OF CHAR;
    LenB:INTEGER; (* length of string in Buff *)
    LastString:BOOLEAN;

PROCEDURE ContinueString?():BOOLEAN;
  VAR i: INTEGER;
BEGIN
  LenB:=StdIO.GetS(S,Buff); INC(Line);
  IF LenB=0 THEN LastString:=TRUE; RETURN FALSE; END;
  (*$T-*) ch:=Buff[0]; i:=0;
  (* Отработка комментариев и пустых строк *)
  IF (ch="C") OR (ch=eol) THEN RETURN ContinueString?(); END;
  WHILE ch=40c DO INC(i); ch:=Buff[i]; END;
  (*$T+*)
  IF i#5 THEN RETURN FALSE;
  ELSIF ch="0" THEN Error(43);
  ELSE RETURN TRUE; END;
END ContinueString?;

PROCEDURE GetFStr;
  VAR i: INTEGER;
BEGIN (*$T-*)
  MOVE(ADR(In),ADR(Buff),(LenB+3) DIV 4); No:=LenB-1;
  (* Отработка подолжения строки *)
  WHILE ContinueString?() DO
    i:=6; ch:=Buff[6];
    WHILE ch#eol DO
      In[No]:=ch; INC(i);
      ch:=Buff[i]; INC(No);
      IF No=InSize THEN Fault(6); END;
    END;
    In[No]:=eol;
  END; (*$T+*)
END GetFStr;

PROCEDURE NewLine;
  VAR sum: INTEGER;
BEGIN (*$T-*)
  IF BuffIsEmpty THEN Fault(12); END;
  GetFStr; BuffIsEmpty:=LastString;
  sum := 0; No:=0; Getch;            (* Начало разбора поля метки *)
  IF No<6 THEN  (* Label exist *)
    WHILE BOOLEAN(dig?[ch]) AND (No<6) DO
      sum:=sum*10+ORD(ch)-ORD('0');
      ch:=In[No]; INC(No);
    END;
    IF sum=0 THEN Error(4); END;
    IF ch=40c THEN Getch; END;
  END;
  IF No<7 THEN Error(6); No:=6; Go:=TRUE; RETURN; END;
  Label:=sum; Go:=FALSE;             (* Конец разбора поля метки  *)
  MarkPos; RETURN;
  (*$T+*)
END NewLine;

PROCEDURE SetWork(Type: Work);
BEGIN worktype:=Type; END SetWork;

PROCEDURE InitScan;
BEGIN
  InitObjects; InitHeap; InitId; ErrorCo:=0;
  (* порядок инициализации существеннен *)
END InitScan;

PROCEDURE InitZeroScan;
VAR i: INTEGER; t:ARRAY [0..79] OF CHAR;
BEGIN
  FOR ch:=0c TO 377c DO let?[ch]:=CHAR(Letter?(ch)) END;
  FOR ch:=0c TO 377c DO dig?[ch]:=CHAR(Digit? (ch)) END;
  StdIO.WriteLn; ScanFlags;
  TakeWord(FilNm); StdIO.WriteString("  FORTRAN-KRONOS /11-Apr-87/ ");
  ChangeExt(FilNm,"f"); StdIO.print('"%s"\n',FilNm);
  S:=StdIO.Open(FilNm);
  IF S<0 THEN StdIO.Why?(S,t); StdIO.print("%s%s\n",FilNm,t); HALT END;
  Debugg:=Flag?("d");
  i:=0; REPEAT codfile[i]:=FilNm[i]; INC(i); UNTIL FilNm[i]=".";
  codfile[i]:=eol; Line:=-1; BuffIsEmpty:=FALSE; LastString:=FALSE;
  CALL:="CALL";
  LenB:=StdIO.GetS(S,Buff);
  IF LenB=0 THEN Fault(5); END;
  IF LenB=1 THEN StdIO.Show("Первая строка пустая."); FAULT; END;
  IF Buff[0]="C" THEN StdIO.Show("Первая строка - комментарий."); FAULT; END;
END InitZeroScan;

BEGIN
END fScan.
