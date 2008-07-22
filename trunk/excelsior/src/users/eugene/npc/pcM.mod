IMPLEMENTATION MODULE pcM; (* Leo 05-Jun-88. (c) KRONOS *)
                           (* Ned 04-Mar-90. (c) KRONOS *)
                           (* Ned 17-Nov-90. (c) KRONOS *)

IMPORT FIO, Str, Lib, IO, Storage;

CONST TMP_NAME = "SYM.$$$";

VAR
  sym : FIO.File;
  new : FIO.File;
  symf: ARRAY [0..31] OF CHAR;
(*buf : ARRAY [0..1023+FIO.BufferOverhead] OF BYTE;*)
  null: CARDINAL;

PROCEDURE check;
  VAR n: CARDINAL;
BEGIN
  n:=FIO.IOresult();
  IF n#0 THEN
    IO.WrStr('io error: '); IO.WrCard(n,0); IO.WrLn;
    abort;
  END;
END check;

PROCEDURE create(name: ARRAY OF CHAR; VAR done: BOOLEAN);
BEGIN
  Str.Copy(symf,name); Str.Append(symf,'.sym');
  new:=FIO.Create(TMP_NAME);
  done:=(new#MAX(CARDINAL));
  IF done THEN
    IO.WrStr('create sym file: '); IO.WrStr(symf); IO.WrLn;
(*    FIO.AssignBuffer(new,buf);*)
  ELSE
    IO.WrStr("can't create sym file: "); IO.WrStr(symf); IO.WrLn;
  END;
END create;

PROCEDURE put(x: LONGINT);
  VAR i: SHORTCARD;
BEGIN
  i:=SHORTCARD(x);
  FIO.WrBin(new,i,1);
END put;

PROCEDURE put2(x: LONGINT);
  VAR i: CARDINAL;
BEGIN
  i:=CARDINAL(x);
  FIO.WrBin(new,i,2);
END put2;

PROCEDURE put4(x: LONGINT);
BEGIN
  FIO.WrBin(new,x,4);
END put4;

PROCEDURE put8(x: LONGREAL);
BEGIN
  FIO.WrBin(new,x,8);
END put8;

PROCEDURE put_name(s: ARRAY OF CHAR);
BEGIN
  FIO.WrStr(new,s); FIO.WrChar(new,15C);
END put_name;

PROCEDURE put_bytes(VAR x: ARRAY OF BYTE; len: LONGINT);
BEGIN
  FIO.WrBin(new,x,CARDINAL(len));
END put_bytes;

PROCEDURE close_new(register: BOOLEAN);
BEGIN
  FIO.Close(new); check;
  IF register THEN
    FIO.Erase(symf); check;
    FIO.Rename(TMP_NAME,symf); check;
  ELSE
    FIO.Erase(TMP_NAME); check;
  END;
END close_new;

PROCEDURE open(name: ARRAY OF CHAR; self: BOOLEAN; VAR done: BOOLEAN);
  VAR fn: ARRAY [0..63] OF CHAR;
BEGIN
  Str.Copy(fn,name); Str.Append(fn,'.sym');
  sym:=FIO.Open(fn);
  done:=sym#MAX(CARDINAL);
  IF done THEN
    IO.WrStr(fn); IO.WrLn;
(*    bio.buffers(sym,1,1);*)
  ELSIF NOT self THEN
    IO.WrStr("can't open sym file: "); IO.WrStr(fn); IO.WrLn;
  END;
END open;

PROCEDURE get(VAR x: LONGINT);
BEGIN
  x:=0;
  null:=FIO.RdBin(sym,x,1);
END get;

PROCEDURE get2(VAR x: LONGINT);
BEGIN
  x:=0;
  null:=FIO.RdBin(sym,x,2);
END get2;

PROCEDURE get4(VAR x: LONGINT);
BEGIN
  null:=FIO.RdBin(sym,x,4);
END get4;

PROCEDURE get8(VAR x: LONGREAL);
BEGIN
  null:=FIO.RdBin(sym,x,8);
END get8;

PROCEDURE get_name(VAR s: ARRAY OF CHAR);
BEGIN
  FIO.RdStr(sym,s);
END get_name;

PROCEDURE get_bytes(VAR x: ARRAY OF BYTE; len: LONGINT);
BEGIN
  null:=FIO.RdBin(sym,x,CARDINAL(len));
END get_bytes;

PROCEDURE close;
BEGIN
  FIO.Close(sym);
END close;

PROCEDURE equal(pos: LONGINT): BOOLEAN;
  (*VAR eof,len,i: LONGINT; a,b: DYNARR OF INTEGER;*)
BEGIN
(*
  eof:=bio.eof(sym);
  IF bio.eof(new)#eof THEN RETURN FALSE END;
  len:=(eof-pos+3) DIV 4;
  NEW(a,len); a[len-1]:=0;
  NEW(b,len); b[len-1]:=0;
  bio.seek(sym,pos,0); bio.get(sym,a,eof-pos);
  bio.seek(new,pos,0); bio.get(new,b,eof-pos);
  FOR i:=0 TO len-1 DO
    IF a[i]#b[i] THEN RETURN FALSE END;
  END;
*)
  RETURN TRUE
END equal;

(*----------------------------------------------------------------*)

PROCEDURE wc(c: CHAR); BEGIN IO.WrChar(c) END wc;
PROCEDURE ws(s: ARRAY OF CHAR); BEGIN IO.WrStr(s); END ws;
PROCEDURE wi(x: LONGINT; n: INTEGER); BEGIN IO.WrLngInt(x,n) END wi;
PROCEDURE wl; BEGIN IO.WrLn END wl;

(*----------------------------------------------------------------*)

PROCEDURE abort;
BEGIN
  IO.WrLn; IO.WrStr('#ABORT'); IO.WrLn; HALT;
END abort;

PROCEDURE final(p: CLOSURE);  BEGIN (*env.final(p)*) END final;

PROCEDURE time(): LONGINT;
BEGIN
  RETURN 0
END time;

PROCEDURE str_equ(a,b: ARRAY OF CHAR): BOOLEAN;
  VAR i: CARDINAL;
BEGIN
  i:=0;
  LOOP
    IF a[i]#b[i] THEN RETURN FALSE END;
    IF a[i]=0C THEN RETURN TRUE END;
    INC(i);
  END;
END str_equ;

PROCEDURE str_copy(VAR a: ARRAY OF CHAR; b: ARRAY OF CHAR);
  VAR i: CARDINAL;
BEGIN
  IF HIGH(b)>HIGH(a) THEN abort END;
  FOR i:=0 TO HIGH(b) DO a[i]:=b[i] END;
END str_copy;

PROCEDURE app(VAR s: ARRAY OF CHAR; x: ARRAY OF CHAR);
BEGIN Str.Append(s,x);
END app;

PROCEDURE app_num(VAR s: ARRAY OF CHAR; x: LONGINT);
  VAR a: ARRAY [0..23] OF CHAR; ok: BOOLEAN;
BEGIN
  Str.IntToStr(x,a,10,ok);
  Str.Append(s,a);
END app_num;

(*---------------------------------------------------------------*)

PROCEDURE err_msg(no: INTEGER; VAR s: ARRAY OF CHAR);
BEGIN
  CASE no OF
  |000: Str.Copy(s,"")

  |001: Str.Copy(s,"Непонятный знак игнорируется")
  |002: Str.Copy(s,"Незакрытый коментарий, начавшийся в строке")
  |003: Str.Copy(s,"Неправильное число")
  |004: Str.Copy(s,"Незакрытая или слишком длинная строка!")
  |005: Str.Copy(s,"Неожиданный конец исходного текста!")
  |006: Str.Copy(s,"Слишком длинное имя")
  |007: Str.Copy(s,"Должен быть идентификатор");
  |008: Str.Copy(s,"Должен быть символ")

  |020: Str.Copy(s,"Невидимый объект")
  |021: Str.Copy(s,"Рекурсивное определение типа")
  |022: Str.Copy(s,"Повторно объявлен")
  |024: Str.Copy(s,"Рекурсивный импорт запрещен")
  |023: Str.Copy(s,"Повторное предварительное описание")

  |030: Str.Copy(s,"Типы несовместимы")
  |031: Str.Copy(s,"Должен быть тип")
  |032: Str.Copy(s,"Должен быть скалярный тип")
  |033: Str.Copy(s,"Должен быть простой (1 слово) тип")
  |034: Str.Copy(s,"Недопустимое преобразование типа")
  |036: Str.Copy(s,"Недопустимая реализация скрытого типа")
  |092: Str.Copy(s,"Спецификатор VAL недопустим в описании процедурного типа");
  |038: Str.Copy(s,"Неправильное число параметров")
  |039: Str.Copy(s,"Неправильный спецификатор параметра")
  |040: Str.Copy(s,"Не совпадают имена параметров")
  |041: Str.Copy(s,"Не является расширением типа")
  |042: Str.Copy(s,"Не совместимы по присваиванию")
  |043: Str.Copy(s,"Не может быть типом результата функции")
  |044: Str.Copy(s,"Типы результата несовместимы")

  |050: Str.Copy(s,"Должен быть массив")
  |051: Str.Copy(s,"Должна быть запись")
  |052: Str.Copy(s,"Должен быть указатель")
  |053: Str.Copy(s,"Должно быть множество")
  |054: Str.Copy(s,"Должна быть переменная")
  |055: Str.Copy(s,"Должна быть процедура")
  |056: Str.Copy(s,"Должен быть модуль")
  |057: Str.Copy(s,"Должна быть стандартная процедура")
  |058: Str.Copy(s,"Должен быть дин. массив")
  |059: Str.Copy(s,"Контроль типа должен применяется к указателю или записи (VAR параметр)")
  |060: Str.Copy(s,"Базовым типом указателя должен быть массив или запись")
  |061: Str.Copy(s,"Размерность в LEN слишком большая или отрицательная")
  |062: Str.Copy(s,"Должен быть указатель на запись")
  |063: Str.Copy(s,"Должен быть метод")
  |064: Str.Copy(s,"Метод не определен")
  |065: Str.Copy(s,"Нельзя определять метод для записи из другого модуля")
  |066: Str.Copy(s,"Должен быть указатель или запись (VAR параметр)")
  |067: Str.Copy(s,"Этот метод должен быть применен к указателю")
  |068: Str.Copy(s,"Недопустимый вызов супер-метода")
  |069: Str.Copy(s,"Некорректное переопределение метода")

  |080: Str.Copy(s,"Ошибка в заголовке модуля");
  |081: Str.Copy(s,"Неправильное выражение")
  |082: Str.Copy(s,"Ошибка в описаниях")
  |083: Str.Copy(s,"Ошибка в конструкторе типа")
  |086: Str.Copy(s,"Должен быть оператор")
  |087: Str.Copy(s,"Должно быть константное выражение")
  |088: Str.Copy(s,"Должно быть имя блока")
  |089: Str.Copy(s,"Нереализованная процедура")
  |090: Str.Copy(s,"Вызов функции в позиции оператора")
  |091: Str.Copy(s,"Вызов процедуры в выражении")
  |093: Str.Copy(s,"Недопустимо в определяющем модуле")
  |094: Str.Copy(s,"Разрешено только в определяющем модуле");
  |095: Str.Copy(s,"Разрешено только на уровне единицы компиляции")
  |096: Str.Copy(s,"Не может быть экспортирован")
  |097: Str.Copy(s,"Не реализованный указатель вперед")

  |120: Str.Copy(s,"Не обладает адресом")
  |121: Str.Copy(s,"Не обладает значением")
  |122: Str.Copy(s,"Выход за границы диапазона")
  |123: Str.Copy(s,"Присваивание VAL переменной (Только Для Чтения)");
  |124: Str.Copy(s,"Вырожденный отрезок")
  |125: Str.Copy(s,"EXIT вне LOOP'а")
  |126: Str.Copy(s,"Такая метка уже была")
  |127: Str.Copy(s,"В CASE нужна хоть одна альтернатива");
  |128: Str.Copy(s,"Переменная цикла должна быть локальной")
  |129: Str.Copy(s,"Это не RTS процедура")
  |130: Str.Copy(s,"Переопределение RTS процедуры")

  |190: Str.Copy(s,"Ошибка в заголовке симфайла")
  |191: Str.Copy(s,"Некорректная версия симфайла")
  |192: Str.Copy(s,"Конфликт версий (по времени компиляции)");
  |193: Str.Copy(s,"Запись нового симфайла не разрешена");

  |200: Str.Copy(s,"Еще не реализовано")
  |201: Str.Copy(s,"Слишком большое число")
  |202: Str.Copy(s,"Некорректный базовый тип множества")
  |203: Str.Copy(s,"Деление на ноль");

  |220: Str.Copy(s,"Слишком много переменных")
  |221: Str.Copy(s,"Слишком много процедур")
  |222: Str.Copy(s,"Слишком много параметров")
  |223: Str.Copy(s,"Слишком много альтернатив в операторе CASE")
  |224: Str.Copy(s,"Слишком много указателей")
  |225: Str.Copy(s,"Слишком много экспортированных типов")
  |226: Str.Copy(s,"Слишком большой список импорта")
  |227: Str.Copy(s,"Слишком большая длина перехода")
  |228: Str.Copy(s,"Не хватает регистров (или стека)");
  |229: Str.Copy(s,"Слишком много раз расширенная запись");
  |230: Str.Copy(s,"Слишком много записей с методами")
  |231: Str.Copy(s,"Слишком много кода для процедуры")
  |232: Str.Copy(s,"Too large type size");
  |233: Str.Copy(s,"ASSERT");
  |240: Str.Copy(s,"Недоступная RTS процедура")
  ELSE Str.Copy(s,"Неизвестная ошибка");
  END;
END err_msg;

(*---------------------------------------------------------------*)
(*
TYPE
  block_ptr = POINTER TO block_rec;
  block_rec = RECORD size: INTEGER; next: block_ptr END;
  slot_ptr = POINTER TO slot_rec;
  slot_rec = RECORD next: slot_ptr END;

VAR
  slots: ARRAY [0..31] OF slot_ptr;
  stat : ARRAY [0..31] OF INTEGER;
  nodes: block_ptr;

PROCEDURE alloc(VAR a: sys.ADDRESS; size: INTEGER);
  VAR s,l: slot_ptr; n: block_ptr; co,i,sz: INTEGER;
BEGIN
  IF size<=HIGH(slots) THEN
    co:=32;
    INC(stat[size],co);
  ELSE co:=1
  END;
  sz:=size*co+SIZE(n^);
  mem.ALLOCATE(n,sz);
  n^.size:=sz;
  n^.next:=nodes; nodes:=n;
  a:=sys.ADDRESS(n)+SIZE(n^);
  IF size<=HIGH(slots) THEN
    l:=NIL; s:=a+size;
    FOR i:=0 TO co-2 DO
      s^.next:=l; l:=s;
      s:=sys.ADDRESS(s)+size;
    END;
    slots[size]:=l;
  END;
END alloc;

PROCEDURE ALLOCATE(VAR a: sys.ADDRESS; size: INTEGER);
  VAR s: slot_ptr;
BEGIN
  IF (size<=HIGH(slots)) & (slots[size]#NIL) THEN
    s:=slots[size]; slots[size]:=s^.next; a:=s;
  ELSE
    alloc(a,size);
  END;
END ALLOCATE;

PROCEDURE DEALLOCATE(VAR a: sys.ADDRESS; size: INTEGER);
  VAR s: slot_ptr;
BEGIN
  IF (a=NIL) OR (size<=0) THEN RETURN END;
  IF size<=HIGH(slots) THEN
    s:=a;
    s^.next:=slots[size]; slots[size]:=s;
  END;
  a:=NIL;
END DEALLOCATE;

PROCEDURE ini_heap;
  VAR i: INTEGER;
BEGIN
  nodes:=NIL;
  FOR i:=0 TO HIGH(slots) DO slots[i]:=NIL END;
  FOR i:=0 TO HIGH(stat)  DO stat[i]:=0 END;
END ini_heap;

PROCEDURE release;
  VAR x: block_ptr; i: INTEGER;
BEGIN
  WHILE nodes#NIL DO
    x:=nodes; nodes:=nodes^.next;
    mem.DEALLOCATE(x,x^.size);
  END;
  IF args.flag('+','?') THEN
    FOR i:=0 TO HIGH(stat) DO
      IF stat[i]#0 THEN
        tty.print('%2d: %4d  %4dKb\n',i,stat[i],stat[i]*i DIV 256);
      END;
    END;
  END;
  ini_heap;
END release;
*)

PROCEDURE ALLOCATE(VAR a: ADDRESS; n: LONGINT);
BEGIN
  Storage.ALLOCATE(a,CARDINAL(n));
END ALLOCATE;

PROCEDURE DEALLOCATE(VAR a: ADDRESS; n: LONGINT);
BEGIN
  Storage.DEALLOCATE(a,CARDINAL(n));
END DEALLOCATE;

PROCEDURE release; END release;
PROCEDURE ini_heap; END ini_heap;

(*----------------------------------------------------------------*)

PROCEDURE _getstr(VAR s: ARRAY OF CHAR; VAR done: BOOLEAN);
BEGIN abort END _getstr;

PROCEDURE _error(l,c: INTEGER; source,msg: ARRAY OF CHAR);
BEGIN abort END _error;

PROCEDURE _message(msg: ARRAY OF CHAR); BEGIN abort END _message;

VAR i: CHAR;

BEGIN
  cardinal:=2;
  integer :=2;
  longint :=4;
  real    :=4;
  longreal:=8;
  boolean :=1;
  bitset  :=2;
  byte    :=1;
  word    :=2;
  addr    :=4;
  proctype:=4;
  nilval  :=INTEGER(NIL);

  min_sint:=MIN(SHORTINT);  max_sint:=MAX(SHORTINT);
  min_int :=MIN(INTEGER);   max_int :=MAX(INTEGER);
  min_lint:=MIN(LONGINT);   max_lint:=MAX(LONGINT);
  max_scard:=255;           max_card:=MAX(CARDINAL);

  max_hex_dig:=8;
  max_real:=REAL(7FFFFFFFH);
  min_real:=-max_real;
  max_dig :=9;
  max_exp :=64;
  oberon:=FALSE;
(*----------------------------------------------------------------*)
  FOR i:=0C TO 377C DO alpha[i]:=0C END;
  FOR i:='0' TO '9' DO alpha[i]:=2C END;
  FOR i:='a' TO 'z' DO alpha[i]:=2C END;
  FOR i:='A' TO 'Z' DO alpha[i]:=2C END;
  FOR i:=300C TO 377C DO alpha[i]:=1C END;
  alpha['_']:=1C;
  alpha['?']:=1C;
(*----------------------------------------------------------------*)
  getstr:=_getstr;
  error:=_error;
  message:=_message;
  pass2:=TRUE;
  ini_heap;
  FIO.IOcheck:=FALSE;
  Lib.EnableBreakCheck;
END pcM.
