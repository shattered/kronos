IMPLEMENTATION MODULE fcScan;

FROM SYSTEM    IMPORT   WORD,ADR, ADDRESS;
(* FROM Args      IMPORT   TakeWord, ScanFlags, Flag?; *)
FROM fcObj     IMPORT   IdStr, StrId, Idname, idno, Ident, Types,
                        InitObjects;
FROM fcHeap    IMPORT   InitHeap ;
FROM fcDefs    IMPORT   InSize, maxSlen;
FROM objFile   IMPORT   pLabel;

IMPORT StdIO;
FROM StdIO     IMPORT Show, WriteString, Write, WriteLn, print;

FROM fcBugs    IMPORT InitStr,AddStr, OutStr, VisPosKW, Visfsym;

CONST eol=0c;
      MaxReal=REAL(7FFFFFFFh);  (* TYPE REAL=[-MaxReal..MaxReal] *)
      Delta  =REAL(00800000h);  (* Delta / 2.0 = 0.0             *)

TYPE
     FortranStr=ARRAY [0..InSize] OF CHAR;
     Fstr      = RECORD no,pos:INTEGER; END;

VAR In: FortranStr;               (* Входная строка              *)
    No: INTEGER;                 (* ее счетчик                  *)
    ch: CHAR;                     (* Текущая литера              *)
    FileNm: Filename;              (* Имя входного потока         *)
    S: StdIO.Stream;              (* поток                       *)
    Mark,Mark1:INTEGER;          (* Номер маркированной позиции *)
    Go:BOOLEAN;                   (* Go=FALSE, если ch уже взята *)
    GoMark,GoMark1:BOOLEAN;       (* Go в момент маркирования    *)
    Line:INTEGER;                (* номер текущей строки        *)
    Fline: ARRAY [0..19] OF Fstr;
    ncont: INTEGER;
    worktype: Work;               (* Режим работы                *)
    let?,dig?:ARRAY [0c..377c] OF CHAR;  (*IF BOOLEAN(let?[ch])..*)
    Keys: BITSET;
VAR St:Idname; (* для визуализации *)

PROCEDURE MOVE(t,f: ADDRESS; s: INTEGER); CODE 0C0h END MOVE;

PROCEDURE CloseSou;
  VAR i:INTEGER;
BEGIN
  i:=StdIO.Close(S);
  IF i<0 THEN
     StdIO.Why?(i,St);
     StdIO.print(' %s %s \n',St,FileNm);
     HALT;
  END;
END CloseSou;

(* --------------------- E R R O R S ------------------------ *)
MODULE ERRORS;

(*FROM Main*) IMPORT St, IdStr, Idname, In, No, eol, ErrorCo,
                     Symbol, idno, GetSy, sy, cType, Ival, Line,
                     Fline,ncont,Fstr, Types, NoStmErr;
FROM StdIO    IMPORT Show, WriteString, Write, WriteLn, print;

EXPORT Error, Fault, VisSy, ErrorId, Expected, Warning, FAULT,
       ShowLine, ShowErrors; -- ExpectedS;

CONST ErrorLimit=66;
      maxNoerr=5;
VAR E:ARRAY [0..79] OF CHAR;
    ErrInfo:ARRAY [0..maxNoerr*3-1] OF INTEGER;
    showline:BOOLEAN;
    lineno,fpos,lpos,epos:INTEGER;

PROCEDURE ShowLine(show:BOOLEAN);
BEGIN
   showline:=show;
END ShowLine;

PROCEDURE AddErr(type,err:INTEGER);
  VAR i:INTEGER;
BEGIN
  IF err>=0 THEN INC(ErrorCo) END;
  IF NoStmErr>=maxNoerr THEN RETURN END;
  i:=NoStmErr*3; INC(NoStmErr);
  ErrInfo[i]:=type; ErrInfo[i+1]:=No; ErrInfo[i+2]:=err;
END AddErr;

PROCEDURE DefErrorLine(errpos:INTEGER);
  VAR i: INTEGER;
BEGIN epos:=errpos;
  IF In[epos-1]=eol THEN  DEC(epos); END;
  i:=0;
  LOOP
    IF epos<=Fline[i].pos THEN EXIT
    ELSE INC(i); IF i>ncont THEN i:=0; EXIT END;
    END;
  END;
  IF i=0 THEN fpos:=0 ELSE fpos:=Fline[i-1].pos END;
  lpos:=Fline[i].pos-1; lineno:=Fline[i].no;
END DefErrorLine;

PROCEDURE ShowErrorLine;
  VAR i,l:INTEGER;
BEGIN
  print("*%d*",lineno);
  FOR i:=fpos TO epos-1 DO Write(In[i]); END; Write(177c);
  i:=epos; l:=lpos-epos+1;
  WHILE (l>0) AND (In[i]#eol) DO
    Write(In[i]); INC(i); DEC(l);
  END;
  WriteLn;
END ShowErrorLine;

PROCEDURE Warning(n:INTEGER);
BEGIN AddErr(idno,-(n+1));
END Warning;

PROCEDURE msgWarning(n:INTEGER);
  VAR N:Idname;
BEGIN
  WriteString("*** Внимание ! ");
  CASE n OF
   00: Show("Слишком длинный идентификатор");
  |01: Show("Переменная впервые появляется");
       WriteString("не в  левой части оператора присваивания:");
       IdStr(idno,N); Show(N);
  |02: WriteString("Подпрограмма не может обладать типом:");
       IdStr(idno,N); Show(N);
  ELSE Show("invalid WARNING"); END;
END msgWarning;

PROCEDURE Error(n:INTEGER);
BEGIN AddErr(-1,n);
END Error;

PROCEDURE msgError(n:INTEGER);
BEGIN
  CASE n OF
   00: E:="Незнакомая литера:"
  |01: E:="Строковая константа не заключена в кавычки:"
  |02: E:="Нет открывающего оператора IF"
  |03: E:="Ожидалась буква"
  |04: E:="Неправильная метка"
  |05: E:="Ошибка в описателе длины текстовой переменной"
  |06: E:="В поле метки не цифра:"
  |07: E:="Тип выражения должен быть логический"
  |08: Show("Передача управления внутрь тела DO-цикла,");
       E:="IF - ,ELSEIF - ,ELSE-блоков запрещена."
  |09: E:="Дважды встречалась одна метка"
  |10: E:="Число строк продолжения больше 19"
  |11: E:="Ошибка в спецификации формата"
  |12: E:="Недопустимая операция"
  |13: E:="Несовместимы типы операндов"
  |14: E:="Ожидалась переменная или константа"
  |15: E:="Неправильное выражение"
  |16: E:="Ожидался конец строки"
  |17: E:="Тип выражения не совместим с типом переменной"
  |18: E:="Недопустимо в логическом IF"
  |19: E:="Неверный тип выражения"
  |20: E:="Слишком длинная текстовая переменная (константа) (>255)"
  |21: E:="Нарушен порядок описаний"
  |22: E:="Ожидалось имя типа"
  |23: E:="Неверный диапазон"
  |24: E:="Ожидалoсь константное выражение"
  |25: E:="Ожидался идентификатор"
  |26: E:="Нельзя передавать управление на этот оператор"
  |27: E:="Тип выражения должен быть целый"
  |28: E:="Размерность массива не может быть больше семи"
  |29: E:="Не закрыто тело DO либо IF - оператора"
  |30: E:="Ожидалось текстовое выражение"
  |31: E:="Ожидалось арифметическое выражение"
  |32: E:="Нарушена размерность массива"
  |33: E:="Выход за границы массива"
  |34: E:="Цикл не может заканчиваться таким оператором"
  |35: E:="Только последняя размерность может быть '*'"
  |36: E:="ENTRY в структурном операторе"
  |37: E:="Некорректное размещение объектов в общем блоке"
  |38: E:="Ожидалось положительное значение"
  |39: E:="Неожиданный конец оператора"
  |40: E:="Нет спецификации канала"
  |41: E:="Неверное ключевое слово"
  |42: E:="Упростите выражение, не хватает стека"
  |43: E:="В строке продолжения в 6-ой позиции ожидался не '0'"
  |44: E:="Недопустимо в данном модуле"
  |45: E:="Слишком много параметров"
  |46: E:="Недопустимое ключевое слово"
  |47: E:="Ключевое слово не единственное"
  |48: E:="Функция вместо подпрограммы или наоборот"
  |49: E:="Неверный тип переменной DO-цикла"
  |50: E:="Ошибка в операторах EQUIVALENCE"
  |51: E:="Ошибка в неявном DO-цикле оператора DATA"
  |52: E:="Типом может обладать только подпрограмма-функция"
  |53: E:="Ожидалось имя переменной или массива"
  |54: E:="Параметров у подпрограммы больше, чем описано"
  |55: E:="Текстовые и арифметические переменные в общем блоке"
  |56: E:="Альтернативный возврат опущен в определении подпрограммы"
  |57: E:="Альтернативный возврат недопустим в функции"
  |58: E:="Метка альтернативного возврата в обращении к функции"
  |59: E:="FORMAT без метки"
  |60: E:="Ожидалась строка"
  |61: E:="Переполнение( исчерпание ) константы"
  |62: E:="Опущена экспонента после E или D"
  |63: E:="Ошибка в типе параметра стандартной функции"
  |64: E:="Извините, еще не реализовано"
  |65: E:="Не хватает буфера для оператора";
  |66: E:="Должен присутствовать список меток";
  |67: E:="Число меток больше 255";
  |68: E:="Слишком большая подпрограмма";
  ELSE E:="Незнакомая Error, комп,"
  END;
  Show(E);
END msgError;

PROCEDURE ErrorId(n:INTEGER);
BEGIN AddErr(idno,n);
END ErrorId;

PROCEDURE msgErrorId(n:INTEGER);
  VAR IdName:Idname;
BEGIN
  CASE n OF
   00: E:="Не может обладать значением:";
  |01: E:="Повторно об'явлен об'ект: ";
  |03: E:="Не может иметь начального значения";
  |04: E:="Это не подпрограмма";
  |05: E:="Это не INTRINSIC-функция:";
  |06: E:="Это не текстовая переменная";
  |07: E:="Переменная уже была использована:";
  |08: E:="Не является параметром:";
  |09: E:="Неописанная переменная:";
  |10: E:="Не может быть массивом:";
  |11: E:="Это не массив:";
  |12: E:="Ожидалась константа:";
  |13: E:="Тип индекса массива не Integer:";
  |14: E:="Индекс массива не параметр:";
  |15: E:="Неправильная инициализация переменной:";
  ELSE E:="Незнакомая ErrorId, комп,";
  END;
  WriteString(E); IdStr(idno,IdName); Write(40c); Show(IdName);
END msgErrorId;

PROCEDURE FAULT;
BEGIN
  Show("Фатально, компиляция прервана");
  HALT;
END FAULT;

PROCEDURE Fault(n:INTEGER);
BEGIN
  CASE n OF
    00: E:="Не хватает буфера для генерируемого кода";
   |05: E:="Пустой файл"; Show(E); HALT;
   |07: E:="не реализована";
   |08: E:="Не реализована";
   |10: E:="Пустой стек;комп.";
   |12: E:="Неожиданный конец исходного текста.";
   |13: E:="Нет памяти для кучи; комп.";
   |14: E:="Попытка создать Empty-об'ект; комп.";
   |15: E:="Попытка распаковать Empty-об'ект; комп.";
   |16: E:="Попытка удалить пустой об'ект; комп.";
  ELSE  E:="Неверный параметр Fault; комп";
  END; (* case *) Show(E);
  FAULT;
END Fault;

PROCEDURE VisSy(sy: Symbol; VAR S:Idname);
BEGIN
  CASE sy OF
   lpar:        S:= "'('";
  |rpar:        S:= "')'";          |comma:     S:= "','";
  |becomes:     S:= "'='";          |plus:      S:= "'+'";
  |minus:       S:= "'-'";          |slash:     S:= "'/'";
  |times:       S:= "'*'";          |power:     S:= "'**'";
  |cat:         S:= "'//'";         |col:       S:= "':'";
  |int:         S:= "'INTEGER'";    |real:      S:="'REAL'";
  |double:      S:= "'DOUBLE'";     |complex:   S:="'COMPLEX'";
  |log:         S:= "'LOGICAL'";    |char:      S:="'CHARACTER'";
  |subr:        S:= "'SUBROUTINE'"; |func:      S:="'FUNCTION'";
  |prog:        S:= "'PROGRAM'";    |bdata:     S:="'BLOCK DATA'";
  |entry:       S:= "'ENTRY'";      |impl:      S:="'IMPLICIT'";
  |comm:        S:= "'COMMON'";     |data:      S:="'DATA'";
  |external:    S:= "'EXTERNAL'";   |equi:      S:="'EQUIVALENCE'";
  |param:       S:= "'PARAMETER'";  |intr:      S:="'INTRINSIC'";
  |save:        S:= "'SAVE'";       |dim:       S:="'DIMENSION'";
  |call:        S:= "'CALL'";       |goto:      S:="'GO TO'";
  |cont:        S:= "'CONTINUE'";   |return:    S:="'RETURN'";
  |stop:        S:= "'STOP'";       |rd:        S:="'READ'";
  |wr:          S:= "'WRITE'";      |pr:        S:="'PRINT'";
  |asgn:        S:= "'ASSIGN'";     |pause:     S:="'PAUSE'";
  |rewind:      S:= "'REWIND'";     |backsp:    S:="'BACKSPACE'";
  |endfile:     S:= "'ENDFILE'";    |open:      S:="'OPEN'";
  |close:       S:= "'CLOSE'";      |then:      S:="'THEN'";
  |else:        S:= "'ELSE'";       |elseif:    S:="'ELSEIF'";
  |endif:       S:= "'END IF'";     |do:        S:="'DO'";
  |if:          S:= "'IF'";         |frmt:      S:="'FORMAT'";
  |end:         S:= "'END'";        |inquire:   S:="'INQUIRE'";
  |to:          S:= "'TO'";         |EOL:       S:="'EOL'";
  |lt:          S:= "'.LT.'";       |le:        S:="'.LE.'";
  |eq:          S:= "'.EQ.'";       |ne:        S:="'.NE.'";
  |ge:          S:= "'.GE.'";       |gt:        S:="'.GT.'";
  |and:         S:= "'.AND.'";      |or:        S:="'.OR.'";
  |eqv:         S:= "'.EQV.'";      |neqv:      S:="'.NEQV.'";
  |not:         S:= "'.NOT.'";      |invKW:     S:="'invKW'";
  |id:          IdStr(idno,S);
  |const: IF cType=Int THEN S:="'Icnst'";
          ELSIF cType=Real    THEN S:="'Rcnst'";
          ELSIF cType=Double  THEN S:="'Dcnst'";
          ELSIF cType=Complex THEN S:="'xcnst'";
          ELSIF cType=Logic   THEN S:="'Lcnst'";
          ELSIF cType=Char    THEN S:="'Ccnst'";
          ELSIF cType=Holl    THEN S:="'Hcnst'";
          END;
  ELSE WriteString(' Что-то не то в VisSy ');
  END; (* case *)
END VisSy;

PROCEDURE Expected(Sy:Symbol);
BEGIN AddErr(-2,ORD(Sy));
END Expected;

PROCEDURE msgExpected(Sy:Symbol);
BEGIN
--  VisSy(sy,St);
--  print("  %s ", St);
  VisSy(Sy,St);
  print("ожидался символ %s\n", St);
END msgExpected;

PROCEDURE ExpectedS(S:ARRAY OF CHAR);
BEGIN
  print("ожидался символ %s\n", S);
END ExpectedS;

PROCEDURE ShowErrors;
  VAR type, pvspos,errpos, pvsno,no:INTEGER;
      i:INTEGER;
BEGIN
  pvspos:=-1; pvsno:=-1; i:=0;
  WHILE NoStmErr>0 DO
    type:=  ErrInfo[i]; INC(i);
    errpos:=ErrInfo[i]; INC(i);
    no:=    ErrInfo[i]; INC(i);
    DEC(NoStmErr);
    IF showline THEN
      IF errpos#pvspos THEN
        DefErrorLine(errpos); pvspos:=errpos;
        ShowErrorLine;
      END;
    END;
    IF type=-1 THEN
      msgError(no);
    ELSIF type=-2 THEN
      msgExpected(Symbol(no));
    ELSE
      idno:=type;
      IF no>=0 THEN msgErrorId(no);
      ELSE          msgWarning(-(no+1));
      END;
    END;
  END; -- while
  IF ErrorCo>=ErrorLimit THEN
    Show("*******************************");
    Show(" Слишком много ошибок. "); HALT;
  END;
END ShowErrors;

END ERRORS;
(* --------------- E N D   E R R O R S ---------------------- *)

PROCEDURE Digit?(ch:CHAR):BOOLEAN;
BEGIN RETURN (ORD(ch)-ORD('0')) IN {0..9} END Digit?;

PROCEDURE Letter?(ch:CHAR):BOOLEAN;
BEGIN
  RETURN ((ORD(ch)>=ORD('A')) AND (ORD(ch)<=ORD('Z'))) OR (* Capital Latin *)
         ((ORD(ch)>=ORD('a')) AND (ORD(ch)<=ORD('z'))) OR (* Bold Latin    *)
         ((ORD(ch)>=300b    ) AND (ORD(ch)<=ORD('Ч'))) OR (* Capital Russ. *)
         ((ORD(ch)>=ORD('ю')) AND (ORD(ch)<=ORD('ч')));   (* Bold Russian  *)
END Letter?;

PROCEDURE Getch;
BEGIN  (* Пробелы игнорируются *)
  ch:=In[No]; INC(No);
  WHILE ch=40c DO ch:=In[No]; INC(No); END;
END Getch;

PROCEDURE lookAhead(c:CHAR):BOOLEAN;
VAR i,level:INTEGER; ch:CHAR;
BEGIN
  i:=No; level:=0;
  LOOP
    ch:=In[i]; INC(i);
    IF (ch=eol) OR (level < 0) THEN RETURN FALSE END;
    IF (ch=c) AND (level = 0) THEN RETURN TRUE END;
    IF ch='(' THEN INC(level);
    ELSIF ch=')' THEN DEC(level)
    END;
    IF ch="'" THEN
      LOOP
        ch:=In[i]; INC(i);
        IF ch=eol THEN RETURN FALSE END;
        IF ch="'" THEN ch:=In[i];
          IF ch#"'" THEN EXIT END;
          INC(i);
        END;
      END; -- loop
      DEC(i);
    END; -- if
  END; -- loop
END lookAhead;

PROCEDURE GetString;
VAR err:BOOLEAN;
BEGIN Slen:=0; err:=FALSE;
  LOOP
    ch:=In[No]; INC(No);
    IF ch=eol THEN Error(1);
    Sval[Slen]:=eol; sy:=const; cType:=Char;
    RETURN;
    END;
    IF ch="'" THEN ch:=In[No];
       IF ch#"'" THEN EXIT END;
       INC(No);
    END;
    Sval[Slen]:=ch;
    IF Slen=maxSlen THEN
       IF NOT err THEN Error(20); err:=TRUE; END;
    ELSE INC(Slen);
    END;
  END;
  Sval[Slen]:=eol; sy:=const; cType:=Char
END GetString;

PROCEDURE GetHoll(len:INTEGER);
  VAR l,i: INTEGER;
BEGIN Slen:=0;
  IF len>maxSlen THEN Error(20); l:=maxSlen;
  ELSE l:=len;
  END;
  FOR i:=1 TO l DO
    ch:=In[No]; INC(No);
    IF ch=eol THEN ch:=40c; DEC(No); END;
    Sval[Slen]:=ch; INC(Slen);
  END;
  i:=Slen; WHILE (i MOD 4)#0  DO Sval[i]:=40c; INC(i); END;
  Sval[i]:=eol; sy:=const; cType:=Holl;
END GetHoll;

PROCEDURE match(kw:ARRAY OF CHAR):BOOLEAN;
  VAR Mark, i: INTEGER;
  BEGIN Mark:=No; i:=0;
    WHILE (kw[i]=ch) & (ch#eol) DO
          Getch; INC(i)
    END;
    IF i#HIGH(kw) THEN No:=Mark; ch:=In[No-1]; RETURN FALSE
    ELSE Go:=FALSE;
  END;
  RETURN TRUE
END match;

PROCEDURE GetKW;
BEGIN
  sy:=invKW;
  CASE ch OF
   'A': IF match('ASSIGN') THEN sy:=asgn END;
  |'B': IF match('BACKSPACE') THEN sy:=backsp
        END;
  |'C': IF match('CALL') THEN sy:=call
        ELSIF match('CLOSE') THEN sy:=close
        ELSIF match('CONTINUE') THEN sy:=cont
        END;
  |'D': IF match('DO') THEN sy:=do
        END;
  |'E': IF match('ELSE') THEN
           IF ch=eol THEN sy:=else
           ELSIF match('IF') THEN sy:=elseif
           END;
        ELSIF match('END') THEN
           IF ch=eol THEN sy:=end
           ELSIF match('IF') THEN sy:=endif
           ELSIF match('FILE') THEN sy:=endfile
           END;
        ELSIF match('ENTRY') THEN sy:=entry
        END;
  |'F': IF match('FORMAT') THEN sy:=frmt
        END;
  |'G': IF match('GOTO') THEN sy:=goto
        END;
  |'I': IF match('IF') THEN sy:=if
        ELSIF match('INQUIRE') THEN sy:=inquire
        END;
  |'O': IF match('OPEN') THEN sy:=open
        END;
  |'P': IF match('PRINT') THEN sy:=pr
        ELSIF match('PAUSE') THEN sy:=pause
        END;
  |'R': IF match('READ') THEN sy:=rd
        ELSIF match('RETURN') THEN sy:=return
        ELSIF match('REWIND') THEN sy:=rewind
        END;
  |'S': IF match('STOP') THEN sy:=stop
        END;
  |'T': IF match('THEN') THEN sy:=then
        ELSIF match('TO') THEN sy:=to
        END;
  |'W': IF match('WRITE') THEN sy:=wr
        END;
  ELSE  sy:=invKW; Go:=FALSE;
  END;
END GetKW;

PROCEDURE GetdclKW;
BEGIN
  sy:=invKW;
  CASE ch OF
   'B': IF match('BLOCKDATA') THEN sy:=bdata END;
  |'C': IF match('CHARACTER') THEN sy:=char
        ELSIF match('COMMON') THEN sy:=comm
        ELSIF match('COMPLEX') THEN sy:=complex
        END;
  |'D': IF match('DIMENSION') THEN sy:=dim
        ELSIF match('DATA') THEN sy:=data
        ELSIF match('DOUBLEPRECISION') THEN sy:=double
        END;
  |'E': IF match('EXTERNAL') THEN sy:=external
        ELSIF match('EQUIVALENCE') THEN sy:=equi
        ELSIF match('ENTRY') THEN sy:=entry
        END;
  |'F': IF match('FUNCTION') THEN sy:=func
        ELSIF match('FORMAT') THEN sy:=frmt
        END;
  |'I': IF match('INTEGER') THEN sy:=int
        ELSIF match('IMPLICIT') THEN sy:=impl
        ELSIF match('INTRINSIC') THEN sy:=intr
        END;
  |'L': IF match('LOGICAL') THEN sy:=log
        END;
  |'P': IF match('PARAMETER') THEN sy:=param
        ELSIF match('PROGRAM') THEN sy:=prog
        END;
  |'R': IF match('REAL') THEN sy:=real
        END;
  |'S': IF match('SAVE') THEN sy:=save
        ELSIF match('SUBROUTINE') THEN sy:=subr
        END;
  ELSE  sy:=invKW; Go:=FALSE;
  END;
END GetdclKW;

PROCEDURE Getop;
BEGIN
  sy:=invKW;
  CASE ch OF
   'A': IF match('AND.') THEN sy:=and END;
  |'E': IF match('EQ.') THEN sy:=eq
        ELSIF match('EQV.') THEN sy:=eqv
        END;
  |'F': IF match('FALSE.') THEN sy:=const; cType:=Logic;
                                Ival:=INTEGER(FALSE)
                           END;
  |'G': IF match('GT.') THEN sy:=gt
        ELSIF match('GE.') THEN sy:=ge
        END;
  |'L': IF match('LT.') THEN sy:=lt
        ELSIF match('LE.') THEN sy:=le
        END;
  |'N': IF match('NE.') THEN sy:=ne
        ELSIF match('NOT.') THEN sy:=not
        ELSIF match('NEQV.') THEN sy:=neqv
        END;
  |'O': IF match('OR.') THEN sy:=or
        END;
  |'T': IF match('TRUE.') THEN sy:=const; cType:=Logic;
                               Ival:=INTEGER(TRUE)
                          END;
  ELSE  sy:=invKW; Go:=FALSE;
  END;
END Getop;

PROCEDURE GetPosKW(VAR sym:Psymbol);
BEGIN
  IF Go THEN Getch ELSE Go:=TRUE END;
  IF ch=')' THEN sym:=Rpar; sy:=rpar;
IF KeyON("l") THEN
    IF sy#invKW THEN  AddStr(")"); END;
END;
  RETURN
  END;
  sym:=invPkw;
  CASE ch OF
   'A': IF match('ACCESS=') THEN sym:=access END;
  |'B': IF match('BLANK=') THEN sym:=blank END;
  |'D': IF match('DIRECT=') THEN sym:=direct
        END;
  |'E': IF match('ERR=') THEN sym:=err
        ELSIF match('END=') THEN sym:=lend
        ELSIF match('EXIST=') THEN sym:=exist
        END;
  |'F': IF match('FORMATTED=') THEN sym:=formatted
        ELSIF match('FORM=') THEN sym:=form
        ELSIF match('FILE=') THEN sym:=file
        ELSIF match('FMT=') THEN sym:=fmt
        END;
  |'I': IF match('IOSTAT=') THEN sym:=iostat
        END;
  |'N': IF match('NAMED=') THEN sym:=named
        ELSIF match('NEXTREC=') THEN sym:=nextrec
        ELSIF match('NUMBER=') THEN sym:=number
        ELSIF match('NAME=') THEN sym:=name
        END;
  |'O': IF match('OPENED=') THEN sym:=opened
        END;
  |'R': IF match('RECL=') THEN sym:=recl
        ELSIF match('REC=') THEN sym:=rec
        END;
  |'S': IF match('SEQUENTIAL=') THEN sym:=sequential
        ELSIF match('STATUS=') THEN sym:=status
        END;
  |'U': IF match('UNFORMATTED=') THEN sym:=unformatted
        ELSIF match('UNIT=') THEN sym:=unit
        END;
  ELSE sym:=invPkw; Go:=FALSE;
  END;
IF KeyON("l") THEN
    IF sym#invPkw THEN VisPosKW(sym,St); AddStr(St); END;
END;
END GetPosKW;

PROCEDURE GetIdent;
  VAR  i,j: INTEGER;
BEGIN
   CASE worktype OF
    kw : GetKW;
   |ass: (* GetId(Id); *)
         Ident[0]:=ch; i:=1; Getch;
         WHILE BOOLEAN(dig?[ch]) OR BOOLEAN(let?[ch]) DO
           IF i>15 THEN i:=15; END;
           Ident[i]:=ch; INC(i); Getch;
         END;
         IF i>15 THEN Ident[15]:=eol; Warning(0);
         ELSE Ident[i]:=eol;
         END;
         Go:=FALSE; idno:=StrId(Ident); sy:=id;
   |dcl: (*declaration*)
         GetdclKW;
   END (* case *);
END GetIdent;

PROCEDURE Fraction(v: INTEGER);
  VAR r,f: REAL;
     e,sg: INTEGER;
BEGIN
  IF ch='.' THEN Getch END;
  cType:=Real; Ival:=INTEGER(1.); r:=FLOAT(v);
  f:=1.;
  WHILE ORD(ch)-ORD('0') IN {0..9} DO f:=f/10.;
    IF f>=Delta THEN
      r:=r + FLOAT(ORD(ch)-ORD('0'))*f;
    ELSE Error(61)
    END; Getch
  END;
  IF ch='D' THEN cType:=Double; ch:='E' END;
  IF CAP(ch)='E' THEN Getch; sg:=+1;
    IF ch='-' THEN sg:=-1; Getch ELSIF ch='+' THEN Getch END;
    e:=0;
    IF NOT((ORD(ch)-ORD('0')) IN {0..9}) THEN Error(62); RETURN END;
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
  Go:=FALSE;
END Fraction;

PROCEDURE GetNumber;
  VAR d: INTEGER;
BEGIN Ival:=0;
  WHILE BOOLEAN(dig?[ch]) DO
    d:=ORD(ch)-ORD('0');
    IF Ival >= (MAX(INTEGER)-d) DIV 10 THEN Error(61)
    ELSE
      Ival:=Ival*10+d;
    END; Getch;
  END;
  sy:=const; cType:=Int;
  Go:=FALSE;
END GetNumber;

PROCEDURE GetConst;
  VAR d,Mark: INTEGER; Frac,GoMark:BOOLEAN;
BEGIN Ival:=0;
  WHILE BOOLEAN(dig?[ch]) DO
    d:=ORD(ch)-ORD('0');
    IF Ival >= (MAX(INTEGER)-d) DIV 10 THEN Error(61)
    ELSE
      Ival:=Ival*10+d;
    END; Getch;
  END; sy:=const; cType:=Int;
  IF (ch ='E') OR (ch='D') THEN Fraction(Ival)
  ELSIF ch='H' THEN GetHoll(Ival)
  ELSIF ch='.' THEN
    Mark:=No; GoMark:=Go; Frac:=FALSE; Getch;
    IF NOT BOOLEAN(let?[ch]) THEN Frac:=TRUE;
    ELSIF ch='E' THEN Getch;
       IF ch#'Q' THEN Frac:=TRUE; END;
    ELSIF ch='D' THEN Frac:=TRUE
    END; No:=Mark; ch:=In[No-1]; Go:=GoMark; -- BackUp1;
    IF Frac THEN Fraction(Ival)
    ELSE Go:=FALSE
    END;
  ELSE Go:=FALSE;
  END;
END GetConst;

PROCEDURE GetSy;
  VAR Id:Idname; i: INTEGER;
BEGIN
  IF Go THEN Getch; ELSE Go:=TRUE; END;
  CASE ch OF
   eol: sy:=EOL; Go:=FALSE;
  |'(': sy:=lpar; |',': sy:=comma;  |'+': sy:=plus;
  |'/': Getch; IF ch='/' THEN sy:=cat ELSE sy:=slash; Go:=FALSE END;
  |')': sy:=rpar; |'-': sy:=minus; |':': sy:=col;  |'=': sy:=becomes;
  |"'"     : GetString;
  |'0'..'9': GetConst;
  |'.': Getch;
        IF Digit?(ch) THEN sy:=const; Fraction(0)
        ELSE Getop END;
  |'*': Getch; IF ch='*' THEN sy:=power; ELSE sy:=times; Go:=FALSE; END;
  ELSE   IF NOT (BOOLEAN(let?[ch])) THEN Error(3); sy:=invKW; RETURN;
         ELSE GetIdent; END;
  END; (* c a s e *)
IF KeyON("l") THEN
    IF sy#invKW THEN VisSy(sy,St); AddStr(St); END;
END;
END GetSy;

PROCEDURE Getfsym;
BEGIN
  IF Go THEN Getch; ELSE Go:=TRUE; END;
  CASE ch OF
   eol: fsym:=invd; Go:=FALSE; sy:=EOL;
  |'(': fsym:=dlpar; |',': fsym:=dcomma; sy:=comma;
  |'+': fsym:=dplus; |'/': fsym:=dslash;
  |')': fsym:=drpar; sy:=rpar;
  |'-': fsym:=dminus; |':': fsym:=dcolon;
  |"'"     : GetString; fsym:=dstring;
  |'0'..'9': GetNumber; fsym:=dnum;
             IF Ival>255 THEN Error(11) END;
  |'.': fsym:=dot; | 'A': fsym:=dA;
  |'I': fsym:=dI;  | 'F': fsym:=dF; | 'E': fsym:=dE;
  |'G': fsym:=dG;  | 'D': fsym:=dD; | 'L': fsym:=dL;
  |'X': fsym:=dX;  | 'P': fsym:=dP; | 'H': fsym:=dH;
  |'T': IF In[No]='L' THEN fsym:=dTL; ch:=In[No]; INC(No);
        ELSIF In[No]='R' THEN fsym:=dTR; ch:=In[No]; INC(No);
        ELSE fsym:=dT
        END;
  |'S': IF In[No]='P' THEN fsym:=dSP; ch:=In[No]; INC(No);
        ELSIF In[No]='S' THEN fsym:=dSS; ch:=In[No]; INC(No);
        ELSE fsym:=dS
        END;
  |'B': IF In[No]='N' THEN fsym:=dBN; ch:=In[No]; INC(No);
        ELSIF In[No]='Z' THEN fsym:=dBZ; ch:=In[No]; INC(No);
        ELSE fsym:=invd;
        END;
  ELSE  Error(0); fsym:=invd; Go:=FALSE;
  END; (* c a s e *)
IF KeyON("l") THEN
    IF fsym#invd THEN Visfsym(fsym,St); AddStr(St); END;
END;

END Getfsym;


PROCEDURE GetLetter(VAR c:CHAR):BOOLEAN; (* for implicit *)
BEGIN
  IF Go THEN Getch; ELSE Go:=TRUE; END;
  IF BOOLEAN(let?[ch]) THEN c:=ch; RETURN TRUE;
  ELSE Error(3); RETURN FALSE; END;
END GetLetter;

PROCEDURE MarkPos;
BEGIN Mark:=No; GoMark:=Go; END MarkPos;

PROCEDURE BackUp;
BEGIN No:=Mark; ch:=In[No-1]; Go:=GoMark; END BackUp;

PROCEDURE MarkPos1;
BEGIN Mark1:=No; GoMark1:=Go; END MarkPos1;

PROCEDURE BackUp1;
BEGIN No:=Mark1; ch:=In[No-1]; Go:=GoMark1; END BackUp1;

PROCEDURE SkipTo(s:Symbol);
BEGIN
  WHILE (sy#s) AND (sy#EOL) DO GetSy; END;
END SkipTo;

PROCEDURE GetSnumber(VAR R:REAL;):BOOLEAN;
  VAR neg:BOOLEAN;
BEGIN
  GetSy; neg:=FALSE;
  IF sy=minus THEN  neg:=TRUE; GetSy;
  ELSIF sy=plus THEN  GetSy;
  END;
  IF sy#const THEN RETURN TRUE END;
  IF cType=Real THEN
    IF neg THEN R:=-Rval;
    ELSE        R:=Rval;
    END; RETURN FALSE;
  ELSIF cType=Int THEN R:=FLOAT(Ival);
    IF neg THEN R:=-R; END;
    RETURN FALSE;
  ELSE RETURN TRUE
  END;
END GetSnumber;

PROCEDURE CxConst():BOOLEAN;
  VAR Re,Im:REAL;
BEGIN
   MarkPos1;
   IF GetSnumber(Re) THEN BackUp1; RETURN FALSE; END;
   GetSy;
   IF sy#comma       THEN BackUp1; RETURN FALSE; END;
   IF GetSnumber(Im) THEN BackUp1; RETURN FALSE; END;
   GetSy;
   IF sy#rpar THEN Expected(rpar) END;
   Rval:=Re; Ival:=WORD(Im); cType:=Complex; sy:=const;
   RETURN TRUE;
END CxConst;

VAR Buff:ARRAY [0..79] OF CHAR;
    LenB:INTEGER; (* length of string in Buff *)
    LastString:BOOLEAN;

PROCEDURE KeyON(c:CHAR):BOOLEAN;
BEGIN
  IF (c<"a") OR (c>"z") THEN RETURN FALSE
  ELSE RETURN ORD(c)-ORD("a") IN Keys
  END
END KeyON;

PROCEDURE SetKey(c:CHAR;Yes:BOOLEAN);
BEGIN
  IF (c<"a") OR (c>"z") THEN RETURN
  ELSE
    IF Yes THEN INCL(Keys, ORD(c)-ORD("a"))
    ELSE        EXCL(Keys, ORD(c)-ORD("a"))
    END;
  END
END SetKey;

PROCEDURE ONKeys;
BEGIN
  Keys:={0..25};
END ONKeys;

PROCEDURE OFFKeys;
BEGIN
  Keys:={}; OutStr;
END OFFKeys;

PROCEDURE ParsKeys;
VAR i: INTEGER; Yes:BOOLEAN;
BEGIN
  i:=1; ch:=Buff[i];
  IF ch="N" THEN Yes:=TRUE
  ELSIF ch="F" THEN Yes:=FALSE; i:=2;
  ELSE RETURN
  END; INC(i);
  ch:=Buff[i]; INC(i);
  WHILE ch=40c DO ch:=Buff[i]; INC(i); END;
  IF ch=0c THEN
    IF Yes THEN ONKeys ELSE OFFKeys END;
    RETURN;
  END;
  WHILE BOOLEAN(let?[ch]) DO
    SetKey(ch,Yes);
    ch:=Buff[i]; INC(i);
    WHILE ch=40c DO ch:=Buff[i]; INC(i); END;
  END;
END ParsKeys;

PROCEDURE ContinueString?():BOOLEAN;
  VAR i: INTEGER;
BEGIN
  LenB:=StdIO.GetS(S,Buff); INC(Line);
  IF LenB=0 THEN LastString:=TRUE;
                 CloseSou;
                 RETURN FALSE;
  END;
  (*$T-*) ch:=Buff[0]; i:=0;
  (* Отработка комментариев и пустых строк *)
  IF (ch="C") OR (ch=eol) OR (ch="*") THEN
    RETURN ContinueString?();
  END;
  (* отработка ON OFF оператора *)
  IF ch="O" THEN
    ParsKeys;
    RETURN ContinueString?();
  END;
  WHILE ch=40c DO INC(i); ch:=Buff[i]; END;
  (*$T+*)
  IF i#5 THEN RETURN FALSE;
  ELSIF ch="0" THEN Error(43);
  END;
  RETURN TRUE;
END ContinueString?;

PROCEDURE GetFStr;
  VAR i,err:INTEGER;
BEGIN   err:=0;
  MOVE(ADR(In),ADR(Buff),(LenB+3) DIV 4); No:=LenB-1;
-- IF KeyON('x')  THEN Show(Buff); END;
  Fline[0].no:=Line; Fline[0].pos:=No; ncont:=0;
  (* Отработка продолжения строки *)
  WHILE ContinueString?() DO
    INC(ncont);
    IF ncont>19 THEN err:=10; ncont:=19; END;
    Fline[ncont].no:=Line;
    i:=6; ch:=Buff[6];
    WHILE ch#eol DO
      In[No]:=ch; INC(i);
      ch:=Buff[i]; INC(No);
      IF No>InSize THEN err:=65; DEC(No); END;
    END;
    In[No]:=eol;
    Fline[ncont].pos:=No;
  END;
  IF err#0 THEN Error(err); END;
END GetFStr;

PROCEDURE NewLine;
  VAR sum: INTEGER;
BEGIN (*$T-*)
--    OutStr;
  IF NoStmErr#0 THEN ShowErrors; END;
  IF BuffIsEmpty THEN Fault(12); END;
  GetFStr; BuffIsEmpty:=LastString;
  sum := 0; No:=0; Getch;
  IF No<6 THEN  (* Label exist *)
    WHILE BOOLEAN(dig?[ch]) AND (No<6) DO
      sum:=sum*10+ORD(ch)-ORD('0');
      ch:=In[No]; INC(No);
    END;
    IF sum=0 THEN Error(4); END;
    IF ch=40c THEN Getch; END;
  END;
  IF No<7 THEN
    IF sum#0 THEN Error(6); END;
--  Label:=0; No:=6; Go:=TRUE; RETURN;
  END;
  Label:=sum; Go:=FALSE;
  IF Label>0 THEN pLabel(Label); END;
  MarkPos; RETURN;
  (*$T+*)
END NewLine;

PROCEDURE SetWork(Type: Work);
BEGIN worktype:=Type; END SetWork;

PROCEDURE InitScan;
BEGIN
  InitObjects; InitHeap; ErrorCo:=0;
END InitScan;

PROCEDURE InitZeroScan(VAR FilNm:ARRAY OF CHAR);
VAR i: INTEGER; t:ARRAY [0..79] OF CHAR; c: CHAR;
BEGIN
  FOR c:=0c TO 377c DO let?[c]:=CHAR(Letter?(c)) END;
  FOR c:=0c TO 377c DO dig?[c]:=CHAR(Digit? (c)) END;
  StdIO.WriteLn;
  StdIO.WriteString("  FORTRAN-77 /11-Nov-89/ ");
  StdIO.print('"%s"\n',FilNm);
  S:=StdIO.Open(FilNm); FileNm:=FilNm;
  IF S<0 THEN StdIO.Why?(S,t); StdIO.print("%s%s\n",FilNm,t); HALT END;
  Line:=0; BuffIsEmpty:=FALSE; LastString:=FALSE;
  InitStr; OFFKeys;
  ch:="C";
  WHILE (ch="C") OR (ch="*") OR (ch=eol) DO
    LenB:=StdIO.GetS(S,Buff); INC(Line);
    IF LenB=0 THEN Fault(5); END;
-- IF KeyON('x') THEN Show(Buff); END;
    ch:=Buff[0];
    IF ch="O" THEN
      ParsKeys; ch:="C"
    END;
  END;
  NoStmErr:=0; ShowLine(TRUE);
END InitZeroScan;

BEGIN
END fcScan.
