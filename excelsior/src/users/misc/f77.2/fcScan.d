DEFINITION MODULE fcScan;

FROM SYSTEM IMPORT ADDRESS;
FROM fcObj  IMPORT  Types, Idname;
FROM fcDefs IMPORT  maxSlen;

TYPE Symbol=( int, real, double, complex, log, char, holl,
              subr, func, prog, bdata, entry,
              impl, comm, data, external, equi, param, intr, save, dim,
              call, goto, cont, return, stop, rd, wr, pr, asgn, pause, rewind,
              backsp, endfile, open, close, then, else, elseif, endif,
              do, if, frmt, end, inquire, to,
              comma, col, becomes,  dollar,
              lpar, rpar, EOL, id, const,
              power, cat, slash, times, plus, minus, lt, le, eq, ne,
              ge, gt, and, or, eqv, neqv, not,  invKW );

     Psymbol=( access, blank, direct, err, lend, exist, formatted,
               form, fmt, file, iostat, named, name, number,
               nextrec, opened, recl, rec, sequential, status,
               unformatted, unit, Rpar, invPkw );

     Work  =( kw, ass, dcl );
     Formsym=( invd,  startPos, dlpar, drpar, dslash, dP,
               eofFt, dI,  dI2, dF,    dE,    dE3,    dD,
               dG,    dG3, dL,  dA,    dAw,   dcolon, dH,
               dT,    dTL, dTR, dX,    dS,    dSP,    dSS,
               dBN,   dBZ,
               dcomma, dplus, dminus, dot, dnum, dstring );

     Filename  =ARRAY [0..79] OF CHAR;

VAR
    cType: Types;                (* Тип константы *)
    Ival: INTEGER;               (* Значение при sy=const *)
    Rval: REAL;                  (* Значение при sy=realconst *)
    Slen: INTEGER;              (* Длина текстовой константы *)
    Sval: ARRAY [0..maxSlen] OF CHAR; (* Значение текстовой константы  *)
    Label: INTEGER;             (* >0 если метка в строке есть, =0 иначе *)
    sy: Symbol;                  (* Текущий Символ *)
    ErrorCo:INTEGER;            (* Счетчик ошибок *)
    NoStmErr:INTEGER;            (* Счетчик ошибок в операторе *)
    BuffIsEmpty:BOOLEAN;         (* была взята последняя строка из программы*)
    fsym : Formsym;

PROCEDURE GetSy; (* Берет следующую лексему *)

PROCEDURE NewLine;
(* Берет новую строку, если ее нет то Fault(12);
   если в строке есть метка, то возвращает Label=Метке.*)

PROCEDURE MarkPos; PROCEDURE MarkPos1;
(* Маркируют позицию в строке для возврата при отходе *)

PROCEDURE BackUp;  PROCEDURE BackUp1;
(* Отход в маркированную позицию *)

PROCEDURE SetWork(Type:Work);
(* Устанавливает режим работы сканера *)

PROCEDURE Fault(n:INTEGER);
(* Фатальная ошибка *)

PROCEDURE Error(n:INTEGER);
(* Сообщение об ошибке  *)

PROCEDURE Expected(Sy:Symbol);
(* Сообщение o символе, который должен был быть *)

--PROCEDURE ExpectedS(S:ARRAY OF CHAR);

PROCEDURE ErrorId(n:INTEGER);
(* Сообщение об ошибке, связанное с идентификатором *)
PROCEDURE Warning(n:INTEGER);

PROCEDURE ShowLine(show:BOOLEAN);

PROCEDURE ShowErrors;

PROCEDURE GetPosKW(VAR sym:Psymbol);

PROCEDURE lookAhead(c:CHAR):BOOLEAN;

PROCEDURE SkipTo(Sy:Symbol);

PROCEDURE GetHoll(len:INTEGER);

PROCEDURE match(kw:ARRAY OF CHAR):BOOLEAN;

PROCEDURE Getfsym;
PROCEDURE CxConst():BOOLEAN;

PROCEDURE GetLetter(VAR c:CHAR):BOOLEAN;
PROCEDURE InitScan;
PROCEDURE InitZeroScan(VAR nm:ARRAY OF CHAR);

PROCEDURE VisSy(sy:Symbol;VAR S:Idname);

PROCEDURE KeyON(c:CHAR):BOOLEAN;

END fcScan.
