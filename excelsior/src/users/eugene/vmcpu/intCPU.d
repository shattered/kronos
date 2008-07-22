DEFINITION MODULE intCPU; (* Sem 05-Jun-88. (c) KRONOS *)

CONST
    Nil           = 07FFFFF80h;
    ExternalBit   = 31;
    ChangeMaskBit = 30;

VAR
    P       : INTEGER;  -- адрес дискриптора текущего процесса
    L       : INTEGER;  -- адрес локальных переменных текущей процедуры
    S       : INTEGER;  -- начало свободного места на P-стеке
    H       : INTEGER;  -- верхняя граница P-стека
    M       : BITSET;   -- маска прерываний
    G       : INTEGER;  -- адрес заголовка модуля
    D       : INTEGER;  -- адрес глобальных переменных текущего модуля
    F       : INTEGER;  -- адрес кода текущего модуля
    PC      : INTEGER;  -- счетчик команд
    AStack  : ARRAY [0..7] OF INTEGER; -- стек выражений
    sp      : INTEGER;  -- указатель стека выражений

PROCEDURE Execute;

END intCPU.
