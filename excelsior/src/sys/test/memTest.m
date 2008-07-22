IMPLEMENTATION MODULE memTest; (* Leo 17-Aug-91. (c) KRONOS *)

IMPORT  cod: defCodes;

PROCEDURE pop(): ADDRESS; CODE END pop;
PROCEDURE push(w: WORD) ; CODE END push;
PROCEDURE copt;           CODE cod.copt END copt;
PROCEDURE swap;           CODE cod.swap  END swap;
PROCEDURE lsw0(a: WORD): WORD; CODE cod.lsw0 END lsw0;
PROCEDURE ssw0(a,w: WORD);     CODE cod.ssw0 END ssw0;

PROCEDURE WritePeace(lo,hi: ADDRESS; pat: WORD);
  VAR a,x: ADDRESS; pat1: WORD; min: INTEGER;
BEGIN
  IF lo>hi THEN RETURN END;
  FOR x:=lo TO hi BY 33 DO
    pat1:=pat; min:=x+32;
    IF min>hi THEN min:=hi END;
    checked:=x;
    push(x);              (*1*)
    REPEAT
      copt;               (*2*)  (* x,x                      *)
      push(pat1<<1);      (*3*)  (* x,x,pat1<<1              *)
      copt;               (*4*)  (* x,x,pat1<<1,pat1<<1      *)
      pat1:=pop();        (*3*)  (* x,x,pat1<<1              *)
      ssw0(pop(),pop());  (*1*)  (* x                        *)
      push(pop()+1);      (*1*)  (* x                        *)
      copt;               (*2*)  (* x,x                      *)
    UNTIL pop()>min;      (*1*)
    IF pop()#0 THEN END;
  END;
END WritePeace;

PROCEDURE ReadPeace(lo,hi: ADDRESS; pat: WORD; error: SimpleError);
  VAR a,x: ADDRESS; pat1,rd: WORD; min: INTEGER; break: BOOLEAN;
BEGIN
  IF lo>hi THEN RETURN END;
  FOR x:=lo TO hi BY 33 DO
    pat1:=pat; min:=x+32;
    IF min>hi THEN min:=hi END;
    checked:=x;
    push(x);              (*1*)
    REPEAT
      copt;               (*2*)
      push(lsw0(pop()));  (*2*)
      copt;               (*3*)
      rd:=pop();          (*2*)
      push(pat1<<1);      (*3*)
      copt;               (*4*)
      pat1:=pop();        (*3*)
      IF pop()#pop()      (*1*)
      THEN
        a:=pop();         (*0*)
        break:=error(a,rd,pat1);
        IF break THEN RETURN END;
        push(a);          (*1*)
      END;
      push(pop()+1);      (*1*)
      copt;               (*2*)
    UNTIL pop()>min;      (*1*)
    IF pop()#0 THEN END;
  END;
END ReadPeace;



(********************* ReadAfterWrite *************************)
(* По каждому адресу пишется образец и немедленно читается.   *)
(* Ошибка показывает несовпадение записанного и прочитанного  *)
(* значений, и может являться следствием неисправного бита в  *)
(* кристале памяти. При переходе к следующему адресу образец  *)
(* сдвигается циклически влево на один бит,                   *)
(**************************************************************)

PROCEDURE ReadAfterWrite(lo,hi: ADDRESS; pat: WORD; error: SimpleError);
  VAR a,x: ADDRESS; pat1,rd: WORD; min: INTEGER; break: BOOLEAN;
BEGIN
  IF lo>hi THEN RETURN END;
  FOR x:=lo TO hi BY 33 DO
    pat1:=pat; min:=x+32;
    IF min>hi THEN min:=hi END;
    checked:=x;
    push(x);                      (*1*)
    REPEAT
      copt;                       (*2*)  (*                 x,x                 *)
      push(pat1<<1);              (*3*)  (*                 x,x,pat1<<1         *)
      copt;                       (*4*)  (*                 x,x,pat1<<1,pat1<<1 *)
      pat1:=pop();                (*3*)  (* pat1:=pat1<<1   x,x,pat1<<1         *)
      ssw0(pop(),pop());          (*1*)  (* x^:=pat1<<1     x                   *)
      copt;                       (*2*)  (*                 x,x                 *)
      push(lsw0(pop()));          (*2*)  (*                 x,x^                *)
      copt;                       (*3*)  (*                 x,x^,x^             *)
      rd:=pop();                  (*2*)  (*                 x,x^                *)
      IF WORD(pop())#pat1 THEN    (*1*)  (* x^#pat1         x                   *)
        a :=pop();                (*0*)  (* a:=x;                               *)
        break:=error(a,rd,pat1);
        IF break THEN RETURN END;
        push(a);                  (*1*)  (*                 x                   *)
      END;                        (*1*)  (*                 x                   *)
      push(pop()+1);              (*1*)  (* x:=x+1          x                   *)
      copt;                       (*2*)  (*                 x,x                 *)
    UNTIL pop()>min;              (*1*)  (* x>min           x                   *)
    IF pop()#0 THEN END;
  END;
END ReadAfterWrite;

(********************* WriteAllAndRead  ***********************)
(* По каждому адресу пишется образец. При переходе к          *)
(* следующему адресу образец сдвигается циклически влево на   *)
(* один бит. После того как вся область прописана, вся она    *)
(* читается и сравнивается с тем же образцом.                 *)
(* Ошибка показывает несовпадение записанного и прочитанного  *)
(* значений, и может являться следствием неисправного бита в  *)
(* кристале памяти или искажения адреса при чтении записи.    *)
(**************************************************************)


PROCEDURE WriteAllAndRead(lo,hi: ADDRESS; pat: WORD; error: SimpleError);
  VAR i,j: INTEGER;
BEGIN
  WritePeace(lo,hi,pat);
  FOR i:=0 TO 100*50 DO END; i:=i; i:=i;
  FOR j:=0 TO 100*50 DO END;
  ReadPeace (lo,hi,pat,error);
END WriteAllAndRead;

(************************ FastAddress *************************)
(* По каждому адресу записывает значение побитового XOR       *)
(* самого адреса и образца. Запись производится в прямом или  *)
(* обратном направлении. После заполнение всей области        *)
(* производится чтение и проверка.                            *)
(* Ошибка показывает несовпадение записанного и прочитанного  *)
(* значений, и может являться следствием искажения адреса при *)
(* чтении/записи, перепутывания адресов, или неисправного     *)
(* бита в кристале памяти.                                    *)
(**************************************************************)

PROCEDURE FastAddresses(lo,hi: ADDRESS; pat: BITSET; dir: INTEGER;
                        error: SimpleError);
  VAR a,x: ADDRESS; min: ADDRESS; rd: WORD; break: BOOLEAN;
BEGIN
  IF lo>hi THEN RETURN END;
  IF dir=+1 THEN
    push(lo);                       (*1*)
    REPEAT
      copt;                         (*2*)
      copt;                         (*3*)
      push(BITSET(pop()) / pat);
      ssw0(pop(),pop());            (*1*)
      push(pop()+1);                (*1*)
      copt;                         (*2*)
    UNTIL pop()>hi;                 (*1*)
    IF pop()#0 THEN END;
  END;
  IF dir=-1 THEN
    push(hi);                       (*1*)
    REPEAT
      copt;                         (*2*)
      copt;                         (*3*)
      push(BITSET(pop()) / pat);
      ssw0(pop(),pop());            (*1*)
      push(pop()-1);                (*1*)
      copt;                         (*2*)
    UNTIL pop()<lo;                 (*1*)
    IF pop()#0 THEN END;
  END;
  x:=lo;
  WHILE x<=hi DO
    min:=x+400h;
    IF min>hi THEN min:=hi END;
    checked:=x;
    push(x);                        (*1*)
    REPEAT
      copt;                         (*2*)
      copt;                         (*3*)
      rd:=lsw0(pop());              (*2*)
      push(BITSET(pop())/pat);      (*2*)
      IF WORD(pop()) # rd                 (*1*)
      THEN
        a:=pop();                   (*0*)
        break:=error(a,rd,BITSET(a)/pat);
        IF break THEN RETURN END;
        push(a);                    (*1*)
      END;
      push(pop()+1);                (*1*)
      copt;                         (*2*)
    UNTIL pop()>min;                (*1*)
    IF pop()#0 THEN END;
    x:=min+1;
  END;
END FastAddresses;

(************************** Addresses *************************)
(* По каждому адресу записывает значение образца. При переходе*)
(* к следующему адресу значение образца сдвигается циклически *)
(* влево на один бит. Немедленно после записи образца по всем *)
(* адресам отличающегося от текущего в одном бите производится*)
(* побитовое инвертирование их содержимого и при каждом       *)
(* инвертировании проверяется значение по текущему адресу.    *)
(* Ошибка показывает несовпадение записанного и прочитанного  *)
(* значений, и может являться следствием "слипания" адресов   *)
(* чтения/записи, перепутывания адресов, или неисправного     *)
(* бита в кристале памяти.                                    *)
(**************************************************************)


PROCEDURE Addresses(lo,hi: ADDRESS; pat: WORD; error: AddressError);
  TYPE bool=BITSET;  CONST false={};

  VAR    a,x,b: ADDRESS;   break: BOOLEAN;
           min: INTEGER;
            rd: WORD;
        barrel: BITSET;

         save0: WORD;
         save1: WORD;
         save2: WORD;
         save3: WORD;

BEGIN   barrel:={0};
  WritePeace(lo,hi,pat);
  REPEAT barrel:=barrel<<1 UNTIL ADDRESS(barrel)>hi;
  IF lo>hi THEN RETURN END;
  x:=lo;
  WHILE x<=hi DO
    min:=x+3FFh;
    IF min>hi THEN min:=hi END;
    checked:=x;
    a:=x;
    REPEAT
      pat:=pat<<1;
      a^:=pat;
      push(barrel);                             (*1*)
      REPEAT
        push(pop()>>1);                         (*1*)
        copt;                                   (*2*)
        b:=ADDRESS(BITSET(pop())/BITSET(a));    (*1*)
        IF bool(b>=lo)*bool(b<=hi)*bool(b#a)#false THEN
          push(b);                              (*2*)  (* barrel,b *)
          copt;                                 (*3*)  (* barrel,b,b *)
          push(lsw0(pop()));                    (*3*)  (* barrel,b,b^ *)
          push(b);                              (*4*)  (* barrel,b,b^,b *)
          copt;                                 (*5*)  (* barrel,b,b^,b,b *)
          push(lsw0(pop()));                    (*5*)  (* barrel,b,b^,b,b^ *)
          push(BITSET(pop()) / {0..31});        (*3*)  (* barrel,b,b^ *)
          ssw0(pop(),pop());
          rd:=a^;
          IF rd#pat THEN                        (*3*)
            save0:=pop();
            save1:=pop();
            save2:=pop();

            break:=error(a,rd,pat,b);
            IF break THEN RETURN END;

            push(save2);
            push(save1);
            push(save0)
          END;                                  (*3*)
          ssw0(pop(),pop());                    (*1*)
        END;
        copt;                                   (*2*)
      UNTIL BITSET(pop())={0};                  (*1*)
      IF pop()#0 THEN END;
      a:=a+1;
    UNTIL a>min;
    x:=min+1;
  END;
END Addresses;

(*********************** Crowl Test ***************************)
(* Аналогичен Read/Write тестам но работает кусочками по 33   *)
(* слова.                                                     *)
(**************************************************************)

PROCEDURE CrowlWriteAndRead(lo,hi: ADDRESS; pat: WORD; error: SimpleError);
  VAR a,x: ADDRESS; pat1,rd: WORD; min: INTEGER;  break: BOOLEAN;
BEGIN
  IF lo>hi THEN RETURN END;
  FOR x:=lo TO hi BY 33 DO
    pat1:=pat; min:=x+32;
    IF min>hi THEN min:=hi END;
    checked:=x;
    push(x);             (*1*)
    REPEAT
      copt;              (*2*)
      push(pat1<<1);     (*3*)
      copt;              (*4*)
      pat1:=pop();       (*3*)
      ssw0(pop(),pop()); (*1*)
      push(pop()+1);     (*1*)
      copt;              (*2*)
    UNTIL pop()>min;     (*1*)
    IF pop()#0 THEN END;
    pat1:=pat;
    checked:=x;
    push(x);              (*1*)
    REPEAT
      copt;               (*2*)
      push(lsw0(pop()));  (*2*)
      copt;               (*3*)
      rd:=pop();          (*2*)
      push(pat1<<1);      (*3*)
      copt;               (*4*)
      pat1:=pop();        (*3*)
      IF pop()#pop()      (*1*)
      THEN
        a:=pop();         (*0*)
        break:=error(a,rd,pat1);
        IF break THEN RETURN END;
        push(a);          (*1*)
      END;
      push(pop()+1);      (*1*)
      copt;               (*2*)
    UNTIL pop()>min;      (*1*)
    IF pop()#0 THEN END;
  END;
END CrowlWriteAndRead;

(************************** Overlap ***************************)
(* Записывае инвертированным образцом участок в 33 слова и    *)
(* проверяет что значения в остальной области при этом не     *)
(* исказились. Двигает такой участок от начала до конца       *)
(* области, по 33 слова.                                      *)
(* Ошибка показывает несовпадение записанного и прочитанного  *)
(* значений, и может являться следствием взаимного влияния    *)
(* смежных областей памяти, "слипания" адресов                *)
(* чтения/записи, перепутывания адресов, или неисправного     *)
(* бита в кристале памяти.                                    *)
(**************************************************************)


PROCEDURE Overlap(lo,hi: ADDRESS; pat: WORD; error: OverlapError);

  VAR OverlapLoInv, OverlapHiInv: ADDRESS;   break: BOOLEAN;

  PROCEDURE ReadPeace(lo,hi: ADDRESS; pat: WORD);
    VAR a,x: ADDRESS; pat1,rd: WORD; min: INTEGER;
  BEGIN
    IF lo>hi THEN RETURN END;
    FOR x:=lo TO hi BY 33 DO
      pat1:=pat; min:=x+32;
      IF min>hi THEN min:=hi END;
      checked:=x;
      push(x);              (*1*)
      REPEAT
        copt;               (*2*)
        push(lsw0(pop()));  (*2*)
        copt;               (*3*)
        rd:=pop();          (*2*)
        push(pat1<<1);      (*3*)
        copt;               (*4*)
        pat1:=pop();        (*3*)
        IF pop()#pop()      (*1*)
        THEN
          a:=pop();         (*0*)
          break:=error(a,rd,pat1,OverlapLoInv,OverlapHiInv);
          IF break THEN RETURN END;
          push(a);          (*1*)
        END;
        push(pop()+1);      (*1*)
        copt;               (*2*)
      UNTIL pop()>min;      (*1*)
      IF pop()#0 THEN END;
    END;
  END ReadPeace;

  VAR x: ADDRESS; min: INTEGER;
BEGIN x:=lo;
  WritePeace(lo,hi,pat);
  WHILE x<=hi DO min:=x+(33*33*3-1);
    IF min>hi THEN min:=hi END;
    OverlapLoInv:=x;  OverlapHiInv:=min;
    WritePeace(x,min,{0..31}/BITSET(pat));
    ReadPeace(lo,x-1,pat);
    IF break THEN RETURN END;
    ReadPeace(min+1,hi,pat);
    IF break THEN RETURN END;
    ReadPeace(x,min,{0..31}/BITSET(pat));
    IF break THEN RETURN END;
    WritePeace(x,min,pat);
    ReadPeace(x,hi,pat);
    IF break THEN RETURN END;
    x:=min+1;
  END;
END Overlap;

BEGIN
  checked:=0FFFFFFFFh
END memTest.
