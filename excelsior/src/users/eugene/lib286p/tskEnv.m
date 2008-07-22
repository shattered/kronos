IMPLEMENTATION MODULE tskEnv; (* Ned 28-Sep-89. (c) KRONOS *)

IMPORT  sys: SYSTEM;
IMPORT  err: defErrors;
IMPORT  def: def_GDT;
IMPORT  mem: Heap;

WITH STORAGE : mem;

CONST ok=err.ok;

PROCEDURE get_str(VAL name: ARRAY OF CHAR; VAR s: STRING);
  VAR p: POINTER TO ARRAY [0..0FFFFh] OF CHAR; i,j,k: INTEGER; equ: BOOLEAN;
BEGIN
  done:=TRUE;
  NEW(s);
  IF name=args THEN
    p:=def.prm_a; i:=0;
    WHILE p^[i]#0c DO INC(i) END; INC(i);
    NEW(s,i);
    FOR j:=0 TO HIGH(s) DO s[j]:=p^[j] END;
  ELSE
    p:=def.env_a; i:=0;
    LOOP
      IF p^[i]=0c THEN done:=FALSE; error:=err.bad_name; RETURN END;
      equ:=TRUE; j:=0;
      LOOP
        IF p^[i]='=' THEN INC(i); EXIT END;
        IF name[j]=0c THEN
          equ:=FALSE;
        ELSE
          equ:=equ & (name[j]=p^[i]); INC(j);
        END;
        INC(i);
      END;
      equ:=equ & (name[j]=0c);
      IF equ THEN
        j:=i;
        WHILE p^[j]#0c DO INC(j) END; INC(j);
        NEW(s,j-i);
        FOR k:=0 TO HIGH(s) DO s[k]:=p^[i]; INC(i) END;
        RETURN
      ELSE
        WHILE p^[i]#0c DO INC(i) END; INC(i);
      END;
    END;
  END;
END get_str;

PROCEDURE get_env(VAL nm: ARRAY OF CHAR;
                      ms: INTEGER;
                   VAR s: STRING;
                   VAR p: BOOLEAN);
BEGIN
  done:=TRUE;
  get_str(nm,s); p:=TRUE;
END get_env;

BEGIN done:=TRUE; error:=0;
END tskEnv.
