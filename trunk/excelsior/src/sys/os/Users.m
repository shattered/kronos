IMPLEMENTATION MODULE Users[1]; (*$U+ Leo 28-Feb-90. (c) KRONOS *)

IMPORT  str: Strings;
IMPORT  low: lowLevel;
IMPORT   os: osKernel;

TYPE
  USR = POINTER TO
        RECORD
          gro : INTEGER;
          priv: BOOLEAN;
          name: ARRAY [0..7] OF CHAR;
          pass: ARRAY [0..7] OF CHAR;
          run : STRING;
        END;

  GRO = POINTER TO
        RECORD
          priv: BOOLEAN;
          name: ARRAY [0..7] OF CHAR
        END;

VAR usr: ARRAY [0..127] OF USR;
    gro: ARRAY [0..127] OF GRO;

PROCEDURE user(): INTEGER;
  VAR t: os.task_ptr;
BEGIN
  t:=os.self(); RETURN t^.user
END user;

PROCEDURE su(user: INTEGER): BOOLEAN;
BEGIN
  RETURN (user=0) OR (BITSET(user)*{7}#{})
END su;

PROCEDURE pack(u,g: INTEGER; priv: BOOLEAN): INTEGER;
BEGIN
  u:=u MOD 80h;
  g:=g MOD 80h + INTEGER((1-ORD(NOT priv))<<7);
  RETURN u*100h+g
END pack;

PROCEDURE unpack(p: INTEGER; VAR u,g: INTEGER; VAR priv: BOOLEAN);
BEGIN
  priv:=(p=0) OR (p MOD 100h>=80h);
  g:=p MOD  80h;
  u:=p DIV 100h MOD 80h
END unpack;

PROCEDURE get_user(VAR u: USER);
BEGIN
  u.done:=FALSE;
  WITH u DO gro :=-1; priv:=FALSE; name:=""; pass:="" END;
  IF usr[u.usr]=NIL THEN RETURN END;
  WITH usr[u.usr]^ DO
    u.gro:=gro; u.priv:=priv; u.name:=name;
    IF su(user()) THEN        u.pass:=pass
    ELSE low.fill(u.pass,-1); u.pass[7]:=0c
    END
  END;
  u.done:=TRUE
END get_user;

PROCEDURE get_shell(VAR u: USER; VAR shell: ARRAY OF CHAR);
BEGIN
  u.done:=FALSE;
  IF usr[u.usr]=NIL THEN RETURN END;
  WITH usr[u.usr]^ DO str.copy(shell,run) END;
  u.done:=TRUE
END get_shell;

PROCEDURE get_group(VAR u: USER);
BEGIN
  u.done:=FALSE;
  WITH u DO usr:=-1; priv:=FALSE; name:=""; pass:="" END;
  IF gro[u.gro]=NIL THEN RETURN END;
  u.done:=TRUE;
  WITH gro[u.gro]^ DO u.usr:=0; u.name:=name; u.priv:=priv END
END get_group;

PROCEDURE set_user (VAR u: USER; VAL shell: ARRAY OF CHAR);
  VAR len: INTEGER;
BEGIN
  u.done:=FALSE;
  IF NOT su(user()) THEN RETURN END;
  len:=str.len(shell)+1;
  IF usr[u.usr]=NIL THEN
    os.ALLOCATE(os.system,usr[u.usr],SIZE(usr[u.usr]^));
    IF usr[u.usr]=NIL THEN RETURN END;
    WITH usr[u.usr]^ DO run^.ADR :=NIL; run^.HIGH:=-1 END
  ELSE
    WITH usr[u.usr]^ DO
      IF BYTES(run)<len THEN
        os.DEALLOCATE(os.system,run^.ADR,SIZE(run)); run^.HIGH:=-1
      END
    END
  END;
  INC(no_users);
  WITH usr[u.usr]^ DO
    gro :=u.gro;
    priv:=u.priv;
    name:=u.name; name[7]:=0c;
    pass:=u.pass; pass[7]:=0c;
    IF BYTES(run)<len THEN
      os.ALLOCATE(os.system,run^.ADR,(len+3) DIV 4);
      IF run^.ADR#NIL THEN run^.HIGH:=len-1
      ELSE                 run^.HIGH:=-1; RETURN END
    END;
    str.copy(run,shell)
  END;
  u.done:=TRUE
END set_user;

PROCEDURE set_group(VAR u: USER);
  VAR len: INTEGER;
BEGIN
  u.done:=FALSE;
  IF NOT su(user()) THEN RETURN END;
  IF gro[u.gro]=NIL THEN
    os.ALLOCATE(os.system,gro[u.gro],SIZE(gro[u.gro]^));
    IF gro[u.gro]=NIL THEN RETURN END
  END;
  INC(no_groups);
  WITH gro[u.gro]^ DO priv:=u.priv; name:=u.name; name[7]:=0c END;
  u.done:=TRUE
END set_group;

PROCEDURE find(VAR u: USER);
  VAR i: INTEGER; x: USR;
BEGIN
  u.done:=TRUE;
  u.usr:=-1; u.gro:=-1;
  FOR i:=0 TO 127 DO
    x:=usr[i];
    IF (x#NIL) & (x^.name=u.name) THEN u.usr:=i; RETURN END
  END;
  u.done:=FALSE
END find;

BEGIN
  low.fill(usr,NIL);
  low.fill(gro,NIL);
  no_users:=0; no_groups:=0
END Users.
