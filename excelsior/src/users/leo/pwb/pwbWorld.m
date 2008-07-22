IMPLEMENTATION MODULE pwbWorld; (*    Ilx  29-Mar-90. (c) KRONOS *)
                                (*$N- John 26-Apr-91. (c) KRONOS *)

IMPORT       SYSTEM;
IMPORT  cod: defCodes;
IMPORT  err: defErrors;
IMPORT  str: Strings;
IMPORT  bio: BIO;
IMPORT  mem: Heap;

WITH STORAGE (NEW    : mem.allocate;
              DISPOSE: mem.deallocate;
              RESIZE : mem.reallocate);

TYPE
  Dws = DYNARR OF DYNARR OF DYNARR OF DYNARR OF INTEGER;

  DWS = POINTER TO Dws;

  SIGNAL = POINTER TO signal_rec;
  MODEL  = POINTER TO mdl_rec;
  CU     = DYNARR OF SEGMENT;

  signal_rec =
  RECORD
    host: MODEL;                   (* Модель, которой принадлежит сигнал    *)
    cu  : CU;                    (* Список проводников сигнала на плате   *)
    type: BITSET;                (* Тип сигнала                           *)
    no  : INTEGER;               (* Номер пина (сигнала)                  *)
    pins: DYNARR OF INTEGER;     (* Список ножек подключенных к сигналу   *)
    gang: INTEGER;               (* Зарезервировано для трассировщика     *)
    hard: INTEGER;               (* Зарезервировано для трассировщика     *)
    name: STRING;
  END;

  mdl_rec =
  RECORD
    type  : MODEL;                (* Тип чипа                              *)
    host  : MODEL;                (* Модель, которой принадлежит модель    *)
    sigs  : DYNARR OF SIGNAL;
    chps  : DYNARR OF MODEL;
    mdls  : DYNARR OF MODEL;
    epns  : DYNARR OF SIGNAL;   (* Интерфейсные пины модели              *)
    X,Y,R : INTEGER;            (* Координаты и ориентация чипа на плате *)
    pict  : SYSTEM.ADDRESS;     (* .R used as changed for none chip      *)
    l_name: STRING;
    e_name: STRING;
    openco: INTEGER;
    sig   : DWS;
    ext   : DWS;
    clip  : TOPOLOGY
  END;

CONST
  max_name=256;
  changed   = -2;
  unchanged = -1;

PROCEDURE new_mdl(VAR mdl: MODEL; VAL name: ARRAY OF CHAR);

  PROCEDURE undo;
    VAR i: INTEGER;
  BEGIN
    WITH mdl^ DO
      FOR i:=0 TO HIGH(sigs) DO DISPOSE(sigs[i]^.name); DISPOSE(sigs[i]) END;
      DISPOSE(sigs);
      FOR i:=0 TO HIGH(epns) DO DISPOSE(epns[i]) END;
      DISPOSE(epns); DISPOSE(l_name); DISPOSE(mdl)
    END
  END undo;

  VAR sig: SIGNAL;
      len: INTEGER;
BEGIN
  len:=str.len(name)+1;
  IF len>max_name THEN error:=err.bad_name; done:=FALSE; RETURN END;
  NEW(mdl);
  IF NOT mem.done THEN error:=mem.error;    done:=FALSE; RETURN END;

  WITH mdl^ DO
    host:=NIL; type:=NIL;
    NEW(sigs,0); NEW(chps,0); NEW(mdls,0); NEW(epns,0);
    X:=0; Y:=0; R:=changed; pict:=NIL; ext:=NIL; sig:=NIL; openco:=0;
    NEW(l_name,0); NEW(e_name,0);
    RESIZE(l_name,len);
    IF NOT mem.done THEN error:=mem.error;    undo; done:=FALSE; RETURN END;
    str.copy(l_name,name)
  END;
  reset_changes(mdl);
  new_sig(sig,mdl,FALSE,{},''); IF NOT done THEN undo; RETURN END;
  new_sig(sig,mdl,TRUE, {},''); IF NOT done THEN undo; RETURN END;
  new_sig(sig,mdl,TRUE,{fixed},'..free..');
                                IF NOT done THEN undo; RETURN END;
  done:=TRUE
END new_mdl;

PROCEDURE mdl_changed(mdl: MODEL): BOOLEAN;
BEGIN
  IF mdl=NIL       THEN done:=FALSE; error:=err.bad_desc; RETURN FALSE END;
  IF mdl^.type#NIL THEN done:=FALSE; error:=err.bad_parm; RETURN FALSE END;
  RETURN (mdl^.R#unchanged)
END mdl_changed;

PROCEDURE ins_mdl(dest,sou: MODEL);
  VAR i: INTEGER;
BEGIN
  IF (dest=NIL) OR (sou=NIL) THEN done:=FALSE; error:=err.bad_desc; RETURN END;
  IF sou^.host#NIL THEN done:=FALSE; error:=err.unsuitable; RETURN END;
  FOR i:=0 TO HIGH(dest^.mdls) DO
    IF dest^.mdls[i]^.l_name=sou^.l_name THEN
      done:=FALSE; error:=err.duplicate; RETURN
    END
  END;
  RESIZE(dest^.mdls,HIGH(dest^.mdls)+2);
  IF NOT mem.done THEN done:=FALSE; error:=mem.error; RETURN END;
  sou^.host:=dest; dest^.mdls[HIGH(dest^.mdls)]:=sou;
  done:=TRUE; dest^.R:=changed
END ins_mdl;

PROCEDURE rem_mdl(VAR mdl: MODEL);

  PROCEDURE disp_mdl(VAR mdl: MODEL);
    VAR i: INTEGER;
  BEGIN
    FOR i:=0 TO HIGH(mdl^.sigs) DO
      DISPOSE(mdl^.sigs[i]^.cu);   DISPOSE(mdl^.sigs[i]^.pins);
      DISPOSE(mdl^.sigs[i]^.name); DISPOSE(mdl^.sigs[i])
    END;
    DISPOSE(mdl^.sigs);
    FOR i:=0 TO HIGH(mdl^.chps) DO
      DISPOSE(mdl^.chps[i]^.l_name); DISPOSE(mdl^.chps[i]^.e_name);
      DISPOSE(mdl^.chps[i])
    END;
    DISPOSE(mdl^.chps);
    FOR i:=0 TO HIGH(mdl^.mdls) DO disp_mdl(mdl^.mdls[i]) END;
    DISPOSE(mdl^.mdls);
    FOR i:=0 TO HIGH(mdl^.epns) DO
      DISPOSE(mdl^.epns[i]^.cu); DISPOSE(mdl^.epns[i]^.name);
      DISPOSE(mdl^.epns[i])
    END;
    DISPOSE(mdl^.epns);
    DISPOSE(mdl^.l_name);
    DISPOSE(mdl^.e_name);
    DISPOSE(mdl)
  END disp_mdl;

  VAR i,j: INTEGER;

BEGIN
  IF mdl=NIL       THEN done:=FALSE; error:=err.bad_desc; RETURN END;
  IF mdl^.type#NIL THEN done:=FALSE; error:=err.bad_parm; RETURN END;
  done:=TRUE;
  IF mdl^.host#NIL THEN
    i:=0;
    LOOP
      IF mdl^.host^.mdls[i]=mdl THEN
        FOR j:=0 TO HIGH(mdl^.host^.chps) DO
          IF mdl^.host^.chps[j]^.type=mdl THEN
            done:=FALSE; error:=err.ill_desc; RETURN
          END
        END;
        mdl^.host^.mdls[i]:=mdl^.host^.mdls[HIGH(mdl^.host^.mdls)];
        RESIZE(mdl^.host^.mdls,HIGH(mdl^.host^.mdls));
        EXIT
      END
    END
  END;
  close(mdl,TRUE);
  close(mdl,FALSE);
  disp_mdl(mdl)
END rem_mdl;

PROCEDURE mdl_rename(mdl: MODEL; VAL name: ARRAY OF CHAR);
  VAR i,len: INTEGER;
BEGIN
  IF mdl=NIL       THEN done:=FALSE; error:=err.bad_desc; RETURN END;
  IF mdl^.type#NIL THEN done:=FALSE; error:=err.bad_parm; RETURN END;
  len:=str.len(name)+1;
  IF len>max_name  THEN error:=err.bad_name; done:=FALSE; RETURN END;

  IF mdl^.host#NIL THEN
    FOR i:=0 TO HIGH(mdl^.host^.mdls) DO
      IF mdl^.host^.mdls[i]^.l_name=name THEN
        done:=FALSE; error:=err.duplicate; RETURN
      END
    END
  END;
  WITH mdl^ DO
    RESIZE(l_name,len);
    IF NOT mem.done THEN done:=FALSE; error:=mem.error; RETURN END;
    str.copy(l_name,name)
  END;
  done:=TRUE; mdl^.R:=changed
END mdl_rename;

PROCEDURE mdl_name(mdl: MODEL; VAR name: ARRAY OF CHAR);
BEGIN
  IF mdl=NIL       THEN done:=FALSE; error:=err.bad_desc; RETURN END;
  IF mdl^.type#NIL THEN done:=FALSE; error:=err.bad_parm; RETURN END;
  WITH mdl^ DO
    IF str.len(l_name)>HIGH(name) THEN
      done:=FALSE; error:=err.bad_parm; RETURN
    END;
    str.copy(name,l_name)
  END;
  done:=TRUE
END mdl_name;

PROCEDURE mdl_set_size(mdl: MODEL; x,y: INTEGER);
BEGIN
  IF mdl=NIL       THEN done:=FALSE; error:=err.bad_desc; RETURN END;
  IF mdl^.type#NIL THEN done:=FALSE; error:=err.bad_parm; RETURN END;
  mdl^.X:=x; mdl^.Y:=y;
  done:=TRUE; mdl^.R:=changed
END mdl_set_size;

PROCEDURE mdl_get_size(mdl: MODEL; VAR x,y: INTEGER);
BEGIN
  IF mdl=NIL       THEN done:=FALSE; error:=err.bad_desc; RETURN END;
  IF mdl^.type#NIL THEN done:=FALSE; error:=err.bad_parm; RETURN END;
  x:=mdl^.X; y:=mdl^.Y;
  done:=TRUE
END mdl_get_size;

PROCEDURE find_mdl(VAR mdl: MODEL; where: MODEL; VAL name: ARRAY OF CHAR);
  VAR i: INTEGER;
BEGIN
  IF where=NIL       THEN done:=FALSE; error:=err.bad_desc; RETURN END;
  IF where^.type#NIL THEN done:=FALSE; error:=err.bad_parm; RETURN END;
  FOR i:=0 TO HIGH(where^.mdls) DO
    mdl:=where^.mdls[i]; IF mdl^.l_name=name THEN done:=TRUE; RETURN END;
  END;
  mdl:=NIL; done:=FALSE; error:=err.no_entry
END find_mdl;

PROCEDURE read_mdl(VAR mdl: MODEL; VAL name: ARRAY OF CHAR);

  VAR file: bio.FILE;

  PROCEDURE rd_sig(VAR sig: SIGNAL; h: MODEL; n: INTEGER);

    VAR sz: INTEGER;

    PROCEDURE bio_err;
    BEGIN
      DISPOSE(sig^.cu); DISPOSE(sig^.pins); DISPOSE(sig^.name); DISPOSE(sig);
      done:=FALSE; error:=bio.error
    END bio_err;

    PROCEDURE mem_err;
    BEGIN
      DISPOSE(sig^.cu); DISPOSE(sig^.pins); DISPOSE(sig^.name); DISPOSE(sig);
      done:=FALSE; error:=mem.error
    END mem_err;

  BEGIN
    NEW(sig); IF NOT mem.done THEN done:=FALSE; error:=mem.error; RETURN END;
    WITH sig^ DO
      host:=h;
      NEW(cu,0);
      no:=n;
      NEW(pins,0);
      gang:=0; hard:=0;
      NEW(name,0);

      bio.read(file,SYSTEM.ADR(sz),4); IF NOT bio.done THEN bio_err; RETURN END;
      NEW(cu,sz+1);                    IF NOT mem.done THEN mem_err; RETURN END;
      bio.get(file,cu,BYTES(cu));      IF NOT bio.done THEN bio_err; RETURN END;

      bio.read(file,SYSTEM.ADR(type),4);
                                       IF NOT bio.done THEN bio_err; RETURN END;

      bio.read(file,SYSTEM.ADR(sz),4); IF NOT bio.done THEN bio_err; RETURN END;
      NEW(pins,sz+1);                  IF NOT mem.done THEN mem_err; RETURN END;
      bio.get(file,pins,BYTES(pins));  IF NOT bio.done THEN bio_err; RETURN END;
      bio.read(file,SYSTEM.ADR(sz),4); IF NOT bio.done THEN bio_err; RETURN END;

      NEW(name,sz+1);                  IF NOT mem.done THEN mem_err; RETURN END;
      bio.get(file,name,BYTES(name));  IF NOT bio.done THEN bio_err; RETURN END;
    END;
    done:=TRUE
  END rd_sig;

  PROCEDURE rd_chp(VAR chip: MODEL; h: MODEL);
    VAR sz: INTEGER;

    PROCEDURE mem_err;
    BEGIN
      DISPOSE(chip^.l_name); DISPOSE(chip^.e_name); DISPOSE(chip);
      done:=FALSE; error:=mem.error;
    END mem_err;

    PROCEDURE bio_err;
    BEGIN
      DISPOSE(chip^.l_name); DISPOSE(chip^.e_name); DISPOSE(chip);
      done:=FALSE; error:=bio.error;
    END bio_err;

  BEGIN
    NEW(chip);
    IF NOT mem.done THEN done:=FALSE; error:=mem.error; RETURN END;
    WITH chip^ DO
      host:=h;
      NEW(sigs,0); NEW(chps,0);
      NEW(mdls,0); NEW(epns,0);
      pict:=NIL;
      NEW(l_name,0); NEW(e_name,0);
      ext:=NIL;
      sig:=NIL;
      bio.read(file,SYSTEM.ADR(type),4); type:=h^.mdls[INTEGER(type)];
      bio.read(file,SYSTEM.ADR(X),4);
      bio.read(file,SYSTEM.ADR(Y),4);
      bio.read(file,SYSTEM.ADR(R),4);

      bio.read(file,SYSTEM.ADR(sz),4); IF NOT bio.done THEN bio_err; RETURN END;
      NEW(l_name,sz+1);                IF NOT mem.done THEN mem_err; RETURN END;
      bio.get(file,l_name,BYTES(l_name));
                                       IF NOT bio.done THEN bio_err; RETURN END;

      bio.read(file,SYSTEM.ADR(sz),4); IF NOT bio.done THEN bio_err; RETURN END;
      NEW(e_name,sz+1);                IF NOT mem.done THEN mem_err; RETURN END;
      bio.get(file,e_name,BYTES(e_name));
                                       IF NOT bio.done THEN bio_err; RETURN END;
    END;
    done:=TRUE
  END rd_chp;

  PROCEDURE rd_epin(VAR epn: SIGNAL; h: MODEL; n: INTEGER);
    VAR sz: INTEGER;

    PROCEDURE bio_err;
    BEGIN
      DISPOSE(epn^.cu); DISPOSE(epn^.name); DISPOSE(epn);
      done:=FALSE; error:=bio.error
    END bio_err;

    PROCEDURE mem_err;
    BEGIN
      DISPOSE(epn^.cu); DISPOSE(epn^.name); DISPOSE(epn);
      done:=FALSE; error:=mem.error
    END mem_err;

  BEGIN
    NEW(epn);
    IF NOT mem.done THEN done:=FALSE; error:=mem.error; RETURN END;
    WITH epn^ DO
      host:=h;
      type:={31};
      no:=n;
      NEW(pins,0);
      gang:=0; hard:=0;

      bio.read(file,SYSTEM.ADR(sz),4); IF NOT bio.done THEN bio_err; RETURN END;
      NEW(cu,sz+1);                    IF NOT mem.done THEN mem_err; RETURN END;
      bio.get(file,cu,BYTES(cu));      IF NOT bio.done THEN bio_err; RETURN END;

      bio.read(file,SYSTEM.ADR(sz),4); IF NOT bio.done THEN bio_err; RETURN END;
      NEW(name,sz+1);                  IF NOT mem.done THEN mem_err; RETURN END;
      bio.get(file,name,BYTES(name));  IF NOT bio.done THEN bio_err; RETURN END;
    END;
    done:=TRUE
  END rd_epin;

  PROCEDURE rd_mdl(VAR mdl: MODEL; h: MODEL);
    VAR i: INTEGER; sz: INTEGER;
    PROCEDURE err;
      PROCEDURE disp_mdl(VAR mdl: MODEL);
        VAR i: INTEGER;
      BEGIN
        FOR i:=0 TO HIGH(mdl^.sigs) DO
          DISPOSE(mdl^.sigs[i]^.cu); DISPOSE(mdl^.sigs[i]^.pins);
          DISPOSE(mdl^.sigs[i]^.name); DISPOSE(mdl^.sigs[i]);
        END;
        DISPOSE(mdl^.sigs);
        FOR i:=0 TO HIGH(mdl^.chps) DO
          DISPOSE(mdl^.chps[i]^.l_name); DISPOSE(mdl^.chps[i]^.e_name);
          DISPOSE(mdl^.chps[i]);
        END;
        DISPOSE(mdl^.chps);
        FOR i:=0 TO HIGH(mdl^.mdls) DO disp_mdl(mdl^.mdls[i]) END;
        DISPOSE(mdl^.mdls);
        FOR i:=0 TO HIGH(mdl^.epns) DO
          DISPOSE(mdl^.epns[i]^.cu); DISPOSE(mdl^.epns[i]^.name);
          DISPOSE(mdl^.epns[i]);
        END;
        DISPOSE(mdl^.epns);
        DISPOSE(mdl^.l_name);
        DISPOSE(mdl^.e_name);
        DISPOSE(mdl);
      END disp_mdl;
    BEGIN
      disp_mdl(mdl)
    END err;

    PROCEDURE bio_err; BEGIN done:=FALSE; error:=bio.error; err END bio_err;
    PROCEDURE mem_err; BEGIN done:=FALSE; error:=mem.error; err END mem_err;

  BEGIN
    NEW(mdl);
    IF NOT mem.done THEN done:=FALSE; error:=mem.error; RETURN END;
    WITH mdl^ DO
      type:=NIL; host:=h;
      NEW(sigs,0); NEW(chps,0);
      NEW(mdls,0); NEW(epns,0);
      pict:=NIL;
      ext :=NIL;
      sig :=NIL;
      openco:=0;
      NEW(l_name,0); NEW(e_name,0);
      reset_changes(mdl);
      bio.read(file,SYSTEM.ADR(sz),4); IF NOT bio.done THEN bio_err; RETURN END;
      NEW(mdls,sz+1);                  IF NOT mem.done THEN mem_err; RETURN END;
      FOR i:=0 TO HIGH(mdls) DO
        rd_mdl(mdls[i],mdl);
        IF NOT done THEN RESIZE(mdls,i); err; RETURN END;
      END;
      bio.read(file,SYSTEM.ADR(sz),4); IF NOT bio.done THEN bio_err; RETURN END;
      NEW(sigs,sz+1);                  IF NOT mem.done THEN mem_err; RETURN END;
      FOR i:=0 TO HIGH(sigs) DO
        rd_sig(sigs[i],mdl,i);
        IF NOT done THEN RESIZE(sigs,i); err; RETURN END;
        sigs[i]^.type:=sigs[i]^.type*{0..2,30}
      END;
      bio.read(file,SYSTEM.ADR(sz),4); IF NOT bio.done THEN bio_err; RETURN END;
      NEW(chps,sz+1);                  IF NOT mem.done THEN mem_err; RETURN END;
      FOR i:=0 TO HIGH(chps) DO
        rd_chp(chps[i],mdl);
        IF NOT done THEN RESIZE(chps,i); err; RETURN END;
      END;
      bio.read(file,SYSTEM.ADR(sz),4); IF NOT bio.done THEN bio_err; RETURN END;
      NEW(epns,sz+1);                  IF NOT mem.done THEN mem_err; RETURN END;
      FOR i:=0 TO HIGH(epns) DO
        rd_epin(epns[i],mdl,i);
        IF NOT done THEN RESIZE(epns,i); err; RETURN END;
        epns[i]^.type:=epns[i]^.type*{0..2,30}+{31}
      END;
      bio.read(file,SYSTEM.ADR(X),4);  IF NOT bio.done THEN bio_err; RETURN END;
      bio.read(file,SYSTEM.ADR(Y),4);  IF NOT bio.done THEN bio_err; RETURN END;
      bio.read(file,SYSTEM.ADR(R),4);  IF NOT bio.done THEN bio_err; RETURN END;
      bio.read(file,SYSTEM.ADR(sz),4); IF NOT bio.done THEN bio_err; RETURN END;
      NEW(l_name,sz+1);                IF NOT mem.done THEN mem_err; RETURN END;
      bio.get(file,l_name,BYTES(l_name));
                                       IF NOT bio.done THEN bio_err; RETURN END;
      mdl^.R:=unchanged
    END;
    done:=TRUE
  END rd_mdl;

BEGIN
  bio.open(file,name,'r');
  IF NOT bio.done THEN done:=FALSE; error:=bio.error; RETURN END;
  bio.buffers(file,1,4096);
  rd_mdl(mdl,NIL);
  bio.close(file);
  IF NOT done THEN RETURN END;
  IF NOT bio.done THEN
    rem_mdl(mdl); done:=FALSE; error:=bio.error; RETURN
  END;
  done:=TRUE
END read_mdl;

PROCEDURE write_mdl(mdl: MODEL; VAL name: ARRAY OF CHAR);

  VAR file: bio.FILE;

  PROCEDURE wr_sig(sig: SIGNAL);
    VAR i: INTEGER;
  BEGIN
    WITH sig^ DO
      i:=HIGH(cu);
      bio.write(file,SYSTEM.ADR(i),4);    IF NOT bio.done THEN RETURN END;
      bio.put(file,cu,BYTES(cu));         IF NOT bio.done THEN RETURN END;
      bio.write(file,SYSTEM.ADR(type),4); IF NOT bio.done THEN RETURN END;
      i:=HIGH(pins);
      bio.write(file,SYSTEM.ADR(i),4);    IF NOT bio.done THEN RETURN END;
      bio.put(file,pins,BYTES(pins));     IF NOT bio.done THEN RETURN END;
      i:=HIGH(name);
      bio.write(file,SYSTEM.ADR(i),4);    IF NOT bio.done THEN RETURN END;
      bio.put(file,name,BYTES(name))
    END
  END wr_sig;

  PROCEDURE wr_chp(chip: MODEL);
    VAR no: INTEGER;
  BEGIN
    no:=0;
    LOOP
      ASSERT(no<=HIGH(chip^.host^.mdls));
      IF chip^.type=chip^.host^.mdls[no] THEN EXIT END;
      no:=no+1
    END;
    WITH chip^ DO
      bio.write(file,SYSTEM.ADR(no),4);   IF NOT bio.done THEN RETURN END;
      bio.write(file,SYSTEM.ADR(X),4);    IF NOT bio.done THEN RETURN END;
      bio.write(file,SYSTEM.ADR(Y),4);    IF NOT bio.done THEN RETURN END;
      bio.write(file,SYSTEM.ADR(R),4);    IF NOT bio.done THEN RETURN END;
      no:=HIGH(l_name);
      bio.write(file,SYSTEM.ADR(no),4);   IF NOT bio.done THEN RETURN END;
      bio.put(file,l_name,BYTES(l_name)); IF NOT bio.done THEN RETURN END;
      no:=HIGH(e_name);
      bio.write(file,SYSTEM.ADR(no),4);   IF NOT bio.done THEN RETURN END;
      bio.put(file,e_name,BYTES(e_name))
    END
  END wr_chp;

  PROCEDURE wr_epin(epn: SIGNAL);
    VAR i: INTEGER;
  BEGIN
    WITH epn^ DO
      i:=HIGH(cu);
      bio.write(file,SYSTEM.ADR(i),4);    IF NOT bio.done THEN RETURN END;
      bio.put(file,cu,BYTES(cu));         IF NOT bio.done THEN RETURN END;
      i:=HIGH(name);
      bio.write(file,SYSTEM.ADR(i),4);    IF NOT bio.done THEN RETURN END;
      bio.put(file,name,BYTES(name))
    END
  END wr_epin;

  PROCEDURE wr_mdl(mdl: MODEL);
    VAR i: INTEGER;
  BEGIN
    WITH mdl^ DO
      i:=HIGH(mdls);
      bio.write(file,SYSTEM.ADR(i),4);    IF NOT bio.done THEN RETURN END;
      FOR i:=0 TO HIGH(mdls) DO
        wr_mdl(mdls[i]);                  IF NOT bio.done THEN RETURN END;
      END;
      i:=HIGH(sigs);
      bio.write(file,SYSTEM.ADR(i),4);    IF NOT bio.done THEN RETURN END;
      FOR i:=0 TO HIGH(sigs) DO
        wr_sig(sigs[i]);                  IF NOT bio.done THEN RETURN END;
      END;
      i:=HIGH(chps);
      bio.write(file,SYSTEM.ADR(i),4);    IF NOT bio.done THEN RETURN END;
      FOR i:=0 TO HIGH(chps) DO
        wr_chp(chps[i]);                  IF NOT bio.done THEN RETURN END;
      END;
      i:=HIGH(epns);
      bio.write(file,SYSTEM.ADR(i),4);    IF NOT bio.done THEN RETURN END;
      FOR i:=0 TO HIGH(epns) DO
        wr_epin(epns[i]);                 IF NOT bio.done THEN RETURN END;
      END;
      bio.write(file,SYSTEM.ADR(X),4);    IF NOT bio.done THEN RETURN END;
      bio.write(file,SYSTEM.ADR(Y),4);    IF NOT bio.done THEN RETURN END;
      bio.write(file,SYSTEM.ADR(R),4);    IF NOT bio.done THEN RETURN END;
      i:=HIGH(l_name);
      bio.write(file,SYSTEM.ADR(i),4);    IF NOT bio.done THEN RETURN END;
      bio.put(file,l_name,BYTES(l_name));
    END
  END wr_mdl;

BEGIN
  IF mdl=NIL       THEN done:=FALSE; error:=err.bad_desc; RETURN END;
  IF mdl^.type#NIL THEN done:=FALSE; error:=err.ill_desc; RETURN END;
  bio.create(file,name,'w',0);
  IF NOT bio.done THEN done:=FALSE; error:=bio.error; RETURN END;
  bio.buffers(file,1,4096);
  wr_mdl(mdl);
  IF NOT bio.done THEN
    done:=FALSE; error:=bio.error; bio.purge(file); RETURN
  END;
  bio.close(file);
  IF NOT bio.done THEN
    done:=FALSE; error:=bio.error; bio.purge(file); RETURN
  END;
  done:=TRUE; mdl^.R:=unchanged
END write_mdl;

PROCEDURE new_sig(VAR sig: SIGNAL; mdl: MODEL; internal?: BOOLEAN;
                  tp: BITSET; VAL name: ARRAY OF CHAR);

  VAR sigs: POINTER TO DYNARR OF SIGNAL; i,len: INTEGER;

BEGIN
  IF mdl=NIL THEN done:=FALSE; error:=err.bad_desc; RETURN END;
  len:=str.len(name)+1;
  IF len>max_name  THEN error:=err.bad_name; done:=FALSE; RETURN END;

  tp:=tp*{0..2}+{30};

(*$<$U+*)
  IF internal? THEN sigs:=SYSTEM.ADR(mdl^.sigs^)
  ELSE              sigs:=SYSTEM.ADR(mdl^.epns^); tp:=tp+{31}
  END;
(*$>*)

  FOR i:=0 TO HIGH(sigs^) DO
    IF sigs^[i]^.name=name THEN done:=FALSE; error:=err.duplicate; RETURN END
  END;
  NEW(sig); IF NOT mem.done THEN done:=FALSE; error:=mem.error; RETURN END;
  WITH sig^ DO
    host:=mdl;
    NEW(cu,0);
    type:=tp;
    NEW(pins,0);
    gang:=0; hard:=0;
    NEW(name,0)
  END;
  RESIZE(sig^.name,len);
  IF NOT mem.done THEN done:=FALSE; error:=mem.error; DISPOSE(sig); RETURN END;
  str.copy(sig^.name,name);
  RESIZE(sigs^,HIGH(sigs^)+2);
  IF NOT mem.done THEN
    error:=mem.error; DISPOSE(sig^.name); DISPOSE(sig); done:=FALSE; RETURN
  END;
  sigs^[HIGH(sigs^)]:=sig; sig^.no:=HIGH(sigs^);
  done:=TRUE; mdl^.R:=changed
END new_sig;

VAR mask : BITSET;
    bmask: BITSET;
    msk12: BITSET;
   m8000h: INTEGER;

PROCEDURE clear(dws: DWS; VAL seg: SEGMENT; sig_id,seg_id: INTEGER); FORWARD;

PROCEDURE rem_sig(VAR sig: SIGNAL);
  VAR i,j,k,l,n: INTEGER;
      a: POINTER TO DYNARR OF SIGNAL;
      chip: MODEL;
      dws: DWS;
BEGIN
  IF (sig=sig^.host^.sigs[0]) OR (sig=sig^.host^.sigs[1])
  OR (sig=sig^.host^.epns[0])
  THEN
    done:=FALSE; error:=err.ill_desc; RETURN
  END;

  IF sig^.type*{31}={} THEN dws:=sig^.host^.sig
  ELSE                      dws:=sig^.host^.ext
  END;
  IF HIGH(dws^)<0 THEN dws:=NIL END;

  IF sig^.type*{31}={} THEN
(*$<$U+*) a:=SYSTEM.ADR(sig^.host^.sigs^); (*$>*)
    IF HIGH(sig^.pins)>=0 THEN done:=FALSE; error:=err.ill_desc; RETURN END
  ELSE
(*$<$U+*) a:=SYSTEM.ADR(sig^.host^.epns^); (*$>*)
    IF sig^.host^.host#NIL THEN
      FOR i:=0 TO HIGH(sig^.host^.host^.chps) DO
        IF sig^.host=sig^.host^.host^.chps[i]^.type THEN
          done:=FALSE; error:=err.ill_desc; RETURN
        END
      END
    END
  END;
  IF dws#NIL THEN
    FOR i:=0 TO HIGH(sig^.cu) DO
      clear(dws,sig^.cu[i],sig^.no,i); ASSERT(done)
    END;
------------------------ flush DWS !!!
    FOR i:=0 TO HIGH(dws^) DO
      FOR j:=0 TO HIGH(dws^[i]) DO
        FOR k:=0 TO HIGH(dws^[i][j]) DO
          FOR l:=0 TO HIGH(dws^[i][j][k]) DO
            n:=INTEGER(BITSET(dws^[i][j][k][l])*mask);
            IF n=HIGH(a^) THEN
              dws^[i][j][k][l]:=INTEGER(BITSET(dws^[i][j][k][l])-mask)+sig^.no
            END
          END
        END
      END
    END
------------------------
  END;
  RESIZE(sig^.cu,0);
  RESIZE(sig^.name,0);
  sig^.host^.R:=changed;
  i:=sig^.no;
  DISPOSE(sig);
  a^[i]:=a^[HIGH(a^)]; RESIZE(a^,HIGH(a^));
  a^[i]^.no:=i;
  done:=TRUE
END rem_sig;

PROCEDURE get_sig_gang(s: SIGNAL; VAR g: INTEGER);
BEGIN
  IF s=NIL THEN done:=FALSE; error:=err.bad_desc; RETURN END;
  g:=s^.gang;
  done:=TRUE
END get_sig_gang;

PROCEDURE get_sig_hard(s: SIGNAL; VAR h: INTEGER);
BEGIN
  IF s=NIL THEN done:=FALSE; error:=err.bad_desc; RETURN END;
  h:=s^.hard;
  done:=TRUE
END get_sig_hard;

PROCEDURE set_sig_gang(s: SIGNAL; g: INTEGER);
BEGIN
  IF s=NIL THEN done:=FALSE; error:=err.bad_desc; RETURN END;
  s^.gang:=g;
  done:=TRUE
END set_sig_gang;

PROCEDURE set_sig_hard(s: SIGNAL; h: INTEGER);
BEGIN
  IF s=NIL THEN done:=FALSE; error:=err.bad_desc; RETURN END;
  s^.hard:=h;
  done:=TRUE
END set_sig_hard;

PROCEDURE sig_rename(sig: SIGNAL; VAL name: ARRAY OF CHAR);
  VAR i,len: INTEGER;
BEGIN
  IF sig=NIL THEN done:=FALSE; error:=err.bad_desc; RETURN END;
  IF (sig=sig^.host^.sigs[0]) OR (sig=sig^.host^.sigs[1]) OR
    (sig=sig^.host^.epns[0]) THEN done:=FALSE; error:=err.ill_desc; RETURN END;
  len:=str.len(name)+1;
  IF len>max_name  THEN error:=err.bad_name; done:=FALSE; RETURN END;

  IF sig^.type*{31}={} THEN
    FOR i:=0 TO HIGH(sig^.host^.sigs) DO
      IF sig^.host^.sigs[i]^.name=name THEN
        IF sig^.host^.sigs[i]#sig THEN
          done:=FALSE; error:=err.duplicate; RETURN
        END
      END
    END
  ELSE
    FOR i:=0 TO HIGH(sig^.host^.epns) DO
      IF sig^.host^.epns[i]^.name=name THEN
        IF sig^.host^.epns[i]#sig THEN
          done:=FALSE; error:=err.duplicate; RETURN
        END
      END
    END
  END;
  RESIZE(sig^.name,len);
  IF NOT mem.done THEN done:=FALSE; error:=mem.error; RETURN END;
  str.copy(sig^.name,name);
  done:=TRUE; sig^.host^.R:=changed
END sig_rename;

PROCEDURE sig_set_type(sig: SIGNAL; type: BITSET);
BEGIN
  IF sig=NIL THEN done:=FALSE; error:=err.bad_desc; RETURN END;
  IF (sig=sig^.host^.sigs[0]) OR (sig=sig^.host^.sigs[1]) OR
    (sig=sig^.host^.epns[0]) THEN done:=FALSE; error:=err.ill_desc; RETURN END;
  sig^.type:=sig^.type*{30,31}+type*{0..2};
  done:=TRUE; sig^.host^.R:=changed
END sig_set_type;

PROCEDURE sig_name(sig: SIGNAL; VAR name: ARRAY OF CHAR);
BEGIN
  IF sig=NIL THEN done:=FALSE; error:=err.bad_desc; RETURN END;
  IF str.len(sig^.name)>HIGH(name) THEN
    done:=FALSE; error:=err.bad_parm; RETURN
  END;
  str.copy(name,sig^.name);
  done:=TRUE
END sig_name;

PROCEDURE sig_get_type(sig: SIGNAL; VAR type: BITSET);
BEGIN
  IF sig=NIL THEN done:=FALSE; error:=err.bad_desc; RETURN END;
  type:=sig^.type*{0..2};
  done:=TRUE
END sig_get_type;

PROCEDURE find_sig(VAR sig: SIGNAL; mdl: MODEL; VAL name: ARRAY OF CHAR);
  VAR i: INTEGER;
BEGIN
  IF mdl=NIL       THEN done:=FALSE; error:=err.bad_desc; RETURN END;
  IF mdl^.type#NIL THEN done:=FALSE; error:=err.ill_desc; RETURN END;
  FOR i:=1 TO HIGH(mdl^.sigs) DO
    sig:=mdl^.sigs[i];
    IF sig^.name=name THEN done:=TRUE; RETURN END
  END;
  sig:=NIL; done:=FALSE; error:=err.no_entry
END find_sig;

PROCEDURE empty_sig(VAR sig: SIGNAL; mdl: MODEL);
BEGIN
  IF mdl=NIL       THEN done:=FALSE; error:=err.bad_desc; RETURN END;
  IF mdl^.type#NIL THEN done:=FALSE; error:=err.ill_desc; RETURN END;
  sig:=mdl^.sigs[0]; done:=TRUE
END empty_sig;

PROCEDURE new_chip(VAR chip: MODEL; master,mdl: MODEL; VAL l_n,e_n: ARRAY OF CHAR);
  VAR i,j,l_len,e_len: INTEGER;
BEGIN
  IF (mdl=NIL) OR (master=NIL) THEN
    done:=FALSE; error:=err.bad_desc; RETURN
  END;
  IF master^.host#mdl THEN done:=FALSE; error:=err.inconsistency; RETURN END;

  l_len:=str.len(l_n)+1; e_len:=str.len(e_n)+1;
  IF (l_len>max_name) OR (e_len>max_name) THEN
    done:=FALSE; error:=err.bad_parm; RETURN
  END;

  FOR i:=0 TO HIGH(mdl^.chps) DO
    IF mdl^.chps[i]^.l_name=l_n THEN
      done:=FALSE; error:=err.duplicate; RETURN
    END
  END;
  NEW(chip);
  IF NOT mem.done THEN done:=FALSE; error:=mem.error; RETURN END;
  WITH chip^ DO
    type:=master;
    host:=mdl;
    NEW(sigs,0); NEW(chps,0); NEW(mdls,0); NEW(epns,0);
    X:=0; Y:=0; R:=-1;  openco:=0;
    pict:=NIL;
    NEW(l_name,0); NEW(e_name,0);
    ext:=NIL;
    RESIZE(l_name,l_len);
    IF NOT mem.done THEN
      error:=mem.error; DISPOSE(chip); done:=FALSE; RETURN
    END;
    str.copy(l_name,l_n);
    RESIZE(e_name,e_len);
    IF NOT mem.done THEN
      error:=mem.error; DISPOSE(l_name); DISPOSE(chip); done:=FALSE; RETURN
    END;
    str.copy(e_name,e_n);
    RESIZE(mdl^.chps,HIGH(mdl^.chps)+2);
    IF NOT mem.done THEN
      error:=mem.error;
      DISPOSE(e_name); DISPOSE(l_name); DISPOSE(chip);
      done:=FALSE; RETURN
    END;
    mdl^.chps[HIGH(mdl^.chps)]:=chip;
    j:=HIGH(mdl^.sigs[1]^.pins)+1;
    RESIZE(mdl^.sigs[1]^.pins,
           HIGH(mdl^.sigs[1]^.pins)+HIGH(chip^.type^.epns)+1);
    IF NOT mem.done THEN
      error:=mem.error;
      RESIZE(mdl^.chps,HIGH(mdl^.chps));
      DISPOSE(e_name); DISPOSE(l_name); DISPOSE(chip);
      done:=FALSE; RETURN
    END;
    FOR i:=1 TO HIGH(chip^.type^.epns) DO
      mdl^.sigs[1]^.pins[j]:=HIGH(mdl^.chps)<<16+i; j:=j+1
    END
  END;
  done:=TRUE; mdl^.R:=changed
END new_chip;

PROCEDURE disconnect(mdl: MODEL; cno: INTEGER; VAR s: SIGNAL);
  VAR i,j: INTEGER; sig: SIGNAL;
BEGIN
  i:=1;
  REPEAT
    sig:=mdl^.sigs[i];
    IF sig#s THEN
      FOR j:=0 TO HIGH(sig^.pins) DO
        IF sig^.pins[j]=cno THEN
          sig^.pins[j]:=sig^.pins[HIGH(sig^.pins)];
          RESIZE(sig^.pins,HIGH(sig^.pins));
          s:=sig;
          RETURN
        END
      END
    END;
    INC(i)
  UNTIL i>HIGH(mdl^.sigs)
END disconnect;

PROCEDURE rem_chip(VAR chip: MODEL);
  VAR i,j,k,n: INTEGER; s: SIGNAL;
BEGIN
  IF chip=NIL       THEN done:=FALSE; error:=err.bad_desc; RETURN END;
  IF chip^.type=NIL THEN done:=FALSE; error:=err.ill_desc; RETURN END;
  IF chip^.host=NIL THEN done:=FALSE; error:=err.ill_desc; RETURN END;

  FOR i:=0 TO HIGH(chip^.host^.chps) DO
    IF chip^.host^.chps[i]=chip THEN
      FOR j:=1 TO HIGH(chip^.type^.epns) DO
        s:=NIL; disconnect(chip^.host,i<<16+j,s)
      END;
      chip^.host^.chps[i]:=chip^.host^.chps[HIGH(chip^.host^.chps)];
------------- flush chip_pins !!!!!
      j:=0;
      REPEAT
        FOR k:=0 TO HIGH(chip^.host^.sigs[j]^.pins) DO
          n:=chip^.host^.sigs[j]^.pins[k];
          IF INTEGER(BITSET(n)-mask)=HIGH(chip^.host^.chps) THEN
            chip^.host^.sigs[j]^.pins[k]:=INTEGER(BITSET(n)*mask) + i<<16
          END
        END;
      INC(j)
      UNTIL j>HIGH(chip^.host^.sigs);
-------------
      RESIZE(chip^.host^.chps,HIGH(chip^.host^.chps));
      chip^.host^.R:=changed;
      DISPOSE(chip^.e_name); DISPOSE(chip^.l_name); DISPOSE(chip);
      done:=TRUE; RETURN
    END
  END;
  ASSERT(FALSE)
END rem_chip;

PROCEDURE chip_rename(chip: MODEL; VAL l_n,e_n: ARRAY OF CHAR);
  VAR l_nm,e_nm: STRING; i,l_len,e_len: INTEGER;
BEGIN
  IF chip=NIL       THEN done:=FALSE; error:=err.bad_desc; RETURN END;
  IF chip^.type=NIL THEN done:=FALSE; error:=err.ill_desc; RETURN END;
  IF chip^.host=NIL THEN done:=FALSE; error:=err.ill_desc; RETURN END;

  l_len:=str.len(l_n)+1; e_len:=str.len(e_n)+1;
  IF (l_len>max_name) OR (e_len>max_name) THEN
    done:=FALSE; error:=err.bad_parm; RETURN
  END;

  FOR i:=0 TO HIGH(chip^.host^.chps) DO
    IF chip^.host^.chps[i]^.l_name=l_n THEN
      done:=FALSE; error:=err.duplicate; RETURN
    END
  END;

  RESIZE(l_nm,l_len);
  IF NOT mem.done THEN done:=FALSE; error:=mem.error; RETURN END;
  RESIZE(e_nm,e_len);
  IF NOT mem.done THEN done:=FALSE; error:=mem.error; DISPOSE(l_nm); RETURN END;
  str.copy(l_nm,l_n); str.copy(e_nm,e_n);

  DISPOSE(chip^.l_name); DISPOSE(chip^.e_name);
  (*$<$U+*)
  chip^.l_name^:=l_nm^; chip^.e_name^:=e_nm^;
  (*$>*)
  done:=TRUE; chip^.host^.R:=changed
END chip_rename;

PROCEDURE chip_name(chip: MODEL; VAR l_n,e_n: ARRAY OF CHAR);
BEGIN
  IF chip=NIL       THEN done:=FALSE; error:=err.bad_desc; RETURN END;
  IF chip^.type=NIL THEN done:=FALSE; error:=err.ill_desc; RETURN END;
  IF chip^.host=NIL THEN done:=FALSE; error:=err.ill_desc; RETURN END;

  WITH chip^ DO
    IF (str.len(l_name)>HIGH(l_n)) OR (str.len(e_name)>HIGH(e_n)) THEN
      done:=FALSE; error:=err.bad_parm; RETURN
    END;
    str.copy(l_n,l_name);
    str.copy(e_n,e_name)
  END;
  done:=TRUE
END chip_name;

PROCEDURE chip_type(chip: MODEL; VAR type: MODEL);
BEGIN
  IF chip=NIL       THEN done:=FALSE; error:=err.bad_desc; RETURN END;
  IF chip^.type=NIL THEN done:=FALSE; error:=err.ill_desc; RETURN END;
  IF chip^.host=NIL THEN done:=FALSE; error:=err.ill_desc; RETURN END;

  type:=chip^.type; done:=TRUE
END chip_type;

PROCEDURE find_chip(VAR chip: MODEL; mdl: MODEL; VAL l_name: ARRAY OF CHAR);
  VAR i: INTEGER;
BEGIN
  IF mdl=NIL       THEN done:=FALSE; error:=err.bad_desc; RETURN END;
  IF mdl^.type#NIL THEN done:=FALSE; error:=err.ill_desc; RETURN END;

  FOR i:=0 TO HIGH(mdl^.chps) DO
    chip:=mdl^.chps[i];
    IF chip^.l_name=l_name THEN done:=TRUE; RETURN END
  END;
  chip:=NIL; done:=FALSE; error:=err.no_entry
END find_chip;

PROCEDURE move_chip(chip: MODEL; x,y,r: INTEGER);
  VAR i,p: INTEGER;
      sig: SIGNAL;
      top: TOPOLOGY;
    short: BOOLEAN;
   b1, b2: INTEGER;
  sid,cno: INTEGER;
BEGIN
  IF chip=NIL       THEN done:=FALSE; error:=err.bad_desc; RETURN END;
  IF chip^.type=NIL THEN done:=FALSE; error:=err.ill_desc; RETURN END;
  IF chip^.host=NIL THEN done:=FALSE; error:=err.ill_desc; RETURN END;

  IF (chip^.host^.sig=NIL) OR (HIGH(chip^.host^.sig^)<0) THEN
    done:=FALSE; error:=err.ill_access; RETURN
  END;

  cno:=-1;
  REPEAT INC(cno)
  UNTIL (cno>HIGH(chip^.host^.chps)) OR (chip^.host^.chps[cno]=chip);
  IF cno>HIGH(chip^.host^.chps) THEN
    sig:=NIL; done:=FALSE; error:=err.bad_parm; RETURN
  END;

  cno:=cno<<16;
  FOR i:=1 TO HIGH(chip^.type^.epns) DO
    cno:=INTEGER(BITSET(cno)-mask+BITSET(i));
    sid:=0; sig:=NIL;
    LOOP
      ASSERT(sid<=HIGH(chip^.host^.sigs));
      sig:=chip^.host^.sigs[sid];
      FOR p:=0 TO HIGH(sig^.pins) DO IF sig^.pins[p]=cno THEN EXIT END END;
      INC(sid)
    END;
    ASSERT(sig#NIL);
    FOR p:=0 TO HIGH(chip^.type^.epns[i]^.cu) DO
      IF chip^.R>=0 THEN
        unpack(chip^.type^.epns[i]^.cu[p],top);
        WITH top DO
          CASE chip^.R OF
            |0: x1:= x1+chip^.X; x2:= x2+chip^.X;
                y1:= y1+chip^.Y; y2:= y2+chip^.Y
            |1: b1:=x1; b2:=x2;
                x1:= y1+chip^.X; x2:= y2+chip^.X;
                y1:=-b1+chip^.Y; y2:=-b2+chip^.Y
            |2: x1:=-x1+chip^.X; x2:=-x2+chip^.X;
                y1:=-y1+chip^.Y; y2:=-y2+chip^.Y
            |3: b1:=x1; b2:=x2;
                x1:=-y1+chip^.X; x2:=-y2+chip^.X;
                y1:= b1+chip^.Y; y2:= b2+chip^.Y
          END
        END;
        del_range(sig,top);
        IF NOT done THEN RETURN END
      END;
      IF r>=0 THEN
        unpack(chip^.type^.epns[i]^.cu[p],top);
        WITH top DO
          CASE r OF
            |0: x1:= x1+x; x2:= x2+x; y1:= y1+y; y2:= y2+y
            |1: b1:=x1; b2:=x2;
                x1:= y1+x; x2:= y2+x; y1:=-b1+y; y2:=-b2+y
            |2: x1:=-x1+x; x2:=-x2+x; y1:=-y1+y; y2:=-y2+y
            |3: b1:=x1; b2:=x2;
                x1:=-y1+x; x2:=-y2+x; y1:= b1+y; y2:= b2+y
          END
        END;
        ins_range(sig,top,TRUE,short);
        IF NOT done THEN RETURN END;
      END
    END
  END;
  chip^.X:=x; chip^.Y:=y; chip^.R:=r; done:=TRUE; chip^.host^.R:=changed
END move_chip;

PROCEDURE chip_pos(chip: MODEL; VAR x,y,r: INTEGER);
BEGIN
  IF chip=NIL       THEN done:=FALSE; error:=err.bad_desc; RETURN END;
  IF chip^.type=NIL THEN done:=FALSE; error:=err.ill_desc; RETURN END;
  IF chip^.host=NIL THEN done:=FALSE; error:=err.ill_desc; RETURN END;

  WITH chip^ DO x:=X; y:=Y; r:=R END; done:=TRUE;
END chip_pos;

PROCEDURE connect(sig: SIGNAL; chip: MODEL; VAL pin_name: ARRAY OF CHAR);
  VAR p,cno: INTEGER; s: SIGNAL;
BEGIN
  IF (chip=NIL) OR (sig=NIL) THEN done:=FALSE; error:=err.bad_desc; RETURN END;
  IF chip^.type=NIL THEN done:=FALSE;          error:=err.ill_desc; RETURN END;
  IF chip^.host=NIL THEN done:=FALSE;          error:=err.ill_desc; RETURN END;

  IF chip^.host#sig^.host THEN
    done:=FALSE; error:=err.inconsistency; RETURN
  END;

  p:=1;
  LOOP
    IF p>HIGH(chip^.type^.epns) THEN
      done:=FALSE; error:=err.bad_name; RETURN
    END;
    IF pin_name=chip^.type^.epns[p]^.name THEN EXIT END;
    INC(p)
  END;

  cno:=-1;
  REPEAT INC(cno) UNTIL chip=chip^.host^.chps[cno];

  RESIZE(sig^.pins,HIGH(sig^.pins)+2);
  IF NOT mem.done THEN done:=FALSE; error:=mem.error; RETURN END;
  cno:=cno<<16+p;
  sig^.pins[HIGH(sig^.pins)]:=cno;

  s:=sig;
  disconnect(chip^.host,cno,s);
  IF s=sig THEN s:=NIL; disconnect(chip^.host,cno,s) END;
  done:=TRUE; sig^.host^.R:=changed
END connect;

PROCEDURE tied_to(VAR sig: SIGNAL; chip: MODEL; VAL pin_name: ARRAY OF CHAR);
  VAR i,j,cno: INTEGER;
BEGIN
  IF chip=NIL       THEN sig:=NIL; done:=FALSE; error:=err.bad_desc; RETURN END;
  IF chip^.host=NIL THEN sig:=NIL; done:=FALSE; error:=err.ill_desc; RETURN END;
  IF chip^.type=NIL THEN sig:=NIL; done:=FALSE; error:=err.ill_desc; RETURN END;

  i:=0;
  REPEAT INC(i)
  UNTIL (i>HIGH(chip^.type^.epns)) OR (chip^.type^.epns[i]^.name=pin_name);
  IF i>HIGH(chip^.type^.epns) THEN
    sig:=NIL; done:=FALSE; error:=err.bad_name; RETURN
  END;

  cno:=-1;
  REPEAT INC(cno)
  UNTIL (cno>HIGH(chip^.host^.chps)) OR (chip^.host^.chps[cno]=chip);
  IF cno>HIGH(chip^.host^.chps) THEN
    sig:=NIL; done:=FALSE; error:=err.bad_parm; RETURN
  END;
  cno:=cno<<16+i;

  i:=1;
  REPEAT
    sig:=chip^.host^.sigs[i];
    FOR j:=0 TO HIGH(sig^.pins) DO
      IF sig^.pins[j]=cno THEN done:=TRUE; RETURN END
    END;
    INC(i)
  UNTIL i>HIGH(chip^.host^.sigs);
  sig:=NIL; done:=FALSE; error:=err.bad_parm
END tied_to;

PROCEDURE first_tied
           (VAR pin_name: ARRAY OF CHAR; VAR chip: MODEL;
            VAR where: ITERSIG; sig: SIGNAL): BOOLEAN;
  VAR i: INTEGER;
BEGIN
  IF sig=NIL       THEN done:=FALSE; error:=err.bad_desc; RETURN FALSE END;
  IF sig^.host=NIL THEN done:=FALSE; error:=err.bad_desc; RETURN FALSE END;

  IF HIGH(sig^.pins)<0 THEN done:=TRUE; RETURN FALSE END;

  where.sig:=sig;
  where.id:=0;
  i:=INTEGER(BITSET(where.sig^.pins[0]<<16)*mask);
  chip:=where.sig^.host^.chps[i];
  i:=INTEGER(BITSET(where.sig^.pins[0])*mask);

  IF HIGH(pin_name)<str.len(chip^.type^.epns[i]^.name) THEN
    done:=FALSE; error:=err.bad_parm; RETURN FALSE
  END;

  str.copy(pin_name,chip^.type^.epns[i]^.name);
  done:=TRUE; RETURN TRUE
END first_tied;

PROCEDURE next_tied
           (VAR pin_name: ARRAY OF CHAR; VAR chip: MODEL; VAR where: ITERSIG): BOOLEAN;
  VAR i: INTEGER;
BEGIN
  IF (where.sig=NIL) OR (where.sig^.host=NIL) THEN
    done:=FALSE; error:=err.bad_desc; RETURN FALSE
  END;

  where.id:=where.id+1;
  IF HIGH(where.sig^.pins)<where.id THEN done:=TRUE; RETURN FALSE END;
  i:=INTEGER(BITSET(where.sig^.pins[where.id]<<16)*mask);
  chip:=where.sig^.host^.chps[i];
  i:=INTEGER(BITSET(where.sig^.pins[where.id])*mask);

  IF HIGH(pin_name)<str.len(chip^.type^.epns[i]^.name) THEN
    done:=FALSE; error:=err.bad_parm; RETURN FALSE
  END;

  str.copy(pin_name,chip^.type^.epns[i]^.name);
  done:=TRUE; RETURN TRUE
END next_tied;

PROCEDURE first_mdl(VAR mdl: MODEL; VAR where: ITERMODEL; host: MODEL): BOOLEAN;
BEGIN
  IF host=NIL       THEN done:=FALSE; error:=err.bad_desc; RETURN FALSE END;
  IF host^.type#NIL THEN done:=FALSE; error:=err.ill_desc; RETURN FALSE END;

  IF HIGH(host^.mdls)<0 THEN done:=TRUE; RETURN FALSE END;
  where.mdl:=host;
  where.id:=0; mdl:=where.mdl^.mdls[0];
  done:=TRUE; RETURN TRUE
END first_mdl;

PROCEDURE next_mdl(VAR mdl: MODEL; VAR where: ITERMODEL): BOOLEAN;
BEGIN
  IF where.mdl=NIL       THEN done:=FALSE; error:=err.bad_desc; RETURN FALSE END;
  IF where.mdl^.type#NIL THEN done:=FALSE; error:=err.ill_desc; RETURN FALSE END;

  where.id:=where.id+1;
  IF HIGH(where.mdl^.mdls)<where.id THEN done:=TRUE; RETURN FALSE END;
  mdl:=where.mdl^.mdls[where.id];
  done:=TRUE; RETURN TRUE
END next_mdl;

PROCEDURE first_sig(VAR sig: SIGNAL; VAR where: ITERMODEL; host: MODEL): BOOLEAN;
BEGIN
  IF host=NIL       THEN done:=FALSE; error:=err.bad_desc; RETURN FALSE END;
  IF host^.type#NIL THEN done:=FALSE; error:=err.ill_desc; RETURN FALSE END;

  IF HIGH(host^.sigs)<1 THEN done:=TRUE; RETURN FALSE END;
  where.mdl:=host;
  where.id:=1; sig:=where.mdl^.sigs[1];
  done:=TRUE; RETURN TRUE
END first_sig;

PROCEDURE next_sig(VAR sig: SIGNAL; VAR where: ITERMODEL): BOOLEAN;
BEGIN
  IF where.mdl=NIL       THEN done:=FALSE; error:=err.bad_desc; RETURN FALSE END;
  IF where.mdl^.type#NIL THEN done:=FALSE; error:=err.ill_desc; RETURN FALSE END;

  where.id:=where.id+1;
  IF HIGH(where.mdl^.sigs)<where.id THEN done:=TRUE; RETURN FALSE END;
  sig:=where.mdl^.sigs[where.id];
  done:=TRUE; RETURN TRUE
END next_sig;

PROCEDURE first_epin(VAR epin: SIGNAL; VAR where: ITERMODEL; host: MODEL): BOOLEAN;
BEGIN
  IF host=NIL       THEN done:=FALSE; error:=err.bad_desc; RETURN FALSE END;
  IF host^.type#NIL THEN done:=FALSE; error:=err.ill_desc; RETURN FALSE END;

  IF HIGH(host^.epns)<1 THEN done:=TRUE; RETURN FALSE END;
  where.mdl:=host;
  where.id:=1; epin:=where.mdl^.epns[1];
  done:=TRUE; RETURN TRUE
END first_epin;

PROCEDURE next_epin(VAR epin: SIGNAL; VAR where: ITERMODEL): BOOLEAN;
BEGIN
  IF where.mdl=NIL       THEN done:=FALSE; error:=err.bad_desc; RETURN FALSE END;
  IF where.mdl^.type#NIL THEN done:=FALSE; error:=err.ill_desc; RETURN FALSE END;

  where.id:=where.id+1;
  IF HIGH(where.mdl^.epns)<where.id THEN done:=TRUE; RETURN FALSE END;
  epin:=where.mdl^.epns[where.id];
  done:=TRUE; RETURN TRUE
END next_epin;

PROCEDURE first_chip(VAR chip: MODEL; VAR where: ITERMODEL; host: MODEL): BOOLEAN;
BEGIN
  IF host=NIL       THEN done:=FALSE; error:=err.bad_desc; RETURN FALSE END;
  IF host^.type#NIL THEN done:=FALSE; error:=err.ill_desc; RETURN FALSE END;

  IF HIGH(host^.chps)<0 THEN done:=TRUE; RETURN FALSE END;
  where.mdl:=host;
  where.id:=0; chip:=where.mdl^.chps[0];
  done:=TRUE; RETURN TRUE
END first_chip;

PROCEDURE next_chip(VAR chip: MODEL; VAR where: ITERMODEL): BOOLEAN;
BEGIN
  IF where.mdl=NIL       THEN done:=FALSE; error:=err.bad_desc; RETURN FALSE END;
  IF where.mdl^.type#NIL THEN done:=FALSE; error:=err.ill_desc; RETURN FALSE END;

  where.id:=where.id+1;
  IF HIGH(where.mdl^.chps)<where.id THEN done:=TRUE; RETURN FALSE END;
  chip:=where.mdl^.chps[where.id];
  done:=TRUE; RETURN TRUE
END next_chip;

CONST cell_size=512; --256;
      delta    =10;

(* bits
  0 - fixed
  1 - main direction
  2 - circle mask
  3 - stars terminator
*)

---------------------------Topology--------------------------
                           --------

PROCEDURE pack(VAR s: SEGMENT; VAL top: TOPOLOGY);
  VAR X1,X2,Y1,Y2: INTEGER;
BEGIN
  WITH top DO
    IF x1>x2 THEN   X1:=x2-size+m8000h; X2:=x1+size+m8000h;
      IF y1>y2 THEN Y1:=y2-size+m8000h; Y2:=y1+size+m8000h; s.size:=2
      ELSE          Y1:=y1-size+m8000h; Y2:=y2+size+m8000h; s.size:=0 END
    ELSE            X1:=x1-size+m8000h; X2:=x2+size+m8000h;
      IF y1>y2 THEN Y1:=y2-size+m8000h; Y2:=y1+size+m8000h; s.size:=0
      ELSE          Y1:=y1-size+m8000h; Y2:=y2+size+m8000h; s.size:=2 END
    END;
    IF X1<0      THEN X1:=0      END; IF X2<0      THEN X2:=0      END;
    IF Y1<0      THEN Y1:=0      END; IF Y2<0      THEN Y2:=0      END;
    IF X1>0FFFFh THEN X1:=0FFFFh END; IF X2>0FFFFh THEN X2:=0FFFFh END;
    IF Y1>0FFFFh THEN Y1:=0FFFFh END; IF Y2>0FFFFh THEN Y2:=0FFFFh END;
--    IF CircleTool THEN s.size:=INTEGER(BITSET(s.size)+{2}) END;
    IF fixed     THEN s.size:=INTEGER(BITSET(s.size)+{0}) END;
    INC(s.size,INTEGER(size>>12)+INTEGER(layer<<12)+INTEGER(vsize<<4));
    s.start:=X1+INTEGER(Y1>>16);
    s.end  :=X2+INTEGER(Y2>>16)
  END
END pack;

PROCEDURE unpack0(VAL s: SEGMENT; VAR x1,y1,x2,y2: INTEGER);
BEGIN
  x1:=INTEGER(BITSET(s.start    )*mask)-m8000h;
  y1:=INTEGER(BITSET(s.start>>16)*mask)-m8000h;
  x2:=INTEGER(BITSET(s.end      )*mask)-m8000h;
  y2:=INTEGER(BITSET(s.end  >>16)*mask)-m8000h
END unpack0;

PROCEDURE unpack(VAL s: SEGMENT; VAR top: TOPOLOGY);
BEGIN
  WITH top DO
    size:=INTEGER(BITSET(s.size<<12)*msk12);
    IF BITSET(s.size)*{1}#{} THEN
      x1:=INTEGER(BITSET(s.start    )*mask)+size-m8000h;
      y1:=INTEGER(BITSET(s.start>>16)*mask)+size-m8000h;
      x2:=INTEGER(BITSET(s.end      )*mask)-size-m8000h;
      y2:=INTEGER(BITSET(s.end  >>16)*mask)-size-m8000h
    ELSE
      x1:=INTEGER(BITSET(s.start    )*mask)+size-m8000h;
      y1:=INTEGER(BITSET(s.end  >>16)*mask)-size-m8000h;
      y2:=INTEGER(BITSET(s.start>>16)*mask)+size-m8000h;
      x2:=INTEGER(BITSET(s.end      )*mask)-size-m8000h
    END;
    layer:=        BITSET(s.size>>12)*bmask;
    vsize:=INTEGER(BITSET(s.size>>04)*bmask);
    fixed:=BOOLEAN(BITSET(s.size)*{0})
--    CircleTool:=2 IN BITSET(size);
  END
END unpack;

TYPE pSegment=POINTER TO SEGMENT;

PROCEDURE chk_box(p1,p2: pSegment): BOOLEAN; CODE 0F8h END chk_box;

PROCEDURE fill(dws: DWS; VAL seg: SEGMENT; sig_id,seg_id: INTEGER);
  VAR x1,y1,x2,y2,i,j: INTEGER;
BEGIN
  unpack0(seg,x1,y1,x2,y2);
  x1:=x1 DIV cell_size + delta; y1:=y1 DIV cell_size + delta;
  x2:=x2 DIV cell_size + delta; y2:=y2 DIV cell_size + delta;
  IF (x1<0) OR (x1>HIGH(dws^[0])) OR (y1<0) OR (y1>HIGH(dws^[0][0])) OR
     (x2<0) OR (x2>HIGH(dws^[0])) OR (y2<0) OR (y2>HIGH(dws^[0][0])) THEN
    done:=FALSE; error:=err.bad_parm; RETURN
  END;

  i:=0;
  IF (x1#x2) OR (y1#y2) THEN
    REPEAT
      INC(i);
      x1:=x1 DIV 2; y1:=y1 DIV 2;
      x2:=x2 DIV 2; y2:=y2 DIV 2
    UNTIL (x1=x2)&(y1=y2)
  END;
  j:=HIGH(dws^[i][x1][y1])+2;
  RESIZE(dws^[i][x1][y1],j);
  IF NOT mem.done THEN done:=FALSE; error:=mem.error; RETURN END;
  dws^[i][x1][y1][j-1]:=seg_id<<16+sig_id;
  done:=TRUE
END fill;

PROCEDURE clear(dws: DWS; VAL seg: SEGMENT; sig_id,seg_id: INTEGER);
  VAR x1,y1,x2,y2,i,j,k,ident: INTEGER;
BEGIN
  unpack0(seg,x1,y1,x2,y2);

  x1:=x1 DIV cell_size + delta; y1:=y1 DIV cell_size + delta;
  x2:=x2 DIV cell_size + delta; y2:=y2 DIV cell_size + delta;
  IF (x1<0) OR (x1>HIGH(dws^[0])) OR (y1<0) OR (y1>HIGH(dws^[0][0])) OR
     (x2<0) OR (x2>HIGH(dws^[0])) OR (y2<0) OR (y2>HIGH(dws^[0][0])) THEN
    ASSERT(FALSE)
  END;

  i:=0;
  IF (x1#x2) OR (y1#y2) THEN
    REPEAT
      INC(i);
      x1:=x1 DIV 2; y1:=y1 DIV 2;
      x2:=x2 DIV 2; y2:=y2 DIV 2
    UNTIL (x1=x2)&(y1=y2)
  END;
  j:=HIGH(dws^[i][x1][y1]);
  k:=0;
  ident:=seg_id<<16+sig_id;
  WHILE dws^[i][x1][y1][k]#ident DO INC(k) END;
  dws^[i][x1][y1][k]:=dws^[i][x1][y1][j];
  RESIZE(dws^[i][x1][y1],j);
  done:=TRUE
END clear;

PROCEDURE csnum(dws: DWS; VAL seg: SEGMENT; sig_id,seg_id,new: INTEGER);
  VAR x1,y1,x2,y2,i,k,ident: INTEGER;
BEGIN
  unpack0(seg,x1,y1,x2,y2);

  x1:=x1 DIV cell_size + delta; y1:=y1 DIV cell_size + delta;
  x2:=x2 DIV cell_size + delta; y2:=y2 DIV cell_size + delta;
  IF (x1<0) OR (x1>HIGH(dws^[0])) OR (y1<0) OR (y1>HIGH(dws^[0][0])) OR
     (x2<0) OR (x2>HIGH(dws^[0])) OR (y2<0) OR (y2>HIGH(dws^[0][0])) THEN
    ASSERT(FALSE)
  END;

  i:=0;
  IF (x1#x2) OR (y1#y2) THEN
    REPEAT
      INC(i);
      x1:=x1 DIV 2; y1:=y1 DIV 2;
      x2:=x2 DIV 2; y2:=y2 DIV 2
    UNTIL (x1=x2)&(y1=y2)
  END;
  k:=0;
  ident:=seg_id<<16+sig_id;

  WHILE dws^[i][x1][y1][k]#ident DO INC(k) END;

  dws^[i][x1][y1][k]:=new<<16+sig_id;
  done:=TRUE
END csnum;

PROCEDURE open(mdl: MODEL; int: BOOLEAN);

  VAR sig : SIGNAL;
      sizeX,sizeY,j,sid,lev: INTEGER;
      cur: DWS;

  PROCEDURE undo;
    VAR i,j,k: INTEGER;
  BEGIN
    FOR i:=0 TO HIGH(cur^) DO
      FOR j:=0 TO HIGH(cur^[i]) DO
        FOR k:=0 TO HIGH(cur^[i][j]) DO DISPOSE(cur^[i][j][k]) END;
        DISPOSE(cur^[i][j])
      END;
      DISPOSE(cur^[i])
    END;
    DISPOSE(cur^)
  END undo;

BEGIN
  done:=TRUE;
  IF (mdl^.openco#0) THEN
    IF     int & (mdl^.sig#NIL) THEN INC(mdl^.openco); RETURN END;
    IF NOT int & (mdl^.ext#NIL) THEN INC(mdl^.openco); RETURN END;
  END;
  IF mdl^.ext=NIL THEN
    NEW(mdl^.ext);
    IF NOT mem.done THEN
      error:=mem.error; done:=FALSE; RETURN
    END;
    NEW(mdl^.ext^,0)
  END;
  IF mdl^.sig=NIL THEN
    NEW(mdl^.sig);
    IF NOT mem.done THEN
      error:=mem.error; DISPOSE(mdl^.ext); done:=FALSE; RETURN
    END;
    NEW(mdl^.sig^,0)
  END;
  WITH mdl^ DO
    IF int THEN cur:=sig ELSE cur:=ext END;
    IF HIGH(cur^)<0 THEN
      sizeX:=((mdl^.X+cell_size-1) DIV cell_size + delta*2)*2;
      sizeY:=((mdl^.Y+cell_size-1) DIV cell_size + delta*2)*2;

      lev:=0;
      REPEAT
        sizeX:=(sizeX+1) DIV 2; sizeY:=(sizeY+1) DIV 2;
        RESIZE(cur^,lev+1);
        IF NOT mem.done THEN error:=mem.error; undo; done:=FALSE; RETURN END;
        NEW(cur^[lev],0);
        RESIZE(cur^[lev],sizeX);
        IF NOT mem.done THEN error:=mem.error; undo; done:=FALSE; RETURN END;
        FOR j:=0 TO sizeX-1 DO NEW(cur^[lev][j],0) END;
        FOR j:=0 TO sizeX-1 DO
          RESIZE(cur^[lev][j],sizeY);
          IF NOT mem.done THEN error:=mem.error; undo; done:=FALSE; RETURN END;
          FOR sid:=0 TO HIGH(cur^[lev][j]) DO NEW(cur^[lev][j][sid],0) END
        END;
        lev:=lev+1
      UNTIL (sizeX=1)&(sizeY=1);

      IF int THEN
        FOR sid:=0 TO HIGH(mdl^.sigs) DO
          FOR j:=0 TO HIGH(mdl^.sigs[sid]^.cu) DO
            fill(sig,mdl^.sigs[sid]^.cu[j],sid,j);
            IF NOT done THEN undo; RETURN END
          END
        END
      ELSE
        FOR sid:=0 TO HIGH(mdl^.epns) DO
          FOR j:=0 TO HIGH(mdl^.epns[sid]^.cu) DO
            fill(ext,mdl^.epns[sid]^.cu[j],sid,j);
            IF NOT done THEN undo; RETURN END
          END
        END
      END
    END
  END;
  INC(mdl^.openco)
END open;

PROCEDURE close(mdl: MODEL; int: BOOLEAN);

  PROCEDURE remove_dws(dws: DWS);
    VAR i,j,k: INTEGER;
  BEGIN
    FOR i:=0 TO HIGH(dws^) DO
      FOR j:=0 TO HIGH(dws^[i]) DO
        FOR k:=0 TO HIGH(dws^[i][j]) DO DISPOSE(dws^[i][j][k]) END;
        DISPOSE(dws^[i][j])
      END;
      DISPOSE(dws^[i])
    END;
    DISPOSE(dws^)
  END remove_dws;

  VAR dws: DWS;

BEGIN
  done:=TRUE;
  DEC(mdl^.openco);
  IF mdl^.openco#0 THEN RETURN END;
  IF int     THEN dws:=mdl^.sig; mdl^.sig:=NIL
  ELSE            dws:=mdl^.ext; mdl^.ext:=NIL
  END;
  IF dws#NIL THEN remove_dws(dws) END
END close;

PROCEDURE opencount(mdl: MODEL): INTEGER;
BEGIN
  RETURN mdl^.openco
END opencount;


PROCEDURE sig2wire(s: SIGNAL; VAR wire: WIRE);
BEGIN
  IF s=NIL THEN done:=FALSE; error:=err.bad_desc; RETURN END;
  WITH wire DO
    start:=0; end:=HIGH(s^.cu);
    ident:=-1;
    sig:=s;
    IF s^.type*{31}={} THEN dws:=s^.host^.sig
    ELSE                    dws:=s^.host^.ext
    END;
    IF (dws=NIL) OR (HIGH(dws^)<0) THEN dws:=NIL END
  END;
  done:=TRUE
END sig2wire;

PROCEDURE first_seg(VAR seg: SEGMENT; VAL box: SEGMENT; VAR w: WIRE): BOOLEAN;
BEGIN
  w.ident:=w.start;
  LOOP
    IF w.ident>w.end THEN RETURN FALSE
    ELSE
      seg:=w.sig^.cu[w.ident];
      IF chk_box(SYSTEM.ADR(box),SYSTEM.ADR(seg)) THEN RETURN TRUE END;
      w.ident:=w.ident+1
    END
  END
END first_seg;

PROCEDURE next_seg(VAR seg: SEGMENT; VAL box: SEGMENT; VAR w: WIRE): BOOLEAN;
BEGIN
  LOOP
    w.ident:=w.ident+1;
    IF w.ident>w.end THEN RETURN FALSE
    ELSE
      seg:=w.sig^.cu[w.ident];
      IF chk_box(SYSTEM.ADR(box),SYSTEM.ADR(seg)) THEN RETURN TRUE END
    END
  END
END next_seg;

PROCEDURE first_in_box
     (VAR seg: SEGMENT; box: SEGMENT; VAR sig: SIGNAL; VAR ibox: ITERBOX;
      mdl: MODEL; int: BOOLEAN): BOOLEAN;

  VAR ind: BITSET;
      pi : SYSTEM.ADDRESS;
      ei : INTEGER;
BEGIN
  IF (mdl=NIL)      THEN done:=FALSE; error:=err.bad_parm; RETURN FALSE END;
  WITH ibox DO
(*$<$U+*)
    IF int THEN sigs:=SYSTEM.ADR(mdl^.sigs^); dws:=mdl^.sig
    ELSE        sigs:=SYSTEM.ADR(mdl^.epns^); dws:=mdl^.ext
    END;
(*$>*)
    IF (dws=NIL) OR (HIGH(dws^)<0) THEN
      error:=err.ill_access; done:=FALSE; RETURN FALSE
    END;
    unpack0(box,x1,y1,x2,y2);

    x1:=x1 DIV cell_size + delta; y1:=y1 DIV cell_size + delta;
    x2:=x2 DIV cell_size + delta; y2:=y2 DIV cell_size + delta;

    IF (x1>HIGH(dws^[0])) OR (y1>HIGH(dws^[0][0])) OR (x2<0) OR (y2<0) THEN
      done:=TRUE; RETURN FALSE
    END;

    IF x1<0                   THEN x1:=0                   END;
    IF y1<0                   THEN y1:=0                   END;
    IF x2>HIGH(dws^[0])       THEN x2:=HIGH(dws^[0])       END;
    IF y2>HIGH(dws^[0][0])    THEN y2:=HIGH(dws^[0][0])    END;

    lid:=0;
    WHILE lid<=HIGH(dws^) DO
      xid:=x1;
      WHILE xid<=x2 DO
        yid:=y1;
        IF yid<=y2 THEN
          REPEAT
            did:=0;
            ei:=HIGH(dws^[lid][xid][yid]);
            IF did<=ei THEN
              pi:=SYSTEM.ADR(dws^[lid][xid][yid][did]);
              REPEAT
                ind:=BITSET(pi^);
                IF chk_box(
                   SYSTEM.ADR(box),
                   SYSTEM.ADR(sigs^[INTEGER(ind*mask)]^.cu[INTEGER(ind<<16*mask)]))
                THEN
                  sig:=sigs^[INTEGER(ind*mask)]; seg:=sig^.cu[INTEGER(ind<<16*mask)];
                  done:=TRUE; RETURN TRUE
                END;
                INC(did); INC(pi);
              UNTIL did>ei
            END;
            INC(yid);
          UNTIL yid>y2
        END;
        INC(xid)
      END;
      INC(lid);
      x1:=x1 DIV 2; y1:=y1 DIV 2;
      x2:=x2 DIV 2; y2:=y2 DIV 2
    END
  END;
  sig:=NIL; seg.start:=0; seg.end:=0; seg.size:=0;
  done:=TRUE; RETURN FALSE
END first_in_box;

PROCEDURE next_in_box
         (VAR seg: SEGMENT; box: SEGMENT; VAR sig: SIGNAL; VAR ibox: ITERBOX): BOOLEAN;
  VAR ind: BITSET;
      pi : SYSTEM.ADDRESS;
      ei : INTEGER;
BEGIN
  WITH ibox DO
    INC(did);
    WHILE lid<=HIGH(dws^) DO
      WHILE xid<=x2 DO
        IF yid<=y2 THEN
          REPEAT
            ei:=HIGH(dws^[lid][xid][yid]);
            IF did<=ei THEN
              pi:=SYSTEM.ADR(dws^[lid][xid][yid][did]);
              REPEAT
                ind:=BITSET(pi^);
                IF chk_box(
                   SYSTEM.ADR(box),
                   SYSTEM.ADR(sigs^[INTEGER(ind*mask)]^.cu[INTEGER(ind<<16*mask)]))
                THEN
                  sig:=sigs^[INTEGER(ind*mask)]; seg:=sig^.cu[INTEGER(ind<<16*mask)];
                  done:=TRUE; RETURN TRUE;
                END;
                INC(did); INC(pi);
              UNTIL did>ei
            END;
            did:=0; INC(yid)
          UNTIL yid>y2
        END;
        yid:=y1; INC(xid)
      END;
      x1:=x1 DIV 2; y1:=y1 DIV 2;
      x2:=x2 DIV 2; y2:=y2 DIV 2;
      xid:=x1; yid:=y1; INC(lid)
    END
  END;
  sig:=NIL; seg.start:=0; seg.end:=0; seg.size:=0;
  done:=TRUE; RETURN FALSE
END next_in_box;

PROCEDURE on_line(VAL line0,line1: TOPOLOGY): BOOLEAN;
  VAR dx,dy: INTEGER;
BEGIN
  dx:=line0.x2-line0.x1; dy:=line0.y2-line0.y1;
  RETURN ((line1.x1-line0.x1)*dy=(line1.y1-line0.y1)*dx)&
         ((line1.x2-line0.x1)*dy=(line1.y2-line0.y1)*dx);
END on_line;

PROCEDURE side(x1,y1,x2,y2,x,y: INTEGER): INTEGER;
  VAR dx,dy,z1,z2,r: INTEGER;
BEGIN
  dx:=x2-x1; dy:=y2-y1;
  IF (dx=0)&(dy=0) THEN
    IF (x=x1)&(y=y1) THEN RETURN 0 ELSE RETURN 1 END;
  END;
  r:=x*dx+y*dy;
  z1:=r-x1*dx-y1*dy;
  z2:=x2*dx+y2*dy-r;
  IF (z1>=0)&(z2>=0) THEN RETURN  0
  ELSIF z1<0         THEN RETURN -1
  ELSE                    RETURN  1 END
END side;

PROCEDURE strong_side(x1,y1,x2,y2,x,y: INTEGER): INTEGER;
  VAR dx,dy,z1,z2,r: INTEGER;
BEGIN
  dx:=x2-x1; dy:=y2-y1;
  IF (dx=0)&(dy=0) THEN RETURN 1 END;
  r:=x*dx+y*dy;
  z1:=r-x1*dx-y1*dy;
  z2:=x2*dx+y2*dy-r;
  IF (z1>0)&(z2>0) THEN RETURN  0
  ELSIF z1<=0      THEN RETURN -1
  ELSE                  RETURN  1 END
END strong_side;

PROCEDURE ISQ(i: INTEGER): INTEGER;
CODE cod.copt cod.mul END ISQ;

PROCEDURE SQ(r: REAL): REAL;
CODE cod.copt cod.fmul END SQ;

PROCEDURE len(x1,y1,x2,y2,x,y: INTEGER): INTEGER;
  VAR sd,A,B,C: INTEGER;
BEGIN
  B:=x2-x1; A:=y1-y2;
  IF NOT BOOLEAN(BITSET(A)+BITSET(B)) THEN RETURN ISQ(x1-x)+ISQ(y1-y) END;
  sd:=side(x1,y1,x2,y2,x,y);
  IF sd<0 THEN RETURN ISQ(x1-x)+ISQ(y1-y) END;
  IF sd>0 THEN RETURN ISQ(x2-x)+ISQ(y2-y) END;
  C:=-x1*A-y1*B;
  RETURN TRUNC(SQ(FLOAT(A*x+B*y+C))/FLOAT(ISQ(A)+ISQ(B)))
END len;

PROCEDURE reset_changes(mdl: MODEL);
BEGIN
  IF mdl=NIL       THEN done:=FALSE; error:=err.bad_desc; RETURN END;
  IF mdl^.type#NIL THEN done:=FALSE; error:=err.bad_parm; RETURN END;
  done:=TRUE;
  WITH mdl^ DO
    clip.x1:=07FFFh; clip.y1:=07FFFh; clip.x2:=-08000h; clip.y2:=-08000h;
    clip.size:=0;    clip.vsize:=0
  END
END reset_changes;

PROCEDURE last_changes(mdl: MODEL; VAR clip: TOPOLOGY);
BEGIN
  IF mdl=NIL       THEN done:=FALSE; error:=err.bad_desc; RETURN END;
  IF mdl^.type#NIL THEN done:=FALSE; error:=err.bad_parm; RETURN END;
  done:=TRUE; clip:=mdl^.clip
END last_changes;

PROCEDURE update_clip(mdl: MODEL; VAL seg: SEGMENT);
  VAR x1,y1,x2,y2: INTEGER;
BEGIN
  WITH mdl^ DO
    unpack0(seg,x1,y1,x2,y2);
    IF clip.x1>x1 THEN clip.x1:=x1 END;
    IF clip.y1>y1 THEN clip.y1:=y1 END;
    IF clip.x2<x2 THEN clip.x2:=x2 END;
    IF clip.y2<y2 THEN clip.y2:=y2 END
  END
END update_clip;

PROCEDURE app_seg(VAR w: WIRE; VAL seg: SEGMENT);
BEGIN
  done:=TRUE;
  WITH w DO
    end:=end+1;
    RESIZE(sig^.cu,HIGH(sig^.cu)+2);
    IF NOT mem.done THEN done:=FALSE; error:=mem.error; RETURN END;
    sig^.cu[HIGH(sig^.cu)]:=seg;
    IF dws#NIL THEN
      fill(dws,seg,sig^.no,HIGH(sig^.cu));
      IF NOT done THEN RESIZE(sig^.cu,HIGH(sig^.cu)) END
    END;
    update_clip(w.sig^.host,seg);
    sig^.type:=sig^.type-{30};
    sig^.host^.R:=changed
  END
END app_seg;

PROCEDURE del_seg(VAR w: WIRE);
  VAR h: INTEGER;
BEGIN
  done:=TRUE;
  WITH w DO
    ASSERT((ident<=end)&(ident>=start));
    h:=HIGH(sig^.cu);
    update_clip(sig^.host,sig^.cu[ident]);
    IF dws#NIL THEN clear(dws,sig^.cu[ident],sig^.no,ident); ASSERT(done) END;

    IF ident#h THEN
      IF dws#NIL THEN csnum(dws,sig^.cu[h],sig^.no,h,ident) END;
      sig^.cu[ident]:=sig^.cu[h];
    END;

    DEC(end); DEC(ident);
    RESIZE(sig^.cu,h);
    sig^.type:=sig^.type-{30};
    sig^.host^.R:=changed
  END
END del_seg;

PROCEDURE shorted(VAL top: TOPOLOGY; sig: SIGNAL; VAR sg: SEGMENT;
                                               VAR short: SIGNAL): BOOLEAN;
  VAR xs,xe,ys,ye,r,dx,dy,dX,dY: INTEGER;
      l,seg: SEGMENT;
      tp   : TOPOLOGY;
      w    : WIRE;
      ibox : ITERBOX;
BEGIN
  done:=TRUE;
  IF sig=NIL THEN done:=FALSE; error:=err.bad_desc; RETURN FALSE END;

  IF top.x1>top.x2 THEN xs:=top.x2+m8000h; xe:=top.x1+m8000h
  ELSE                  xs:=top.x1+m8000h; xe:=top.x2+m8000h
  END;
  IF top.y1>top.y2 THEN ys:=top.y2+m8000h; ye:=top.y1+m8000h
  ELSE                  ys:=top.y1+m8000h; ye:=top.y2+m8000h
  END;
  r:=top.size+Clearens;
  DEC(xs,r); DEC(ys,r); INC(xe,r); INC(ye,r);
  l.start:=xs+ys<<16;
  l.end  :=xe+ye<<16;
  dX:=top.x2-top.x1;
  dY:=top.y2-top.y1;

  IF (NOT (fantom IN sig^.type)) &
     first_in_box(seg,l,short,ibox,sig^.host,({31}*sig^.type)={})
  THEN
    REPEAT
      unpack(seg,tp);
      IF (short#sig) & (top.layer*tp.layer#{}) & NOT (fantom IN short^.type)
      THEN
        r:=ISQ(Clearens+tp.size+top.size);
        dx:=tp.x2-tp.x1; dy:=tp.y2-tp.y1;
        IF ((dX*(tp.y1-top.y1)-dY*(tp.x1-top.x1)>=0)#
            (dX*(tp.y2-top.y1)-dY*(tp.x2-top.x1)>=0))
          &((dx*(top.y1-tp.y1)-dy*(top.x1-tp.x1)>=0)#
            (dx*(top.y2-tp.y1)-dy*(top.x2-tp.x1)>=0))
         OR (len(top.x1,top.y1,top.x2,top.y2,tp.x1,tp.y1)<r)
         OR (len(top.x1,top.y1,top.x2,top.y2,tp.x2,tp.y2)<r)
         OR (len(tp.x1,tp.y1,tp.x2,tp.y2,top.x1,top.y1)<r)
         OR (len(tp.x1,tp.y1,tp.x2,tp.y2,top.x2,top.y2)<r)
        THEN
          sg:=seg; RETURN TRUE
        END
      END
    UNTIL NOT next_in_box(seg,l,short,ibox)
  END;
  short:=NIL; RETURN FALSE
END shorted;

PROCEDURE ins_range(sig: SIGNAL; VAL top: TOPOLOGY; chk: BOOLEAN;
                                 VAR  sc: BOOLEAN);
  VAR tp: TOPOLOGY;
    wire: WIRE;         l,l1,xs,ys,xe,ye: INTEGER;
   short: SIGNAL;
  ln,seg: SEGMENT;
BEGIN
  done:=TRUE;  sc:=FALSE;
  IF (top.x1=top.x2) & (top.y1=top.y2) & (top.vsize=0) THEN RETURN END;
  IF chk THEN
    IF shorted(top,sig,seg,short) THEN sc:=TRUE; RETURN END
  END;
  sig2wire(sig,wire);
  pack(ln,top);
  xe:=top.x2; xs:=top.x1; ye:=top.y2; ys:=top.y1;
  IF first_seg(seg,ln,wire) THEN
    REPEAT
      unpack(seg,tp);
      IF (tp.layer=top.layer)&(top.fixed=tp.fixed) THEN
        IF (tp.vsize=0)&(top.vsize=0) THEN
          IF on_line(top,tp) THEN
            l :=side(tp.x1,tp.y1,tp.x2,tp.y2,top.x1,top.y1);
            l1:=side(tp.x1,tp.y1,tp.x2,tp.y2,top.x2,top.y2);
            IF (l1=0)&(l=0) THEN
              IF tp.size>=top.size THEN done:=TRUE; RETURN END;
            ELSIF l=0 THEN
              IF tp.size=top.size THEN
                IF l1>0 THEN xs:=tp.x1; ys:=tp.y1
                ELSE         xs:=tp.x2; ys:=tp.y2
                END;
                del_seg(wire)
              END;
            ELSIF l1=0 THEN
              IF tp.size=top.size THEN
                IF l>0 THEN xe:=tp.x1; ye:=tp.y1
                ELSE        xe:=tp.x2; ye:=tp.y2
                END;
                del_seg(wire)
              END;
            ELSIF ((l<0)#(l1<0))&(tp.size<=top.size) THEN
              del_seg(wire)
            END
          END;
        ELSIF (top.vsize#0)&(tp.vsize#0)&(top.x1=tp.x1)&(top.y1=tp.y1) THEN
          IF (tp.size>=top.size) & (tp.vsize=top.vsize) THEN
            done:=TRUE; RETURN
          END;
          del_seg(wire)
        END
      END
    UNTIL NOT next_seg(seg,ln,wire)
  END;
  tp.x1:=xs;           tp.y1:=ys;
  tp.x2:=xe;           tp.y2:=ye;
  tp.layer:=top.layer; tp.size:=top.size;
  tp.vsize:=top.vsize; tp.fixed:=top.fixed;
  pack(seg,tp);
  sig2wire(sig,wire);
  app_seg(wire,seg)
END ins_range;

PROCEDURE del_range(sig: SIGNAL; top: TOPOLOGY);
  VAR l,l1,xs,ys,xe,ye,r1,r2,x,y: INTEGER;
      ln,seg,sg: SEGMENT;
      wire: WIRE;
      tp: TOPOLOGY;
      Del?: BOOLEAN;
BEGIN
  sig2wire(sig,wire);
  pack(ln,top);
  LOOP
    Del?:=FALSE;
    IF first_seg(seg,ln,wire) THEN
      REPEAT
        unpack(seg,tp);
        IF (top.layer=tp.layer) THEN
          IF (top.vsize=0)&(tp.vsize=0) THEN
            IF on_line(top,tp) THEN
              l :=strong_side(tp.x1,tp.y1,tp.x2,tp.y2,top.x1,top.y1);
              l1:=strong_side(tp.x1,tp.y1,tp.x2,tp.y2,top.x2,top.y2);
              IF (l=0)&(l1=0) THEN
                r1:=(tp.x1-top.x1)*(tp.x1-top.x1)+(tp.y1-top.y1)*(tp.y1-top.y1);
                r2:=(tp.x1-top.x2)*(tp.x1-top.x2)+(tp.y1-top.y2)*(tp.y1-top.y2);
                IF r1<r2 THEN
                  x:=tp.x2; y:=tp.y2;
                  Del?:=TRUE; del_seg(wire);
                  tp.x2:=top.x1; tp.y2:=top.y1; pack(sg,tp);
                  app_seg(wire,sg);
                  tp.x1:=top.x2; tp.y1:=top.y2; tp.x2:=x; tp.y2:=y; pack(sg,tp);
                  app_seg(wire,sg);
                ELSE
                  x:=tp.x2; y:=tp.y2;
                  Del?:=TRUE; del_seg(wire);
                  tp.x2:=top.x2; tp.y2:=top.y2; pack(sg,tp);
                  app_seg(wire,sg);
                  tp.x1:=top.x1; tp.y1:=top.y1; tp.x2:=x; tp.y2:=y; pack(sg,tp);
                  app_seg(wire,sg);
                END;
              ELSIF l=0 THEN
                IF l1>0 THEN
                  Del?:=TRUE; del_seg(wire);
                  tp.x2:=top.x1; tp.y2:=top.y1; pack(sg,tp);
                  app_seg(wire,sg);
                ELSE
                  Del?:=TRUE; del_seg(wire);
                  tp.x1:=top.x1; tp.y1:=top.y1; pack(sg,tp);
                  app_seg(wire,sg);
                END;
              ELSIF l1=0 THEN
                IF l>0 THEN
                  Del?:=TRUE; del_seg(wire);
                  tp.x2:=top.x2; tp.y2:=top.y2; pack(sg,tp);
                  app_seg(wire,sg);
                ELSE
                  Del?:=TRUE; del_seg(wire);
                  tp.x1:=top.x2; tp.y1:=top.y2; pack(sg,tp);
                  app_seg(wire,sg);
                END;
              ELSIF ((l<0)#(l1<0))& NOT((tp.x1=tp.x2)&(tp.y1=tp.y2)) THEN
                Del?:=TRUE; del_seg(wire);
              END;
            END;
          ELSIF (top.vsize#0)&(tp.vsize#0) THEN
            IF (tp.x1=tp.x2)&(tp.y1=tp.y2)&(top.x1=tp.x1)&(top.y1=tp.y1) THEN
              Del?:=TRUE; del_seg(wire);
            END;
          END;
        END;
      UNTIL Del? OR NOT next_seg(seg,ln,wire);
    END;
    IF NOT Del? THEN EXIT END
  END;
  done:=TRUE
END del_range;

PROCEDURE sort(VAR w: WIRE);

  VAR i: INTEGER;
 sorted: CU;
  marks: DYNARR OF BOOLEAN;
  renum: DYNARR OF INTEGER;

  dX,dY: INTEGER;
 sg,seg: pSegment;
    top: TOPOLOGY;

  PROCEDURE merge(): BOOLEAN;
    VAR dx,dy,r: INTEGER; tp: TOPOLOGY;
  BEGIN
    IF chk_box(sg,seg) THEN
      unpack(sg^,tp);
      WITH top DO
        IF layer*tp.layer#{} THEN
          r:=ISQ(Clearens+tp.size+size);
          dx:=tp.x2-tp.x1; dy:=tp.y2-tp.y1;
          IF ((dX*(tp.y1-y1)-dY*(tp.x1-x1)>=0)#
               (dX*(tp.y2-y1)-dY*(tp.x2-x1)>=0))
             &((dx*(y1-tp.y1)-dy*(x1-tp.x1)>=0)#
               (dx*(y2-tp.y1)-dy*(x2-tp.x1)>=0))
             OR (len(x1,y1,x2,y2,tp.x1,tp.y1)<r)
             OR (len(x1,y1,x2,y2,tp.x2,tp.y2)<r)
             OR (len(tp.x1,tp.y1,tp.x2,tp.y2,x1,y1)<r)
             OR (len(tp.x1,tp.y1,tp.x2,tp.y2,x2,y2)<r)
          THEN
            RETURN TRUE
          END
        END
      END
    END;
    RETURN FALSE
  END merge;

  VAR id,td: INTEGER;
BEGIN
  IF w.sig^.type*{30}#{} THEN done:=TRUE; RETURN END;

  IF HIGH(w.sig^.cu)<=0 THEN
    IF HIGH(w.sig^.cu)=0 THEN
      w.sig^.cu[0].size:=INTEGER(BITSET(w.sig^.cu[0].size) + {3})
    END;
    w.sig^.type:=w.sig^.type+{30}; done:=TRUE; RETURN
  END;

  NEW(sorted,HIGH(w.sig^.cu)+1);
  IF NOT mem.done THEN done:=FALSE; error:=mem.error; RETURN END;
  NEW(renum,HIGH(w.sig^.cu)+1);
  IF NOT mem.done THEN
    error:=mem.error; DISPOSE(sorted); done:=FALSE; RETURN
  END;
  NEW(marks,HIGH(w.sig^.cu)+1);
  IF NOT mem.done THEN
    error:=mem.error; DISPOSE(sorted); DISPOSE(renum); done:=FALSE; RETURN
  END;
  FOR i:=0 TO HIGH(marks) DO marks[i]:=FALSE END;


  id:=0;
  td:=0;
  LOOP
    IF id>HIGH(sorted) THEN EXIT END;

    i:=0;
    LOOP
      IF i>HIGH(marks) THEN EXIT END;
      IF NOT marks[i] THEN
        renum[td]:=i; sorted[td]:=w.sig^.cu[i];
        sorted[td].size:=INTEGER(BITSET(sorted[td].size) - {3});
        marks[i]:=TRUE; INC(td); EXIT
      END;
      INC(i)
    END;

    LOOP
      IF id=td THEN EXIT END;
      seg:=SYSTEM.ADR(sorted[id]);
      unpack(seg^,top);
      dX:=top.x2-top.x1; dY:=top.y2-top.y1;

      FOR i:=1 TO HIGH(w.sig^.cu) DO
        IF NOT marks[i] THEN
          sg:=SYSTEM.ADR(w.sig^.cu[i]);
          IF merge() THEN
            renum[td]:=i;
            marks[i]:=TRUE;
            sorted[td]:=sg^;
            sorted[td].size:=INTEGER(BITSET(sorted[td].size) - {3});
            INC(td); IF td>HIGH(sorted) THEN id:=td; EXIT END
          END
        END
      END;
      INC(id)
    END;
    sorted[id-1].size:=INTEGER(BITSET(sorted[id-1].size) + {3})
  END;

  IF w.dws#NIL THEN
    FOR i:=0 TO HIGH(sorted) DO
      csnum(w.dws,sorted[i],w.sig^.no,renum[i],i)
    END
  END;
  DISPOSE(w.sig^.cu);
  DISPOSE(marks);
  DISPOSE(renum);
(*$<$U+*) w.sig^.cu^:=sorted^; (*$>*)
  w.sig^.type:=w.sig^.type+{30}; done:=TRUE; w.sig^.host^.R:=changed
END sort;

PROCEDURE stars(sig: SIGNAL): INTEGER;
  VAR s,i: INTEGER;
     wire: WIRE;
BEGIN
  IF sig=NIL THEN done:=FALSE; error:=err.bad_desc; RETURN -1 END;
  sig2wire(sig,wire); sort(wire);
  IF NOT done THEN RETURN -1 END;
  s:=0;
  FOR i:=0 TO HIGH(sig^.cu) DO
    IF {3}*BITSET(sig^.cu[i].size)#{} THEN s:=s+1 END
  END;
  RETURN s
END stars;

PROCEDURE first_star(VAR w: WIRE): BOOLEAN;
  VAR i: INTEGER;
BEGIN
  sort(w); IF NOT done THEN RETURN FALSE END;
  w.start:=0; i:=0;
  LOOP
    IF i>HIGH(w.sig^.cu) THEN RETURN FALSE END;
    IF {3}*BITSET(w.sig^.cu[i].size)#{} THEN w.end:=i; RETURN TRUE END;
    i:=i+1
  END
END first_star;

PROCEDURE next_star(VAR w: WIRE): BOOLEAN;
  VAR i: INTEGER;
BEGIN
  w.start:=w.end+1; i:=w.end+1;
  LOOP
    IF i>HIGH(w.sig^.cu) THEN RETURN FALSE END;
    IF {3}*BITSET(w.sig^.cu[i].size)#{} THEN w.end:=i; RETURN TRUE END;
    i:=i+1
  END
END next_star;

BEGIN
  m8000h:=8000h; mask:={0..15}; msk12:={0..11}; bmask:={0..7};
  done:=TRUE; error:=err.ok;
  snull:=NIL;
  mnull:=NIL;
END pwbWorld.
