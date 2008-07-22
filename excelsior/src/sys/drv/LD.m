MODULE LD; (* Leg 19-Oct-90. (c) KRONOS *)

IMPORT       SYSTEM;
IMPORT  req: defRequest;
IMPORT  err: defErrors;
IMPORT  os : osKernel;
IMPORT   fs: osFiles;
IMPORT  env: tskEnv;
IMPORT  arg: tskArgs;
IMPORT  bio: BIO;

(* run: {.BASE=0} LD -name- -file_name- d=blocks [ -c s=blocks ]
   where -d- is disk size(in 4K blocks), visible for system.
   Real size is (eof(file) DIV 4096) blocks.
*)

CONST SECSZ =  512;
      SSC   =    9;
      BLK   = 4096;
      CXE=00455843h; (*"CXE"0c*)

VAR  sz,rs: INTEGER;
       dev: bio.FILE;

PROCEDURE get_spec(VAR r: req.REQUEST);
BEGIN
  r.dmode :=req.ready+req.wint;
  r.ssc   :=SSC;       r.secsize:=SECSZ;
  r.minsec:=0;         r.maxsec :=sz-1;
  r.cyls  :=-1;        r.ressec :=rs;
  r.heads :=-1;        r.precomp:=-1;
  r.rate  :=-1;        r.dsecs  :=sz-rs
END get_spec;

PROCEDURE set_spec(VAR r: req.REQUEST);
  VAR i: INTEGER;
BEGIN
  i:=r.ressec*SECSZ;;
  IF i>bio.eof(dev) THEN
    bio.extend(dev,i);
    IF NOT bio.done THEN r.res:=bio.error; RETURN END
  END;
  rs:=r.ressec
END set_spec;

PROCEDURE doio(VAR r: req.REQUEST);
BEGIN
  WITH r DO
    res:=err.ok;
    CASE r.op OF
      |req.NOP      :
      |req.READ     : bio.seek(dev,ofs*SECSZ,0);
                      IF NOT bio.done THEN res:=bio.error; RETURN  END;
                      bio.read(dev,buf,len*SECSZ);
                      IF NOT bio.done THEN res:=bio.error END;
      |req.WRITE    : bio.seek(dev,ofs*SECSZ,0);
                      IF NOT bio.done THEN res:=bio.error; RETURN  END;
                      bio.write(dev,buf,len*SECSZ);
                      IF NOT bio.done THEN res:=bio.error END;
      |req.MOUNT    :
      |req.UNMOUNT  :
      |req.POWER_OFF:
      |req.GET_SPEC : get_spec(r)
      |req.SET_SPEC : set_spec(r)
      |req.FORMAT   :
    ELSE r.res:=err.inv_op;
    END;
  END;
END doio;

PROCEDURE get_file;
  VAR hdr: ARRAY [0..6] OF INTEGER;

  PROCEDURE e;
  BEGIN IF bio.done THEN RETURN END; ASSERT(FALSE,bio.error)
  END e;

BEGIN
  IF HIGH(arg.words)<1 THEN HALT(1) END;
  IF HIGH(arg.words[0])>7 THEN HALT(1) END;
  IF arg.flag('-','c') THEN
    IF NOT arg.number('s',sz) THEN HALT(1)  END;
    IF sz<6 THEN HALT(1) END;
    hdr[6]:=CXE; hdr[5]:=sz;
    bio.create(dev,arg.words[1],'rw',4096+BYTES(hdr));    e;
    bio.seek(dev,4096,0);                                 e;
    bio.put(dev,hdr,BYTES(hdr));                          e;
    bio.link(dev,arg.words[1],'rw');                      e;
  ELSE
    bio.open(dev,arg.words[1],'rw');                      e;
    bio.seek(dev,4096,0);                                 e;
    bio.get(dev,hdr,BYTES(hdr));                          e;
    IF hdr[6]#CXE THEN HALT(1) END;
    sz:=hdr[5]
  END;
  sz:=sz * BLK DIV SECSZ;
END get_file;

PROCEDURE define;
  VAR   i: INTEGER;
BEGIN
  i:=fs.define_driver(arg.words[0],'',0,fs.disk,doio);
  IF i#err.ok THEN ASSERT(FALSE,i) END;
  env.put_str(env.info,arg.words[0],TRUE);
  rs:=0;
END define;

PROCEDURE halt;
BEGIN
  IF fs.remove_driver(arg.words[0])#0 THEN END;
  bio.close(dev)
END halt;

BEGIN
  get_file;
  env.final(halt);
  define;
  env.become_ipr;
  os.suspend(os.active(),-1)
END LD.
