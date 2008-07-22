MODULE fsdb; (* Hady. 03-Jan-90. (c) KRONOS *)

(*$U+*)
(*$A8 Leo 12-Jan-91 *)

(*
  06-Jul-90: введено понятие физического размера носителя (Hady)
  07-Jul-90: проверка непрерывности директорий
             поправлен интерфейс, help'ы                  (Hady)
  26-Jul-90: "busy"=>"free" в информации о диске,
             "block area" от 0 до dio.d_size а не до dio.no_b
  02-Nov-90: подхакирована распаковка суперблока с целью
             оптимизации и недопускания зацикливания.
             Новая функция "verify_disk".                 (Hady)
  04-Jan-91: поправлена "verify_disk"; исправлена ошибка
             в перемонтировании диска. Легализован запуск
             без параметров.                              (Hady)
  10-Jan-91: Ошибки в check_ref. Поиск по неполному имени
             узла в директории.                           (Hady)
*)

IMPORT  BIO, ASCII;
IMPORT  Time, tskArgs, Lexicon;
IMPORT  SYSTEM;

IMPORT  img: Strings  , tty: Terminal
      , kbr: Keyboard , sle: strEditor
      , err: defErrors, mem: Heap
      , low: lowLevel;

WITH STORAGE: mem;

CONST version = 'File System DeBug  v0.041 04-Jan-91 (c) KRONOS';

MODULE dio; (* Hady. 20-Oct-89. (c) KRONOS *)

  IMPORT  tty; -- for debug purposes ONLY !

  IMPORT       SYSTEM;
  IMPORT  bio: BIO;
  IMPORT  lex: Lexicon;

  IMPORT  img, err;

  EXPORT QUALIFIED dir, lng, esc, sys, all_modes
                 , d_del, d_file, d_dir, d_hidden, d_entry, d_esc, d_sys
                 , all_kinds, drv, vol_name
                 , i_node, dir_node, Dir, no_i, no_b, iSET, bSET
                 , free_i, free_b, sys_0, d_size
                 , s_put, message, error, reset, get_inode, put_inode
                 , get_block, put_block, dir_walk, end_walk, mount
                 , release, dNode;

  CONST
    dir = 1;  lng = 2;  esc = 3; sys = 4; all_modes ={dir,lng,esc,sys};

  TYPE
    i_node =
    RECORD
      Ref   : ARRAY [0..7] OF INTEGER;            -- 32
      Mode  : BITSET;                             -- 36
      Links : INTEGER;                            -- 40
      Eof   : INTEGER;                            -- 44
      cTime : INTEGER;  (* Creation time *)       -- 48
      wTime : INTEGER;  (* Last write time *)     -- 52
      pro   : INTEGER;                            -- 56
      res   : ARRAY [0..1] OF INTEGER;            -- 64
    END (* i_node *);

    b_ptr = POINTER TO ARRAY [0..1023] OF SYSTEM.WORD;

  CONST
    d_del = {0};   d_file = {1};   d_dir = {2};   d_hidden = {3};
    d_esc = {4};   d_sys  = {5};

    d_entry   = d_dir + d_file + d_esc + d_sys;

    all_kinds = d_del + d_entry + d_hidden;

  TYPE  dir_node = RECORD
                     name: ARRAY [0..31] OF CHAR;     -- 32
                     rfe0: ARRAY [0..3] OF INTEGER;   -- 48
                     inod: INTEGER;                   -- 52
                     kind: BITSET;                    -- 56
                     rfe1: ARRAY [0..1] OF INTEGER;   -- 64
                   END;

        dNode = POINTER TO dir_node;

       DIR = RECORD
                 no: INTEGER;
                  n: i_node;
               buff: DYNARR OF dir_node;
                REF: DYNARR OF INTEGER;
             END;

        Dir = POINTER TO DIR;

  VAR     vol_name: ARRAY [0..79] OF CHAR;
      no_i, free_i: INTEGER; -- inodes account on device
      no_b, free_b: INTEGER; -- blocks account on device
      d_size,sys_0: INTEGER;

  VAR iSET: DYNARR OF BITSET; -- free iNodes
      bSET: DYNARR OF BITSET; -- free blocks

  VAR   drv: bio.FILE;
      super: b_ptr;    s_put: BOOLEAN;
      ibuff: b_ptr;    i_put: BOOLEAN;
       iblk: INTEGER;

  VAR error: BOOLEAN;
    message: ARRAY [0..79] OF CHAR;

  PROCEDURE reset; BEGIN error:=FALSE; message:="" END reset;

  CONST no_volume = "";

  PROCEDURE init_state;
  BEGIN
    vol_name:=no_volume;
    reset;        drv:=bio.null;
    no_i  :=-1;   free_i:=-1;
    no_b  :=-1;   free_b:=-1;
    d_size:=-1;   sys_0 :=-1;  iblk  :=-1;
    NEW(iSET) ;   NEW(bSET) ;
    super:=NIL;   ibuff:=NIL;
    s_put:=FALSE; i_put:=FALSE;
  END init_state;

  PROCEDURE check();
  BEGIN
    IF bio.done THEN RETURN END;
    error:=TRUE; lex.perror(message,bio.error,"%%s");
  END check;

  PROCEDURE get_block(no: INTEGER; buff: SYSTEM.ADDRESS; sz: INTEGER);
    VAR r: BOOLEAN; i: INTEGER;
  BEGIN
    bio.seek(drv,no*4096,0); check;
    IF error THEN RETURN END;
    bio.read(drv,buff,sz); check;
  END get_block;

  PROCEDURE put_block(no: INTEGER; buff: SYSTEM.ADDRESS; sz: INTEGER);
  BEGIN
    bio.seek(drv,no*4096,0); check;
    IF error THEN RETURN END;
    bio.write(drv,buff,sz); check;
    --tty.print('put_block: no: %5d, size: %4d\n',no,sz);
  END put_block;

  PROCEDURE unpack_sup;

    CONST inods = 4096 DIV BYTES(i_node);

    VAR lab: POINTER TO ARRAY [0..7] OF INTEGER;
    isz,bsz: INTEGER;
          s: POINTER TO ARRAY [0..3] OF CHAR;
          i: INTEGER;

  BEGIN
    lab:=SYSTEM.ADDRESS(super);
    no_b:=lab^[5];
    no_i:=lab^[4];
    -- s:=SYSTEM.ADR(lab^[6]); -- CHECK "CXE ?";
    isz:=(no_i+31) DIV 32;
    bsz:=(no_b+31) DIV 32;
    bSET^.ADR:=SYSTEM.ADDRESS(super)+16; bSET^.HIGH:=bsz-1;
    iSET^.ADR:=bSET^.ADR+bsz;          iSET^.HIGH:=isz-1;
    IF no_i>d_size*2 THEN -- мокропальцевая оценка
      free_i:=-1
    ELSE
      free_i:=0; i:=0;
      WHILE i<no_i DO
        IF i MOD 32=0 THEN
          WHILE (i<no_i) & (iSET[i DIV 32]={}) DO INC(i,32) END;
          WHILE (i<no_i) & (iSET[i DIV 32]={0..31}) DO
            INC(i,32); INC(free_i,32)
          END
        END;
        IF (i<no_i) & ((i MOD 32) IN iSET[i DIV 32])
        THEN free_i:=free_i+1
        END; INC(i)
      END
    END;
    IF no_b>d_size*2 THEN -- мокропальцевая оценка
      free_b:=-1
    ELSE
      free_b:=0; i:=0;
      WHILE i<no_b DO
        IF i MOD 32=0 THEN
          WHILE (i<no_b) & (bSET[i DIV 32]={}) DO INC(i,32) END;
          WHILE (i<no_b) & (bSET[i DIV 32]={0..31}) DO
            i:=i+32; free_b:=free_b+32
          END
        END;
        IF (i<no_b) & ((i MOD 32) IN bSET[i DIV 32])
        THEN free_b:=free_b+1
        END; INC(i)
      END
    END;
    sys_0:=1   +((no_i+inods-1) DIV inods);
  END unpack_sup;

  PROCEDURE mount(drv_name: ARRAY OF CHAR);
    VAR a: SYSTEM.ADDRESS;
  BEGIN
    vol_name:=no_volume;
    NEW(super); s_put:=FALSE;
    bio.open(drv,drv_name,"mc");
    IF NOT bio.done & (bio.error=err.sec_vio) THEN
      bio.open(drv,drv_name,"r")
    END;
    check;
    IF bio.is_disk*bio.kind(drv)={} THEN
      img.print(message,'"%s" is not disk device',drv_name);
      error:=TRUE; RETURN
    END;
    d_size:=bio.eof(drv) DIV 4096;
    bio.lock(-1,drv); check;
    IF error THEN RETURN END;
    get_block(1,super,4096);
    IF error THEN RETURN END;
    unpack_sup;
    img.copy(vol_name,drv_name)
  END mount;

  TYPE iNode = POINTER TO i_node;

  CONST inods = 64; -- no inodes per one 4KB block;

  PROCEDURE get_inode0(no: INTEGER): iNode;
    VAR b: INTEGER;
        s: ARRAY [0..79] OF CHAR;
  BEGIN
    IF ibuff=NIL THEN NEW(ibuff); i_put:=FALSE; END;
    b:=2+(no DIV inods);
    IF b#iblk THEN
      IF i_put THEN -- flush inode buffer
        put_block(iblk,ibuff,4096); i_put:=FALSE;
      END;
      get_block(b,ibuff,4096);
      IF error THEN
        s:=message;
        img.print(message,"reading inode block %05d: %s",b,s);
        RETURN NIL;
      END;
      iblk:=b;
    END;
    RETURN SYSTEM.ADDRESS(ibuff)+(no MOD inods)*SIZE(i_node);
  END get_inode0;

  PROCEDURE get_inode(no: INTEGER; VAR in: i_node);  VAR n: iNode;
  BEGIN n:=get_inode0(no); IF n#NIL THEN in:=n^ END END get_inode;

  PROCEDURE put_inode(no: INTEGER; VAL in: i_node);
    VAR n: iNode;
  BEGIN
    n:=get_inode0(no);
    IF n#NIL THEN n^:=in; i_put:=TRUE END;
  END put_inode;

  PROCEDURE min(x,y: INTEGER): INTEGER;
  BEGIN IF x>y THEN RETURN y ELSE RETURN x END END min;

  PROCEDURE dir_walk(ino: INTEGER): Dir;

    VAR p_n: iNode; d: Dir; max,b: INTEGER;
        free: INTEGER;

    PROCEDURE acc_ref(VAR ref: ARRAY OF INTEGER): INTEGER;
      VAR i: INTEGER;
    BEGIN i:=0;
      WHILE (i<=HIGH(ref)) & (ref[i]>0) & (ref[i]<no_b) DO
        IF free>HIGH(d^.REF) THEN
          RESIZE(d^.REF,SIZE(d^.REF)+8);
          IF free>HIGH(d^.REF) THEN
            error:=TRUE; message:='No memory'; RETURN -1
          END;
        END;
        d^.REF[free]:=ref[i]; i:=i+1; free:=free+1;
      END;
      RETURN i*4096;
    END acc_ref;

    PROCEDURE acc_eof(VAR N: i_node);
      VAR buff: POINTER TO ARRAY [0..1023] OF INTEGER;
             i: INTEGER;
    BEGIN
      NEW(d^.REF,8);
      WITH N DO
        IF {lng}*Mode#{} THEN
          buff:=NIL; NEW(buff);
          IF buff=NIL THEN error:=TRUE;
            message:='No memory'; RETURN
          END;
          i:=0; max:=0;
          LOOP
            IF i>7 THEN EXIT END;
            IF (Ref[i]<=0) OR (Ref[i]>=no_b) THEN EXIT END;
            get_block(Ref[i],buff,4096);
            IF error THEN EXIT END;
            max:=max+acc_ref(buff^);
            IF (max<i*1024*4096) THEN EXIT END;
            i:=i+1;
          END;
          DISPOSE(buff);
        ELSE
          max:=acc_ref(Ref);
        END;
        IF (Eof>0) & (Eof<max) THEN max:=Eof END;
      END;
      max:=max DIV BYTES(dir_node);
      RESIZE(d^.REF,free);
    END acc_eof;

  BEGIN
    p_n:=get_inode0(ino);
    IF p_n=NIL THEN RETURN NIL END;
    IF p_n^.Mode*{dir}={} THEN
      error:=TRUE; img.print(message,"Is not directory");
      RETURN NIL;
    END;
    NEW(d); NEW(d^.REF); free:=0;
    WITH d^ DO  no:=ino;  n:=p_n^;
      acc_eof(n);
      IF error THEN DISPOSE(REF); DISPOSE(d); RETURN NIL END;
      IF max=0 THEN DISPOSE(d);
        error:=TRUE; img.print(message,'Bad Eof in dir %d ',ino);
        RETURN NIL
      END;
      NEW(buff,max); b:=0;
      WHILE max>0 DO
        get_block(REF[b],buff^.ADR+b*1024,min(4096,max*BYTES(dir_node)));
        IF error THEN DISPOSE(REF); DISPOSE(d); RETURN NIL END;
        max:=max-min(max,4096 DIV BYTES(dir_node)); INC(b);
      END;
    END;
    RETURN d;
  END dir_walk;

  PROCEDURE end_walk(d: Dir; write: BOOLEAN);
    VAR max,b: INTEGER; n: iNode;
  BEGIN
    IF d=NIL THEN RETURN END;
    IF write THEN
      max:=HIGH(d^.buff)+1; b:=0;
      WHILE max>0 DO
        put_block(d^.REF[b],d^.buff^.ADR+b*1024,
                                   min(4096,max*BYTES(dir_node)));
        max:=max-min(4096 DIV BYTES(dir_node),max); INC(b);
      END;
      put_inode(d^.no,d^.n);
      IF error THEN RETURN END;
    END;
    DISPOSE(d^.buff); DISPOSE(d^.REF); DISPOSE(d);
  END end_walk;

  PROCEDURE release;
    VAR s: ARRAY [0..79] OF CHAR;
  BEGIN
    IF s_put THEN
      put_block(1, super, 4096); s_put:=FALSE;
      IF error THEN s:=message;
        img.print(message,"ATTENTION! writting super block: %s",s);
        RETURN
      END;
      DISPOSE(super);
    END;
    IF i_put THEN
      put_block(iblk,ibuff,4096); i_put:=FALSE;
      IF error THEN s:=message;
        img.print(message,"ATTENTION! writting inode block %05d: %s",iblk,s);
        RETURN
      END;
      DISPOSE(ibuff);
    END;
    bio.unlock(drv); check;
    bio.close(drv);  check;
    init_state;
  END release;

BEGIN
  init_state;
END dio;

MODULE nums; (* Hady. 11-Jan-90. (c) KRONOS *)

  EXPORT QUALIFIED int_expr, message;

  VAR message: ARRAY [0..39] OF CHAR;

  CONST (* errors *)
    overflow    = -01;
    ill_number  = -02;
    not_bracket = -03;
    ill_symbol  = -04;
    div_zero    = -05;

  PROCEDURE vis_error(error: INTEGER);
  BEGIN
    IF    error=overflow    THEN message:="переполнение целого"
    ELSIF error=ill_number  THEN message:="некорректное число"
    ELSIF error=not_bracket THEN message:="нет закрывающей скобки"
    ELSIF error=ill_symbol  THEN message:="некорректный символ"
    ELSIF error=div_zero    THEN message:="деление на ноль"
    ELSE message:="неизвестная ошибка";
    END;
  END vis_error;

  PROCEDURE int_expr(VAL   s: ARRAY OF CHAR;
                         pos: INTEGER;
                     VAR res: INTEGER): INTEGER;

    CONST (* symbols *)
      plus=00;    min=01;    num=02;   div=03;   mod=04;
       mul=05;   obra=06;   cbra=07;   eol=08;

      add_ops = {plus, min};  mul_ops = {mul, div, mod};

    VAR ch: CHAR;
       val: INTEGER;
       sym: INTEGER;
     error: INTEGER;

    PROCEDURE next;
    BEGIN
      IF pos>HIGH(s) THEN ch:=0c;
      ELSE ch:=s[pos]; pos:=pos+1;
      END;
    END next;
  
    PROCEDURE get_num;
  
      CONST _oct={0}; _dec={1}; _hex={2}; _all=_oct+_dec+_hex;
            max_o=MAX(INTEGER) DIV 10b; rem_o=MAX(INTEGER) MOD 10b;
            max_d=MAX(INTEGER) DIV 10 ; rem_d=MAX(INTEGER) MOD 10 ;
            max_h=MAX(INTEGER) DIV 10h; rem_h=MAX(INTEGER) MOD 10h;
  
      VAR     active: BITSET;
         oct,dec,hex: INTEGER;
                  nn: INTEGER;
  
    BEGIN oct:=0; dec:=0; hex:=0; active:=_all;
      nn:=ORD(ch)-ORD("0");
      IF nn>10 THEN nn:=ORD(ch)-ORD("A")+10 END;
      IF (nn<0) OR (nn>31) THEN nn:=31 END;
      WHILE (_all#{}) & (nn IN {0..15}) DO
        IF _oct*active#{} THEN
          IF nn IN {0..7} THEN
            IF (oct<max_o) OR ((oct=max_o) & (nn<=rem_o)) THEN oct:=oct*8+nn;
            ELSE error:=overflow;    active:=active-_oct;
            END;
          ELSE active:=active-_oct;
          END;
        END;
        IF _dec*active#{} THEN
          IF nn IN {0..9} THEN
            IF (dec<max_d) OR ((dec=max_d) & (nn<=rem_d)) THEN dec:=dec*10+nn;
            ELSE error:=overflow;    active:=active-_dec;
            END;
          ELSE active:=active-_dec;
          END;
        END;
        IF _hex*active#{} THEN
          IF nn IN {0..15} THEN
            IF (hex<max_h) OR ((hex=max_h) & (nn<=rem_h)) THEN hex:=hex*10h+nn
            ELSE error:=overflow; active:=active-_hex;
            END;
          ELSE active:=active-_hex;
          END;
        END;
        next;
        nn:=ORD(ch)-ORD("0");
        IF nn>10 THEN nn:=ORD(ch)-ORD("A")+10 END;
        IF (nn<0) OR (nn>31) THEN nn:=31 END;
      END;
      IF    (ch="h") & (active*_hex#{}) THEN val:=hex; error:=0; next
      ELSIF (ch="b") & (active*_oct#{}) THEN val:=oct; error:=0; next
      ELSIF (active*_dec#{}) THEN val:=dec; error:=0;
      ELSIF error>=0 THEN error:=ill_number;
      END;
    END get_num;
  
    PROCEDURE get;
    BEGIN
      WHILE ch=" " DO next END;
      IF    ch="+" THEN sym:=plus; next
      ELSIF ch="-" THEN sym:=min ; next
      ELSIF ch="*" THEN sym:=mul ; next
      ELSIF ch="%" THEN sym:=mod ; next
      ELSIF ch="/" THEN sym:=div ; next
      ELSIF ch="(" THEN sym:=obra; next
      ELSIF ch=")" THEN sym:=cbra; next
      ELSIF (ch>="0") & (ch<="9") THEN sym:=num; get_num;
      ELSE sym:=eol
      END;
    END get;

    PROCEDURE check_mul(a,b: INTEGER): INTEGER;
      VAR minus: BOOLEAN;
          t,d,r: INTEGER;
    BEGIN
      minus:=(a>0)#(b>0);
      IF (a=MIN(INTEGER)) OR (b=MIN(INTEGER)) THEN
        error:=overflow; RETURN 0
      END;
      IF a<0 THEN a:=-a END;
      IF b<0 THEN b:=-b END;
      IF a>b THEN t:=a; a:=b; b:=t END;
      d:=MAX(INTEGER) DIV a;
      r:=MAX(INTEGER) MOD a; ASSERT(r<b);
      IF b>d THEN error:=overflow; RETURN 0 ELSE t:=a*b END;
      IF minus THEN t:=-t END;
      RETURN t;
    END check_mul;
  
    PROCEDURE expr(): INTEGER; FORWARD;
  
    PROCEDURE factor(): INTEGER;
      VAR result,sy: INTEGER;
    BEGIN
      IF sym=obra THEN get; result:=expr();
        IF sym=cbra THEN get ELSE error:=not_bracket END;
      ELSIF sym=num THEN result:=val; get;
      ELSE error:=ill_symbol;
      END;
      RETURN result;
    END factor;
  
    PROCEDURE term(): INTEGER;
      VAR result,sy,t: INTEGER;
    BEGIN
      result:=factor();
      WHILE ({sym}*mul_ops#{}) & (error>=0) DO
        sy:=sym; get; t:=factor();
        IF error>=0 THEN
          IF    sy=mul THEN result:=check_mul(result,t);
          ELSIF sy=div THEN
            IF t=0 THEN error:=div_zero ELSE result:=result DIV t END;
          ELSIF sy=mod THEN
            IF t=0 THEN error:=div_zero ELSE result:=result MOD t END;
          END;
        END;
      END;
      RETURN result;
    END term;
  
    PROCEDURE expr(): INTEGER;
  
      VAR        not: BOOLEAN;
         result,sy,t: INTEGER;
  
    BEGIN not:=FALSE;
      IF    sym=min THEN not:=TRUE; get;
      ELSIF sym=plus THEN get
      END;
      result:=term();
      IF (error>=0) & not THEN
        IF result=MIN(INTEGER) THEN error:=overflow
        ELSE result:=-result
        END;
      END;
      WHILE ({sym}*add_ops#{}) & (error>=0) DO
        sy:=sym; get; t:=term();
        IF error>=0 THEN
          IF   sy=plus THEN
            IF ((t<0) & (MIN(INTEGER)-t <= result)) OR
               ((t>0) & (MAX(INTEGER)-t >= result)) THEN result:=result+t
            ELSE error:=overflow;
            END;
          ELSE
            IF ((t<0) & (MAX(INTEGER)+t >= result)) OR
               ((t>0) & (MIN(INTEGER)+t <= result)) THEN result:=result-t
            ELSE error:=overflow
            END;
          END;
        END;
      END;
      RETURN result;
    END expr;
  
    VAR result: INTEGER;
  
  BEGIN
    IF pos<0 THEN pos:=0 END;
    error:=0; next; get; result:=expr();
    IF error<0 THEN vis_error(error); pos:=error END;
    res:=result; RETURN pos;
  END int_expr;

END nums;

MODULE errors; (* Hady. 05-Nov-89. (c) KRONOS *)

  EXPORT QUALIFIED release  , put_dbl  , get_dbl
                 , no_anon  , put_anon , get_anon;

  VAR no_anon: INTEGER;

  TYPE item = POINTER TO error;
      error = RECORD
                   one,
                   two: INTEGER;
                  next: item;
              END;

  VAR dbl, anon: item;

  PROCEDURE make_err(one,two: INTEGER): item;
    VAR e: item;
  BEGIN NEW(e);
    e^.one :=one;  e^.two:=two;
    e^.next:=NIL;  RETURN  e  ;
  END make_err;

  PROCEDURE put_err(VAR to: item; one,two: INTEGER);
    VAR e,l: item;
  BEGIN
    e:=make_err(one,two);
    IF to=NIL THEN to:=e;
    ELSE l:=to;
      IF (l^.one=one) & (l^.two=two) THEN DISPOSE(e); RETURN END;
      WHILE l^.next#NIL DO
        l:=l^.next;
        IF (l^.one=one) & (l^.two=two) THEN DISPOSE(e); RETURN END;
      END;
      l^.next:=e;
    END;
  END put_err;

  PROCEDURE put_dbl(f,no: INTEGER); BEGIN put_err(dbl,f,no)   END put_dbl;

  PROCEDURE put_anon(no: INTEGER);
  BEGIN put_err(anon,no,-1); INC(no_anon) END put_anon;

  PROCEDURE get_err(VAR from: item; VAR o,t: INTEGER): BOOLEAN;
    VAR e: item;
  BEGIN
    IF from=NIL THEN RETURN FALSE END;
    e:=from;    from:=from^.next;
    o:=e^.one;  t:=e^.two;
    DISPOSE(e); RETURN TRUE;
  END get_err;

  PROCEDURE get_dbl(VAR f,no: INTEGER): BOOLEAN;
  BEGIN RETURN get_err(dbl,f,no) END get_dbl;

  PROCEDURE get_anon(VAR no: INTEGER): BOOLEAN;
    VAR b: INTEGER;
  BEGIN DEC(no_anon); RETURN get_err(anon,no,b) END get_anon;

  PROCEDURE release;
    VAR e: item;
  BEGIN
    WHILE dbl #NIL DO e:=dbl;  dbl :=dbl^.next ; DISPOSE(e) END;
    WHILE anon#NIL DO e:=anon; anon:=anon^.next; DISPOSE(e) END;
    no_anon:=0;
  END release;

BEGIN anon:=NIL; dbl:=NIL; no_anon:=0;
END errors;

MODULE notes; (* 25-Dec-89. (c) KRONOS *)

  IMPORT  SYSTEM;
  IMPORT  tty;
  IMPORT  kbr;
  IMPORT  img;

  EXPORT QUALIFIED print, show, release, edit, line;

  TYPE string = POINTER TO str_body;
     str_body = RECORD
                  next,
                  prev: string;
                   vis: INTEGER;
                  body: DYNARR OF CHAR;
                END;

  VAR text: string;
       cur: string;
     first: INTEGER;
        ln: INTEGER;
      bump: ARRAY [0..79] OF CHAR;
      line: INTEGER;

  PROCEDURE make_bump(s: string);
    VAR i: INTEGER;
  BEGIN
    i:=0;
    WHILE (i<HIGH(bump)) & (i+s^.vis<=HIGH(s^.body)) DO
      bump[i]:=s^.body[i+s^.vis]; i:=i+1;
    END;
    bump[i]:=0c;
  END make_bump;

  PROCEDURE new_string(VAL s: ARRAY OF CHAR);
    VAR i,len: INTEGER;
        t, ss: string;
  BEGIN
    len:=0;
    WHILE (len<=HIGH(s)) & (s[len]#0c) DO INC(len) END;
    IF len=0 THEN RETURN END;
    NEW(ss);             IF ss=NIL THEN RETURN END;
    ss^.next:=NIL; ss^.prev:=NIL; ss^.vis:=0;
    NEW(ss^.body,len+1);
    IF ss^.body^.ADR=NIL THEN DISPOSE(ss); RETURN END;
    i:=0;
    WHILE i<=len DO ss^.body[i]:=s[i]; INC(i) END;
    IF text=NIL THEN text:=ss
    ELSE t:=text;
      WHILE t^.next#NIL DO t:=t^.next END;
      t^.next:=ss; ss^.prev:=t
    END;
    IF cur=NIL THEN cur:=text END;
  END new_string;

  PROCEDURE release;
    VAR t: string;
  BEGIN
    WHILE text#NIL DO
      t:=text; text:=text^.next;
      DISPOSE(t^.body); DISPOSE(t);
    END;
    first:=0; text:=NIL; cur:=NIL; ln:=0;
  END release;

  PROCEDURE ref_string(ln: INTEGER; s: string);
  BEGIN
    make_bump(s);          tty.set_pos(ln,0);
    tty.WriteString(bump); tty.erase_line(0);
  END ref_string;

  PROCEDURE refresh;
    VAR t: string; i: INTEGER;
  BEGIN
    i:=0; t:=text; tty.home;
    WHILE (t#NIL) & (i<first) DO t:=t^.next; i:=i+1 END;
    IF t=NIL THEN tty.erase(0);
    ELSE i:=0;
      WHILE (t#NIL) & (i<23) DO
        IF i=ln THEN cur:=t END;
        ref_string(i,t); i:=i+1; t:=t^.next;
      END;
      IF i<23 THEN tty.set_pos(i,0); tty.erase(0) END;
    END;
  END refresh;

  PROCEDURE down(no: INTEGER);
    VAR i: INTEGER; t: string;
  BEGIN
    t:=cur; i:=0;
    WHILE (i<no) & (t^.next#NIL) DO
      t:=t^.next; i:=i+1; ln:=ln+1;
      IF ln>=22 THEN ln:=21;
        first:=first+1; tty.roll_up(1); ref_string(ln,t);
      END;
      tty.set_pos(ln,0);
    END;
    cur:=t;
  END down;
  
  PROCEDURE upper(no: INTEGER);
    VAR i: INTEGER; t: string;
  BEGIN
    t:=cur; i:=0;
    WHILE (i<no) & (t^.prev#NIL) DO
      t:=t^.prev; i:=i+1; ln:=ln-1;
      IF ln<0 THEN ln:=0;
        first:=first-1; tty.roll_down(1); ref_string(ln,t);
      END;
      tty.set_pos(ln,0);
    END;
    cur:=t;
  END upper;

  PROCEDURE left;
  BEGIN
    IF cur^.vis+8 < HIGH(cur^.body) THEN
      cur^.vis:=cur^.vis+8; ref_string(ln,cur)
    END;
    tty.set_pos(ln,0);
  END left;

  PROCEDURE right;
  BEGIN
    IF cur^.vis>=8 THEN
      cur^.vis:=cur^.vis-8; ref_string(ln,cur)
    END;
    tty.set_pos(ln,0);
  END right;

  PROCEDURE help;
    VAR x: CHAR;
  BEGIN
    tty.home; tty.erase(0);
    tty.print(
      ' ЗАПИСНАЯ КНИЖКА.\n\n'
      '   Стрелки ВВЕРХ, ВНИЗ - перемещения по книжке.  \n'
      '   TAB      - подтяжка     текущей строки вправо.\n');
    tty.print(
      '   BACK_TAB - выталкивание текущей строки вправо.\n'
      '   F1       - подсказка.\n\n'
      '                ДЛЯ ПРОДОЛЖЕНИЯ НАЖМИТЕ ЛЮБУЮ КЛАВИШУ\n');
    kbr.read(x);
  END help;

  PROCEDURE edit(): BOOLEAN;
    VAR ch: CHAR;
  BEGIN
    IF cur=NIL THEN RETURN FALSE END;
    refresh; tty.set_pos(ln,0);
    LOOP kbr.read(ch);
      CASE ch OF
        |kbr.dw  : down(1)       |kbr.up    : upper(1)
        |kbr.pgdw: down(8)       |kbr.pgup  : upper(8)
        |kbr.tab : left          |kbr.bcktab: right
        |033c    : EXIT
        |kbr.f1  : help; refresh; tty.set_pos(ln,0)
      ELSE
      END
    END;
    RETURN TRUE
  END edit;

  PROCEDURE show(VAL s: ARRAY OF CHAR);
  BEGIN
    tty.set_pos(line,0);
    tty.erase_line(0);
    tty.WriteString(s);
    new_string(s);
  END show;

  PROCEDURE print(VAL fmt: ARRAY OF CHAR; SEQ arg: SYSTEM.WORD);
    VAR bump: ARRAY [0..255] OF CHAR;
  BEGIN img.print(bump,fmt,arg); show(bump) END print;

BEGIN
  text:=NIL; cur:=NIL;
  first:=0;  ln:=0;
  line:=19;
END notes;

(*$<*) (*$T-*)

PROCEDURE incl(VAR b: ARRAY OF BITSET; no: INTEGER);
  VAR i,j: INTEGER;
BEGIN
  i:=no DIV 32; j:=no MOD 32;
  IF (i<0) OR (i>HIGH(b)) THEN RETURN END;
  INCL(b[i],j)
END incl;

PROCEDURE excl(VAR b: ARRAY OF BITSET; no: INTEGER);
  VAR i,j: INTEGER;
BEGIN
  i:=no DIV 32; j:=no MOD 32;
  IF (i<0) OR (i>HIGH(b)) THEN RETURN END;
  EXCL(b[i],j)
END excl;

PROCEDURE in?(VAL b: ARRAY OF BITSET; no: INTEGER): BOOLEAN;
  VAR i,j: INTEGER;
BEGIN
  i:=no DIV 32; j:=no MOD 32;
  IF (i<0) OR (i>HIGH(b)) THEN RETURN FALSE END;
  RETURN j IN b[i]
END in?;

PROCEDURE excl?(VAR b: ARRAY OF BITSET; no: INTEGER): BOOLEAN;
  VAR i,j: INTEGER; r: BOOLEAN;
BEGIN
  i:=no DIV 32; j:=no MOD 32;
  IF (i<0) OR (i>HIGH(b)) THEN RETURN FALSE END;
  r:=NOT (j IN b[i]); EXCL(b[i],j); RETURN r
END excl?;

(*$>*)

PROCEDURE _query(): BOOLEAN;
  VAR ch: CHAR;
BEGIN
  LOOP
    kbr.read(ch);
    CASE CAP(ch) OF
    |"Y": tty.Write(ch); RETURN TRUE;
    |"N": tty.Write(ch); RETURN FALSE;
    ELSE kbr.bell(1)
    END
  END;
END _query;

PROCEDURE message(l: INTEGER; VAL f: ARRAY OF CHAR; SEQ arg: SYSTEM.WORD);
BEGIN
  tty.set_pos(14+l,30); tty.print(f,arg); tty.erase_line(0);
END message;

PROCEDURE warn(VAL s: ARRAY OF CHAR; SEQ args: SYSTEM.WORD);
BEGIN
  IF dio.error THEN
    tty.set_pos(notes.line,0); tty.erase_line(0);
    tty.print('Error %h: %s\n',INTEGER(dio.error),dio.message);
    tty.erase_line(0);
    tty.print(s,args);
    tty.print(' Это серьезно [Yes/No]? '); tty.erase_line(0);
    IF _query() THEN HALT(1) END;
    dio.reset;
  END;
END warn;

PROCEDURE ask(VAL fmt: ARRAY OF CHAR; SEQ arg: SYSTEM.WORD): BOOLEAN;
BEGIN message(0,fmt,arg); RETURN _query(); END ask;

MODULE search; (* Hady. 07-Dec-89. (c) KRONOS *)

  IMPORT  SYSTEM;

  EXPORT QUALIFIED prepare, search, _prepare;

  VAR patt: ARRAY [0..255] OF CHAR;
       tab: ARRAY CHAR OF INTEGER;
       len: INTEGER;

  PROCEDURE prepare(VAL s: ARRAY OF CHAR);
    VAR ch: CHAR;
  BEGIN
    ASSERT(HIGH(s)<=255);
    FOR ch:=MIN(CHAR) TO MAX(CHAR) DO tab[ch]:=-1 END;
    IF s="" THEN patt:=""; len:=-1; RETURN END;
    patt:=s; len:=0;
    REPEAT tab[patt[len]]:=len; len:=len+1;
    UNTIL (len>HIGH(s)) OR (patt[len]=0c);
  END prepare;

  PROCEDURE _prepare(VAL s: ARRAY OF CHAR; sz: INTEGER);
    VAR ch: CHAR;
  BEGIN
    ASSERT((HIGH(s)<=255)&(sz<=HIGH(s)));
    FOR ch:=MIN(CHAR) TO MAX(CHAR) DO tab[ch]:=-1 END;
    IF sz<=0 THEN patt:=""; len:=-1; RETURN END;
    patt:=s; len:=0;
    REPEAT tab[patt[len]]:=len; len:=len+1 UNTIL (len>=sz);
  END _prepare;

  PROCEDURE search(B: SYSTEM.ADDRESS; pos,fin: INTEGER): INTEGER;
    VAR   C: POINTER TO ARRAY [0..8196] OF CHAR;
        i,c: INTEGER;
  BEGIN C:=B;
    IF (len<0) OR (pos>fin) THEN RETURN -1 END;
    i:=len-1; c:=pos+i;
    WHILE (c<=fin) DO
      WHILE (i>=0) & (C^[c]=patt[i]) DO DEC(i); DEC(c) END;
      IF i<0 THEN RETURN pos
      ELSE c:=tab[C^[c]];
        IF c>i THEN pos:=pos+len-c ELSE pos:=pos+i-c END;
        i:=len-1; c:=pos+i;
      END;
    END;
    RETURN -1
  END search;

BEGIN len:=-1; patt:="";
END search;

VAR DESC: sle.descriptor;

MODULE editors; (*$N+ Hady. 13-Dec-89. (c) KRONOS *)

  IMPORT  _query, DESC;

  IMPORT       SYSTEM;
  IMPORT  low;
  IMPORT  tty;
  IMPORT  kbr;
  IMPORT  dio, Time;
  IMPORT  sle;
  IMPORT  img;
  IMPORT  mem;
  IMPORT  search;
  IMPORT  nums;

  EXPORT QUALIFIED edit_block, edit_file, edit_inode, edit_dir, vis_char;

  -------------------------  SERVICE  ----------------------------
                           -----------

  PROCEDURE min(x,y: INTEGER): INTEGER;
  BEGIN IF x<y THEN RETURN x ELSE RETURN y END END min;

  PROCEDURE message(VAL fmt: ARRAY OF CHAR; SEQ arg: SYSTEM.WORD);
  BEGIN tty.set_pos(23,0); tty.print(fmt,arg) END message;

  PROCEDURE ask(VAL fmt: ARRAY OF CHAR; SEQ arg: SYSTEM.WORD): BOOLEAN;
  BEGIN message(fmt,arg); RETURN _query() END ask;

  PROCEDURE warn(VAL s: ARRAY OF CHAR; SEQ args: SYSTEM.WORD);
  BEGIN
    IF dio.error THEN  tty.set_pos(23,0);
      tty.print('%s ',dio.message); tty.print(s,args);
      tty.print('Это серьезно [Yes/No]? ');
      IF _query() THEN HALT(1) END;
      dio.reset;
    END;
  END warn;

  PROCEDURE app_dec(w: SYSTEM.WORD; len: INTEGER;
                VAR s: ARRAY OF CHAR; VAR cc: INTEGER);
    VAR i,l,one: INTEGER;
  BEGIN ASSERT(len<=10); i:=len-1;
    IF INTEGER(w)<0 THEN w:=-INTEGER(w); s[0]:="-"; l:=1 ELSE l:=0 END;
    WHILE i>=l DO
      one:=INTEGER(w) MOD 10; w:=INTEGER(w) DIV 10;
      s[cc+i]:=CHAR(ORD('0')+one); DEC(i);
    END;
    INC(cc,len);
  END app_dec;

  PROCEDURE app_hex(w: SYSTEM.WORD; len: INTEGER;
                VAR s: ARRAY OF CHAR; VAR cc: INTEGER);
    VAR i,one: INTEGER;
  BEGIN ASSERT(len<=8);
    i:=len-1;
    WHILE i>=0 DO
      one:=INTEGER(BITSET(w) * BITSET(0Fh));
      w  :=INTEGER(BITSET(w) >> 4);
      IF one<10 THEN s[cc+i]:=CHAR(ORD('0')+one   );
      ELSE           s[cc+i]:=CHAR(ORD('A')+one-10);
      END;
      DEC(i);
    END;
    INC(cc,len);
  END app_hex;

  PROCEDURE app(VAR dest: ARRAY OF CHAR;
                VAL sour: ARRAY OF CHAR;
                VAR from: INTEGER);
    VAR i: INTEGER;
  BEGIN i:=0;
    WHILE (from<HIGH(dest)) & (i<HIGH(sour)) & (sour[i]#0c) DO
      dest[from]:=sour[i]; INC(from); INC(i);
    END;
    dest[from]:=0c;
  END app;
  

  CONST digit = ARRAY OF CHAR {"0","1","2","3","4","5","6","7",
                               "8","9","A","B","C","D","E","F"};
  
  CONST vis_char = ARRAY OF CHAR {
" ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ",
" ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ",
" ", "!", '"', "#", "$", "%", "&", "'", "(", ")", "*", "+", ",", "-", ".", "/",
"0", "1", "2", "3", "4", "5", "6", "7", "8", "9", ":", ";", "<", "=", ">", "?",
"@", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O",
"P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", "[", "\", "]", "^", "_",
"`", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o",
"p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z", "{", "|", "}", "~", "",
" ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ",
" ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ",
" ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ",
" ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ",
"ю", "а", "б", "ц", "д", "е", "ф", "г", "х", "и", "й", "к", "л", "м", "н", "о",
"п", "я", "р", "с", "т", "у", "ж", "в", "ь", "ы", "з", "ш", "э", "щ", "ч", "ъ",
"Ю", "А", "Б", "Ц", "Д", "Е", "Ф", "Г", "Х", "И", "Й", "К", "Л", "М", "Н", "О",
"П", "Я", "Р", "С", "Т", "У", "Ж", "В", "Ь", "Ы", "З", "Ш", "Э", "Щ", "Ч", "Ъ"};

  --------------------------  EDITOR  ----------------------------
                            ----------

  PROCEDURE edit_inode(no: INTEGER);        FORWARD;
  PROCEDURE edit_dir  (no: INTEGER);        FORWARD;
  PROCEDURE edit_file (no,start: INTEGER);  FORWARD;
  PROCEDURE edit_block(no,start: INTEGER);  FORWARD;

  TYPE buff_int = POINTER TO ARRAY [0..2047] OF INTEGER;
       buff_chr = POINTER TO ARRAY [0..8195] OF CHAR;

  PROCEDURE _edit_file(no,max_byte,start: INTEGER;
                          VAL type: ARRAY OF CHAR;
                          VAL file: ARRAY OF INTEGER);

    CONST read_warn = " при чтении блока %d";
         write_warn = " при записи блока %d";

    VAR bump: ARRAY [0..79] OF CHAR;

    VAR C: buff_chr;
        I: buff_int;
  
    VAR c_pos, w_pos, f_pos: INTEGER;
        block: INTEGER;
      changed: BITSET;
        words: INTEGER;
        bytes: INTEGER;
     max_word: INTEGER;
        chars: BOOLEAN;

    PROCEDURE get_block(no: INTEGER; a: SYSTEM.ADDRESS; sz,ofst: INTEGER);
    BEGIN
      ASSERT( (no>=0) & (no<=HIGH(file)) );
      dio.get_block(file[no],SYSTEM.ADDRESS(INTEGER(a)+ofst),sz);
      warn(read_warn,file[no]);
    END get_block;
  
    PROCEDURE put_block(no: INTEGER; a: SYSTEM.ADDRESS; sz,ofst: INTEGER);
    BEGIN
      ASSERT( (no>=0) & (no<=HIGH(file)) );
      message(" Блок %d был модифицирован. Записать на диск? ",no);
      IF _query() THEN
        dio.put_block(file[no],SYSTEM.ADDRESS(INTEGER(a)+ofst),sz);
        warn(write_warn,file[no]);
      END;
    END put_block;

    PROCEDURE flash_block(no: BITSET);
      VAR i: INTEGER;
    BEGIN
      ASSERT(no-{0,1}={});
      IF no*changed={} THEN RETURN END;
      FOR i:=0 TO 1 DO
        IF ({i}*no#{})&({i}*changed#{}) THEN
          put_block(block+i,I,4096,1024*i);
          changed:=changed-{i};
        END;
      END;
    END flash_block;

    PROCEDURE swap_up;
    BEGIN
      flash_block({0});
      low.move(I,SYSTEM.ADDRESS(INTEGER(I)+1024),1024);
      changed:=(changed>>1)*{0,1};
    END swap_up;

    PROCEDURE swap_dw;
    BEGIN
      flash_block({1});
      low.move(SYSTEM.ADDRESS(INTEGER(I)+1024),I,1024);
      changed:=(changed<<1)*{0,1};
    END swap_dw;

    PROCEDURE make_first(no: INTEGER);
      VAR i: INTEGER;
    BEGIN ASSERT(no>=0);
      IF no=block THEN RETURN END;
      i:=max_byte-no*4096;
      IF    no+1=block THEN swap_dw;
        get_block(no,I,min(i,4096),0);
      ELSIF no-1=block THEN swap_up;
        IF no<HIGH(file) THEN
          get_block(no+1,I,min(i-4096,4096),1024)
        END;
      ELSE flash_block({0,1});
        get_block(no,I,min(i,4096),0); i:=i-min(i,4096);
        IF (no<HIGH(file)) & (i>0) THEN
          get_block(no+1,I,min(i,4096),1024);
        END;
      END;
      block:=no;
    END make_first;
  
    PROCEDURE make_buffs;
    BEGIN
      max_word:=max_byte DIV 4;
      IF max_byte<8196 THEN bytes:=max_byte ELSE bytes:=8196 END;
      words:=bytes DIV 4;
      mem.ALLOCATE(I,(bytes+3) DIV 4); ASSERT(I#NIL); C:=SYSTEM.ADDRESS(I);
      make_first(0);
    END make_buffs;
  
  (*
  0         1         2         3         4         5         6         7
  0123456789012345678901234567890123456789012345678901234567890123456789012345678
  <<< EDIT       000000               000000.000000                WORD MODE >>>
  *)
    CONST INFO = "<<< EDIT       000000               000000"
                       ".000000                WORD MODE >>>";
  
    VAR info : ARRAY [0..79] OF CHAR;
  
    PROCEDURE init_info;  VAR i: INTEGER;
    BEGIN
      info:=INFO;
      i:=15; app_dec(no,6,info,i);
      i:=09; app(info,type,i); info[i]:=" ";
    END init_info;
  
    PROCEDURE clear_info; BEGIN tty.set_pos(22,0); tty.erase(0) END clear_info;

    PROCEDURE ref_info;
    BEGIN
      tty.set_pos(22,0); tty.erase(0);
      tty.set_reverse(1);
      tty.WriteString(info);
      tty.set_reverse(0);
    END ref_info;
  
    PROCEDURE make(VAR s: ARRAY OF CHAR; VAR from: INTEGER);   (*$N-*)
      VAR cc, i,j,k: INTEGER; ss: ARRAY [0..11] OF CHAR;
    BEGIN
      i:=from; cc:=0;
      FOR i:=from DIV 4 TO (from DIV 4) + 5 DO
        IF i+block*1024<max_word THEN
          app_hex(I^[i],8,s,cc);
        ELSIF i+block*1024=max_word THEN
          ss:="        ";  j:=6;  k:=i*4;
          WHILE k+block*4096<max_byte DO
            app_hex(C^[k],2,ss,j); j:=j-4; k:=k+1;
          END;  app(s,ss,cc);
        ELSE
          FOR j:=0 TO 7 DO s[cc]:=" "; INC(cc) END;
        END;
        s[cc]:=" "; INC(cc);
      END;
      s[cc]:="|"; INC(cc);
      FOR i:=from TO from+23 DO
        IF i+4096*block<max_byte THEN s[cc]:=vis_char[INTEGER(C^[i])];
        ELSE s[cc]:=" ";
        END;
        INC(cc);
      END;
      s[cc]:=0c; from:=from+24;
    END make;                                                     (*$N+*)

    PROCEDURE refresh(from, l_cc: INTEGER);
      VAR i,ln: INTEGER;
    BEGIN
      i:=f_pos+from*24;
      ln:=from;
      WHILE ln<l_cc DO make(bump,i);
        tty.set_pos(ln,0); tty.erase_line(0); tty.WriteString(bump);
        INC(ln);
      END;
    END refresh;
  
    PROCEDURE ref_crs;
      VAR ln,cl: INTEGER;
    BEGIN cl:=c_pos-f_pos;
      ln:=cl DIV 24;  cl:=cl MOD 24;
      IF chars THEN cl:=cl+55;
      ELSE cl:=((cl DIV 4)+1)*9-1-2*((cl MOD 4)+1);
      END;
      tty.set_pos(ln,cl);
    END ref_crs;

    PROCEDURE mark(on: BOOLEAN);
      VAR w0,w1: BITSET; i,j: INTEGER;
    BEGIN
      IF on THEN tty.set_reverse(1)
      ELSE       tty.set_reverse(0)
      END;
      IF chars THEN
        chars:=FALSE; ref_crs;
         w0:=BITSET(C^[c_pos]);
         w1:=(w0 >> 4) * {0..3}; w0:=w0 * {0..3};
         tty.Write(digit[INTEGER(w1)]);
         tty.Write(digit[INTEGER(w0)]);
        chars:=TRUE;
      ELSE
        chars:=TRUE;  ref_crs;
         tty.Write(vis_char[INTEGER(C^[c_pos])]);
        chars:=FALSE;
      END;
      IF on THEN
        j:=c_pos+block*4096; i:=0;
        app_dec((j DIV 4),6,bump,i); bump[i]:="."; INC(i);
        app_dec(j,6,bump,i);         bump[i]:=0c;
        tty.set_pos(22,36); tty.WriteString(bump);
        tty.set_reverse(0);
      END;
      ref_crs;
    END mark;
  
    PROCEDURE dw_pos(add: INTEGER; break: BOOLEAN);

      VAR     np: INTEGER;
         nln,ncl: INTEGER; -- first byte in future string
             nlp: INTEGER; -- first invisible byte upper of screen
           rolls: INTEGER;
              cc: INTEGER;
             abl: INTEGER;

    BEGIN ASSERT(add>0);
      np:=c_pos+add; rolls:=0;
      ncl:=(np-f_pos) MOD 24; nln:=np-ncl;
      nlp:=nln+24;
      IF np>=max_byte-block*4096 THEN
        IF break THEN cc:=max_byte-block*4096-1;
          nln:=cc-((cc-f_pos) MOD 24); nlp:=nln+24;
          IF (cc-f_pos) MOD 24 < ncl THEN nln:=nln-24 END;
          np:=nln+ncl;
        ELSE RETURN
        END;
      END;
      mark(FALSE);
      cc:=f_pos+22*24;
      IF nlp>cc THEN rolls:=(nlp-cc) DIV 24;
        ASSERT(rolls>0);
        IF (nlp>=bytes) & (nln+block*4096<=max_byte-(max_byte MOD 24)) THEN
          ASSERT((nlp>np)&(np+48>=nlp));
          abl:=(nln+block*4096) DIV 4096;
          IF (abl>1) & (abl<=HIGH(file)) &
                     (abl*4096<max_byte) THEN
            nlp:=nlp-(abl-1+block)*4096;
            nln:=nln-(abl-1+block)*4096;
            make_first(abl-1);
          END;
          np:=nln+ncl; f_pos:=nlp-(22+rolls)*24;
        END;
        IF rolls<22 THEN
          clear_info;
          FOR cc:=0 TO rolls-1 DO
            tty.roll_up(1); f_pos:=f_pos+24; refresh(21,22);
          END;
          ref_info;
        ELSE f_pos:=f_pos+rolls*24;
          refresh(0,22);
        END;
      END;
      c_pos:=np; mark(TRUE);
    END dw_pos;

    PROCEDURE up_pos(add: INTEGER; break: BOOLEAN);
      VAR  np: INTEGER;
        rolls: INTEGER;
      ncl,nln: INTEGER;
          abl: INTEGER;
           cc: INTEGER;
    BEGIN ASSERT(add>0);
      np:=c_pos-add; rolls:=0;
      IF np<f_pos THEN
        nln:=f_pos;
        WHILE nln>np DO nln:=nln-24; rolls:=rolls+1 END;
        ncl:=np-nln;
        IF nln+block*4096<0 THEN
          IF break THEN
            REPEAT nln:=nln+24; rolls:=rolls-1 UNTIL nln+block*4096>=0;
          ELSE RETURN
          END;
        END;
        mark(FALSE);
        IF nln<0 THEN -- подчитать блок
          abl:=(4095-nln) DIV 4096;               ASSERT(abl<=block);
          make_first(block-abl);
          nln:=nln+4096*abl; f_pos:=f_pos+4096*abl;
        END;
        c_pos:=nln+ncl;
        IF rolls=0 THEN mark(TRUE); RETURN END;
        IF rolls<22 THEN
          FOR cc:=0 TO rolls-1 DO
            tty.roll_down(1); f_pos:=f_pos-24; refresh(0,1);
          END;
          ref_info;
        ELSE f_pos:=f_pos-rolls*24; refresh(0,22);
        END;
        mark(TRUE);
      ELSE mark(FALSE); c_pos:=np; mark(TRUE);
      END;
    END up_pos;
  
    PROCEDURE right;
    BEGIN
      IF chars              THEN dw_pos(1,FALSE)
      ELSIF (c_pos MOD 4)>0 THEN up_pos(1,FALSE)
      ELSE dw_pos(7,FALSE);
      END;
    END right;
  
    PROCEDURE left;
    BEGIN
      IF chars              THEN up_pos(1,FALSE)
      ELSIF (c_pos MOD 4)<3 THEN dw_pos(1,FALSE)
      ELSE up_pos(7,FALSE)
      END;
    END left;
  
    PROCEDURE abs_pos(new: INTEGER);
    BEGIN
      IF (new<0) OR (new>max_byte) THEN ref_crs; RETURN END;
      IF    new-block*4096<c_pos THEN up_pos(c_pos-new+block*4096,FALSE)
      ELSIF new-block*4096>c_pos THEN dw_pos(new-block*4096-c_pos,FALSE)
      END;
    END abs_pos;
  
    PROCEDURE find_patt(from: INTEGER);
      VAR bump: SYSTEM.ADDRESS;
        cur,to: INTEGER;
    BEGIN
      bump:=SYSTEM.ADDRESS(I); to:=min(max_byte-4096*block,bytes)-1;
      to:=search.search(bump,from,to);
      IF to>=0 THEN abs_pos(to+block*4096); RETURN END;
      IF block+1<HIGH(file) THEN
        mem.ALLOCATE(bump,1024+20);
        IF bump=NIL THEN
          message(" Переполнена динамическая память"); RETURN
        END;
        cur:=block+2; from:=0;
        WHILE (to<0) & (cur<=HIGH(file)) DO
          get_block(cur,bump,min(4096,max_byte-4096*cur),0);
          IF cur<HIGH(file) THEN
            get_block(cur+1,bump,min(80,max_byte-4096*(cur+1)),1024);
          END;
          to:=min(4096+80,max_byte-cur*4096)-1;
          to:=search.search(bump,from,to);
          INC(cur);
        END;
        IF to>=0 THEN abs_pos(to+(cur-1)*4096)
        ELSE message(" Образец не найден"); ref_crs;
        END;
        mem.DEALLOCATE(bump,1024+20);
      ELSE message(" Образец не найден"); ref_crs;
      END;
    END find_patt;
  
    PROCEDURE command;
      VAR i: INTEGER;

      PROCEDURE pool_str(VAR s: ARRAY OF CHAR);
      BEGIN
        i:=1; WHILE (i<=HIGH(s))&(s[i]#0c) DO s[i-1]:=s[i]; i:=i+1 END;
        s[i-1]:=0c;
      END pool_str;

      PROCEDURE hex_str(VAR s: ARRAY OF CHAR): INTEGER;
        VAR i,j,nn,d: INTEGER;
      BEGIN
        i:=1; j:=0;
        LOOP
          IF (i+1>HIGH(s)) THEN EXIT END;
          d:=ORD(s[i])-ORD("0");
          IF (d<0) OR (d>9) THEN
            d:=ORD(s[i])-ORD("A")+10;
            IF (d<10) OR (d>15) THEN EXIT END;
          END;
          nn:=d; i:=i+1;
          d:=ORD(s[i])-ORD("0");
          IF (d<0) OR (d>9) THEN
            d:=ORD(s[i])-ORD("A")+10;
            IF (d<10) OR (d>15) THEN EXIT END;
          END;
          nn:=nn*16+d; i:=i+1;
          s[j]:=CHAR(nn); j:=j+1;
        END;
        RETURN j;
      END hex_str;

    BEGIN
      sle.edit_str(">>> ",bump,23,0,78,DESC,033c);
      tty.Write(015c); tty.erase_line(0);
      IF (DESC^.last=033c) OR (bump="") THEN RETURN END;
      IF (bump[0]="f") & (bump[1]#0c) THEN
        pool_str(bump); search.prepare(bump); find_patt(c_pos);
      ELSIF (bump[0]="x") & (bump[1]#0c) THEN
        search._prepare(bump,hex_str(bump));
        find_patt(c_pos);
      ELSIF (bump[0]="?") & (bump[1]#0c) THEN
        IF nums.int_expr(bump,1,i)>=0 THEN
          tty.print('%..1s=%d',bump,i)
        END;
      ELSE
        IF nums.int_expr(bump,0,i)>=0 THEN abs_pos(i) END;
      END;
    END command;

    PROCEDURE gold;
      VAR ch: CHAR; nn: INTEGER;
    BEGIN
      kbr.read(ch);
      IF    ch=kbr.pgup THEN abs_pos(0)
      ELSIF ch=kbr.pgdw THEN abs_pos(max_byte-1)
      ELSIF ch=kbr.f10 THEN command;
      ELSIF CAP(ch)="N" THEN find_patt(c_pos+1);
      ELSIF CAP(ch)="E" THEN
        nn:=c_pos DIV 4;
        IF nn<words THEN nn:=I^[nn];
          IF (nn>=0) & (nn<dio.no_b) THEN edit_block(nn,0);
            refresh(0,22); ref_info; mark(TRUE);
         END;
        END;
      END;
    END gold;

    PROCEDURE help;
      VAR x: CHAR;
    BEGIN
      tty.home; tty.erase(0);
      tty.print(
        'File/Block Editor Help\n'
        '    UP,DOWN,LEFT,RIGHT -- перемещения курсора\n'
        '    PAGE_UP, PAGE_DOWN -- перемещения курсора\n'
        '    TAB                -- переход на символьное поле(справа)\n'
        '    BACK_TAB           -- переход на поле слов(слева)\n');
      tty.print(
        '    "/" -- переход на позицию с номером в выбранном слове\n'
        '    F10 PAGE_UP       -- переход на начало файла/блока\n'
        '    F10 PAGE_DOWN     -- переход на конец  файла/блока\n'
        '    F10 "N"  -- найти следующее вхождение контекста\n');
      tty.print(
        '    F10 "E"  -- редактировать блок с номером\n'
        '                 в текущем слове, если такой существует\n'
        '    F10 F10 -- вход в командный режим\n');
      tty.print(
        'Возможные команды в командном режиме:\n'
        '    fcontext -- найти строку "context" в файле/блоке\n'
        '    xcontext -- то же, но строка рассматривается как\n'
        '       последовательность байтов в шестнадцатеричном виде\n');
      tty.print(
        '    ?expr -- вычислить выражение "expr"\n'
        '    expr  -- перейти на байт с номером,\n'
        '             равным результату "expr"\n');
      tty.print(
        'Модификация:\n'
        '    На символьном поле: любые клавиши кроме функциональных\n'
        '    На поле слов      : шестнадцатеричные цифры\n'
        '    ESC -- выход без записи; EXIT -- выход с записью\n'
        '                        ДЛЯ ПРОДОЛЖЕНИЯ НАЖМИТЕ ЛЮБУЮ КЛАВИШУ');
      kbr.read(x)
    END help;

    CONST page = 16;

    VAR ch: CHAR;
        nn: INTEGER;
       crs: INTEGER;

  BEGIN
    IF max_byte<=0 THEN
      message(" Это пустой файл. Нажмите любую клавишу");
      kbr.read(ch);
      RETURN
    END;
    crs:=tty.state^.cursor;
    tty.set_cursor(1);
    chars:=FALSE;
    block:=-2; changed:={}; make_buffs;
    f_pos:=0; c_pos:=0; init_info;
    refresh(0,22); ref_info; mark(TRUE);
    abs_pos(start);
    LOOP
      kbr.read(ch);
      CASE ch OF
        |  033c  : changed:={}; EXIT;
        |kbr.exit: EXIT;
        |kbr.dw  : dw_pos(24,TRUE);
        |kbr.up  : up_pos(24,TRUE);
        |kbr.pgup: up_pos(24*page,TRUE);
        |kbr.pgdw: dw_pos(24*page,TRUE);
        |kbr.right: right;
        |kbr.left:  left;
        |kbr.bcktab: mark(FALSE); chars:=FALSE; mark(TRUE);
        |kbr.tab: mark(FALSE); chars:=TRUE;  mark(TRUE);
        |kbr.f10: gold;
        |kbr.f1: help; refresh(0,22); ref_info; mark(TRUE);
        |   "/"  : nn:=c_pos DIV 4;
          IF (nn<max_word) & (I^[nn]>=0) & (I^[nn]<=max_word) THEN
            abs_pos(I^[nn]*4)
          END;
      ELSE
        IF chars THEN
          IF (ch=" ") OR (vis_char[INTEGER(ch)]#" ") THEN
            C^[c_pos]:=ch;
            INCL(changed,c_pos DIV 4096);
            tty.Write(ch); dw_pos(1,FALSE);
          END;
        ELSIF (ORD(ch)-ORD("0") IN {0..9}) OR
              (ORD(CAP(ch))-ORD("A") IN {0..5}) THEN
          IF ORD(ch)-ORD("0") IN {0..9} THEN nn:=ORD(ch)-ORD("0");
          ELSE ch:=CAP(ch); nn:=ORD(ch)-ORD("A")+10;
          END;
          nn:=nn*10h; tty.Write(ch); tty.Write("0"); tty.left(1);
          kbr.read(ch);
          IF (ORD(ch)-ORD("0") IN {0..9}) OR
              (ORD(CAP(ch))-ORD("A") IN {0..5}) THEN
            IF ORD(ch)-ORD("0") IN {0..9} THEN nn:=(ORD(ch)-ORD("0"))+nn;
            ELSE ch:=CAP(ch); nn:=(ORD(ch)-ORD("A")+10)+nn;
            END;
            C^[c_pos]:=CHAR(nn);
            INCL(changed,c_pos DIV 4096);
            tty.Write(ch);
          ELSE chars:=TRUE; mark(FALSE); chars:=FALSE; mark(TRUE)
          END;
          right
        END
      END
    END;
    flash_block({0,1});
    tty.set_cursor(crs);
    mem.DEALLOCATE(I,(bytes+3) DIV 4)
  END _edit_file;

  PROCEDURE edit_file(no,start: INTEGER);
    VAR inode: dio.i_node;
            B: POINTER TO ARRAY [0..1023] OF INTEGER;
          pos: INTEGER;
         file: DYNARR OF INTEGER;

    PROCEDURE put(i: INTEGER);
    BEGIN
      IF (i>=0) & (i<dio.no_b) THEN
        IF pos>HIGH(file) THEN RESIZE(file,HIGH(file)+9) END;
        file[pos]:=i; INC(pos);
      END;
    END put;

    VAR i,b: INTEGER;

  BEGIN
    dio.get_inode(no,inode); warn(" при чтении инода %d",no);
    NEW(file,8); ASSERT(file^.ADR#NIL); pos:=0;
    IF inode.Mode*{dio.lng}#{} THEN
      NEW(B); ASSERT(B#NIL);
      FOR b:=0 TO HIGH(inode.Ref) DO
        IF (inode.Ref[b]>=0) & (inode.Ref[b]<dio.no_b) THEN
          dio.get_block(inode.Ref[b],B,4096);
          IF dio.error THEN warn(" при чтении блока %d",inode.Ref[b]);
          ELSE
            FOR i:=0 TO 1023 DO put(B^[i]) END;
          END;
        END;
      END;
      DISPOSE(B);
    ELSE
      FOR i:=0 TO HIGH(inode.Ref) DO put(inode.Ref[i]) END;
    END;
    RESIZE(file,pos);
    IF (HIGH(file)>=0) THEN _edit_file(no,inode.Eof,start,"FILE", file) END;
    DISPOSE(file);
  END edit_file;

  PROCEDURE edit_block(no,start: INTEGER);
    VAR t: ARRAY [0..0] OF INTEGER;
  BEGIN
    t[0]:=no;
    _edit_file(no,4096,start,"BLOCK",t);
  END edit_block;

  VAR term: ARRAY [0..7] OF BITSET;

  PROCEDURE _edit_dir(D: dio.Dir; NO: INTEGER): BOOLEAN;
  
  (*
  0         1         2         3         4         5         6         7
  0123456789012345678901234567890123456789012345678901234567890123456789012345678
                                    i000000  kind {       ,      ,      ,       }
  *)
    VAR bump: ARRAY [0..79] OF CHAR;
          ln: INTEGER;
         cur: INTEGER; -- current line & slot
         scr: INTEGER; -- max line on screen;
  
    PROCEDURE vis_name(VAR vs: ARRAY OF CHAR; VAR cc: INTEGER;
                       VAL nm: ARRAY OF CHAR);
      VAR i: INTEGER;
    BEGIN i:=0;
      WHILE (i<=HIGH(nm)) DO vs[cc]:=vis_char[INTEGER(nm[i])];
        cc:=cc+1; i:=i+1;
      END;
    END vis_name;

    PROCEDURE vis_bit(no: INTEGER; on: BOOLEAN;
                 VAR bump: ARRAY OF CHAR; VAR cc: INTEGER);
      VAR bit: ARRAY [0..7] OF CHAR; i: INTEGER;
    BEGIN
      IF on THEN
        CASE no OF
          |0: bit:=" del"         |1: bit:="file"
          |2: bit:=" dir"         |3: bit:="hidd"
          |4: bit:=" esc"         |5: bit:=" sys"
        END
      ELSE    bit:="    "
      END;
      FOR i:=0 TO 3 DO bump[cc]:=bit[i]; INC(cc) END
    END vis_bit;

  (*
  0         1         2         3         4         5         6         7
  0123456789012345678901234567890123456789012345678901234567890123456789012345678
     EDIT DIRECTORY 000153          SLOT 000153             NAME[00] = 000c
                                                                 INODE
                                                               KIND(file)
  *)
    CONST INFO = "   EDIT DIRECTORY 000000          SLOT "
                 "000000                                 ";

    VAR info : ARRAY [0..79] OF CHAR;

    PROCEDURE init_info;   VAR cc: INTEGER;
    BEGIN info:=INFO; cc:=18; app_dec(NO,6,info,cc) END init_info;

    PROCEDURE ref_info;
    BEGIN
      tty.set_pos(scr,0); tty.erase(0);
      tty.set_reverse(1); tty.WriteString(info);
      tty.set_reverse(0);
    END ref_info;

    PROCEDURE clear_info; BEGIN tty.set_pos(scr,0); tty.erase(0) END clear_info;

    PROCEDURE refresh(cur,ln,scr: INTEGER);
                  -- 01234567890123 0123 0123 0123 0123 01234
      CONST dummy = " i000000 {    ,    ,    ,    ,    ,     }";
      VAR cc,i: INTEGER;
    BEGIN
      WHILE ln<scr DO
        tty.set_pos(ln,0); tty.erase_line(0);
        WITH D^.buff[cur] DO
          cc:=0;  vis_name(bump,cc,name); bump[cc]:=0c;
          tty.set_underline(1);
          tty.WriteString(bump);
          tty.set_underline(0);
          bump:=dummy;
          cc:=2; app_dec(inod,6,bump,cc);
          cc:=10;
          FOR i:=0 TO 5 DO
            vis_bit(i,i IN kind,bump,cc); INC(cc);
          END;
          tty.WriteString(bump);
        END;
        INC(cur); INC(ln);
      END;
    END refresh;

    VAR ch: CHAR;
       pos: INTEGER;
     write: BOOLEAN;

    PROCEDURE term?(c: CHAR): BOOLEAN;
    BEGIN RETURN ORD(c) MOD 32 IN term[ORD(c) DIV 32] END term?;

    PROCEDURE inp_dec(VAR w,pos: INTEGER;
                      VAR    ch: CHAR;
                      ln,cl,len: INTEGER): BOOLEAN;
    
      VAR bump: ARRAY [0..11] OF CHAR;
             i: INTEGER;
         write: BOOLEAN;
    BEGIN
      i:=0; app_dec(w,len,bump,i); bump[i]:=0c;
      len:=len-1; write:=FALSE;
      LOOP
        IF pos>len THEN ch:=kbr.right END;
        IF (pos<0) OR (pos>len) THEN EXIT END;
        tty.set_pos(ln,cl+pos);
        kbr.read(ch);
        IF ch=" " THEN ch:="0" END;
        IF    ch=kbr.left  THEN pos:=pos-1;
        ELSIF ch=kbr.right THEN pos:=pos+1;
        ELSIF term?(ch) THEN EXIT
        ELSIF (ORD(ch)-ORD("0") IN {0..9}) THEN tty.Write(ch);
          bump[pos]:=ch; pos:=pos+1; write:=TRUE;
        END;
      END;
      IF ch#033c THEN ASSERT(nums.int_expr(bump,0,w)>=0) END;
      RETURN write;
    END inp_dec;

    PROCEDURE edit_name;
      VAR bump: ARRAY [0..31] OF CHAR;
    BEGIN
      bump:=D^.buff[cur].name;
      tty.set_pos(scr,58);
      tty.set_reverse(1); tty.print('NAME[%$2d] = %$3bc',pos,bump[pos]);
      tty.set_reverse(0);
      LOOP
        IF pos>31 THEN ch:=kbr.right END;
        IF (pos<0) OR (pos>31) THEN EXIT END;
        tty.set_underline(0);
        tty.set_reverse(1);
         tty.set_pos(scr,63); tty.print('%$2d',pos);
         tty.set_pos(scr,69); tty.print('%$3b',bump[pos]);
        tty.set_reverse(0);
        tty.set_underline(1);
        tty.set_pos(ln,pos);
        kbr.read(ch);
        IF ch=" " THEN ch:=0c END;
        IF    ch=kbr.left  THEN pos:=pos-1;
        ELSIF ch=kbr.right THEN pos:=pos+1;
        ELSIF ch=kbr.cr    THEN pos:=0;
        ELSIF term?(ch)    THEN EXIT
        ELSE tty.Write(vis_char[INTEGER(ch)]); write:=TRUE;
          bump[pos]:=ch; pos:=pos+1
        END;
      END;
      tty.set_underline(0);
      IF ch#033c THEN
        D^.buff[cur].name:=bump;
      END;
    END edit_name;

    PROCEDURE edit_no;
      CONST mark = "     INODE      ";
    BEGIN
      tty.set_pos(scr,58);
      tty.set_reverse(1); tty.WriteString(mark); tty.set_reverse(0);
      IF inp_dec(D^.buff[cur].inod,pos,ch,ln,34,6) THEN write:=TRUE END;
    END edit_no;

    PROCEDURE edit_kind;
      CONST mark = "  KIND {    }  ";
      VAR kind: BITSET; bump: ARRAY [0..7] OF CHAR; i: INTEGER;
    BEGIN kind:=D^.buff[cur].kind;
      tty.set_pos(scr,58);
      tty.set_reverse(1); tty.WriteString(mark); tty.set_reverse(0);
      LOOP
        IF (pos<0) OR (pos>5) THEN EXIT END;
        i:=0; vis_bit(pos,TRUE,bump,i); -- bump[i]:=0c;
        tty.set_pos(scr,66);
        tty.set_reverse(1); tty.write(bump,0,i); tty.set_reverse(0);
        tty.set_pos(ln,42+pos*5);
        kbr.read(ch);
        IF    ch=kbr.left  THEN pos:=pos-1;
        ELSIF ch=kbr.right THEN pos:=pos+1;
        ELSIF (ch=" ") OR (ch=kbr.cr) THEN
          kind:=kind/{pos};  write:=TRUE;
          i:=0; vis_bit(pos,{pos}*kind#{},bump,i); bump[i]:=0c;
          tty.WriteString(bump)
        ELSIF term?(ch)  THEN EXIT
        END;
      END;
      IF ch#033c THEN D^.buff[cur].kind:=kind END;
    END edit_kind;

    CONST max_pos = ARRAY OF INTEGER { 31, 5, 5 };

    VAR item: INTEGER; -- {0..2} !!!

    PROCEDURE edit_item;
    BEGIN
      CASE item OF
        |0: edit_name;
        |1: edit_no;
        |2: edit_kind;
      END;
    END edit_item;

    PROCEDURE set_cur(new: INTEGER);
      VAR delta,rolls,i: INTEGER;
    BEGIN
      IF (new<0) OR (new>HIGH(D^.buff)) THEN RETURN END;
      delta:=new-cur; rolls:=0;
      IF    ln+delta<0    THEN
        rolls:=-delta-ln;
        IF rolls<scr THEN
          FOR i:=1 TO rolls DO tty.roll_down(1) END;
          refresh(new,0,rolls);
          ln:=ln+rolls;
        ELSE
          refresh(new,0,scr);
          ln:=ln+rolls; rolls:=0;
        END;
      ELSIF ln+delta>=scr THEN
        rolls:=1-scr+ln+delta;
        IF rolls<scr THEN
          clear_info;
          FOR i:=1 TO rolls DO tty.roll_up(1) END;
          refresh(cur+ scr-ln ,scr-rolls,scr);
          ln:=ln-rolls;
        ELSE
          refresh(new-scr+1,0,scr);
          ln:=ln-rolls; rolls:=0;
        END;
      END;
      cur:=new; ln:=ln+delta;
      i:=39; app_dec(cur,6,info,i);
      IF rolls=0 THEN
        i:=0;  app_dec(cur,6,bump,i); bump[i]:=0c;
        tty.set_pos(scr,39);
        tty.set_reverse(1); tty.WriteString(bump); tty.set_reverse(0);
      ELSE ref_info;
      END;
    END set_cur;

    PROCEDURE up(no: INTEGER);
      VAR nc,i: INTEGER;
    BEGIN
      nc:=cur-no;
      IF nc<0 THEN nc:=0 END;
      set_cur(nc);
    END up;

    PROCEDURE dw(no: INTEGER);
      VAR nc,i: INTEGER;
    BEGIN
      nc:=cur+no;
      i:=HIGH(D^.buff);
      IF nc>i THEN nc:=i END;
      IF nc=cur THEN RETURN END;
      set_cur(nc);
    END dw;
  
    PROCEDURE edit_next(ind: INTEGER);
    BEGIN
      WITH D^.buff[cur] DO
        IF    ind=0 THEN edit_inode(inod)
        ELSIF ind=1 THEN edit_file (inod,0)
        ELSIF (ind=2) &
              (dio.d_dir * kind={}) & (name#"..") THEN edit_dir(inod);
        END;
      END;
      refresh(cur-ln,0,scr); ref_info;
    END edit_next;

    PROCEDURE command;
      VAR bump: ARRAY [0..79] OF CHAR; i: INTEGER;

      PROCEDURE pool_str(VAR s: ARRAY OF CHAR);
      BEGIN
        i:=1; WHILE (i<=HIGH(s))&(s[i]#0c) DO s[i-1]:=s[i]; i:=i+1 END;
        s[i-1]:=0c;
      END pool_str;

      PROCEDURE find_inode(no: INTEGER): INTEGER;
        VAR i: INTEGER;
      BEGIN i:=cur;
        WHILE (i<=HIGH(D^.buff)) & (D^.buff[i].inod#no) DO i:=i+1 END;
        RETURN i
      END find_inode;

      PROCEDURE find_name(VAL nm: ARRAY OF CHAR): INTEGER;
        VAR i,pos: INTEGER; done: BOOLEAN;
      BEGIN i:=cur;
        WITH D^ DO
          WHILE (i<=HIGH(buff)) DO
            pos:=0;
            img.scan(buff[i].name,pos,nm,done);
            IF done THEN RETURN i END;
            i:=i+1
          END
        END;
        RETURN i
      END find_name;

    BEGIN
      tty.set_pos(23,0);
      tty.erase_line(0);
      sle.edit_str(">>> ",bump,23,0,78,DESC,033c);
      tty.Write(015c); tty.erase_line(0);
      IF (DESC^.last=033c) OR (bump="") THEN RETURN END;
      IF    bump[0]="i" THEN
        IF nums.int_expr(bump,1,i)>=0 THEN
          i:=find_inode(i); set_cur(i)
        END;
      ELSIF bump[0]="n" THEN pool_str(bump);
        i:=find_name(bump); set_cur(i);
      ELSIF nums.int_expr(bump,0,i)>=0 THEN set_cur(i)
      END;
    END command;

    PROCEDURE gold;
      VAR ch: CHAR;
    BEGIN
      kbr.read(ch);
      IF    ch=kbr.pgup   THEN set_cur(0)
      ELSIF ch=kbr.pgdw   THEN set_cur(HIGH(D^.buff))
      ELSIF ch=kbr.f10    THEN command;
      ELSIF CAP(ch)="I"   THEN edit_inode(D^.buff[cur].inod);
                               refresh(cur-ln,0,scr); ref_info;
      ELSIF CAP(ch)="F"   THEN edit_file (D^.buff[cur].inod,0);
                               refresh(cur-ln,0,scr); ref_info;
      ELSIF CAP(ch)="D"   THEN
        IF (dio.d_dir*D^.buff[cur].kind#{}) &
           (D^.buff[cur].name#"..") THEN edit_dir(D^.buff[cur].inod);
           refresh(cur-ln,0,scr); ref_info;
        END;
      ELSIF CAP(ch)="E"   THEN
        IF (dio.d_dir*D^.buff[cur].kind#{}) &
           (D^.buff[cur].name#"..") THEN edit_dir(D^.buff[cur].inod);
        ELSE edit_file(D^.buff[cur].inod,0);
        END;
        refresh(cur-ln,0,scr); ref_info;
      END;
    END gold;

    PROCEDURE help;
      VAR x: CHAR;
    BEGIN
      tty.home; tty.erase(0);
      tty.print(
        'Directory Editor Help\n'
        '    UP,DOWN,LEFT,RIGHT,PAGE_UP,PAGE_DOWN -- перемещения курсора\n'
        '    TAB, BACK_TAB -- переход на следующий/предыдущий раздел\n'
        '    F10 PAGE_UP   -- переход на начало\n'
        '    F10 PAGE_DOWN -- переход на конец\n');
      tty.print(
        '    F10 F10 -- командный режим\n'
        '    F10 "I" -- редактировать инод текущего узла\n'
        '    F10 "D" -- редактировать текущий узел как директорию\n'
        '    F10 "F" -- редактировать текущий узел как файл\n');
      tty.print(
        '    F10 "E" -- редактировать текущий узел как файл/директорию\n'
        '    F1 -- подсказка\n');
      tty.print(
        'В командном режиме:\n'
        '  nname -- поиск узла с именем "name"\n'
        '  iexpr -- поиск узла со ссылкой на инод "expr"\n'
        '  expr  -- прыжок на узел с номером "expr"\n');
      tty.print(
        'Модификации:\n'
        '  Для имени файла : любые клавиши кроме фунциональных\n'
        '      (ПРОБЕЛ обозначает 000c)\n'
        '  Для номера инода: десятичные цифры\n'
        '      (ПРОБЕЛ обозначает "0")\n'
        '  Для множества признаков: ПРОБЕЛ переворачивает текущий бит\n\n');
      tty.print(
        '    ESC -- выход без записи; EXIT -- выход с записью\n'
        '                        ДЛЯ ПРОДОЛЖЕНИЯ НАЖМИТЕ ЛЮБУЮ КЛАВИШУ');
      kbr.read(x)
    END help;

    VAR crs: INTEGER;

  BEGIN
    IF HIGH(D^.buff)<22 THEN scr:=HIGH(D^.buff)+1 ELSE scr:=22 END;
    init_info;
    crs:=tty.state^.cursor;
    tty.set_cursor(1);
    refresh(0,0,scr); ref_info;
    cur:=0; item:=0; ln:=0; pos:=0; write:=FALSE;
    LOOP
      edit_item;
      CASE ch OF
        | 033c   : write:=FALSE; EXIT
        |kbr.exit: EXIT
        |kbr.up  : up(1)
        |kbr.dw  : dw(1)
        |kbr.pgup: up(8)
        |kbr.pgdw: dw(8)
        |kbr.left, kbr.bcktab: item:=(item+2) MOD 3; pos:=max_pos[item]
        |kbr.right,kbr.tab: item:=(item+1) MOD 3; pos:=0
        |kbr.newln: dw(1); pos:=0; item:=0
        |kbr.f10: gold
        |kbr.f1:  help; refresh(cur-ln,0,scr); ref_info
      ELSE
      END
    END;
    tty.set_cursor(crs);
    RETURN write;
  END _edit_dir;

  PROCEDURE edit_dir(no: INTEGER);
    VAR D: dio.Dir;
  BEGIN
    D:=dio.dir_walk(no);
    warn("при открытии директории %d",no);
    IF (D#NIL) THEN
      dio.end_walk(D, _edit_dir(D,no) &
          ask("Записать модифицированную директорию %d на диск?",no));
      warn(" при закрытии директории %d", no);
    END;
  END edit_dir;

  PROCEDURE _edit_inode(VAR N: dio.i_node; NO: INTEGER): BOOLEAN;

    PROCEDURE mess(VAL s: ARRAY OF CHAR; SEQ w: SYSTEM.WORD);
    BEGIN tty.set_pos(8,0); tty.erase_line(0); tty.print(s,w) END mess;

    VAR bump: ARRAY [0..79] OF CHAR;

    PROCEDURE wr_time(t: INTEGER);
      VAR y,m,d,h,min,sec: INTEGER;
    BEGIN
      bump[0]:=0c; bump[HIGH(bump)]:=0c;
      Time.unpack(t,y,m,d,h,min,sec);
      tty.print("%$2d/%$2d/%$4d %$2d:%$2d.%$2d",d,m,y,h,min,sec);
    END wr_time;

    PROCEDURE wr_nat(p,f,t,l: INTEGER);
      CONST spc="_________";
    BEGIN
      IF (p<f) OR (p>t) THEN tty.print('%*.*s',l,l,spc);
      ELSE tty.print('%$*d',l,p)
      END;
    END wr_nat;

    PROCEDURE vis_mode(new: INTEGER; on: BOOLEAN);
      VAR cc: INTEGER;
    BEGIN
      IF NOT on THEN tty.WriteString("___"); RETURN END;
      IF    new=dio.lng THEN tty.WriteString("lng");
      ELSIF new=dio.esc THEN tty.WriteString("esc");
      ELSIF new=dio.dir THEN tty.WriteString("dir");
      ELSIF new=dio.sys THEN tty.WriteString("sys");
      END;
    END vis_mode;

    PROCEDURE ref_mode;
    BEGIN
      tty.set_pos(2,0); tty.WriteString('mode: { ');
      vis_mode(dio.dir,{dio.dir}*N.Mode#{}); tty.Write(',');
      vis_mode(dio.lng,{dio.lng}*N.Mode#{}); tty.Write(',');
      vis_mode(dio.esc,{dio.esc}*N.Mode#{}); tty.Write(',');
      vis_mode(dio.sys,{dio.sys}*N.Mode#{}); tty.WriteString(' }')
    END ref_mode;

    PROCEDURE ref_links;
    BEGIN tty.set_pos(2,34); tty.print('links: [%$6d]',N.Links) END ref_links;

    PROCEDURE ref_eof;
    BEGIN tty.set_pos(2,52); tty.print('eof: [%$6d]',N.Eof) END ref_eof;

    PROCEDURE ref_cre;
    BEGIN
      tty.set_pos(4,0); tty.WriteString("created:  ");  wr_time(N.cTime);
    END ref_cre;

    PROCEDURE ref_mod;
    BEGIN
      tty.set_pos(4,35); tty.WriteString("modified:  "); wr_time(N.wTime);
    END ref_mod;

(*
   EDIT INODE 000153                                      MODIFICATION TIME

0123456789012345678901234567890123456789012345678901234567890123456789012345678
*)
    CONST INFO = "<< EDIT INODE 000000                   "
                 "                                     >>";

    VAR info: ARRAY [0..79] OF CHAR;

    PROCEDURE init_info;   VAR cc: INTEGER;
    BEGIN info:=INFO; cc:=14; app_dec(NO,6,info,cc) END init_info;

    PROCEDURE ref_info;
    BEGIN tty.set_pos(6,0);
      tty.set_reverse(1); tty.WriteString(info); tty.set_reverse(0);
    END ref_info;

    PROCEDURE ch_mode(VAL new: ARRAY OF CHAR);
      VAR cc: INTEGER;
    BEGIN
      cc:=58; app(info,new,cc); tty.set_pos(6,58);
      tty.set_reverse(1); tty.WriteString(new); tty.set_reverse(0);
    END ch_mode;

    PROCEDURE ch_app(VAL new: ARRAY OF CHAR);
      VAR cc: INTEGER;
    BEGIN
      cc:=71; app(info,new,cc); tty.set_pos(6,71);
      tty.set_reverse(1); tty.WriteString(new); tty.set_reverse(0);
    END ch_app;

    PROCEDURE refresh;
      VAR i: INTEGER;
    BEGIN
      tty.set_pos(0,0); tty.erase(0);
      tty.print('blocks: [');
      FOR i:=0 TO HIGH(N.Ref) DO wr_nat(N.Ref[i],0,dio.no_b-1,6);
        IF i#HIGH(N.Ref) THEN tty.Write(",") ELSE tty.Write("]") END;
      END;
      ref_mode;   ref_links;      ref_eof;
      ref_cre;    ref_mod;        ref_info;
    END refresh;

    VAR pos, item: INTEGER;

    CONST columns = ARRAY OF INTEGER {09,08,42,58,10,46};
            lines = ARRAY OF INTEGER { 0, 2, 2, 2, 4, 4};
           lowers = ARRAY OF INTEGER { 0, 4, 5, 5, 0, 0};

    PROCEDURE ref_crs;
    BEGIN
      IF (lines[pos]=2) & (columns[pos]=8) THEN
        tty.set_pos(lines[pos],columns[pos]+item*4)
      ELSE
        tty.set_pos(lines[pos],columns[pos]+item*7)
      END
    END ref_crs;

    PROCEDURE ref_pos;        CONST spaces = "    ";
    BEGIN
      CASE pos OF
        |0: ch_mode("      BLOCKS"); bump:=spaces;
            bump[0]:=CHAR(ORD("0")+item); ch_app(bump);
        |1: ch_mode("        MODE"); tty.set_pos(6,71);
            tty.set_reverse(1); vis_mode(item+1,TRUE) ; tty.Write(' ');
            tty.set_reverse(0)
        |2: ch_mode("       LINKS");      ch_app(spaces)
        |3: ch_mode("         EOF");      ch_app(spaces)
        |4: ch_mode("    CREATION");      ch_app("TIME")
        |5: ch_mode("MODIFICATION");      ch_app("TIME")
      END;
      ref_crs;
    END ref_pos;

    PROCEDURE left;
    BEGIN
      IF     pos=0             THEN item:=(item+7) MOD 8
      ELSIF (pos=1) & (item>0) THEN item:=item-1
      ELSIF  pos=1             THEN pos:=3; item:=0
      ELSIF  pos=2             THEN pos:=1; item:=3
      ELSIF  pos=3             THEN pos:=2
      ELSE   pos:=INTEGER(BITSET(pos)/{0})
      END
    END left;

    PROCEDURE right;
    BEGIN
      IF     pos=0             THEN item:=(item+1) MOD 8
      ELSIF (pos=1) & (item<3) THEN item:=item+1
      ELSIF  pos=3             THEN pos:=1;     item:=0
      ELSIF  pos IN {1,2}      THEN pos:=pos+1; item:=0
      ELSE   pos:=INTEGER(BITSET(pos)/{0})
      END
    END right;

    PROCEDURE step_dw;
    BEGIN
      IF pos=0 THEN
        IF    item IN {0..2} THEN pos:=1;
        ELSIF item IN {3..5} THEN pos:=2; item:=0;
        ELSE                      pos:=3; item:=0;
        END;
      ELSE
        IF pos=5 THEN item:=4 ELSE item:=0 END;
        pos:=lowers[pos];
      END;
    END step_dw;

    PROCEDURE step_up;
    BEGIN
      IF pos=0 THEN
        IF item<4 THEN pos:=4 ELSE pos:=5 END;
        item:=0;
      ELSIF pos=1 THEN pos:=0
      ELSIF pos IN {2,3} THEN item:=pos*3-3; pos:=0;
      ELSIF pos=4 THEN pos:=1; item:=0;
      ELSE  pos:=2; item:=0;
      END;
    END step_up;

    VAR write: BOOLEAN;

    PROCEDURE switch_mode(item: INTEGER);
    BEGIN
      N.Mode:=N.Mode/{item+1};
      vis_mode(item+1,item+1 IN N.Mode); write:=TRUE
    END switch_mode;

    PROCEDURE _space;
    BEGIN
      CASE pos OF
        |0: N.Ref[item]:=-1; tty.WriteString("______"); write:=TRUE;
        |1: switch_mode(item);
        |2: N.Links:=0; ref_links; write:=TRUE;
        |3: N.Eof:=0;   ref_eof;   write:=TRUE;
        |4: N.cTime:=0; ref_cre;   write:=TRUE;
        |5: N.wTime:=0; ref_mod;   write:=TRUE;
      END;
    END _space;

    PROCEDURE inp_integer(old: INTEGER; VAR new: INTEGER): BOOLEAN;
      VAR cc: INTEGER;
    BEGIN
      bump:="       >>> "; cc:=0; app_dec(old,6,bump,cc);
      sle.edit_str(bump,bump,8,0,78,DESC,033c);
      tty.set_pos(8,0); tty.erase_line(0);
      IF (DESC^.last#033c) & (bump#"") THEN
        IF nums.int_expr(bump,0,new)>=0 THEN RETURN TRUE
        ELSE mess(nums.message)
        END;
      END;
      RETURN FALSE
    END inp_integer;
  
    PROCEDURE inp_time(VAR t: INTEGER);

      VAR d,m,y,h,min,sec,pos: INTEGER; done: BOOLEAN;

      PROCEDURE error; BEGIN mess(" Неправильное время [%d]",pos) END error;

    BEGIN
      bump[0]:=0c; bump[HIGH(bump)]:=0c;
      Time.unpack(t,y,m,d,h,min,sec);
      img.print(bump,"%$2d/%$2d/%$4d %$2d:%$2d.%$2d >> ",d,m,y,h,min,sec);
      sle.edit_str(bump,bump,8,0,78,DESC,033c);
      tty.set_pos(8,0); tty.erase_line(0);
      IF (DESC^.last=033c) OR (bump="") THEN RETURN END;
      pos:=0;   img.iscan(d  ,bump,pos,done); IF NOT done THEN error; RETURN END;
      INC(pos); img.iscan(m  ,bump,pos,done); IF NOT done THEN error; RETURN END;
      INC(pos); img.iscan(y  ,bump,pos,done); IF NOT done THEN error; RETURN END;
      INC(pos); img.iscan(h  ,bump,pos,done); IF NOT done THEN h  :=0 END;
      INC(pos); img.iscan(min,bump,pos,done); IF NOT done THEN min:=0 END;
      INC(pos); img.iscan(sec,bump,pos,done); IF NOT done THEN sec:=0 END;
      IF y<100 THEN y:=y+1900 END;
      pos:=Time.pack(y,m,d,h,min,sec);
      IF pos<0 THEN error; RETURN END;
      t:=pos; write:=TRUE;
    END inp_time;

    PROCEDURE edit_item;
      VAR i: INTEGER;
    BEGIN
      CASE pos OF
        |0:
          IF inp_integer(N.Ref[item],i) THEN
            IF (i>=0) & (i<dio.no_b) THEN N.Ref[item]:=i;
              write:=TRUE; ref_crs; wr_nat(i,0,dio.no_b-1,6);
              item:=(item+1) MOD 8
            ELSE mess(" Номер блока должен быть в диапазоне [0..%d]",dio.no_b-1);
            END;
          END;
        |1: switch_mode(item);
        |2: IF inp_integer(N.Links,i) THEN
              N.Links:=i; write:=TRUE; ref_links
            END;
        |3: IF inp_integer(N.Eof  ,i) THEN
              N.Eof:=i;   write:=TRUE; ref_eof
            END;
        |4: inp_time(N.cTime); ref_cre;
        |5: inp_time(N.wTime); ref_mod;
      END;
    END edit_item;

    PROCEDURE help;
    BEGIN
      tty.set_pos(10,0); tty.erase(0);
      tty.print(
        'Inode Editor Help\n\n'
        '  UP,DOWN,LEFT,RIGHT -- перемещения курсора\n'
        '  CR -- выбор раздела для модификации\n'
        '  "E"-- редактировать текущий блок\n');
      tty.print(
        'Модификация:\n'
        '  ПРОБЕЛ -- логическая очистка раздела или инверсия бита признака\n'
        '  При вводе чисел разрешены выражения.\n'
        '  Время нужно вводить в формате, аналогичном приглашению\n\n'
        '    ESC -- выход бес записи; EXIT -- выход с записью');
    END help;

    VAR ch: CHAR;
       crs: INTEGER;

  BEGIN
    init_info;
    crs:=tty.state^.cursor; tty.set_cursor(1);
    refresh;
    pos:=0; item:=0; ref_pos; write:=FALSE;
    LOOP
      kbr.read(ch);
      CASE ch OF
        | 033c  : write:=FALSE; EXIT;   |" " : _space;
        |kbr.up : step_up;              |kbr.dw: step_dw;
        |kbr.cr : edit_item;            |kbr.left : left;
        |kbr.right: right;              |kbr.exit : EXIT;
        |kbr.f1 : help;
        |"e","E": IF pos=0 THEN
                     edit_block(N.Ref[item],0); refresh;
                     item:=(item+1) MOD 8
                   END
      ELSE
      END;
      ref_pos
    END;
    tty.set_cursor(crs);
    RETURN write
  END _edit_inode;
  
  PROCEDURE edit_inode(no: INTEGER);
    VAR cur: dio.i_node;
  BEGIN
    dio.get_inode(no,cur); warn("при чтении инода %d",no);
    IF _edit_inode(cur,no) &
        ask("Записать модифицированный инод %d на диск?",no)
    THEN
      dio.put_inode(no,cur); warn(" при записи инода %d",no);
    END;
  END edit_inode;

  PROCEDURE init_term;
    VAR i: INTEGER;

    PROCEDURE set_term(c: CHAR);
    BEGIN
      INCL(term[ORD(c) DIV 32],ORD(c) MOD 32)
    END set_term;

  BEGIN
    FOR i:=0 TO HIGH(term) DO term[i]:={} END;
    set_term(   033c   );       set_term(  kbr.up  );
    set_term(  kbr.dw  );       set_term( kbr.pgdw );
    set_term( kbr.pgup );       set_term( kbr.f10  );
    set_term(kbr.newln );       set_term(kbr.bcktab);
    set_term( kbr.tab  );       set_term( kbr.exit );
    set_term(  kbr.f1  );
  END init_term;

BEGIN init_term;
END editors;

MODULE disks; (* Hady. 10-Dec-89. (c) KRONOS *)

  IMPORT  SYSTEM;
  IMPORT  incl, excl, in?, excl?;
  IMPORT  warn, message, ask;
  IMPORT  low, tty, key: kbr;
  IMPORT  img, sys: SYSTEM, lex: Lexicon;
  IMPORT  dio, bio: BIO, sci: ASCII, errors, err;
  IMPORT  notes, Time;

  FROM  editors   IMPORT  vis_char;

  EXPORT QUALIFIED find_file, find_block, check_volume,
                   release, verify_disk;

  TYPE SHOW = PROCEDURE (ARRAY OF CHAR);

  VAR  iBusy: DYNARR OF BITSET;  -- busy      inodes set iterated
       iDirs: DYNARR OF BITSET;  -- directory inodes set iterated
       bBusy: DYNARR OF BITSET;  -- busy blocks set
      iLinks: DYNARR OF INTEGER;
      inDirs: DYNARR OF BITSET;

  PROCEDURE release;
  BEGIN
    IF iBusy^.ADR#NIL  THEN DISPOSE(iBusy);  NEW(iBusy)  END;
    IF iDirs^.ADR#NIL  THEN DISPOSE(iDirs);  NEW(iDirs)  END;
    IF bBusy^.ADR#NIL  THEN DISPOSE(bBusy);  NEW(bBusy)  END;
    IF inDirs^.ADR#NIL THEN DISPOSE(inDirs); NEW(inDirs) END;
    IF iLinks^.ADR#NIL THEN DISPOSE(iLinks); NEW(iLinks) END;
  END release;

  PROCEDURE app(VAR dest: ARRAY OF CHAR;
                VAL sour: ARRAY OF CHAR;
                VAR from: INTEGER);
    VAR i: INTEGER;
  BEGIN i:=0;
    WHILE (from<HIGH(dest)) & (i<HIGH(sour)) & (sour[i]#0c) DO
      dest[from]:=sour[i]; INC(from); INC(i);
    END;
    dest[from]:=0c;
  END app;

  TYPE PATH = ARRAY [0..255] OF CHAR;

  VAR path: PATH;

  PROCEDURE create_sets;
    VAR i: INTEGER;
    CONST inods = 4096 DIV BYTES(dio.i_node);
  BEGIN
    NEW(iBusy, HIGH(dio.iSET)+1); ASSERT(iBusy^.ADR#NIL);
    NEW(bBusy, HIGH(dio.bSET)+1); ASSERT(bBusy^.ADR#NIL);
    NEW(iDirs, HIGH(dio.iSET)+1); ASSERT(iDirs^.ADR#NIL);
    NEW(inDirs,HIGH(dio.iSET)+1); ASSERT(inDirs^.ADR#NIL);
    NEW(iLinks,dio.no_i);         ASSERT(iLinks^.ADR#NIL);
    FOR i:=0 TO dio.no_i-1   DO iLinks[i]:=0       END;
    FOR i:=0 TO HIGH(inDirs) DO inDirs[i]:={0..31} END;
    FOR i:=0 TO HIGH(iDirs)  DO iDirs[i]:={}       END;
    FOR i:=0 TO HIGH(iBusy)  DO iBusy[i]:={0..31}  END;
    FOR i:=0 TO HIGH(bBusy)  DO bBusy[i]:={0..31}  END;
    excl(inDirs,0);
    FOR i:=0         TO dio.sys_0  DO excl(bBusy,i) END;
  END create_sets;

  VAR all, ALL, skip, SKIP: BOOLEAN;

  PROCEDURE cor?(): BOOLEAN;
    VAR ch: CHAR; cap: BOOLEAN;
  BEGIN
    tty.WriteString(' Исправить [y|n|a|A|i|I] ?');
    IF all  THEN tty.Write("Y"); RETURN TRUE  END;
    IF skip THEN tty.Write("N"); RETURN FALSE END;

    REPEAT key.read(ch); cap:=(ch<="Z"); ch:=CAP(ch)
    UNTIL (ch="Y") OR (ch="N") OR (ch="A") OR (ch="I");


    IF    ch="A" THEN tty.Write("Y"); all :=TRUE; ALL :=cap
    ELSIF ch="I" THEN tty.Write("N"); skip:=TRUE; SKIP:=cap
    END;

    RETURN (ch="Y") OR (ch="A")
  END cor?;

  PROCEDURE iter_inodes(correct: BOOLEAN);

  (* REQUIRES:
       1. ARRAYs OF BITSETs have to be initialised by "create_sets" call;
       2. Volume I/O initialised
     EFFECTS:
       1. iBusy, bBusy & iDirs start to contain proper information
  *)

    PROCEDURE check_inode(ino: INTEGER; VAR n: dio.i_node): BOOLEAN;
      VAR r: BOOLEAN;    (* return TRUE when modified    *)
          i: INTEGER;
    BEGIN
      r:=FALSE;
      WITH n DO
        IF Mode-dio.all_modes#{} THEN
          notes.print("inode %d modes=%{} -> %{}.",
                       ino,Mode,Mode*dio.all_modes);
          IF correct & cor?() THEN
            Mode:=Mode*dio.all_modes; r:=TRUE
          END;
        END;
        IF Eof<0 THEN
          notes.print("inode %d eof=%d -> 0.",ino,Eof);
          IF correct & cor?() THEN Eof:=0; r:=TRUE END;
        END;
        IF Mode*{dio.dir}#{} THEN
          IF Eof MOD 64 # 0 THEN
            i:=(Eof DIV 64)*64;
            notes.print("inode %d is dir (eof MOD 64 # 0) %d -> %d.",ino,Eof,i);
            IF correct & cor?() THEN Eof:=i; r:=TRUE END
          END;
          IF Links>1 THEN
            notes.print("inode %d is dir (links > 1) %d -> 1.",ino,Links);
            IF correct & cor?() THEN Links:=1; r:=TRUE END
          END
        END;
        IF Links<0 THEN
          notes.print("inode %d links=%d -> 0.",ino,Links);
          IF correct & cor?() THEN
            Links:=0; iLinks[ino]:=0; r:=TRUE
          END;
        END;
        FOR i:=0 TO HIGH(res) DO
          IF res[i]#0 THEN
            notes.print("inode %d rfe%d=%08h -> 0.",ino,i,res[i]);
            IF correct & cor?() THEN res[i]:=0; r:=TRUE END;
          END
        END
      END;
      RETURN r
    END check_inode;

    VAR buff: POINTER TO ARRAY [0..1023] OF INTEGER;

    PROCEDURE check_ref(VAR   n: dio.i_node;
                        VAR max: INTEGER;
                            ino: INTEGER): BOOLEAN;

      CONST
        IND = " %s indirect-block %05d.Ref[%d]=%05d [%#h]";
        CLR = "Illegal block %05d.ref[%d]=%05d (will be cleared)";

      VAR yes: BOOLEAN;

      PROCEDURE check_blk(VAR no: INTEGER; l_no: INTEGER): BOOLEAN;
        VAR i: INTEGER;
      BEGIN
        IF (no<-1) OR (no>=dio.no_b) THEN
          IF l_no>=0 THEN i:=l_no ELSE i:=HIGH(n.Ref)+1-l_no END;
          notes.print('Некорректный номер блока %d в файле %d.',no,ino);
          IF correct & cor?() THEN no:=-1; RETURN TRUE END;
        END;
        IF no>0 THEN
          IF excl?(bBusy,no) THEN errors.put_dbl(ino,l_no) END;
          IF yes THEN max:=max+1 END
        ELSE yes:=FALSE
        END;
        RETURN FALSE
      END check_blk;

      PROCEDURE check_ind(i: INTEGER): BOOLEAN;
        VAR j,no: INTEGER;   r: BOOLEAN;
      BEGIN
        WITH n DO
          no:=Ref[i];
          IF no<=dio.sys_0 THEN
            notes.print("Некорректный номер индирект-блока i%d.Ref[%d]=%d",
                                                       ino,i,no);
            IF correct & cor?() THEN Ref[i]:=-1 END;
            RETURN TRUE
          END;
          dio.get_block(no,  buff,  4096); r:=dio.error;
          warn(IND,"reading",ino,i,no,no);
          IF r THEN RETURN FALSE END;
          r:=FALSE;
          FOR j:=0 TO HIGH(buff^) DO
            IF (buff^[j]<-1) OR (buff^[j]>0) THEN
              r:=check_blk(buff^[j],i*1024+j) OR r
            END
          END;
          IF NOT r THEN RETURN FALSE END;
          dio.put_block(no,buff,4096);
          warn(IND,"writing",ino, i, no, no)
        END;
        RETURN FALSE
      END check_ind;

      VAR i,j: INTEGER;
          mod: BOOLEAN;

    BEGIN
      mod:=FALSE;
      max:=0; yes:=TRUE;
      WITH n DO
        FOR i:=0 TO HIGH(Ref) DO
          mod:=check_blk(Ref[i],i-HIGH(Ref)-1) OR mod
        END;
        IF n.Mode*{dio.lng}={} THEN RETURN mod END;
        IF buff=NIL            THEN NEW(buff)  END;
        max:=0; yes:=TRUE;
        FOR i:=0 TO HIGH(Ref) DO
          IF Ref[i]>0 THEN mod:=check_ind(i) OR mod END
        END
      END;
      RETURN mod
    END check_ref;

    VAR i: INTEGER;
        N: dio.i_node;
        r: BOOLEAN;
    write: BOOLEAN;
      max: INTEGER;

  BEGIN
    buff:=NIL;
    FOR i:=0 TO dio.no_i-1 DO
      IF i MOD 64 = 0 THEN message(1,'%d%%',i*100 DIV dio.no_i) END;
      dio.get_inode(i,N); r:=dio.error;
      warn("reading inode %d",i);
      IF NOT r THEN
        write:=check_inode(i,N);
        IF N.Links>0 THEN
          WITH N DO
            excl(iBusy,i);
            iLinks[i]:=Links;
            IF Mode*{dio.esc}={} THEN write:=check_ref(N,max,(i)) OR write END;
            IF Mode*{dio.dir}#{} THEN
              incl(iDirs,i); max:=max*4096;
              IF max<Eof THEN
                notes.print(
                  'Dir %d - нарушена непрерывность; eof(%d -> %d).',
                            i,Eof,max);
                IF correct & cor?() THEN Eof:=max; write:=TRUE END
              END
            END
          END
        END;
        IF write & correct THEN
          dio.put_inode(i,N); warn("writing inode %d",i)
        END
      END
    END;
    IF buff#NIL THEN DISPOSE(buff) END
  END iter_inodes;

  PROCEDURE iter_dirs(correct: BOOLEAN);

    VAR invalid: ARRAY CHAR OF CHAR;
            vis: ARRAY [0..31] OF CHAR;

    PROCEDURE check_node(VAR n: dio.dir_node; dir: INTEGER): BOOLEAN;

      VAR mod: BOOLEAN; (* node modified *)

      PROCEDURE check_name;
        VAR i: INTEGER;
           ch: CHAR;
          new: ARRAY [0..31] OF CHAR;
          bad: BOOLEAN;
      BEGIN
        WITH n DO
          i:=0; bad:=FALSE; low.fill(new,0);
          WHILE (i<=HIGH(name)) & (name[i]#0c) DO
            ch:=name[i]; vis[i]:=vis_char[ORD(ch)];
            IF invalid[ch]=0c THEN new[i]:=ch
            ELSE      bad:=TRUE;   new[i]:=CHAR(100b+ORD(ch) MOD 32);
            END; INC(i)
          END;
          IF i>HIGH(vis) THEN vis[31]:=0c ELSE vis[i]:=0c END;
          IF bad THEN
            new[HIGH(new)]:=0c;
            notes.print('.../%d/: некорректное имя "%s" => "%s".',dir,vis,new);
            IF correct & cor?() THEN name:=new; mod:=TRUE; RETURN END;
          END;
          IF i<=HIGH(name) THEN RETURN END;
          notes.print('.../%d/: длинное имя: "%s".',dir,vis);
          IF correct & cor?() THEN name[HIGH(name)]:=0c; mod:=TRUE END;
        END
      END check_name;

      VAR i: INTEGER;  occu: BOOLEAN; (* node occupied *)

    BEGIN
      mod:=FALSE;
      WITH n DO
--        occu:=(name#"") & (kind*dio.d_del={}) & (kind*dio.d_entry#{});
        occu:=(name#"");
        IF occu & (kind*dio.d_del#{}) THEN
          notes.print('.../%d/: некорректно выставлен признак "del".',dir);
          IF correct & cor?() THEN
            kind:=kind-dio.d_del+dio.d_file
          END;
        END;
        IF occu & (inod<0) OR (inod>=dio.no_i) THEN
          notes.print('.../%d/: неверный узел i%d'
                        ' считается свободным.',dir, inod);
          IF correct & cor?() THEN
            inod:=00; name:=""; kind:={}; RETURN TRUE
          END;
        END;
        IF NOT occu & ((inod<-1) OR (inod>=dio.no_i)) THEN
          notes.print('.../%d/: грязный узел "".',dir);
          IF correct & cor?() THEN
            inod:=00; name:=""; kind:={}; RETURN TRUE
          END;
        END;
        IF kind-dio.all_kinds#{} THEN
          notes.print('.../%d/: лишние биты %{}=>%{}.',
                           dir,kind,kind*dio.all_kinds);
          IF correct & cor?() THEN
            kind:=kind*dio.all_kinds; mod:=TRUE
          END;
        END;
        FOR i:=0 TO HIGH(rfe0) DO
          IF rfe0[i]#0 THEN
            notes.print('.../%d/: резервное слово %d=>0.',dir, rfe0[i]);
            IF correct & cor?() THEN rfe0[i]:=0; mod:=TRUE END;
          END;
        END;
        FOR i:=0 TO HIGH(rfe1) DO
          IF rfe1[i]#0 THEN
            notes.print('.../%d/: резервное слово %d=>0.',dir, rfe1[i]);
            IF correct & cor?() THEN rfe1[i]:=0; mod:=TRUE END;
          END;
        END;
        IF occu THEN check_name END
      END; (* WITH n DO *)
      RETURN mod
    END check_node;

    PROCEDURE one_node(VAR dn: dio.dir_node): BOOLEAN;
    BEGIN
      WITH dn DO
        IF (inod>0) & (name#"..") & (inod<dio.no_i) THEN
          excl(inDirs,inod); DEC(iLinks[inod]);
          IF (dio.d_esc*kind={}) & in?(iDirs,inod) # (dio.d_dir*kind#{}) THEN
            IF dio.d_dir*kind={} THEN kind:=kind+dio.d_dir-dio.d_file
            ELSE                      kind:=kind-dio.d_dir+dio.d_file
            END;
            RETURN TRUE
          END
        END
      END;
      RETURN FALSE
    END one_node;

    VAR myDirs: DYNARR OF BITSET; -- local copy of iDirs
       forward: BOOLEAN;

    PROCEDURE iter_tree(prev,no,level: INTEGER);
      VAR   d: dio.Dir;           -- walking directory
        entry: INTEGER;           -- no of current entry
        write: BOOLEAN;           -- was directory changed ?
        inode: INTEGER;
    BEGIN
      message(1,'%8d',no);
      d:=dio.dir_walk(no); warn("reading directory %d ",no);
      IF (d=NIL) THEN RETURN END;

      write:=FALSE; entry:=0;
      IF (prev>=0) & (level>0) THEN            -- проверить имя ".."
        write:=check_node(d^.buff[entry],no);
        WITH d^.buff[entry] DO
          IF name#".." THEN
            notes.print('.../%d/: отсутствует имя "..".',no);
            IF correct & cor?() THEN
              name:=".."; kind:=dio.d_dir+dio.d_hidden; write:=TRUE
            END;
          END;
          IF inod#prev THEN
            notes.print('.../%d/: некорректная ссылка "..": %d=>%d.',
                                                    no, inod, prev);
            IF correct & cor?() THEN inod:=prev; write:=TRUE END;
          END;
        END;
        INC(entry)
      ELSIF prev<=-2 THEN -- поиск корня
        inode:=d^.buff[entry].inod;
        IF (inode>no) & in?(iDirs,inode) & in?(myDirs,inode) THEN
          dio.end_walk(d,FALSE); warn("close directory %d",no);
          forward:=FALSE;
          RETURN
        END
      END;
      excl(myDirs,no);
      WHILE entry<=HIGH(d^.buff) DO
        write:=check_node(d^.buff[entry],no) OR write;
        WITH d^.buff[entry] DO
          IF (name="") # ((kind*dio.d_entry={}) OR (kind*dio.d_del#{})) THEN
            notes.print('.../%d/[%d]: несоответствие "%.32s" <=> %{}.',
                                             no,entry,vis,kind);
            IF correct & cor?() THEN
              IF name="" THEN kind:=kind+dio.d_del-dio.d_entry
              ELSE            kind:=kind-dio.d_del;
                IF kind*dio.d_entry={} THEN kind:=kind+dio.d_file END
              END;
            END;
          END;
          IF (kind*dio.d_del={}) & (kind*dio.d_entry#{}) THEN
            IF in?(dio.iSET,inod) OR in?(iBusy,inod) THEN
              notes.print('.../%d/"%s": ссылка на свободный инод %d.'
                            ,no,vis,inod);
              IF correct & cor?() THEN
                excl(dio.iSET,inod); excl(iBusy,inod);
                dio.s_put:=TRUE
              END;
            END;
            write:=one_node(d^.buff[entry]) OR write;
            IF in?(myDirs,inod) & (name#"..") THEN
              iter_tree(no,inod,level+1);
              warn("iterate directory %d",inod);
            END
          END
        END;
        INC(entry)
      END;
      dio.end_walk(d,write&correct); warn("close directory %d",no)
    END iter_tree;

    PROCEDURE init;
      VAR c: CHAR;
    BEGIN
      low.fill(invalid,0);
      FOR c:=0c TO " " DO invalid[c]:=1c END;
      invalid["/"]:=1c;     invalid[":"]:=1c
    END init;

    PROCEDURE put_anon(i: INTEGER);
    BEGIN
      notes.print("Файл %d аноним.",i);
      errors.put_anon(i);
    END put_anon;

    VAR i: INTEGER;

  BEGIN init;
    NEW(myDirs,HIGH(iDirs)+1);
  (*$<$X+*)
    myDirs:=iDirs;
  (*$>*)
    iter_tree(-1,0,0);             -- итерация основного поддерева от корня
    FOR i:=1 TO dio.no_i-1 DO      -- поиск анонимного поддерева с
      IF in?(myDirs,i) THEN        -- нахождением корня
        forward := TRUE; iter_tree(-2,i,0);
        IF forward THEN put_anon(i) END
      END
    END;
    FOR i:=1 TO dio.no_i-1 DO       -- сборка "мусора" от двух пердыдущих
      IF in?(myDirs,i) THEN put_anon(i); iter_tree(-1,i,0) END
    END;
    DISPOSE(myDirs);
    message(1,"");
  END iter_dirs;

  PROCEDURE check_blocks(correct: BOOLEAN);
    VAR i: INTEGER; err: INTEGER;
  BEGIN
    FOR i:=0 TO dio.no_b-1 DO
      err:=ORD(in?(dio.bSET,i))*2+ORD(in?(bBusy,i));
      CASE err OF
        |0: (* ok *)
        |1: notes.print("Ошибочно считается занятым блок %d.", i);
            IF correct & cor?() THEN incl(dio.bSET,i); dio.s_put:=TRUE END;
        |2: notes.print("Ошибочно считается свободным блок %d.", i);
            IF correct & cor?() THEN excl(dio.bSET,i); dio.s_put:=TRUE END;
        |3: (* ok *)
      END;
    END;
  END check_blocks;

  PROCEDURE check_sets(correct: BOOLEAN);
    VAR i: INTEGER;
      err: INTEGER;
  BEGIN
    FOR i:=0 TO dio.no_i-1 DO
      err:=  ORD(in?(inDirs  ,i))*4
           + ORD(in?(dio.iSET,i))*2
           + ORD(in?(iBusy   ,i));
      CASE err OF
        |0,7: (* ok *)
        |1: notes.print('ошибка в таблице файлов'
                            ' (ошибочно свободен?) i%d.',i);
        |2: notes.print("Ошибочно считается свободным файл %d.",i);
            IF correct & cor?() THEN
              excl(dio.iSET,i); dio.s_put:=TRUE;
            END;
        |3: notes.print('Ошибка в директории. i%d.',i);
        |4: errors.put_anon(i);
        |5: notes.print("Ошибочно считается занятым файл %d.",i);
            IF correct & cor?() THEN incl(dio.iSET,i); dio.s_put:=TRUE END;
        |6: notes.print('ошибка в таблице файлов'
                            ' (ошибочно занят?) i%d.',i);
      END;
    END;
  END check_sets;

  PROCEDURE alloc_blk(): INTEGER; --  RETURN -1 if impossible
    VAR i: INTEGER;
  BEGIN
    FOR i:=0 TO dio.no_b-1 DO
      IF in?(dio.bSET,i) THEN
        excl(dio.bSET,i); dio.s_put:=TRUE; RETURN i
      END;
    END;
    RETURN -1;
  END alloc_blk;

  PROCEDURE alloc_slot(dir: dio.Dir): INTEGER;
  (* in successful final the directory is changed, but not written
  *)

    CONST dn_size = BYTES(dio.dir_node);
           dnodes = 4096 DIV dn_size;

    VAR i,b,eof: INTEGER;

  BEGIN
    FOR i:=1 TO HIGH(dir^.buff) DO
      WITH dir^.buff[i] DO
        IF (* current slot in directory is free *)
             (dio.d_del*kind#{})              OR
             (kind*(dio.d_file+dio.d_dir)={}) OR
             (name="")
        THEN kind:=kind-dio.d_del; RETURN i
        END;
      END;
    END;
    eof:=dir^.n.Eof;
    IF (eof=0) OR (((eof DIV dn_size)+1) MOD 64 = 0) THEN
      FOR b:=0 TO HIGH(dir^.n.Ref) DO
        IF    dir^.n.Ref[b]<=0   THEN
          dir^.n.Ref[b]:=alloc_blk();
          IF dir^.n.Ref[b]=-1 THEN
            message(0,'NO FREE BLOCK ON VOLUME\n');
            RETURN -1
          END;
        ELSIF b=HIGH(dir^.n.Ref) THEN
          message(0,'Не умею работать с длинными директориями');
          RETURN -1;
        END;
      END;
    END;
    dir^.n.Eof:=eof+dn_size;
    RESIZE(dir^.buff, HIGH(dir^.buff)+2);
    i:=HIGH(dir^.buff);
    WITH dir^.buff[i] DO
      FOR b:=0 TO HIGH(name) DO name[b]:=0c END;
      FOR b:=0 TO HIGH(rfe0) DO rfe0[b]:=0  END;
      FOR b:=0 TO HIGH(rfe1) DO rfe1[b]:=0  END;
      kind:={}; inod:=-1;
    END;
    RETURN i;
  END alloc_slot;

  PROCEDURE check_dbl(correct: BOOLEAN);
    VAR f,no: INTEGER;
       b,lno: INTEGER;
      br,new: INTEGER;
           N: dio.i_node;
       write: BOOLEAN;
        buff: POINTER TO ARRAY [0..1023] OF INTEGER;
         ref: ARRAY [0..7] OF CHAR;
  BEGIN
    buff:=NIL;
    WHILE errors.get_dbl(f,no) DO
      IF no<0 THEN lno:=HIGH(N.Ref)+1+no; ref:="Ref."
      ELSE         lno:=no;               ref:=""
      END;
      notes.print("Повторно использован блок %s%d в файле %d.",ref,lno,f);
      IF correct & cor?() THEN
        dio.get_inode(f,N); write:=FALSE;
        warn(" при чтении инода %d",f);
        IF no<0 THEN
          br:=N.Ref[lno] ; new:=alloc_blk();
          N.Ref[lno]:=new; write:=TRUE
        ELSE
          IF buff=NIL THEN NEW(buff) END;
          b:=N.Ref[no DIV 1024];
          dio.get_block(b,buff,4096);
          warn(" при чтении блока %d",b);
          br:=buff^[no MOD 1024];
          new:=alloc_blk(); buff^[no MOD 1024]:=new;
          dio.put_block(b,buff,4096);
          warn(" при записи блока %d",b);
        END;
        IF write THEN dio.put_inode(f,N) END;
        warn(" при записи инода %d",f);
        IF new>0 THEN
          IF buff=NIL THEN NEW(buff) END;
          dio.get_block(br ,buff,4096); warn(" при чтении блока %d",br);
          dio.put_block(new,buff,4096); warn(" при записи блока %d",new);
        END
      END
    END;
    IF buff#NIL THEN DISPOSE(buff) END;
  END check_dbl;

  PROCEDURE collect_anonims(correct: BOOLEAN);

    CONST anon_name = "%d";
          collector = "lost+find";

    VAR coll: INTEGER; -- NO of collector directory

    PROCEDURE find_collector(): BOOLEAN; -- TRUE if found;
      VAR root: dio.Dir; entry: INTEGER;
    BEGIN
      root:=dio.dir_walk(0);
      IF dio.error OR (root=NIL) THEN
        warn("при открытии корневой директории");
        RETURN FALSE
      END;
      entry:=0;
      WHILE entry<=HIGH(root^.buff) DO
        WITH root^.buff[entry] DO
          IF     (name = collector )
               & (dio.d_dir*kind#{})
               & (dio.d_del*kind={}) THEN
            coll:=inod;
            dio.end_walk(root,FALSE);
            warn("при закрытии директории");
            RETURN TRUE;
          END;
        END;
        INC(entry);
      END;
      dio.end_walk(root,FALSE);
      warn("при закрытии корневой директории");
      RETURN FALSE;
    END find_collector;

    VAR   no: INTEGER;
        colr: dio.Dir; write: BOOLEAN;
       entry: INTEGER;
       inode: dio.i_node;
         rep: ARRAY [0..79] OF CHAR;
         dir: dio.Dir;

  BEGIN
    IF errors.no_anon<=0 THEN RETURN END;

    IF NOT find_collector() THEN
      notes.show('Нет поддиректории "lost+find"');
      WHILE errors.get_anon(no) DO
        notes.print("Файл %d аноним. Не могу поднять",no);
      END;
      RETURN;
    END;

    colr:=NIL; colr:=dio.dir_walk(coll); write:=FALSE;
    warn('при открытии директории "lost+find"');

    WHILE errors.get_anon(no) DO
      notes.print("Файл %d аноним.",no);
      IF correct & cor?() THEN
        entry:=alloc_slot(colr); write:=TRUE;
        IF entry<=0 THEN
          message(0,'не нашел свободного места в директории[%d]\n',entry);
          dio.end_walk(colr,FALSE); RETURN
        END;
        dio.get_inode(no,inode);
        IF dio.error THEN warn("при чтении инода %d", no);
          dio.end_walk(colr,FALSE); RETURN
        END;
        IF (inode.Eof>0) OR ({dio.dir}*inode.Mode#{}) THEN
          WITH colr^.buff[entry] DO
            img.print(name,anon_name,no);
            IF {dio.dir}*inode.Mode#{} THEN
              kind:=kind+dio.d_dir; kind:=kind-dio.d_file;
              dir:=dio.dir_walk(no);  warn(" при чтении директории   %d",no);
              dir^.buff[0].inod:=coll; dir^.buff[0].name:="..";
              dio.end_walk(dir,TRUE); warn(" при закрытии директории %d",no);
                                          img.print(rep,'Директория ');
            ELSE  kind:=kind+dio.d_file;  img.print(rep,'Файл ');
            END;
            inod:=no;
            img.append(rep,'%d аноним. Поднят под именем "%s"',no,name);
            notes.show(rep);
          END;
          DEC(iLinks[no]);
        ELSE
          colr^.buff[entry].kind:=colr^.buff[entry].kind+dio.d_del;
          excl(dio.iSET,no); dio.s_put:=TRUE;
        END;
      END;
    END;
    IF colr#NIL THEN
      dio.end_walk(colr,write); warn('при закрытии "lost+find"');
    END;
    RETURN;
  END collect_anonims;

  PROCEDURE check_links(correct: BOOLEAN);
    VAR i: INTEGER; rep: ARRAY [0..79] OF CHAR;
        N: dio.i_node; free: BOOLEAN;
  BEGIN
    FOR i:=1 TO HIGH(iLinks) DO
      IF (iLinks[i]#0) & NOT (in?(iDirs,i) & (iLinks[i]<0)) THEN
        img.print(rep,' Число ссылок на файл %d ',i);
        IF iLinks[i]<0 THEN
          img.append(rep,'больше на %d.',-iLinks[i]);
        ELSE
          img.append(rep,'меньше на %d.', iLinks[i]);
        END;
        notes.show(rep);
        IF correct & cor?() THEN
          dio.get_inode(i,N); warn(" при чтении инода %d.",i);
          IF N.Links=0 THEN excl(iBusy,i) END;
          N.Links:=N.Links-iLinks[i];
          IF N.Links<0 THEN N.Links:=0    END;
          IF N.Links=0 THEN incl(iBusy,i) END;
          dio.put_inode(i,N); warn(" при записи инода %d.",i);
        END;
      END;
    END;
  END check_links;

  PROCEDURE check_volume(correct: BOOLEAN);

    PROCEDURE syn(): BOOLEAN;
    BEGIN
      all:=ALL; skip:=SKIP; RETURN SKIP
    END syn;

  BEGIN
    all:=FALSE; ALL:=FALSE; skip:=FALSE; SKIP:=FALSE;
    release; create_sets;
    message(0,"Check index nodes"); iter_inodes (correct);
    IF syn() THEN RETURN END;
    message(0,'Check files tree');  iter_dirs   (correct);
    IF syn() THEN RETURN END;
    message(0,"Check links");       check_links (correct);
    IF syn() THEN RETURN END;
    message(0,"Check block map");   check_blocks(correct);
    IF syn() THEN RETURN END;
    message(0,"Check files map");   check_sets  (correct);
    IF syn() THEN RETURN END;
    check_dbl(correct);
    IF syn() THEN RETURN END;
    collect_anonims(correct)
  END check_volume;

  PROCEDURE verify_disk(): BOOLEAN;

    VAR bump: POINTER TO ARRAY [0..4095] OF CHAR;
    VAR bno: INTEGER;
        bsy: BOOLEAN;
       free: BOOLEAN;
        all: BOOLEAN;
       skip: BOOLEAN;
        bad: INTEGER;
        was: INTEGER;
       bbad: INTEGER;

    PROCEDURE vis_error(error: INTEGER);
    BEGIN
      lex.perror(bump^,error,"%%s");
      IF bsy THEN notes.print('block %d (busy): %s.',bno,bump^)
      ELSE        notes.print('block %d: %s.',bno,bump^)
      END
    END vis_error;

    PROCEDURE ask0(): BOOLEAN;
      VAR ch: CHAR;
    BEGIN
      tty.print("Это серьезно [Yes/No] ?");
      LOOP
        key.read(ch);
        IF    (ch="y") OR (ch="Y") OR
              (ch="n") OR (ch="N") THEN EXIT
        ELSE key.bell(1)
        END
      END;
      ch:=CAP(ch); tty.Write(ch); RETURN (ch="Y")
    END ask0;

    PROCEDURE ask1(no: INTEGER): BOOLEAN;
      VAR ch: CHAR;
         yes: BOOLEAN;
    BEGIN
      tty.print(" Закрыть [Yes|No|Free|All|Quit] ?");
      IF all OR (free & NOT bsy) THEN
        tty.Write("Y"); RETURN TRUE
      END;
      REPEAT
        key.read(ch);
        ch:=sci.CAPITAL(ch);
        IF    ch="Y" THEN             yes:=TRUE
        ELSIF ch="N" THEN             yes:=FALSE
        ELSIF ch="A" THEN  all:=TRUE; yes:=TRUE
        ELSIF ch="F" THEN free:=TRUE; yes:=NOT bsy
        ELSIF ch="Q" THEN skip:=TRUE; yes:=FALSE
        ELSE ch:=0c
        END
      UNTIL ch#0c;
      tty.Write(ch);
      RETURN yes
    END ask1;

    PROCEDURE check_error(no,error: INTEGER): BOOLEAN;
      VAR y: BITSET;
    BEGIN
      y:=BITSET(error);
      IF INTEGER(y*BITSET(err.io_error))=err.io_error THEN
        vis_error(error); RETURN ask1(no)
      ELSE
        vis_error(error);
        IF ask0() THEN HALT(1) END;
        RETURN TRUE
      END
    END check_error;

    PROCEDURE check_block(no: INTEGER): BOOLEAN;
    BEGIN
      bio.seek(dio.drv,no*4096,0);
      bio.read(dio.drv,bump,BYTES(bump^));
      IF bio.done THEN RETURN FALSE END;
      RETURN check_error(no,bio.error)
    END check_block;

    VAR bads: DYNARR OF BITSET;
         ref: DYNARR OF INTEGER;
        badf: dio.i_node;

    PROCEDURE unpack;
      VAR i,j: INTEGER;
          blk: POINTER TO ARRAY [0..1023] OF INTEGER;
    BEGIN
      dio.get_inode(2,badf);
      IF dio.error THEN
        message(0,"read_inode(2): %s",dio.message);
        RETURN
      END;
      IF dio.lng IN badf.Mode THEN
        blk:=sys.ADDRESS(bump);
        FOR i:=0 TO HIGH(badf.Ref) DO
          IF badf.Ref[i]>=0 THEN
            dio.get_block(badf.Ref[i],blk,4096);
            IF dio.error THEN
              message(0,"read i2.REF[%d]: %s",i,dio.message);
            ELSE
              FOR j:=0 TO HIGH(blk^) DO
                IF blk^[j]>=0 THEN INC(bad); incl(bads,blk^[j]) END
              END
            END
          END
        END
      ELSE
        FOR i:=0 TO HIGH(badf.Ref) DO
          IF badf.Ref[i]>=0 THEN
            INC(bad); incl(bads,badf.Ref[i])
          END
        END
      END
    END unpack;

    PROCEDURE dispose(VAR ref: ARRAY OF INTEGER);
      VAR i: INTEGER;
    BEGIN
      FOR i:=0 TO HIGH(ref) DO
        IF ref[i]>=0 THEN
          incl(dio.bSET,ref[i]); dio.s_put:=TRUE; ref[i]:=-1
        END
      END
    END dispose;

    PROCEDURE pack(VAR ref: ARRAY OF INTEGER);
      VAR i,no: INTEGER;
    BEGIN
      no:=0;
      FOR i:=0 TO dio.d_size-1 DO
        IF in?(bads,i) THEN
          ASSERT(no<=HIGH(ref));
          ref[no]:=i; INC(no)
        END
      END
    END pack;

    PROCEDURE alloc(VAR ref: ARRAY OF INTEGER; no: INTEGER): BOOLEAN;

      PROCEDURE find_good(): INTEGER;
        VAR i: INTEGER;
      BEGIN
        FOR i:=0 TO dio.d_size-1 DO
          IF   in?(dio.bSET,i) &
           NOT in?(bads,i) THEN RETURN i
          END
        END;
        RETURN -1
      END find_good;

      VAR i: INTEGER;

    BEGIN
      IF no>8*1024 THEN
        message(0,"Слишком много плохих блоков (%d)",no);
        RETURN TRUE
      END;
      no:=(no+1023) DIV 1024;
      FOR i:=0 TO HIGH(ref) DO
        IF (i<no) & (ref[i]<0) THEN
          ref[i]:=find_good();
          IF ref[i]<0 THEN RETURN TRUE END;
          excl(dio.bSET,ref[i]);
          dio.s_put:=TRUE
        ELSIF (i>=no) & (ref[i]>=0) THEN
          excl(dio.bSET,ref[i]);
          dio.s_put:=TRUE
        END
      END;
      RETURN FALSE
    END alloc;

    PROCEDURE clear(VAR ref: ARRAY OF INTEGER);
      VAR i: INTEGER;
    BEGIN
      FOR i:=0 TO HIGH(ref) DO ref[i]:=-1 END;
    END clear;

    PROCEDURE write_long;
      VAR no,i,b: INTEGER;
             blk: POINTER TO ARRAY [0..1023] OF INTEGER;

      PROCEDURE put(i: INTEGER);
      BEGIN
        IF no>HIGH(blk^) THEN
          dio.put_block(badf.Ref[b],blk,4096);
          warn("writing i2.REF[%d]",b);
          INC(b); no:=0; low.fill(blk^,-1)
        END;
        blk^[no]:=i; INC(no)
      END put;

    BEGIN
      no:=0; b:=0; blk:=sys.ADDRESS(bump);
      INCL(badf.Mode,dio.lng);
      low.fill(blk^,-1);
      FOR i:=0 TO dio.d_size-1 DO
        IF in?(bads,i) THEN put(i) END;
      END;
      IF no>0 THEN
        dio.put_block(badf.Ref[b],blk,BYTES(blk^));
        warn("writing i2.Ref[%d]",b)
      END
    END write_long;

    PROCEDURE write;
      VAR long: BOOLEAN;
          case: INTEGER;
    BEGIN
      long:=(bad>8);
      case:=ORD(long)+ORD(dio.lng IN badf.Mode)*2;
      CASE case OF
         0:                    pack(badf.Ref);
        |2: dispose(badf.Ref); pack(badf.Ref);
        |1: clear(badf.Ref);
            IF alloc(badf.Ref,bad) THEN RETURN END; write_long
        |3: IF alloc(badf.Ref,bad) THEN RETURN END; write_long
      END;
      badf.Eof:=bad*4096;
      badf.wTime:=Time.time();
      dio.put_inode(2,badf);
      warn('writing inode 2 ("BAD.BLOCKS")');
    END write;

  BEGIN
    NEW(bump);
    NEW(bads,(dio.d_size+31) DIV 32);
    FOR bno:=0 TO HIGH(bads) DO bads[bno]:={} END;
    bad:=0; bbad:=0; unpack; was:=bad;
    bno:=0; all:=FALSE; skip:=FALSE; free:=FALSE;
    message(0,"%8d blocks tested",bno);
    message(1,"%04d(%04d busy) bad blocks detected",bad,bbad);
    WHILE (bno<dio.d_size) & NOT skip DO
      IF bno MOD 32=0 THEN
        message(0,"%8d blocks tested",bno);
      END;
      bsy:=NOT in?(dio.bSET,bno);
      IF NOT in?(bads,bno) & check_block(bno) THEN
        IF bno<dio.sys_0 THEN
          notes.print("Блок %d занят системой!",bno)
        ELSE
          INC(bad); incl(bads,bno); excl(dio.bSET,bno);
          dio.s_put:=TRUE;   IF bsy THEN INC(bbad) END;
          message(1,"%04d(%04d busy) bad blocks detected",bad,bbad)
        END
      END;
      INC(bno)
    END;
    message(1,"");
    IF bad=was THEN DISPOSE(bump); RETURN FALSE END;
    write; DISPOSE(bump);
    RETURN bbad>0
  END verify_disk;

  --------------------------  SEARCH  ----------------------------
                            ----------

  PROCEDURE find_file(VAL patt: ARRAY OF CHAR;
                           PNO: INTEGER; show: SHOW);

    VAR myDirs: DYNARR OF BITSET; -- local copy of iDirs

    PROCEDURE make_dirs;
      VAR i: INTEGER; N: dio.i_node;
    BEGIN
      NEW(iDirs,SIZE(dio.iSET));
      ASSERT(iDirs^.ADR#NIL);
      FOR i:=0 TO HIGH(iDirs) DO iDirs[i]:={} END;
      FOR i:=0 TO dio.no_i-1 DO
        IF i MOD 64 = 0 THEN message(1,'%d%%',i*100 DIV dio.no_i) END;
        dio.get_inode(i,N);
        IF dio.error THEN warn("reading inode %d",i);
        ELSE
          IF (N.Links>0) & (N.Mode*{dio.dir}#{}) THEN incl(iDirs,i) END;
        END
      END;
    END make_dirs;

    PROCEDURE iter_tree(no: INTEGER;
                  VAR path: ARRAY OF CHAR;
                       pos: INTEGER);

      VAR   d: dio.Dir;           -- walking directory
        entry: INTEGER;           -- no of current entry
        s_pos: INTEGER;
         bump: ARRAY [0..47] OF CHAR;

    BEGIN
      d:=dio.dir_walk(no); warn(" чтение директории %d [%hh]", no,no);
      IF (d=NIL) THEN RETURN END;
      entry:=0;
      excl(myDirs,no);
      path[pos]:="/"; INC(pos);
      WHILE entry<=HIGH(d^.buff) DO
        WITH d^.buff[entry] DO
          IF   ((PNO < 0) & (name=patt))
            OR ((inod>=0) & (PNO=inod)) THEN
            s_pos:=pos;
            IF name="" THEN
              img.print(bump,"[%d]***slot%d***",inod,entry);
            ELSE
              img.print(bump,"[%d]%s",inod,name);
            END;
            app(path,bump,s_pos);
            IF (kind*dio.d_del#{}) THEN
              app(path,"  -- deleted",s_pos);
            END;
            path[s_pos]:=0c; show(path);
          END;
          IF (kind*dio.d_del={}) & (kind*(dio.d_file+dio.d_dir)#{}) THEN
            IF in?(myDirs,inod) & (name#"..") THEN
              img.print(bump,"[%d]%s",inod,name);
              s_pos:=pos; app(path,bump,s_pos);
              iter_tree(inod,path,s_pos);
              warn(" при итерации директории %d",inod);
            END
          END
        END; (* WITH dn^ DO *)
        INC(entry);
      END;
      dio.end_walk(d,FALSE); warn(" при закрытии директории %d",no);
    END iter_tree;

    VAR i: INTEGER;

  BEGIN
    IF iDirs^.ADR=NIL THEN
      message(0,'Сбор информации о директориях'); make_dirs;
    END;
    NEW(myDirs,SIZE(iDirs));
    ASSERT(myDirs^.ADR#NIL);
(*$<X+*)
    myDirs:=iDirs;
(*$>*)
    message(1,"");
    IF PNO>0 THEN message(0,'Найти в директориях файл %d'  ,PNO)
    ELSE message(0,'Найти в директориях файл "%s"',patt)
    END;
    iter_tree(0,path,0);            -- итерация основного поддерева от корня
    FOR i:=1 TO dio.no_i-1 DO       -- поиск анонимного поддерева с
      IF in?(myDirs,i) THEN         -- нахождением корня
        img.print(path,"...%$6d/",i);
        iter_tree(i,path,10);
      END;
    END;
    DISPOSE(myDirs);
  END find_file;

  PROCEDURE find_block(    desc: BITSET;
                       VAR free: BOOLEAN;
                       patt,start,final: INTEGER): INTEGER;
    VAR i,j,k: INTEGER;
            N: dio.i_node;
            B: POINTER TO ARRAY [0..1023] OF INTEGER;
  BEGIN B:=NIL; ASSERT(final<dio.no_i);
    IF start<0 THEN start:=0 END;
    FOR i:=start TO final DO
      dio.get_inode(i,N);
      IF dio.error THEN warn(" при чтении инода %d",i)
      ELSE
        WITH N DO
          IF (({1}*desc#{}) & (Links >0)) OR
             (({0}*desc#{}) & (Links<=0)) THEN
            FOR j:=0 TO HIGH(Ref) DO
              IF (Ref[j]>=0) & (Ref[j]=patt) THEN
                free:=(Links<=0); RETURN i
              END;
            END;
            IF {dio.lng}*Mode#{} THEN
              FOR j:=0 TO HIGH(Ref) DO
                IF (Ref[j]>=0) & (Ref[j]<dio.no_b) THEN
                  IF B=NIL THEN NEW(B);
                    IF B=NIL THEN
                      message(0," Нет свободной памяти для буффера");
                      RETURN -1
                    END;
                  END;
                  dio.get_block(Ref[j],B,4096);
                  warn(" при чтении блока %d",Ref[j]);
                  FOR k:=0 TO HIGH(B^) DO
                    IF (B^[k]>=0) & (B^[k]=patt) THEN
                      free:=(Links<=0); DISPOSE(B); RETURN i
                    END;
                  END;
                END;
              END;
            END;
          END;
        END;
      END;
      IF i MOD 64 =0 THEN
        message(1,'%$2d%%',i*100 DIV dio.no_i)
      END;
    END;
    IF B#NIL THEN DISPOSE(B) END;
    RETURN -1;
  END find_block;

BEGIN
  NEW(iBusy); NEW(iDirs); NEW(bBusy);
  NEW(inDirs); NEW(iLinks);
END disks;

PROCEDURE app_dec(w: SYSTEM.WORD; len: INTEGER;
              VAR s: ARRAY OF CHAR; VAR cc: INTEGER);
  VAR i,l,one: INTEGER;
BEGIN ASSERT(len<=10); i:=len-1;
  IF INTEGER(w)<0 THEN w:=-INTEGER(w); s[0]:="-"; l:=1 ELSE l:=0 END;
  WHILE i>=l DO
    one:=INTEGER(w) MOD 10; w:=INTEGER(w) DIV 10;
    s[cc+i]:=CHAR(ORD('0')+one); DEC(i);
  END;
  INC(cc,len);
END app_dec;

PROCEDURE monitor(vt52: BOOLEAN; VN: ARRAY OF CHAR);

  PROCEDURE inp_str(VAR str: ARRAY OF CHAR;
                    VAL pmt: ARRAY OF CHAR;
                    SEQ arg: SYSTEM.WORD): BOOLEAN;
    VAR crs: INTEGER;
  BEGIN
    crs:=tty.state^.cursor;
    tty.set_cursor(1);
    tty.set_pos(6,30);
    tty.set_reverse(1);
    tty.print(pmt,arg);
    sle.edit_str("",str,8,30,61,DESC,033c);
    tty.set_reverse(0);
    tty.set_pos(6,30); tty.erase_line(0);
    tty.set_pos(8,30); tty.erase_line(0);
    tty.set_cursor(crs);
    RETURN (DESC^.last#033c) & (str#"");
  END inp_str;

  PROCEDURE inp_num(VAL pmt: ARRAY OF CHAR;
                    from,to: INTEGER; VAR nm: INTEGER): BOOLEAN;
    VAR bump: ARRAY [0..31] OF CHAR; cc: INTEGER;
  BEGIN
    IF inp_str(bump,"%s in range [%d..%d]",pmt,from,to) THEN
      tty.set_cursor(0);
      IF nums.int_expr(bump,0,cc)<0 THEN message(0,nums.message)
      ELSIF (cc>=from) & (cc<=to)   THEN nm:=cc; RETURN TRUE
      ELSE message(0,'Число %d за границами диапазона',cc)
      END
    END;
    RETURN FALSE
  END inp_num;

  CONST lines = ARRAY OF INTEGER
                    --  0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
                      {00,01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17};

  VAR marks: ARRAY [0..HIGH(lines)] OF STRING;

  PROCEDURE app_mark(no: INTEGER; VAL mark: ARRAY OF CHAR);
  BEGIN
    NEW(marks[no],HIGH(mark)+1); ASSERT(marks[no]^.ADR#NIL);
(*$X+*)
    marks[no]:=mark;
(*$X-*)
  END app_mark;

  PROCEDURE ref_pos(no: INTEGER; mark: BOOLEAN);
  BEGIN
    tty.set_pos(lines[no],0);
    IF NOT vt52 & mark THEN tty.set_reverse(1) END;
    IF mark & vt52 THEN tty.print('<<%s>>',marks[no])
    ELSE tty.print('  %s  ',marks[no])
    END;
    IF mark THEN tty.set_reverse(0) END;
    tty.set_pos(lines[no],0);
  END ref_pos;

  VAR blocks, inodes: BITSET;
      b_from, b_to: INTEGER;
      i_from, i_to: INTEGER;

  CONST su_cl = ARRAY OF INTEGER {28,38,48};

  PROCEDURE ref_set_up(   ln: INTEGER;
                        desc: BITSET;
                       fr,to: INTEGER);

    PROCEDURE mark(s: ARRAY OF CHAR; on: BOOLEAN);
    BEGIN
      IF vt52 THEN
        IF on THEN tty.print('<%s>',s)
        ELSE       tty.print(' %s ',s)
        END;
      ELSE
        IF on THEN tty.set_reverse(1) END;
        tty.print(' %s ',s);
        IF on THEN tty.set_reverse(0) END
      END;
    END mark;

    VAR cc: INTEGER; bump: ARRAY [0..23] OF CHAR;

  BEGIN
    tty.set_pos(ln,su_cl[0]);  mark('FREE',{0}*desc#{});
    tty.set_pos(ln,su_cl[1]);  mark('BUSY',{1}*desc#{});
    tty.set_pos(ln,su_cl[2]);
    bump:="RANGE [000000..000000]";
    cc:=07; app_dec(fr,6,bump,cc);
    cc:=15; app_dec(to,6,bump,cc);
    tty.WriteString(bump)
  END ref_set_up;

  PROCEDURE ed_set_up(   ln: INTEGER;
                  VAR  desc: BITSET;
                  VAR fr,to: INTEGER;
                      st,fn: INTEGER);

    VAR pos,cc: INTEGER;
            ch: CHAR;

  BEGIN pos:=0;
    message(0,"Для выхода нажмите ESC");
    ref_set_up(ln,desc,fr,to);
    tty.set_cursor(1);
    LOOP
      tty.set_pos(ln,su_cl[pos]);
      kbr.read(ch);
      IF    ch=kbr.left  THEN pos:=(pos+2) MOD 3;
      ELSIF ch=kbr.right THEN pos:=(pos+1) MOD 3;
      ELSIF ch=033c      THEN EXIT
      ELSIF ch=kbr.cr    THEN
        CASE pos OF
          |0: desc:=desc/{0}; ref_set_up(ln,desc,fr,to);
          |1: desc:=desc/{1}; ref_set_up(ln,desc,fr,to);
          |2:
            IF  inp_num("Start block",st,fn,fr) THEN
              ref_set_up(ln,desc,fr,to);
            END;
            IF  inp_num("Final block",st,fn,cc) THEN
              IF cc<fr THEN to:=fr; fr:=cc ELSE to:=cc END;
              ref_set_up(ln,desc,fr,to);
            END;
        END;
      END;
    END;
    message(0,"");
    tty.set_pos(ln,20); tty.erase_line(0);
    tty.set_cursor(0)
  END ed_set_up;

  VAR    current: INTEGER;

  PROCEDURE refresh;
    VAR i: INTEGER; on: BOOLEAN;
  BEGIN
    tty.set_cursor(0);
    tty.home; tty.erase(0);
    tty.set_pos(0,21); tty.print(version);
    tty.set_pos(2,21);
    IF dio.vol_name="" THEN
      tty.print('no any disk mounted');
    ELSE
      tty.print('disk: "%s": ',dio.vol_name);
      tty.set_pos(3,21);
      tty.print('size %d (logical %d/%d total/free) blocks,'
              ,dio.d_size, dio.no_b,dio.free_b);
      tty.set_pos(4,21);
      tty.print('files %d(%d free), system area 0..%d blocks.',
                dio.no_i, dio.free_i, dio.sys_0);
    END;
    FOR i:=0 TO HIGH(lines) DO ref_pos(i,FALSE) END;
    ref_pos(current,TRUE)
  END refresh;

  PROCEDURE up_pos;
  BEGIN
    ref_pos(current,FALSE);
    current:=(current+HIGH(lines)) MOD SIZE(lines);
    ref_pos(current,TRUE);
  END up_pos;

  PROCEDURE dw_pos;
  BEGIN
    ref_pos(current,FALSE);
    current:=(current+1) MOD SIZE(lines);
    ref_pos(current,TRUE);
  END dw_pos;

  PROCEDURE srch(VAL patt: ARRAY OF CHAR);
    VAR cc,b,i: INTEGER;
          bump: ARRAY [0..79] OF CHAR;
          buff: POINTER TO ARRAY [0..4095] OF CHAR;
          x,ch: CHAR;
  BEGIN
    search.prepare(patt);
    message(0,'Найти образец "%s".',patt);
    b:=b_from; buff:=NIL;
    WHILE (b<dio.no_b) & (b<=b_to) DO
      IF b MOD 16 = 0 THEN message(1,"%d",b) END;
      IF (kbr.ready()>0) THEN
        kbr.read(x);
        IF x=033c THEN RETURN END
      END;
      IF (    in?(dio.bSET,b) & ({0}*blocks#{})) OR
         (NOT in?(dio.bSET,b) & ({1}*blocks#{})) THEN
        IF buff=NIL THEN mem.ALLOCATE(buff,1024);
          IF buff=NIL THEN message(0,'Sorry, no free memory'); RETURN END;
        END;
        dio.get_block(b,buff,4096); warn(" при чтении блока %d",b);
        i:=search.search(buff,0,4095);
        IF i>=0 THEN
          img.print(bump," образец найден в блоке %d.",b);
          notes.show(bump); tty.print(" Показать [Y/N/ESC] ? ",b);
          LOOP kbr.read(ch);
            IF ch=033c THEN RETURN END;
            ch:=CAP(ch);
            IF    ch="Y" THEN
              editors.edit_block(b,i); refresh;
              search.prepare(patt); EXIT
            ELSIF ch="N" THEN EXIT
            ELSE kbr.bell(1);
            END;
          END;
        END;
      END;
      INC(b);
    END;
    IF buff#NIL THEN mem.DEALLOCATE(buff,1024) END;
    message(1,"");
  END srch;

  PROCEDURE done;
  BEGIN
    message(0,"...Done...");
    Time.delay(300,Time.milisec);
    message(0,"");
    Time.delay(200,Time.milisec);
    message(0,"...Done...");
  END done;

  PROCEDURE find_block;
    VAR patt,no: INTEGER; bump: ARRAY [0..79] OF CHAR;
        _free: BOOLEAN;
  BEGIN
    IF inp_num("Block number",0,dio.no_b-1,patt) THEN
      message(0,"Найти файл, содержащий блок %d",patt);
      no:=i_from-1;
      REPEAT
        no:=disks.find_block(inodes,_free,patt,no+1,i_to);
        IF no>0 THEN
          img.print(bump,' Блок %d найден в ',patt);
          IF _free THEN img.append(bump,'свободном '); END;
          img.append(bump,'файле %d.',no);
          notes.show(bump); tty.print(' Искать дальше ?');
          IF NOT _query() THEN no:=-1 END;
        END;
      UNTIL no<0;
      done;
    END;
    message(1,"");
  END find_block;

  VAR HELP: BOOLEAN;

  PROCEDURE help(pos: INTEGER);

    PROCEDURE editor(kind: ARRAY OF CHAR);
    BEGIN
      tty.print(
        'Редактирует %s с введенным номером.\n'
        'Вводите номер в указанном интервале!',kind);
    END editor;

    VAR x: CHAR;
  BEGIN
    tty.set_pos(notes.line,0); tty.erase(0);
    CASE pos OF
      |00:
        tty.home; tty.erase(0);
        tty.print("                     %s\n\n",version);
        tty.print(
          '     В основном меню:\n'
          ' UP, DOWN  -- перемещения маркера\n'
          ' CR, SPACE -- исполнение выбранного действия\n'
          ' F1 -- печать подсказки о выбранном действии\n');
        tty.print(
          '     Для быстрого выбора альтернативы нажмите "горячую клавишу",\n'
          'указанную  после ее имени в скобках "[]" или "()";  в  последнем\n'
          'случае альтернатива исполняется только после нажатия CR.\n'
          );
        tty.print(
          '     При  вводе  параметров  к альтернативе будьте внимательны к\n'
          'к подсказке: неправильный, пустой ввод, а также  нажатие  ESC  в\n'
          'ESC в ходе ввода обозначают отказ от исполнения.\n\n');
        tty.print(
          '     При вводе числового параметра можно использовать выражения:\n'
          ' числа: в восьмеричной (с суффиксом "b"), десятичной и\n'
          '        шестнадцатеричной (с суффиксом "h") формах;\n');
        tty.print(
          ' операции: "*" - умножение, "/" - деление, "%" - модуль,\n'
          '           "+" - сложение , "-" - вычитание или изменение знака;\n'
          ' скобки "()" меняют традиционный порядок действий.\n\n');
        tty.print(
          '     Во всех редакторах для получения подсказки используйте F1.\n\n'
          '              ДЛЯ ПРОДОЛЖЕНИЯ НАЖМИТЕ ЛЮБУЮ КЛАВИШУ');

        kbr.read(x); refresh; ref_pos(current,TRUE)
      |01:
        tty.print(
          '     Завершает работу с утилитой, записывая модифицированные в\n'
          'ходе работы буффера на диск.  Для аварийного завершения работы\n'
          'используйте CTRL C.'
          );
      |02:
        tty.print(
           '     Завершает работу на текущем диске, закрывая буфферы и\n'
           'записывая модификации и предлагает перейти на другой диск.\n'
           'Если новый диск не смонтировался, то СТАРОЕ  СОСТОЯНИЕ  НЕ\n'
           'ВОССТАНАВЛИВАЕТСЯ!'
           );
      |03:
        tty.print(
           '     Проверяет файловую систему на текущем диске и.\n'
           'сообщает об обнаруженных ошибках.');
      |04:
        tty.print(
           '     Проверяет и корректирует файловую систему на текущем\n'
           'диске, запрашивая подтверждения оператора  на  коррекции.\n'
           'Ответы: (y|n|a|A|i|I) = (Да|Нет|Все на текущем этапе|Все|\n'
           '|Пропустить текущий этап|Пропустить все ошибки)'
                 );
      |05:
        tty.print(
           '     Читает блок за блоком весь диск, сообщает об ошибках\n'
           'чтения и предлагает закрыть плохие блоки (включить их в\n'
           'состав файла BAD.BLOCKS).'
                  )
      |06:
        tty.print(
           '     Просмотр ПРОТОКОЛА, если он не пуст. При просмотре\n'
           'протокола F1 - подсказка.');
      |07:
        tty.print(
           '     Удаление всех сообщений из ПРОТОКОЛА. Рекомендуется\n'
           'использовать при нехватке памяти.');
      |08: editor("инод");
      |09: editor("блок");
      |10: editor("файл");
      |11: editor("директорию");
      |12:
        tty.print(
           '     Установка параметров ОБЛАСТИ БЛОКОВ. "BUSY" и "FREE"\n'
           'означает искать в занятых/свободных  блоках  в  диапазоне\n'
           '"range". CR - изменить выбранный раздел. ESC - выход.');
      |13:
        tty.print(
           '     Установка параметров ОБЛАСТИ ФАЙЛОВ. "BUSY" и "FREE"\n'
           'означает искать в занятых/свободных  файлах  в  диапазоне\n'
           '"range". CR - изменить выбранный раздел. ESC - выход.');
      |14:
        tty.print(
           '     Поиск  блоков  из ОБЛАСТИ БЛОКОВ, содержащих указанную\n'
           'строку. Найденные блоки предлагается просмотреть редактором\n'
           'блока. Для прекращения поиска нажмите ESC.');
      |15:
        tty.print(
           '    Поиск файлов из ОБЛАСТИ ФАЙЛОВ, содержащих блок с\n'
           'указанным номером.\n'
           '        Вводите номер в указанном интервале!'
           );
      |16:
        tty.print(
           '    Поиск всех путей от корня до файла с указанным номером.\n'
           'Сообщения в виде { /[num]name }, где num - номер  файла,  а\n'
           'name - имя узла.\n'
           '           Вводите номер в указанном интервале!');
      |17:
        tty.print(
           '    Поиск всех путей от корня до файла с указанным именем.\n'
           'Сообщения в виде { /[num]name }, где num - номер  файла, а\n'
           'name - имя узла.\n'
           );
    END;
    HELP:=TRUE
  END help;

  CONST all_active  = {0..17};
        no_any_disk = {0..02};

  VAR final: BOOLEAN;
     active: BITSET;

  PROCEDURE edit;
    VAR no: INTEGER; bump: ARRAY [0..255] OF CHAR;
  BEGIN
    IF {current}*active={} THEN
      message(0,"this function is banned now");
      RETURN
    END;
    CASE current OF
      |00: help(current)
      |01: final:=TRUE
      |02: IF inp_str(bump,"%|30.30s","Enter disk device name") THEN
             dio.release;
             active:=all_active;
             IF dio.error THEN
               notes.print(dio.message);
               active:=no_any_disk
             ELSE
               errors.release;
                disks.release;
               dio.mount(bump);
               warn(" при монтировании носителя ")
             END;
             refresh
           END;
      |03: disks.check_volume(FALSE); done
      |04: disks.check_volume(TRUE);  done
      |05: IF disks.verify_disk() THEN
             message(0,"Закрыты занятые блоки, СКОРРЕКТИРУЙТЕ ДИСК!")
           ELSE done
           END
      |06: tty.set_cursor(1);
           IF notes.edit() THEN refresh
           ELSE message(0,"No reports in protocol")
           END;
           tty.set_cursor(0)
      |07: notes.release; done
      |08: IF inp_num("Inode number",0,dio.no_i-1,no) THEN
             editors.edit_inode(no); refresh
           END;
      |09: IF inp_num("Block number",0,dio.d_size-1,no) THEN
             editors.edit_block(no,0); refresh
           END;
      |10: IF inp_num("File number",0,dio.no_i-1,no) THEN
             editors.edit_file(no,0); refresh;
           END;
      |11: IF inp_num("File number",0,dio.no_i-1,no) THEN
             editors.edit_dir(no); refresh;
           END;
      |12: ed_set_up(lines[current],blocks,b_from,b_to,0,dio.d_size-1)
      |13: ed_set_up(lines[current],inodes,i_from,i_to,0,dio.no_i  -1)
      |14: IF inp_str(bump,"%|30.30s","Pattern for search") THEN
             srch(bump); done
           END
      |15: find_block
      |16: IF inp_num("File number",0,dio.no_i-1,no) THEN
             disks.find_file("",no,notes.show); done
           END;
      |17: IF inp_str(bump,"%|30.30s","File name") THEN
             disks.find_file(bump,-1,notes.show); done
           END
    END;
    ref_pos(current,TRUE);
  END edit;

  CONST function = ARRAY OF CHAR {"h","x","v","c","C","V","r","e","i"
                                 ,"b","f","d","B","F","p","l","u","n"};

  PROCEDURE init_menu;
  BEGIN
    app_mark( 0,"    HELP [h]    ");
    app_mark( 1,"    EXIT [x]    ");
    app_mark( 2,"CHANGE  DISK (v)");
    app_mark( 3,"CHECK   DISK (c)");
    app_mark( 4,"CORRECT DISK (C)");
    app_mark( 5,"VERIFY  DISK (V)");
    app_mark( 6,"LOOK   NOTES [r]");
    app_mark( 7,"ERASE  NOTES [e]");
    app_mark( 8,"EDIT   INODE [i]");
    app_mark( 9,"EDIT   BLOCK [b]");
    app_mark(10,"EDIT   FILE  [f]");
    app_mark(11,"EDIT   DIR   [d]");
    app_mark(12,"BLOCKS AREA  [B]");
    app_mark(13,"FILES  AREA  [F]");
    app_mark(14,"FIND PATTERN IN AREA [p]");
    app_mark(15,"FIND FILE BY BLOCK   [l]");
    app_mark(16,"FIND PATH BY NUMBER  [u]");
    app_mark(17,"FIND PATH BY NAME    [n]");
  END init_menu;

  VAR ch: CHAR; i: INTEGER;

BEGIN
  init_menu;
  active:=no_any_disk;
  IF VN#"" THEN
    dio.mount(VN);
    IF dio.error THEN
      notes.print('mount("%s"): %s;    HIT KEY',VN,dio.message);
      kbr.read(ch);
    ELSE active:=all_active
    END
  END;
  current:=0;
  blocks:={0,1};      inodes:={0,1};
  b_from:=dio.sys_0;  i_from:=0;
  b_to:=dio.no_b-1;   i_to:=dio.no_i-1;

  refresh;   final:=FALSE;   HELP:=FALSE;
  message(0,'Для получения подсказки нажмите "h"');

  REPEAT
    kbr.read(ch);

    IF HELP THEN
      tty.set_pos(notes.line,0);
      tty.erase(0); HELP:=FALSE
    END;

    IF    ch=kbr.up THEN up_pos
    ELSIF ch=kbr.dw THEN dw_pos
    ELSIF (ch=kbr.cr) OR (ch=" ") THEN edit
    ELSIF ch=kbr.f1 THEN help(current)
    ELSE i:=0;
      WHILE (i<=HIGH(function)) & (function[i]#ch) DO INC(i) END;
      IF i<=HIGH(function) THEN
        ref_pos(current,FALSE); current:=i;
        ref_pos(current,TRUE);
        IF {i}*{2..5}={} THEN edit END
      ELSE ref_pos(current,TRUE)
      END
    END

  UNTIL final
END monitor;

VAR vt52: BOOLEAN;

BEGIN
  tty.set_reverse(1);
  vt52:=NOT tty.done;
  tty.set_reverse(0);
  sle.new(DESC,16);
  IF HIGH(tskArgs.words)<0 THEN monitor(vt52,"")
  ELSE                          monitor(vt52,tskArgs.words[0])
  END;
  dio.release
END fsdb.
