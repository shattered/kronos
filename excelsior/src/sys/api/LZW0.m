IMPLEMENTATION MODULE LZW0; (*$T+ Leo  20-Jun-91. (c) KRONOS *)
                            (*$N+ Andy 10-Apr-91. (c) KRONOS *)

IMPORT       SYSTEM;
IMPORT  cod: defCodes;
IMPORT  err: defErrors;
IMPORT  low: lowLevel;
IMPORT  mem: Heap;
IMPORT  bio: BIO;

--IMPORT  tty: Terminal;
(*
 * compress.c - File compression ala IEEE Computer, June 1984.
 *
 * Authors: Spencer W. Thomas (decvax!harpo!utah-cs!utah-gr!thomas)
 *  Jim McKie  (decvax!mcvax!jim)
 *  Steve Davies  (decvax!vax135!petsd!peora!srd)
 *  Ken Turkowski  (decvax!decwrl!turtlevax!ken)
 *  James A. Woods  (decvax!ihnp4!ames!jaw)
 *  Joe Orost  (decvax!vax135!petsd!joe)
 *
 *)

TYPE int= INTEGER;
     set= BITSET;

CONST BC= 8;
      B = BYTES(BITSET);
      BS= BITS(BITSET);

      CBITS      = 16;
      CHECK_GAP  = 10000;  (* ratio check interval *)

      EOFs       =  ARRAY OF INTEGER {4096,8192,16384,32768,47000,MAX(INTEGER)};
      HSIZEs     =  ARRAY OF INTEGER {5003,9001,18013,35023,50021,69001};
      HSIZE      =  HSIZEs[HIGH(HSIZEs)];

(* Defines for byte of header *)
CONST   BIT_MASK = {0..4};
CONST BLOCK_MASK = {7};

CONST INIT_BITS= 9; (* initial number of bits/code *)

VAR n_bits    : INTEGER; (* number of bits/code *)
    maxbits   : INTEGER; (* user settable max # bits/code *)
    maxcode   : INTEGER; (* maximum code, given n_bits *)
    maxmaxcode: INTEGER; (* should NEVER generate this code *)

VAR hsize: INTEGER; (* for dynamic table sizing *)

VAR free_ent: INTEGER; (* first unused entry *)

TYPE TABLE = POINTER TO ARRAY [0..HSIZE-1] OF INTEGER;

(*
 * block compression parameters -- after all codes are used up,
 * and compression rate changes, start over.
 *)

VAR block_compress: INTEGER;
    clear_flg     : BOOLEAN;
    ratio         : INTEGER;
    checkpoint    : INTEGER;

(*
 * the next two codes should not be changed lightly, as they must not
 * lie within the contiguous general code space.
 *)

CONST FIRST= 257; (* first free entry *)
CONST CLEAR= 256; (* table clear output code *)


------------------------------ I/O -----------------------------------
                               ---

TYPE BUFFER = POINTER TO ARRAY [0..8191] OF CHAR;

VAR
  inp : bio.FILE;       out : bio.FILE;
  ipos: INTEGER;        opos: INTEGER;        (* saved positions *)
  ibuf: BUFFER;         oeof: INTEGER;
  icnt: INTEGER;        obuf: BUFFER;
  ibnd: INTEGER;        ocnt: INTEGER;
  irem: INTEGER;   iobufsize: INTEGER;     (* iobuf=BYTES(ibuf)=BYTES(obuf) *)

PROCEDURE iodeallocate;
BEGIN
  mem.deallocate(ibuf,iobufsize DIV 4);
  mem.deallocate(obuf,iobufsize DIV 4);
END iodeallocate;

PROCEDURE iallocate;
BEGIN
  iobufsize:=BYTES(ibuf^);
  ibuf:=NIL;
  obuf:=NIL;
  REPEAT
    mem.allocate(ibuf,iobufsize DIV 4);
    IF mem.done THEN RETURN END;
    iobufsize:=iobufsize DIV 2;
  UNTIL iobufsize<512;
  done:=FALSE; error:=err.no_memory
END iallocate;

PROCEDURE ioallocate;
BEGIN
  iobufsize:=BYTES(ibuf^);
  ibuf:=NIL;
  obuf:=NIL;
  REPEAT
    mem.allocate(ibuf,iobufsize DIV 4);
    IF mem.done THEN
      mem.allocate(obuf,iobufsize DIV 4);
      IF mem.done THEN RETURN ELSE iodeallocate END
    END;
    iobufsize:=iobufsize DIV 2;
  UNTIL iobufsize<512;
  done:=FALSE; error:=err.no_memory
END ioallocate;

PROCEDURE input;
  VAR len: INTEGER;
BEGIN
  IF irem<=0          THEN ibnd:=0;  icnt:=0;  RETURN      END;
  IF irem<iobufsize   THEN len:=irem ELSE len:=iobufsize END;
  bio.get(inp,ibuf^,len);  ibnd:=bio.iolen;
  IF NOT bio.done THEN done:=FALSE; error:=bio.error END;
  icnt:=0;
  DEC(irem,len)
END input;

PROCEDURE output;
BEGIN
  IF ocnt=0 THEN RETURN END;
  bio.put(out,obuf^,ocnt);
  ocnt:=0;
  IF NOT bio.done THEN done:=FALSE; error:=bio.error END
END output;


--------------------------- Codes I/O --------------------------------
                            ---------
VAR offset: INTEGER;
   slot_sz: INTEGER;
        bs: INTEGER;

PROCEDURE bblt(des,dofs,sou,sofd,bits: SYSTEM.ADDRESS);
CODE
  cod.bblt
END bblt;

PROCEDURE init_code_output(hsize: INTEGER; info: CHAR);
  VAR i: INTEGER;
BEGIN
  low._zero(obuf,iobufsize DIV 4);
  obuf^[0]:='L';
  obuf^[1]:='Z';
  obuf^[2]:='W';
  obuf^[3]:=info;
  FOR i:=4 TO  7 DO obuf^[i]:=0c END;
  FOR i:=8 TO 11 DO obuf^[i]:=CHAR(hsize MOD 256); hsize:=hsize DIV 256 END;
  bs:=12;
  offset   :=bs*BC;
  slot_sz  :=bs*BC+n_bits*BC
END init_code_output;

PROCEDURE close_code_output(VAR len: INTEGER);
  VAR i,j: INTEGER;
      pos: INTEGER;
     info: ARRAY [0..3] OF CHAR;
BEGIN
  pos:=bio.pos(out);
  len:=pos-opos;
  bio.seek(out,opos+4,0);
  IF NOT bio.done THEN done:=FALSE; error:=bio.error; RETURN END;
  j:=len;
  FOR i:=0 TO 3 DO info[i]:=CHAR(j MOD 256); j:=j DIV 256 END;
  bio.put(out,info,4);
  IF NOT bio.done THEN done:=FALSE; error:=bio.error; RETURN END;
  bio.seek(out,pos,0);
  IF NOT bio.done THEN done:=FALSE; error:=bio.error; RETURN END
END close_code_output;

PROCEDURE init_code_input(VAR hsize: INTEGER);
  VAR i: INTEGER;
   info: ARRAY [0..11] OF CHAR;
BEGIN
  bio.get(inp,info,12);
  IF NOT bio.done THEN done:=FALSE; error:=bio.error; RETURN END;
  IF (info[0]#'L') OR (info[1]#'Z') OR (info[2]#'W') THEN
    done:=FALSE; error:=err.ill_desc; RETURN
  END;

  irem :=0;
  hsize:=0;
  FOR i:= 7 TO 4 BY -1 DO irem :=irem *256+int(info[i]) END;
  FOR i:=11 TO 8 BY -1 DO hsize:=hsize*256+int(info[i]) END;
  maxbits:=int(info[3]);
  IF maxbits=377b THEN RETURN END;
  block_compress:=int(set(maxbits)*BLOCK_MASK);
  maxbits:=int(set(maxbits)*BIT_MASK);
  maxmaxcode:=1<<maxbits;
  IF maxbits > CBITS THEN done:=FALSE; error:=err.inconsistency; RETURN  END;

  DEC(irem,12);
  bs      :=0;
  offset  :=0;
  slot_sz :=0;
  ibnd    :=0;
END init_code_input;

(*****************************************************************
 * TAG( output )
 *
 * Output the given code.
 * Inputs:
 *    code: A n_bits-bit integer.  If == -1, then EOF.  This assumes
 *    that n_bits =< (long)wordsize - 1.
 * Outputs:
 *     Outputs code to the file.
 * Assumptions:
 *     Chars are 8 bits long.
 * Algorithm:
 *     Maintain a CBITS character long buffer (so that 8 codes will
 * fit in it exactly).  Use the VAX insv instruction to insert each
 * code in turn.  When the buffer fills up empty it and start over.
 *)

PROCEDURE flush(bits: INTEGER);
BEGIN
  bio.put(out,obuf^,(bits+BC-1) DIV BC);
  bs:=0; offset:=0;
  slot_sz:=n_bits*BC;
  low._zero(obuf,iobufsize DIV 4)
END flush;

PROCEDURE outEOF;
BEGIN
  IF offset>0 THEN flush(offset) END
END outEOF;

PROCEDURE outcode(code: INTEGER);
BEGIN
  bblt(obuf,offset,SYSTEM.ADR(code),0,n_bits);
  INC(offset,n_bits);
  IF offset >= slot_sz THEN
    IF iobufsize*BC-slot_sz >= CBITS*BC THEN
      INC(bs,n_bits);
      INC(slot_sz,n_bits*BC)
    ELSE
      flush(offset)
    END
  END;
 (* If the next entry is going to be too big for the code size,
  * then increase it, if possible.
  *)
  IF (free_ent<=maxcode) & NOT clear_flg THEN RETURN END;
 (*
  * Write the whole buffer, because the input side won't
  * discover the size increase until after it has read it.
  *)
  IF offset>bs*BC THEN (* (offset-bs*BC)>0 *)
    IF iobufsize*BC-slot_sz >= CBITS*BC THEN
      INC(bs,n_bits);
      INC(slot_sz,n_bits*BC);
      offset:=bs*BC
    ELSE
      flush(slot_sz)
    END
  END;
  IF clear_flg THEN
    n_bits:=INIT_BITS;   maxcode:=(1<<INIT_BITS)-1;  clear_flg:=FALSE
  ELSE
    INC(n_bits);
    IF n_bits=maxbits THEN maxcode:=maxmaxcode ELSE maxcode:=(1<<n_bits)-1 END
  END;
  slot_sz:=(bs+n_bits)*BC  (* slot_sz depends on n_bits! *)
END outcode;

(*****************************************************************
 * TAG( getcode )
 *
 * Read one code from the input buffer.  If EOF, return -1.
 * Inputs:
 *  buffer "in"
 * Outputs:
 *  code or -1 is returned.
 *)

PROCEDURE getcode(): INTEGER;
  VAR code,sh,brem,req,len,i: INTEGER;
BEGIN
  IF clear_flg OR (offset>=slot_sz) OR (free_ent>maxcode) THEN
   (* If the next entry will be too big for the current code
    * size, then we must increase the size.  This implies reading
    * a new buffer full, too.
    *)
    IF free_ent > maxcode THEN
      INC(n_bits);  (* won't get any bigger now *)
      IF n_bits=maxbits THEN maxcode:=maxmaxcode ELSE maxcode:=(1<<n_bits)-1 END
    END;
    IF clear_flg THEN
      n_bits:=INIT_BITS;  maxcode:=(1<<INIT_BITS)-1;  clear_flg:=FALSE
    END;
    bs:=slot_sz DIV BC;
    offset:=slot_sz;
    brem:=ibnd-bs;
    IF brem >= n_bits THEN (* buffer not empty yet *)
      INC(slot_sz,n_bits*BC)
    ELSE
      FOR i:=0 TO brem-1 DO ibuf^[i]:=ibuf^[bs+i] END;
      bs:=0; offset:=0;
      ibnd:=brem;
      req:=iobufsize-brem;
      IF req>irem THEN req:=irem END;
      bio.fread(inp,ibuf,brem,req);
      IF NOT bio.done THEN done:=FALSE; error:=bio.error; RETURN -1 END;
      len:=bio.iolen;
      INC(ibnd,len);
      DEC(irem,len);
      i:=ibnd;  (* i=min(ibnd,n_bits) *)
      IF i>n_bits THEN i:=n_bits END;
      slot_sz:=(i*BC DIV n_bits)*n_bits;
      IF slot_sz<=0 THEN RETURN -1 END
    END
  END;

  code:=0;
  bblt(SYSTEM.ADR(code),0,ibuf,offset,n_bits);
  INC(offset,n_bits);
  RETURN code
END getcode;


(*****************************************************************
 * TAG( main )
 *
 * Algorithm from "A Technique for High Performance Data Compression",
 * Terry A. Welch, IEEE Computer Vol 17, No 6 (June 1984), pp 8-19.
 *
 * Algorithm:
 *     Modified Lempel-Ziv method (LZW).  Basically finds common
 * substrings and replaces them with a variable size code.  This is
 * deterministic, and can be done on the fly.  Thus, the decompression
 * procedure needs no input table, but tracks the way the table was built.
 *****************************************************************
 *)

(*
 * compress
 *
 * Algorithm:  use open addressing double hashing (no chaining) on the
 * prefix code / next character combination.  We do a variant of Knuth's
 * algorithm D (vol. 3, sec. 6.4) along with G. Knott's relatively-prime
 * secondary probe.  Here, the modular division first probe is gives way
 * to a faster exclusive-or manipulation.  Also do block compression with
 * an adaptive reset, whereby the code table is cleared when the compression
 * ratio decreases, but after the table fills.  The variable-length output
 * codes are re-sized at this point, and a special CLEAR code is generated
 * for the decompressor.  Late addition:  construct the table according to
 * file size for noticeable speed improvement on small files.  Please direct
 * questions about this implementation to ames!jaw.
 *
 *)

PROCEDURE pack(unpk,pk: bio.FILE; in: INTEGER; VAR len: INTEGER; mm: BOOLEAN);

  VAR in_count: INTEGER;

  PROCEDURE cl_block(htab: TABLE); (* table clear for block compress *)
    VAR rat,bytes_out: INTEGER;
  BEGIN
    checkpoint:=in_count+CHECK_GAP;
    bytes_out:=bio.pos(out)+(offset+BC-1) DIV BC;
    IF in_count > 007FFFFFh THEN (* shift will overflow *)
      rat:=bytes_out DIV 256;
      IF rat=0 THEN rat:=07FFFFFFFh ELSE rat:=in_count DIV rat END
    ELSE rat:=(in_count*256) DIV bytes_out; (* 8 fractional bits *)
    END;
    IF rat > ratio THEN ratio:=rat; RETURN END;
    ratio:=0;
    low._fill(htab,hsize,-1); (* clear hash table *)
    free_ent:=FIRST;
    clear_flg:=TRUE;
    outcode(CLEAR)
  END cl_block;

  PROCEDURE compress(htab,codetab: TABLE);
    VAR fcode,i,c,ent,disp,bump,hshift,limit: INTEGER;
  BEGIN
    n_bits    :=INIT_BITS;
    maxbits   :=CBITS;
    ratio     :=0;
    clear_flg :=FALSE;
    checkpoint:=CHECK_GAP;
    maxcode   :=(1<<n_bits)-1;
    maxmaxcode:=1<<CBITS; (* should NEVER generate this code *)

    IF block_compress#0 THEN free_ent:=FIRST ELSE free_ent:=256 END;

    hshift:=0;
    fcode:=hsize;
    WHILE fcode < 65536 DO INC(hshift); fcode:=fcode * 2 END;
    hshift:=8-hshift;

    limit:=1;
    WHILE limit<hsize DO limit:=limit*2 END;  limit:=limit DIV 2;

    low._fill(htab,hsize,-1); (* clear hash table *)

    irem:=in;  input;  (* init_byte_input *)
    init_code_output(hsize,CHAR(set(maxbits)+set(block_compress)));

    IF icnt>=ibnd THEN outEOF; RETURN END;
    ent:=int(ibuf^[icnt]);  INC(icnt);  in_count:=1;

    LOOP
      (* getbyte(b) *)
      IF  icnt<ibnd THEN c:=int(ibuf^[icnt]); INC(icnt);  INC(in_count)
      ELSIF irem<=0 THEN EXIT
      ELSE       input;  c:=int(ibuf^[icnt]); INC(icnt);  INC(in_count)
      END;

      fcode:=(c<<maxbits)+ent;
      i:=int(set(c<<hshift) / set(ent)); -- xor hashing
      bump:=htab^[i];
      IF (bump#fcode) & (bump>=0) THEN
        disp:=hsize-i;  (* secondary hash (after G. Knott) *)
        IF i=0 THEN disp:=1 END;
        REPEAT
          IF i<disp THEN INC(i,hsize-disp) ELSE DEC(i,disp) END;
          bump:=htab^[i]
        UNTIL (bump=fcode) OR (bump<0)
      END;
      IF bump=fcode THEN
        ent:=codetab^[i]
      ELSE
        outcode(ent);
        ent:=c;
        IF free_ent < limit THEN (* code -> hashtable *)
          codetab^[i]:=free_ent;  htab^[i]:=fcode;  INC(free_ent);
        ELSIF (in_count>=checkpoint) & (block_compress#0) THEN
          cl_block(htab)
        END
      END
    END;
    outcode(ent);  outEOF  (* Put out the final code. *)
  END compress;

  VAR  htab: TABLE;
    codetab: TABLE;

  PROCEDURE allocate;
    VAR i,eof: INTEGER;
  BEGIN
    eof:=in;
    IF mm THEN
      i:=HIGH(EOFs);
      WHILE (i>=0) & (eof<=EOFs[i]) DO DEC(i) END;
      INC(i)
    ELSE
      i:=0
    END;
    REPEAT
      hsize:=HSIZEs[i];
      mem.allocate(htab,hsize);
      IF mem.done THEN mem.allocate(codetab,hsize) END;
      IF NOT mem.done THEN DEC(i) END
    UNTIL (i<0) OR mem.done;
    IF i<0 THEN
      done:=FALSE; error:=mem.error; mem.deallocate(htab,hsize); RETURN
    END;
    ioallocate
  END allocate;

  VAR i,j: INTEGER;
     info: ARRAY [0..11] OF CHAR;

BEGIN
  done:=TRUE;
  block_compress:=int(BLOCK_MASK); (* :=TRUE *)
  inp:=unpk;   ipos:=bio.pos(inp);
  out:=pk;     opos:=bio.pos(out);   oeof:=bio.eof(out);
  htab   :=NIL;
  codetab:=NIL;

  allocate;
  IF done THEN compress(htab,codetab) END;
  IF done THEN close_code_output(len) END;
  mem.deallocate(codetab,hsize);
  mem.deallocate(htab   ,hsize);
  iodeallocate;

  IF NOT done THEN (* undo *)
    bio.seek(inp,ipos,0);
    bio.seek(out,opos,0);
    IF bio.eof(out)#oeof THEN bio.cut(out,oeof) END;
    RETURN
  END;

  IF done & (len<=in+12) THEN RETURN END;

  (* otherwise just copy *)
  bio.seek(out,opos,0);
  bio.seek(inp,ipos,0);
  info:="LZW"377c;
  j:=in;
  FOR i:=4 TO  7 DO info[i]:=CHAR(j MOD 256); j:=j DIV 256 END;
  FOR i:=8 TO 11 DO info[i]:=0c END;
  bio.put(out,info,12);
  copy(inp,out,in); len:=in+12
END pack;

(*
 * Decompress inp to out.  This routine adapts to the codes in the
 * file building the "string" table on-the-fly; requiring no table to
 * be stored in the compressed file.
 *)

PROCEDURE unpack(pk,unpk: bio.FILE; VAR len: INTEGER);

  TYPE SUFFIX = POINTER TO ARRAY [0..1<<CBITS+7999] OF CHAR;

  PROCEDURE decompress(codetab: TABLE; suffix: SUFFIX);
    VAR stackp,finchar,code,oldcode,incode,limit: INTEGER;
  BEGIN
    n_bits:=INIT_BITS;
    maxcode:=(1<<n_bits)-1;

    clear_flg:=FALSE;

    limit:=1;
    WHILE limit<hsize DO limit:=limit*2 END;  limit:=limit DIV 2;

    (* As above, initialize the first 256 entries in the table. *)
    FOR code:=255 TO 0 BY -1 DO
      codetab^[code]:=0;
      suffix ^[code]:=CHAR(code)
    END;
    IF block_compress#0 THEN free_ent:=FIRST ELSE free_ent:=256 END;
    oldcode:=getcode();
    finchar:=oldcode;
    IF oldcode = -1 THEN RETURN END; (* EOF already? - Get out of here *)

    (* first code must be 8 bits = char; putbyte(finchar) *)
    obuf^[ocnt]:=CHAR(finchar);
    INC(ocnt);

    stackp:=(1<<CBITS);
    code:=getcode();
    WHILE code > -1 DO
       IF (code=CLEAR) & (block_compress#0) THEN
         low._zero(codetab,256);
         clear_flg:=TRUE;
         free_ent:=FIRST-1;
         code:=getcode();
         IF code = -1 THEN RETURN (* O, untimely death! *) END;
       END;
       incode:=code;
       (* Special case for KwKwK string. *)
       IF code >= free_ent THEN
         suffix^[stackp]:=CHAR(finchar);
         INC(stackp);
         code:=oldcode
       END;
       (* Generate output characters in reverse order *)
       WHILE code >= 256 DO
         suffix^[stackp]:=suffix^[code];
         INC(stackp);
         code:=codetab^[code]
       END;
       finchar:=int(suffix^[code]);
       suffix^[stackp]:=CHAR(finchar);
       INC(stackp);

       (* And put them out in forward order *)
       REPEAT
         DEC(stackp);
         (* putbyte(suffix^[stackp]); *)
         IF ocnt>=iobufsize THEN output END;
         obuf^[ocnt]:=suffix^[stackp];
         INC(ocnt)
       UNTIL stackp <= (1<<CBITS);

       (* Generate the new entry. *)
       code:=free_ent;
       IF code < limit THEN
         codetab^[code]:=oldcode;
         suffix^[code]:=CHAR(finchar);
         free_ent:=code+1
       END;
       (* Remember previous code. *)
       oldcode:=incode;
       code:=getcode()
    END;
    output
  END decompress;

  VAR
     suffix: SUFFIX;
    codetab: TABLE;

  PROCEDURE allocate;
  BEGIN
    mem.allocate(suffix,SIZE(suffix^));
    IF NOT mem.done THEN done:=FALSE; error:=mem.error END;
    mem.allocate(codetab,hsize);
    IF NOT mem.done THEN done:=FALSE; error:=mem.error END;
    ioallocate
  END allocate;

BEGIN
  done:=TRUE;
  inp:=pk;              ipos:=bio.pos(inp);
  out:=unpk;            opos:=bio.pos(out);    oeof:=bio.eof(out);
  ocnt:=0;           (* init_byte_output *)
  init_code_input(hsize);
  IF NOT done THEN RETURN END;
  IF hsize=0  THEN (* just copy *) copy(inp,out,irem); len:=irem; RETURN END;
  suffix :=NIL;
  codetab:=NIL;
  allocate;
  IF done THEN decompress(codetab,suffix); len:=bio.pos(out)-opos END;
  mem.deallocate(codetab,hsize);
  mem.deallocate(suffix,SIZE(suffix^));
  iodeallocate;
  IF NOT done THEN (* undo *)
    bio.seek(inp,ipos,0);
    bio.seek(out,opos,0);
    IF bio.eof(out)#oeof THEN bio.cut(out,oeof) END
  END
END unpack;

PROCEDURE copy(inp,out: bio.FILE; len: INTEGER);
  VAR i: INTEGER;
BEGIN
  done:=TRUE;
  ipos:=bio.pos(inp);
  opos:=bio.pos(out);    oeof:=bio.eof(out);
  iallocate;
  IF NOT done THEN RETURN END;
  IF oeof<opos+len THEN
    bio.extend(out,opos+len);
    IF NOT bio.done THEN done:=FALSE; error:=bio.error; RETURN END
  END;
  WHILE len>0 DO
    IF iobufsize>len THEN i:=len ELSE i:=iobufsize END;
    bio.get(inp,ibuf^,i);
    IF NOT bio.done THEN done:=FALSE; error:=bio.error END;
    IF done THEN
      bio.put(out,ibuf^,i);
      IF NOT bio.done THEN done:=FALSE; error:=bio.error END
    END;
    DEC(len,i)
  END;
  iodeallocate;
  IF NOT done THEN (* undo *)
    bio.seek(inp,ipos,0);
    bio.seek(out,opos,0);
    IF bio.eof(out)#oeof THEN bio.cut(out,oeof) END
  END
END copy;


BEGIN
  ASSERT((CBITS MOD B)=0);
  done :=TRUE;
  error:=0
END LZW0.
