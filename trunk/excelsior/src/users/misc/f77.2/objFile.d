DEFINITION MODULE objFile; (* 14-Feb-89.  *)

(*  Модуль определяет стандарт obj-файла  *)

(* F-code:: begin {proc} end EOF.
   begin :: FILE name.
   end   :: SPOOL size {word}.
   proc  :: main|func|subr|subra|bdata .
   main  :: PROC  name procno                 sequence ENDP.
   func  :: FUNC  name procno noparams type   sequence ENDP.
   subr  :: SUBR  name procno noparams        sequence ENDP.
   subra :: SUBRA name procno noparams noalt  sequence ENDP.
   bdata :: BDATA name procno                 sequence ENDP.

   name  :: {char}0c.

*)

CONST -- оформление obj-файла

  FILE  = 0FFh;   -- name
  SPOOL = 0FEh;   -- size {word}
  EOF   = 0FDh;

CONST -- оформление процедуры

  PROG  = 0F1h;   -- name
  FUNC  = 0F2h;   -- name procno noparams type
  SUBR  = 0F3h;   -- name procno noparams
  SUBRA = 0F4h;   -- name procno noparams noaltr
  BDATA = 0F5h;   -- name procno
  ENTRY = 0F6h;   -- name procno noparams noaltr
  ENDP  = 0FAh;   -- name maplen locals   templen

CONST -- теги компонент

  aRRAY    = 020h;   -- bmoffs moffs  offset
  COMMON   = 021h;   -- name   moffs  length
  EXTERNAL = 022h;   -- name
  CALL     = 023h;   -- name   noparams  noaltr
  DCODE    = 024h;   -- len    {bytes}
  PCODE    = 025h;   -- len    {bytes}
  BCODE    = 026h;   -- len    {bytes}
  LABEL    = 027h;   -- label
  ORJUMP   = 028h;
  ANDJUMP  = 029h;
  eNDLAB   = 02Ah;
  JUMP     = 02Bh;   -- label  fixed
  JUMPC    = 02Ch;   -- label  fixed
  LDFORM   = 02Dh;   -- label
  FORMAT   = 02Eh;   -- label  soffs
  NEWSTM   = 02Fh;
  LPC      = 030h;   -- name

PROCEDURE CreateObj(VAR name:ARRAY OF CHAR);
(* создает obj-файл и записывает заголовок *)

PROCEDURE CloseObj;
(* записывает пул констант, EOF и закрывает файл  *)

PROCEDURE OpenObj(VAR name:ARRAY OF CHAR);

PROCEDURE pTag(Tag:INTEGER);
PROCEDURE gTag():INTEGER;
PROCEDURE pName(VAR nm:ARRAY OF CHAR);
PROCEDURE gName(VAR nm:ARRAY OF CHAR);
PROCEDURE pxTag(Tag:INTEGER);
PROCEDURE gxTag():INTEGER;

PROCEDURE pFunc (VAR nm:ARRAY OF CHAR; proc,parms,tp:INTEGER);
PROCEDURE pSubra(VAR nm:ARRAY OF CHAR; proc,parms,aparms:INTEGER);
PROCEDURE pSubr (VAR nm:ARRAY OF CHAR; proc,parms:INTEGER);
PROCEDURE pProg (VAR nm:ARRAY OF CHAR; proc:INTEGER);
PROCEDURE pBData(VAR nm:ARRAY OF CHAR; proc:INTEGER);
PROCEDURE pEntry(VAR nm:ARRAY OF CHAR; proc,parms,aparms:INTEGER);

PROCEDURE gFunc (VAR nm:ARRAY OF CHAR; VAR proc,parms,tp:INTEGER);
PROCEDURE gSubra(VAR nm:ARRAY OF CHAR; VAR proc,parms,aparms:INTEGER);
PROCEDURE gSubr (VAR nm:ARRAY OF CHAR; VAR proc,parms:INTEGER);
PROCEDURE gProg (VAR nm:ARRAY OF CHAR; VAR proc:INTEGER);
PROCEDURE gBData(VAR nm:ARRAY OF CHAR; VAR proc:INTEGER);
PROCEDURE gEntry(VAR nm:ARRAY OF CHAR; VAR proc,parms,aparms:INTEGER);

PROCEDURE pCommon  (VAR nm:ARRAY OF CHAR; moffs,len:INTEGER);
PROCEDURE pCall    (VAR nm:ARRAY OF CHAR; parms,aparms:INTEGER);
PROCEDURE pExternal(VAR nm:ARRAY OF CHAR);
PROCEDURE pLPC     (VAR nm:ARRAY OF CHAR);
PROCEDURE pArray   (bmoffs,moffs,offset:INTEGER);

PROCEDURE gCommon  (VAR nm:ARRAY OF CHAR; VAR moffs,len:INTEGER);
PROCEDURE gCall    (VAR nm:ARRAY OF CHAR; VAR parms,aparms:INTEGER);
PROCEDURE gExternal(VAR nm:ARRAY OF CHAR);
PROCEDURE gLPC     (VAR nm:ARRAY OF CHAR);
PROCEDURE gArray   (VAR bmoffs,moffs,offset:INTEGER);

PROCEDURE pEndproc( VAR nm:ARRAY OF CHAR; maplen,locals,templen:INTEGER);
PROCEDURE gEndproc( VAR nm:ARRAY OF CHAR; VAR maplen,locals,templen:INTEGER);

PROCEDURE pLabel  (label:INTEGER);
PROCEDURE gLabel  (VAR label:INTEGER);

PROCEDURE pORJump;
PROCEDURE pANDJump;
PROCEDURE pENDLab;
PROCEDURE pJump   (label:INTEGER; fixed:BOOLEAN);
PROCEDURE pJumpC  (label:INTEGER; fixed:BOOLEAN);

PROCEDURE pLDForm (label:INTEGER);
PROCEDURE pFormat (label,soffs:INTEGER);
PROCEDURE gFormat (VAR label,soffs:INTEGER);

END objFile.
