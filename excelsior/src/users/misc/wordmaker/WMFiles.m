IMPLEMENTATION MODULE WMFiles;
(* Text File handling, os dependent *)
(* Module for Wordmaker by Andrus Moor started 10-okt-86 *)

FROM WMUtil    IMPORT  Message,InterAct,DROP;
FROM WMBuffer  IMPORT  MoveBytes;
IMPORT FsPublic;
FROM FsPublic  IMPORT  File, VolumeName, dNode;
FROM BIO       IMPORT  OpenOnDir, Create, Close, Link, UnLink, CD,
                       GetEof, SetEof, bRead, bWrite, Ls, Go, GoPath,
                       Open;
FROM Strings   IMPORT  Str0,Str1,Str2,Len,AppStr;
FROM Args      IMPORT  Flag?;
FROM Terminal  IMPORT  Show;
FROM TTYs      IMPORT  Write;
FROM SYSTEM    IMPORT  ADR;

CONST
  blsize=4096;
  one=1;        (*Mod 0 doesnt allow subranges other than zero*)
  MaxInt = 7fffffffh;

VAR
  wf: RECORD
    osFile: File;
    blnr:           CARDINAL; (*Next block to read, current to write*)
    blinx:          CARDINAL; (*First free/not readed CHAR in block*)
    fsize:          CARDINAL; (*File size in bytes for line read*)
    osfn:           FsPublic.FileName;
    block: ARRAY [0..blsize] OF CHAR;
      (*in reading block[blsize]=0c always*)
    END;

PROCEDURE tFNGet( fno: CARDINAL; VAR mess: ARRAY OF CHAR );
VAR i: CARDINAL;
BEGIN
  FOR i:=0 TO HIGH(wf.osfn) DO
    mess[i]:= wf.osfn[i]; END;
END tFNGet;

PROCEDURE tFNSet( fno: CARDINAL;fn: ARRAY OF CHAR );
VAR i: CARDINAL;
BEGIN
  FOR i:=0 TO HIGH(wf.osfn) DO
    wf.osfn[i] := fn[i]; END;
(*wf.osfn := fn *)
END tFNSet;

PROCEDURE ErrProc( fn: ARRAY OF CHAR );

BEGIN
END ErrProc;

PROCEDURE Read_Block( fno: CARDINAL ): BOOLEAN;
(* Read block specified in block # from File. True if read error *)

BEGIN
  IF (wf.fsize-wf.blnr*blsize)>blsize THEN
    (*Read not a last block*)
    IF bRead(wf.osFile, wf.blnr, ADR(wf.block), blsize) THEN RETURN TRUE END;
   ELSE IF
       bRead(wf.osFile, wf.blnr, ADR(wf.block),
                   wf.fsize -wf.blnr*blsize) THEN RETURN TRUE END;
    END;
  INC(wf.blnr);
  wf.blinx := 0;
  RETURN FALSE;
END Read_Block;

PROCEDURE TempFile_Create( fno: CARDINAL ): BOOLEAN;
(* Create a temporary File in current directory *)
BEGIN
  IF Create( wf.osFile ) THEN RETURN TRUE END;
  RETURN FALSE;
END TempFile_Create;

PROCEDURE TempFile_Set( fno: CARDINAL );
(*Setup temporary File for write operations *)
BEGIN
  wf.osfn[0] := 0c; (*no name means temporary File*)
  wf.blnr := 0;
  wf.blinx := 0;
END TempFile_Set;

PROCEDURE Write_Block( fno, cnt: CARDINAL): BOOLEAN;
(* Write current block to File *)
(* True if disk write error *)

BEGIN
  IF (wf.blnr=0) & (wf.osfn[0]=0c) THEN
    IF TempFile_Create(fno) THEN RETURN TRUE END;
    END;
  IF bWrite( wf.osFile, wf.blnr, ADR(wf.block), cnt) THEN RETURN TRUE END;
  INC(wf.blnr);
  wf.blinx:= 0;
  RETURN FALSE;
END Write_Block;

PROCEDURE TempFile_Revert(fno: CARDINAL): BOOLEAN;
(*Revert temporary File from writing to reading*)

BEGIN
  wf.fsize := wf.blnr*blsize +wf.blinx;
  IF wf.blnr>0 THEN
    IF Write_Block(fno,wf.blinx) THEN RETURN TRUE END;
    wf.blnr := 0;
    IF Read_Block(fno) THEN RETURN TRUE END;
   ELSE wf.blnr:= 1; END;
  wf.blinx := 0;
  wf.block[blsize] := 0c;
  RETURN FALSE;
END TempFile_Revert;

PROCEDURE tOpen(fno: CARDINAL): BOOLEAN;
(*Open File for read. True on error *)
VAR filn: FsPublic.FileName;

BEGIN
  filn:= wf.osfn;
  IF Open(wf.osFile,filn,ErrProc) THEN RETURN TRUE END;
  wf.blnr:=0;
  wf.block[blsize]:=0c;
  wf.fsize := GetEof(wf.osFile);
  IF wf.fsize>0 THEN
    IF Read_Block(fno) THEN RETURN TRUE END;
  ELSE (*File is empty*)
    wf.block[0] := 0c;
  END;
  RETURN FALSE;
END tOpen;

PROCEDURE tCreate(fno:CARDINAL): BOOLEAN;
(* If file exists, rename it to backup File. Delete previous backup
file. True if disk write error *)

VAR filn: FsPublic.FileName;
    DestDir: File;
    DestVol: VolumeName;
    BakFName: FsPublic.FileName;

BEGIN
  filn := wf.osfn;
  IF Go(CD(), DestDir, filn, ErrProc) THEN
    RETURN TRUE; END;
  IF Flag?('b') & NOT OpenOnDir( DestDir,wf.osFile,filn) THEN
    (*File already exists*)
    (*rename to backup file*)
    Str1(BakFName, filn);
    AppStr(BakFName, ".b");
    IF Link(DestDir,BakFName,wf.osFile) THEN (*Backup File link error*)
      DROP(Close(wf.osFile));
      RETURN TRUE;
      END;
    (*Delete existing backup file*)
    IF Close(wf.osFile) THEN RETURN TRUE END;
    IF UnLink(DestDir, filn) THEN RETURN TRUE END;
    END;
  IF Create( wf.osFile ) THEN RETURN TRUE END;
  IF Link(DestDir, filn, wf.osFile) THEN RETURN TRUE END;
  IF Close(DestDir) THEN RETURN TRUE END;
  wf.blnr:= 0;
  wf.blinx:=0;
  RETURN FALSE;
END tCreate;

PROCEDURE tClose( fno: CARDINAL): BOOLEAN;
(*Close previously created & linked File *)
(*True on error*)

BEGIN
  SetEof(wf.osFile, wf.blnr*blsize+wf.blinx);
  IF Write_Block(fno, wf.blinx) THEN
    DROP(Close(wf.osFile));
    RETURN TRUE;
    END;
  RETURN Close(wf.osFile);
END tClose;

PROCEDURE tCloseUnchanged(fno: CARDINAL): BOOLEAN;
(*Closes File opened for read*)

BEGIN
  RETURN Close(wf.osFile);
END tCloseUnchanged;

PROCEDURE Write_Line( fno: CARDINAL; VAR line: ARRAY OF CHAR ): BOOLEAN;
(* Write a line to File. True if disk write error *)

VAR
  ChToCopy: [one..255]; (* First CHAR to copy from line *)
  bytes_to_write: [0..255];
  free_bytes: [0..blsize];
  old_blinx: [0..blsize+1];
  old_blnr: CARDINAL;

BEGIN
  ChToCopy := 1;
  old_blinx := wf.blinx;
  old_blnr := wf.blnr;
  free_bytes := blsize- wf.blinx;
  bytes_to_write := CARDINAL(line[0]);
  WHILE bytes_to_write >= free_bytes DO
    (* Copy part of line upon END of block *)
    MoveBytes(free_bytes,line,wf.block,ChToCopy,wf.blinx);
    INC(ChToCopy,free_bytes);
    IF Write_Block( fno, blsize ) THEN
      (*Flush copied part*)
      wf.blinx := old_blinx;
      wf.blnr := old_blnr;
      RETURN TRUE;
      END;
    free_bytes := blsize;
    bytes_to_write := CARDINAL(line[0])-ChToCopy+1;
    END; (*While block needs written*)
  (* Copy remainig right part of line *)
  MoveBytes( bytes_to_write,line,wf.block,ChToCopy,wf.blinx );
  INC(wf.blinx, bytes_to_write);
  wf.block[wf.blinx]:=0c;
  IF wf.blinx=blsize-1 THEN
    (*Next line starts at next block*)
    IF Write_Block( fno, blsize ) THEN
      (*Flush copied part*)
      wf.blinx := old_blinx;
      wf.blnr := old_blnr;
      RETURN TRUE;
      END
  ELSE
    (* Stay in same block *)
    INC(wf.blinx);
  END;
  RETURN FALSE;
END Write_Line;

PROCEDURE Read_Line( fno: CARDINAL; VAR ln: ARRAY OF CHAR ): BOOLEAN;
(* Read next line from File, FALSE if eof, TRUE if line available *)

VAR
  curinx: [0  ..blsize]; (*Current index into block*)
  ffreech: [one..256]; (*First free pos in line*)

PROCEDURE LineLetter(ch: CHAR): BOOLEAN;
CONST Nul=0c; LF=12c;
BEGIN
  RETURN (ch#Nul) & (ch#LF)
END LineLetter;

BEGIN
  IF (blsize*(wf.blnr-1) + wf.blinx)>=wf.fsize THEN RETURN FALSE END;
  (*Next line exists *)
  curinx := wf.blinx;
  ffreech := 1;
  REPEAT  (*Until line copied*)
    WHILE LineLetter(wf.block[curinx]) DO INC(curinx) END;
    MoveBytes( ORD(curinx-wf.blinx),wf.block,ln,ORD(wf.blinx),ORD(ffreech));
    IF ffreech+curinx-wf.blinx>=256 THEN
      (*Line longer than 255 encountered, get only 255 bytes*)
      Message( fno,20,escget );
      curinx:= wf.blinx+255- ffreech;
      END;
    INC(ffreech, curinx - wf.blinx );
    IF curinx= blsize THEN
       IF Read_Block(fno) THEN
         (*Reading error*)
         Message( fno,29, escget );
         wf.blnr := MaxInt;
         wf.block[0] := 0c;
         END;
       curinx := 0;
       END;
   UNTIL (NOT LineLetter(wf.block[curinx])) OR (ffreech>=255);
   ln[0] := CHAR(ffreech-1 );
   wf.blinx := curinx + 1;
   IF wf.blinx=blsize THEN
     IF Read_Block(fno) THEN
       (*Reading error*)
       Message( fno,29, escget );
       wf.blnr := MaxInt;
       wf.block[0] := 0c;
       END; END;
  RETURN TRUE;
END Read_Line;

PROCEDURE File_Delete(fno: CARDINAL): BOOLEAN;

VAR
  fnm: FsPublic.FileName;
  DestDir: File;

BEGIN
  IF wf.osfn[0]=0c THEN
    (*Temporary file delete*)
    IF wf.blnr>0 THEN
      (* .. delete temporary file: not available in OS *)
      RETURN FALSE
      END;
    RETURN FALSE;
    END;
  (*Normal file delete*)
  fnm:= wf.osfn;
  IF Go(CD(),DestDir,fnm, ErrProc) THEN RETURN TRUE END;
  IF UnLink(DestDir,fnm) THEN RETURN TRUE END;
  IF Close(DestDir)      THEN RETURN TRUE END;
  RETURN FALSE;
END File_Delete;

VAR fc,lins: CARDINAL;

PROCEDURE IsDirectory(fno: CARDINAL): BOOLEAN;
(*True if File name is directory*)
BEGIN
  Str2(wf.osfn);
  RETURN wf.osfn[Len(wf.osfn)-1]='/';
END IsDirectory;

PROCEDURE PrintOne(VAR d: dNode);
CONST fnamelen=32;
VAR ccnt: [0..fnamelen-1];

BEGIN
  IF d.Name[0]=0c THEN RETURN; END;
  IF fc=2*lins THEN
    Message(-1,-1,escget);
    fc:=0; END;

  IF ODD(fc) THEN Show(d.Name) ELSE
    ccnt:=0;
    WHILE d.Name[ccnt]#0c DO
      Write(d.Name[ccnt]);
      INC(ccnt); END;
    FOR ccnt:=ccnt TO fnamelen-1 DO Write(' '); END;
    END;
  INC(fc);
END PrintOne;

PROCEDURE PrintDir(fno,lines: CARDINAL): BOOLEAN;
(*Print directory which name is in file name,
  return TRUE on error*)
VAR fn: FsPublic.FileName;

BEGIN
  fn:= wf.osfn;
  IF Go(CD(),wf.osFile,fn,ErrProc) THEN
    RETURN TRUE; END;
  fc:=0;
  lins:= lines;
  IF Ls(wf.osFile,PrintOne,FALSE) THEN RETURN TRUE; END;
  IF Close( wf.osFile ) THEN RETURN TRUE END;
  RETURN FALSE;
END PrintDir;

BEGIN
  (*Must set default names for all files for name getting*)
  wf.osfn[0] := 0c;
END WMFiles.
