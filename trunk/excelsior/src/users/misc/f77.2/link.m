MODULE link; (* 23-Feb-89.  *)

FROM lkPass     IMPORT InitLink, Pass1, AfterPass1, BeforePass2,
                       Pass2, AfterPass2, filename,
                       filelist, curfile, fileDesc, objlist, liblist;

FROM fcHeap     IMPORT Give;

FROM Args       IMPORT TakeWord, ScanFlags;

-- FROM SYSTEM     IMPORT ADR;

PROCEDURE Run;
BEGIN
  Pass1;
  AfterPass1;
  BeforePass2;
  Pass2;
  AfterPass2;
END Run;


VAR
     fname: filename;
--     FL: fileDesc;

BEGIN
   ScanFlags;
   InitLink;
   filelist:=NIL; objlist:=NIL; liblist:=NIL;
   LOOP
     TakeWord(fname);
     IF fname[0]=0c THEN EXIT END;
     Give(curfile,SIZE(fileDesc));
     curfile^.fname:=fname; curfile^.fnext:=NIL;
     IF filelist=NIL THEN filelist:=curfile; END;
     IF objlist#NIL THEN objlist^.fnext:=curfile; END;
     objlist:=curfile;
   END;
   Run;


END link.
