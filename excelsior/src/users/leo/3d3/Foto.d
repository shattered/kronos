DEFINITION MODULE Foto; (* Leo 05-Sep-91. (c) KRONOS *)

IMPORT  defBMG;

VAR done: BOOLEAN;
   error: INTEGER;

PROCEDURE foto(B: defBMG.BITMAP;
                x,y,w,h: INTEGER; filename,picname: ARRAY OF CHAR);

PROCEDURE show(B: defBMG.BITMAP;  filename,picname: ARRAY OF CHAR);

END Foto.
