DEFINITION MODULE PCX; (* John 07-Dec-90. (c) KRONOS *)

IMPORT  SYSTEM,BIO, defBMG, defScreen;

TYPE HEADER  =  RECORD
                  vers   : INTEGER;  -- Version information
                  encod  : INTEGER;  -- Run-length encoding (=1)
                  bitpx  : INTEGER;  -- Bits per pixel
                  x1     : INTEGER;  -- Picture dimensions (incl)
                  y1     : INTEGER;
                  x2     : INTEGER;
                  y2     : INTEGER;
                  hres   : INTEGER;  -- Display horiz resolution
                  vres   : INTEGER;  -- Display vert  resolution
                  clrma  : ARRAY [0..47] OF CHAR;  -- Pallete
                  vmode  : INTEGER;  -- (ignored)
                  nplanes: INTEGER;  -- Number of planes (ver 2.5=0)
                  bplin  : INTEGER;  -- Bytes per line
                  palinfo: INTEGER;  -- Palette Info (1=col, 2=gray)
                  shres  : INTEGER;  -- Scanner resolution
                  svres  : INTEGER;
                  xtra   : ARRAY [0..53] OF CHAR;  -- Extra space (filler)
                END;

VAR done : BOOLEAN;
    error: INTEGER;
    note : ARRAY [0..79] OF CHAR;

PROCEDURE get_header(f: BIO.FILE; VAR h: HEADER): INTEGER;
(* RETURNs memory size in words for unpacked bitmap *)
PROCEDURE unpack(f: BIO.FILE; h: HEADER; a: SYSTEM.ADDRESS);

PROCEDURE put_header(f: BIO.FILE; h: HEADER);
PROCEDURE pack      (f: BIO.FILE; h: HEADER; a: SYSTEM.ADDRESS);

PROCEDURE write(f: BIO.FILE;     bmd: defBMG.BITMAP;     h: HEADER);
PROCEDURE read (f: BIO.FILE; VAR bmd: defBMG.BITMAP; VAR h: HEADER);

END PCX.
