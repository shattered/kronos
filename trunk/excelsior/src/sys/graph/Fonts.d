DEFINITION MODULE Fonts; (* Leo 27-Jan-91. (c) KRONOS *)

IMPORT  defFont;

TYPE
  FONT = defFont.FONT;

CONST
  prop  =defFont.prop;
  packed=defFont.packed;
  italic=defFont.italic;

VAL font: FONT;    (* default constant font always in memory *)
    done: BOOLEAN;
   error: INTEGER;

PROCEDURE new    (VAR fnt: FONT; w,h: INTEGER; f,l: CHAR; state: BITSET);
PROCEDURE dispose(VAR fnt: FONT);

PROCEDURE read (VAR fnt: FONT; file_name: ARRAY OF CHAR);
PROCEDURE write(    fnt: FONT; file_name: ARRAY OF CHAR);

PROCEDURE pack  (fnt: FONT);   (* pack unpacked font           *)
PROCEDURE unpack(fnt: FONT);   (* unpack font for "dch" ussing *)

PROCEDURE copy(des: FONT; to: CHAR; sou: FONT; from: CHAR);
(* des[to]:=sou[from]                                          *)
(* des must be previously "new" with appropriate state,f,l,w,h *)

END Fonts.
(*

    The structure of file *.fnt:

    HEADER:
      name     size   value     meanning
      fMAGIC      4   "FONT"
      state       4             packed,prop,italic
      W           4   0..255    max char W
      H           4   0..255    max char H
      fchar       4   0..255    first char in BASE array
      lchar       4   0..255    last  char in BASE array
      bline       4   0..H-1    base line
      uline       4   0..H-1    line for underline
      size        4             size of BASES array
      space       4   0..W-1    width of empty char (SPACE for example)

    PACKED:  (nochars=lchar-fchar+1)

      cellW   nochars  0..W-1  (see defFont.d about packed fonts)
      cellH   nochars  0..H-1
      cellY   nochars  0..H-1
      bases   nochars           bits offset in BASE array

      All this arrays read into f^.cell* & f^.bases from offset fchar
      Other space in "cell*" filled by 0 in "bases" by -1.

      BASE    size*4            bit patterns for chars
                                BASE*32+bases[ch] first bit of char pattern
                                for next line bit offset add cellW[ch]

    UNPACKED:

      BASE    size*4            bit patterns for chars
                                BASE+(ch-fchar)*H) first word of char pattern
                                for next line address add 1
    TAILER:

      For none proportional fonts empty.
      For proportional:

      propW   nochars
      All this array read into f^.propW from offset fchar
      Other space in "propW" filled by 0.

*)
