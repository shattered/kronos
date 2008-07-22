IMPLEMENTATION MODULE SCRtty; (* 14-May-89. (c) KRONOS *)

IMPORT SYSTEM, defCodes, ASCII, wnd: Windows;

VAR win: wnd.window;

VAR max_l: INTEGER; (* maximum screen sizes *)
    max_c: INTEGER;

VAR lines: INTEGER;  (* screen sizes *)
  columns: INTEGER;
     text: BOOLEAN;  (* text mode on *)
   char_w: INTEGER;  (* char place   *)
   char_h: INTEGER;  (* sizes        *)

VAR wl,wc: INTEGER;  (* write position  *)
    c_on : BOOLEAN;  (* carret  off/on  *)
    c_on_save: BOOLEAN;
    i_on : BOOLEAN;  (* reverse off/on  *)
    u_on : BOOLEAN;  (* underlined off/on *)
    mode : INTEGER;  CONST rep=0; or=1; xor=2; bic=3;

CONST SYS_FNT_size = 3587;

CONST SYS_FNT_font = ARRAY OF INTEGER {
 006h, 00Eh, 002h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 004h,
 004h, 004h, 004h, 004h, 004h, 000h, 000h, 004h, 000h, 000h, 000h, 000h,
 000h, 00Ah, 00Ah, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 00Ah, 00Ah, 01Fh, 00Ah, 00Ah, 01Fh, 00Ah, 00Ah,
 000h, 000h, 000h, 000h, 000h, 00Eh, 015h, 005h, 005h, 00Eh, 014h, 014h,
 015h, 00Eh, 000h, 000h, 000h, 000h, 000h, 011h, 011h, 008h, 008h, 004h,
 002h, 002h, 011h, 011h, 000h, 000h, 000h, 000h, 000h, 004h, 00Ah, 00Ah,
 00Ah, 004h, 00Ah, 009h, 009h, 015h, 016h, 000h, 000h, 000h, 000h, 004h,
 004h, 002h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 008h, 004h, 002h, 002h, 002h, 002h, 002h, 004h, 008h, 000h, 000h,
 000h, 000h, 000h, 002h, 004h, 008h, 008h, 008h, 008h, 008h, 004h, 002h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 00Ah, 004h, 01Fh, 004h, 00Ah,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 004h, 004h, 004h, 01Fh,
 004h, 004h, 004h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 004h, 004h, 002h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 01Fh, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 00Ch, 00Ch, 000h, 000h,
 000h, 000h, 000h, 050h, 010h, 008h, 008h, 004h, 002h, 002h, 001h, 001h,
 000h, 000h, 000h, 000h, 000h, 00Eh, 011h, 011h, 019h, 015h, 013h, 011h,
 011h, 00Eh, 000h, 000h, 000h, 000h, 000h, 004h, 006h, 004h, 004h, 004h,
 004h, 004h, 004h, 00Eh, 000h, 000h, 000h, 000h, 000h, 00Eh, 011h, 010h,
 010h, 00Ch, 002h, 001h, 001h, 01Fh, 000h, 000h, 000h, 000h, 000h, 00Eh,
 011h, 010h, 010h, 00Ch, 010h, 010h, 011h, 00Eh, 000h, 000h, 000h, 000h,
 000h, 00Ch, 00Ah, 009h, 009h, 009h, 01Fh, 008h, 008h, 008h, 000h, 000h,
 000h, 000h, 000h, 01Fh, 001h, 001h, 00Fh, 010h, 010h, 010h, 010h, 00Fh,
 000h, 000h, 000h, 000h, 000h, 00Eh, 011h, 001h, 00Fh, 011h, 011h, 011h,
 011h, 00Eh, 000h, 000h, 000h, 000h, 000h, 01Fh, 010h, 010h, 008h, 008h,
 004h, 004h, 004h, 004h, 000h, 000h, 000h, 000h, 000h, 00Eh, 011h, 011h,
 011h, 00Eh, 011h, 011h, 011h, 00Eh, 000h, 000h, 000h, 000h, 000h, 00Eh,
 011h, 011h, 011h, 01Eh, 010h, 010h, 011h, 00Eh, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 004h, 004h, 000h, 000h, 004h, 004h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 008h, 008h, 000h, 000h, 000h, 008h, 008h,
 004h, 000h, 000h, 000h, 000h, 000h, 008h, 004h, 002h, 001h, 002h, 004h,
 008h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 00Fh, 000h,
 000h, 00Fh, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 002h, 004h,
 008h, 010h, 008h, 004h, 002h, 000h, 000h, 000h, 000h, 000h, 000h, 00Eh,
 011h, 010h, 010h, 008h, 004h, 000h, 000h, 004h, 000h, 000h, 000h, 000h,
 000h, 00Eh, 011h, 01Dh, 015h, 015h, 01Dh, 001h, 001h, 01Eh, 000h, 000h,
 000h, 000h, 000h, 00Eh, 011h, 011h, 011h, 01Fh, 011h, 011h, 011h, 011h,
 000h, 000h, 000h, 000h, 000h, 00Fh, 011h, 011h, 011h, 00Fh, 011h, 011h,
 011h, 00Fh, 000h, 000h, 000h, 000h, 000h, 00Eh, 011h, 001h, 001h, 001h,
 001h, 001h, 011h, 00Eh, 000h, 000h, 000h, 000h, 000h, 00Fh, 011h, 011h,
 011h, 011h, 011h, 011h, 011h, 00Fh, 000h, 000h, 000h, 000h, 000h, 01Fh,
 001h, 001h, 001h, 00Fh, 001h, 001h, 001h, 01Fh, 000h, 000h, 000h, 000h,
 000h, 01Fh, 001h, 001h, 001h, 00Fh, 001h, 001h, 001h, 001h, 000h, 000h,
 000h, 000h, 000h, 00Eh, 011h, 001h, 001h, 001h, 01Dh, 011h, 011h, 00Eh,
 000h, 000h, 000h, 000h, 000h, 011h, 011h, 011h, 011h, 01Fh, 011h, 011h,
 011h, 011h, 000h, 000h, 000h, 000h, 000h, 00Eh, 004h, 004h, 004h, 004h,
 004h, 004h, 004h, 00Eh, 000h, 000h, 000h, 000h, 000h, 01Ch, 008h, 008h,
 008h, 008h, 008h, 009h, 009h, 006h, 000h, 000h, 000h, 000h, 000h, 011h,
 011h, 009h, 005h, 003h, 005h, 009h, 011h, 011h, 000h, 000h, 000h, 000h,
 000h, 001h, 001h, 001h, 001h, 001h, 001h, 001h, 011h, 01Fh, 000h, 000h,
 000h, 000h, 000h, 011h, 01Bh, 01Bh, 015h, 015h, 011h, 011h, 011h, 011h,
 000h, 000h, 000h, 000h, 000h, 011h, 011h, 013h, 015h, 015h, 015h, 019h,
 011h, 011h, 000h, 000h, 000h, 000h, 000h, 00Eh, 011h, 051h, 051h, 051h,
 051h, 051h, 011h, 00Eh, 000h, 000h, 000h, 000h, 000h, 00Fh, 011h, 011h,
 011h, 00Fh, 001h, 001h, 001h, 001h, 000h, 000h, 000h, 000h, 000h, 00Eh,
 011h, 011h, 011h, 011h, 011h, 015h, 009h, 016h, 010h, 000h, 000h, 000h,
 000h, 00Fh, 011h, 011h, 011h, 00Fh, 005h, 009h, 011h, 011h, 000h, 000h,
 000h, 000h, 000h, 00Eh, 011h, 001h, 001h, 00Eh, 010h, 010h, 011h, 00Eh,
 000h, 000h, 000h, 000h, 000h, 01Fh, 004h, 004h, 004h, 004h, 004h, 004h,
 004h, 004h, 000h, 000h, 000h, 000h, 000h, 011h, 011h, 011h, 011h, 011h,
 011h, 011h, 011h, 00Eh, 000h, 000h, 000h, 000h, 000h, 011h, 011h, 011h,
 011h, 011h, 00Ah, 00Ah, 004h, 004h, 000h, 000h, 000h, 000h, 000h, 011h,
 015h, 015h, 015h, 015h, 015h, 015h, 00Ah, 00Ah, 000h, 000h, 000h, 000h,
 000h, 051h, 011h, 00Ah, 00Ah, 004h, 00Ah, 00Ah, 011h, 051h, 000h, 000h,
 000h, 000h, 000h, 011h, 011h, 00Ah, 00Ah, 004h, 004h, 004h, 004h, 004h,
 000h, 000h, 000h, 000h, 000h, 01Fh, 010h, 008h, 008h, 004h, 002h, 002h,
 001h, 01Fh, 000h, 000h, 000h, 000h, 000h, 00Ch, 004h, 004h, 004h, 004h,
 004h, 004h, 004h, 00Ch, 000h, 000h, 000h, 000h, 000h, 001h, 001h, 002h,
 002h, 004h, 008h, 008h, 010h, 010h, 000h, 000h, 000h, 000h, 000h, 00Ch,
 008h, 008h, 008h, 008h, 008h, 008h, 008h, 00Ch, 000h, 000h, 000h, 000h,
 000h, 004h, 00Ah, 011h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 05Fh, 000h, 000h, 000h, 000h, 004h, 004h, 008h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 007h, 008h, 008h,
 00Eh, 009h, 009h, 016h, 000h, 000h, 000h, 000h, 000h, 001h, 001h, 007h,
 009h, 009h, 009h, 009h, 009h, 007h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 00Eh, 001h, 001h, 001h, 001h, 001h, 00Eh, 000h, 000h, 000h, 000h,
 000h, 008h, 008h, 00Eh, 009h, 009h, 009h, 009h, 009h, 00Eh, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 006h, 009h, 009h, 00Fh, 001h, 001h, 00Eh,
 000h, 000h, 000h, 000h, 000h, 004h, 002h, 004h, 00Eh, 004h, 004h, 004h,
 004h, 004h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 00Eh, 011h, 011h,
 00Eh, 004h, 01Ch, 011h, 011h, 00Eh, 000h, 000h, 000h, 001h, 001h, 007h,
 009h, 009h, 009h, 009h, 009h, 009h, 000h, 000h, 000h, 000h, 000h, 002h,
 000h, 002h, 003h, 002h, 002h, 002h, 00Ah, 004h, 000h, 000h, 000h, 000h,
 000h, 004h, 000h, 004h, 006h, 004h, 004h, 004h, 004h, 004h, 005h, 002h,
 000h, 000h, 000h, 001h, 001h, 00Fh, 011h, 011h, 00Fh, 005h, 009h, 011h,
 000h, 000h, 000h, 000h, 000h, 002h, 003h, 002h, 002h, 002h, 002h, 002h,
 002h, 006h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 00Fh, 015h, 015h,
 015h, 015h, 015h, 015h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 007h,
 009h, 009h, 009h, 009h, 009h, 009h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 00Eh, 011h, 011h, 011h, 011h, 011h, 00Eh, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 00Fh, 011h, 011h, 011h, 011h, 011h, 00Fh, 001h, 001h,
 000h, 000h, 000h, 000h, 000h, 00Eh, 009h, 009h, 009h, 009h, 009h, 00Eh,
 008h, 018h, 000h, 000h, 000h, 000h, 000h, 005h, 00Bh, 001h, 001h, 001h,
 001h, 001h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 00Eh, 011h, 001h,
 00Eh, 010h, 011h, 00Eh, 000h, 000h, 000h, 000h, 000h, 004h, 004h, 00Eh,
 004h, 004h, 004h, 004h, 014h, 008h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 009h, 009h, 009h, 009h, 009h, 009h, 016h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 011h, 011h, 011h, 00Ah, 00Ah, 004h, 004h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 011h, 015h, 015h, 015h, 015h, 015h, 00Ah,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 011h, 011h, 00Ah, 004h, 00Ah,
 011h, 011h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 009h, 009h, 009h,
 009h, 00Eh, 008h, 008h, 009h, 006h, 000h, 000h, 000h, 000h, 000h, 00Fh,
 008h, 004h, 006h, 002h, 001h, 00Fh, 000h, 000h, 000h, 000h, 000h, 008h,
 004h, 004h, 004h, 002h, 004h, 004h, 004h, 008h, 000h, 000h, 000h, 000h,
 000h, 004h, 004h, 004h, 004h, 004h, 004h, 004h, 004h, 004h, 000h, 000h,
 000h, 000h, 000h, 002h, 004h, 004h, 004h, 008h, 004h, 004h, 004h, 002h,
 000h, 000h, 000h, 000h, 000h, 002h, 015h, 008h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 05Fh, 05Fh, 05Fh, 05Fh, 05Fh, 05Fh,
 05Fh, 05Fh, 05Fh, 05Fh, 05Fh, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 03Fh, 03Fh, 03Fh,
 03Fh, 03Fh, 03Fh, 03Fh, 03Fh, 03Fh, 03Fh, 03Fh, 03Fh, 03Fh, 03Fh, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 009h, 015h, 015h, 017h, 015h,
 015h, 009h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 007h, 008h, 008h,
 00Eh, 009h, 009h, 017h, 000h, 000h, 000h, 000h, 000h, 018h, 00Fh, 001h,
 001h, 00Eh, 011h, 011h, 011h, 00Eh, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 009h, 009h, 009h, 009h, 009h, 009h, 01Fh, 010h, 008h, 000h, 000h,
 000h, 000h, 000h, 00Ch, 00Ah, 00Ah, 00Ah, 00Ah, 00Ah, 01Fh, 011h, 000h,
 000h, 000h, 000h, 000h, 000h, 00Eh, 011h, 011h, 00Fh, 001h, 011h, 00Eh,
 000h, 000h, 000h, 000h, 000h, 000h, 004h, 00Eh, 015h, 015h, 015h, 015h,
 00Eh, 004h, 004h, 000h, 000h, 000h, 000h, 000h, 000h, 01Fh, 012h, 002h,
 002h, 002h, 002h, 002h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 011h,
 011h, 00Ah, 004h, 00Ah, 011h, 011h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 011h, 019h, 019h, 015h, 013h, 013h, 011h, 000h, 000h, 000h, 000h,
 000h, 004h, 004h, 011h, 019h, 019h, 015h, 013h, 013h, 011h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 011h, 011h, 009h, 007h, 009h, 011h, 011h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 01Eh, 012h, 012h, 012h, 012h,
 012h, 011h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 011h, 01Bh, 015h,
 015h, 011h, 011h, 011h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 011h,
 011h, 011h, 01Fh, 011h, 011h, 011h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 00Eh, 011h, 011h, 011h, 011h, 011h, 00Eh, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 01Fh, 011h, 011h, 011h, 011h, 011h, 011h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 01Eh, 011h, 011h, 01Eh, 014h, 012h, 011h,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 00Fh, 011h, 011h, 011h, 011h,
 00Fh, 001h, 001h, 000h, 000h, 000h, 000h, 000h, 000h, 01Eh, 001h, 001h,
 001h, 001h, 001h, 01Eh, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 01Fh,
 004h, 004h, 004h, 004h, 004h, 004h, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 011h, 011h, 011h, 011h, 01Eh, 010h, 010h, 009h, 006h, 000h, 000h,
 000h, 000h, 000h, 015h, 015h, 015h, 00Eh, 015h, 015h, 015h, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 00Fh, 011h, 011h, 00Fh, 011h, 011h, 00Fh,
 000h, 000h, 000h, 000h, 000h, 000h, 000h, 001h, 001h, 001h, 007h, 009h,
 009h, 007h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 011h, 011h, 011h,
 013h, 015h, 015h, 013h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 00Fh,
 010h, 010h, 00Eh, 010h, 010h, 00Fh, 000h, 000h, 000h, 000h, 000h, 000h,
 000h, 011h, 015h, 015h, 015h, 015h, 015h, 01Fh, 000h, 000h, 000h, 000h,
 000h, 000h, 000h, 00Fh, 010h, 010h, 01Ch, 010h, 010h, 00Fh, 000h, 000h,
 000h, 000h, 000h, 000h, 000h, 011h, 015h, 015h, 015h, 015h, 015h, 01Fh,
 010h, 008h, 000h, 000h, 000h, 000h, 000h, 011h, 011h, 011h, 01Eh, 010h,
 010h, 010h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 003h, 002h, 002h,
 00Eh, 012h, 012h, 00Eh, 000h, 000h, 000h, 000h, 000h, 009h, 015h, 015h,
 015h, 017h, 015h, 015h, 015h, 009h, 000h, 000h, 000h, 000h, 000h, 01Ch,
 012h, 011h, 011h, 011h, 011h, 01Fh, 011h, 011h, 000h, 000h, 000h, 000h,
 000h, 01Fh, 001h, 001h, 001h, 00Fh, 011h, 011h, 011h, 00Fh, 000h, 000h,
 000h, 000h, 000h, 011h, 011h, 011h, 011h, 011h, 011h, 011h, 011h, 01Fh,
 010h, 008h, 000h, 000h, 000h, 00Ch, 00Ah, 00Ah, 00Ah, 00Ah, 00Ah, 00Ah,
 00Ah, 01Fh, 011h, 000h, 000h, 000h, 000h, 01Fh, 001h, 001h, 001h, 00Fh,
 001h, 001h, 001h, 01Fh, 000h, 000h, 000h, 000h, 000h, 00Eh, 015h, 015h,
 015h, 015h, 015h, 015h, 015h, 00Eh, 004h, 004h, 000h, 000h, 000h, 01Fh,
 012h, 002h, 002h, 002h, 002h, 002h, 002h, 007h, 000h, 000h, 000h, 000h,
 000h, 051h, 009h, 00Ah, 00Ah, 004h, 00Ah, 00Ah, 012h, 011h, 000h, 000h,
 000h, 000h, 000h, 011h, 011h, 019h, 019h, 015h, 013h, 013h, 011h, 011h,
 000h, 000h, 000h, 000h, 000h, 015h, 011h, 019h, 019h, 015h, 013h, 013h,
 011h, 011h, 000h, 000h, 000h, 000h, 000h, 011h, 011h, 009h, 005h, 003h,
 005h, 009h, 011h, 011h, 000h, 000h, 000h, 000h, 000h, 01Ch, 012h, 012h,
 012h, 012h, 012h, 012h, 012h, 011h, 000h, 000h, 000h, 000h, 000h, 011h,
 01Bh, 01Bh, 015h, 015h, 011h, 011h, 011h, 011h, 000h, 000h, 000h, 000h,
 000h, 011h, 011h, 011h, 011h, 01Fh, 011h, 011h, 011h, 011h, 000h, 000h,
 000h, 000h, 000h, 00Eh, 011h, 011h, 011h, 011h, 011h, 011h, 011h, 00Eh,
 000h, 000h, 000h, 000h, 000h, 01Fh, 011h, 011h, 011h, 011h, 011h, 011h,
 011h, 011h, 000h, 000h, 000h, 000h, 000h, 01Eh, 011h, 011h, 011h, 01Eh,
 014h, 012h, 011h, 011h, 000h, 000h, 000h, 000h, 000h, 00Fh, 011h, 011h,
 011h, 00Fh, 001h, 001h, 001h, 001h, 000h, 000h, 000h, 000h, 000h, 00Eh,
 011h, 001h, 001h, 001h, 001h, 001h, 011h, 00Eh, 000h, 000h, 000h, 000h,
 000h, 01Fh, 004h, 004h, 004h, 004h, 004h, 004h, 004h, 004h, 000h, 000h,
 000h, 000h, 000h, 011h, 011h, 011h, 011h, 01Eh, 010h, 010h, 009h, 006h,
 000h, 000h, 000h, 000h, 000h, 015h, 015h, 015h, 015h, 00Eh, 015h, 015h,
 015h, 015h, 000h, 000h, 000h, 000h, 000h, 00Fh, 011h, 011h, 011h, 00Fh,
 011h, 011h, 011h, 00Fh, 000h, 000h, 000h, 000h, 000h, 001h, 001h, 001h,
 001h, 00Fh, 011h, 011h, 011h, 00Fh, 000h, 000h, 000h, 000h, 000h, 011h,
 011h, 011h, 013h, 015h, 015h, 015h, 015h, 013h, 000h, 000h, 000h, 000h,
 000h, 00Fh, 010h, 010h, 008h, 006h, 008h, 010h, 010h, 00Fh, 000h, 000h,
 000h, 000h, 000h, 011h, 015h, 015h, 015h, 015h, 015h, 015h, 015h, 01Fh,
 000h, 000h, 000h, 000h, 000h, 00Fh, 010h, 010h, 010h, 01Eh, 010h, 010h,
 010h, 00Fh, 000h, 000h, 000h, 000h, 000h, 011h, 015h, 015h, 015h, 015h,
 015h, 015h, 015h, 01Fh, 010h, 008h, 000h, 000h, 000h, 011h, 011h, 011h,
 011h, 01Eh, 010h, 010h, 010h, 010h, 000h, 000h, 000h, 000h, 000h, 003h,
 002h, 002h, 002h, 00Eh, 012h, 012h, 012h, 00Eh, 000h, 000h, 000h };

TYPE ADDRESS=SYSTEM.ADDRESS;  WORD=SYSTEM.WORD;

TYPE Font = RECORD
              w,h : INTEGER;
              base: ADDRESS;
              body: ARRAY [0..255] OF ARRAY [0..15] OF INTEGER;
            END;

VAR font: POINTER TO Font;
    FONT: Font;

TYPE BMD = RECORD
             w,h: INTEGER;
             wpl: INTEGER;
            base: ADDRESS;
            patt: BITSET;
           END;

VAR bmd, car_bmd: BMD;

-------------------------------------------------------------

PROCEDURE init_font;
  VAR size: INTEGER;
       adr: ADDRESS;
         i: INTEGER;
BEGIN
  size:=(SYS_FNT_size+255) DIV 256 * 256;
  font:=SYSTEM.ADR(FONT);
  adr:=font;
  FOR i:=0 TO SYS_FNT_size-1 DO adr^:=SYS_FNT_font[i]; INC(adr) END;
  font^.base:=SYSTEM.ADR(FONT.body);
END init_font;

---------------------  HARDWARE INTERFACE  ---------------------
                     ----------------------

PROCEDURE dch(mode: WORD; bmd: ADDRESS; x,y: INTEGER; font: ADDRESS; ch: WORD);
CODE 0F9h 003h END dch;

PROCEDURE bblt(to: ADDRESS;   to_ofs: INTEGER;
             from: ADDRESS; from_ofs: INTEGER; nobits: INTEGER);
CODE defCodes.bblt END bblt;

PROCEDURE bbltg(mode: INTEGER;
                to: ADDRESS;   to_ofs: INTEGER;
              from: ADDRESS; from_ofs: INTEGER; nobits: INTEGER);
CODE 0F9h 02h END bbltg;

PROCEDURE move(dest,sou: ADDRESS; size: INTEGER);
CODE defCodes.move END move;

TYPE Line = ARRAY [0..15] OF BITSET;

VAR ln00: Line;
    lnFF: Line;
    bump: Line;

PROCEDURE init_hw;
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO HIGH(ln00) DO ln00[i]:={} END;
  FOR i:=0 TO HIGH(lnFF) DO lnFF[i]:={0..31} END;
END init_hw;

-------------------------------------------------------------

VAR
   pos_x: ARRAY [0..127] OF INTEGER;
   pos_y: ARRAY [0..127] OF INTEGER;

PROCEDURE show_carret;
BEGIN
  dch(xor,SYSTEM.ADR(car_bmd),pos_x[wc],pos_y[wl],font,253c);
END show_carret;

PROCEDURE set_car(l,c: INTEGER);
BEGIN
  IF l<0 THEN l:=0 ELSIF l>=max_l THEN l:=max_l-1 END;
  IF c<0 THEN c:=0 ELSIF c>=max_c THEN c:=max_c-1 END;
  IF (l=wl) & (c=wc) THEN RETURN END;
  wnd.ref_box(win,pos_y[wl],font^.h);
  wl:=l;  wc:=c;
END set_car;

PROCEDURE car_on(on: BOOLEAN);
BEGIN
  IF (NOT on) = (NOT c_on) THEN RETURN END;
  IF c_on THEN show_carret END;
  c_on:=on;
  IF c_on THEN show_carret END;
END car_on;

PROCEDURE inv_on(on: BOOLEAN);
BEGIN
  IF (NOT on) = (NOT i_on) THEN RETURN END;
  i_on:=(NOT (NOT on))<<2;
END inv_on;

PROCEDURE erase_eol; FORWARD;

PROCEDURE bell; BEGIN  END bell;

PROCEDURE ic(no: INTEGER);  (* insert  chars *)
  VAR l,c: INTEGER;
      x,y: INTEGER;
      i,v: INTEGER;
     bits: INTEGER;
      adr: ADDRESS;
     fill: ADDRESS;
BEGIN ASSERT(no>0,4Ah);
  l:=wl; c:=wc;
  IF c+no>max_c-1 THEN erase_eol; RETURN END;
  y:=pos_y[l]; x:=pos_x[c];
  adr:=bmd.base+bmd.wpl*y;
  IF i_on THEN fill:=SYSTEM.ADR(lnFF) ELSE fill:=SYSTEM.ADR(ln00) END;
  bits:=(max_c-c-no)*char_w;
  v:=no*char_w;
  FOR i:=0 TO char_h-1 DO
    bblt(SYSTEM.ADR(bump),0,adr,x,bits);
    bblt(adr,x,fill,x,v);
    bblt(adr,x+v,SYSTEM.ADR(bump),0,bits);
    INC(adr,bmd.wpl);
  END;
  wnd.ref_box(win,pos_y[wl],font^.h);
END ic;

PROCEDURE dc(no: INTEGER);  (* dellete chars *)
  VAR l,c: INTEGER;
      x,y: INTEGER;
      i,v: INTEGER;
     bits: INTEGER;
      adr: ADDRESS;
     fill: ADDRESS;
BEGIN ASSERT(no>0,4Ah);
  l:=wl; c:=wc;
  IF c+no>max_c-1 THEN erase_eol; RETURN END;
  y:=pos_y[l]; x:=pos_x[c];
  adr:=bmd.base+bmd.wpl*y;
  IF i_on THEN fill:=SYSTEM.ADR(lnFF) ELSE fill:=SYSTEM.ADR(ln00) END;
  bits:=(max_c-c-no)*char_w;
  v:=no*char_w;
  FOR i:=0 TO char_h-1 DO
    bblt(adr,x,adr,x+v,bits);
    bblt(adr,x+bits,fill,x,v);
    INC(adr,bmd.wpl);
  END;
  wnd.ref_box(win,pos_y[wl],font^.h);
END dc;

PROCEDURE dl(no: INTEGER);  (* dellete lines *)
  VAR l,y,h,i,n,v: INTEGER;
   fill,adr0,adr1: ADDRESS;

BEGIN
  l:=wl;
  IF no<=0 THEN no:=1 ELSIF no>max_l-l THEN no:=max_l-l END;
  y:=pos_y[l];     h:=(max_l-l)*char_h;
  n:=bmd.wpl;
  adr0:=bmd.base+n*y;
  adr1:=bmd.base+n*(y+char_h*no);
  v:=(max_l-l-no)*char_h*n;
  IF i_on THEN fill:=SYSTEM.ADR(lnFF) ELSE fill:=SYSTEM.ADR(ln00) END;
  move(adr0,adr1,v); INC(adr0,v);
  move(adr0,fill,n); adr1:=adr0+n;
  move(adr1,adr0,n*(no*char_h-1));
  wnd.refresh(win);
END dl;

PROCEDURE il(no: INTEGER);  (* insert  lines *)
  VAR l,y,h,i,n,v: INTEGER;
   fill,adr0,adr1: ADDRESS;

BEGIN l:=wl;
  IF no<=0 THEN no:=1 ELSIF no>max_l-l THEN no:=max_l-l END;
  n:=bmd.wpl;
  y:=pos_y[l];
  h:=(max_l-l)*char_h;
  adr0:=bmd.base+n*(char_h-1+pos_y[max_l-1]);
  adr1:=bmd.base+n*(char_h-1+pos_y[max_l-1-no]);
  IF i_on THEN fill:=SYSTEM.ADR(lnFF) ELSE fill:=SYSTEM.ADR(ln00) END;
  v:=char_h*(max_l-(l+no))-1;
  FOR i:=0 TO v DO move(adr0,adr1,n); DEC(adr0,n); DEC(adr1,n) END;
  v:=char_h*no-1;
  FOR i:=0 TO v DO move(adr0,fill,n); DEC(adr0,n) END;
  wnd.refresh(win);
END il;

PROCEDURE scroll_up(no: INTEGER);
  VAR sav_l,sav_c: INTEGER;
BEGIN
  sav_l:=wl; sav_c:=wc;
  set_car(0,0);
  dl(no);
  set_car(sav_l,sav_c);
  wnd.refresh(win);
END scroll_up;

PROCEDURE scroll_dw(no: INTEGER);
  VAR sav_l,sav_c: INTEGER;
BEGIN
  sav_l:=wl; sav_c:=wc;
  set_car(0,0);
  il(no);
  set_car(sav_l,sav_c);
  wnd.refresh(win);
END scroll_dw;

PROCEDURE erase_eos;
  VAR l: INTEGER;
    v,h: INTEGER;
      y: INTEGER;
    adr: ADDRESS;
   fill: ADDRESS;
BEGIN
  erase_eol;
  l:=wl+1;
  IF l>max_l-1 THEN RETURN END;
  IF i_on THEN fill:=SYSTEM.ADR(lnFF) ELSE fill:=SYSTEM.ADR(ln00) END;
  y:=pos_y[l]; h:=(max_l-l)*char_h;
  adr:=bmd.wpl*y+bmd.base;
  v:=(max_l-l)*char_h-1;
  FOR l:=0 TO v DO move(adr,fill,bmd.wpl); INC(adr,bmd.wpl) END;
  wnd.refresh(win);
END erase_eos;

PROCEDURE erase_eol;
  VAR l,c: INTEGER;
      x,y: INTEGER;
     bits: INTEGER;
      adr: ADDRESS;
     fill: ADDRESS;
BEGIN
  l:=wl; c:=wc;
  IF i_on THEN fill:=SYSTEM.ADR(lnFF) ELSE fill:=SYSTEM.ADR(ln00) END;
  x:=pos_x[c]; y:=pos_y[l];
  adr:=bmd.wpl*y+bmd.base;  bits:=pos_x[max_c-1]-x+char_w;
  FOR l:=0 TO char_h-1 DO bblt(adr,x,fill,0,bits); INC(adr,bmd.wpl) END;
  wnd.ref_box(win,pos_y[wl],font^.h);
END erase_eol;

----------------------------  INIT  ----------------------------
                            --------

PROCEDURE set_mode(m: INTEGER);
BEGIN mode:=m MOD 4;
END set_mode;

PROCEDURE mark_pos;
  VAR i: INTEGER;
    x,y: INTEGER;
BEGIN
  char_w:=font^.w;
  char_h:=font^.h;
  x:=0; i:=0;
  WHILE (x<bmd.w) & (i<max_c) DO pos_x[i]:=x; INC(i); INC(x,char_w) END;
  columns:=i;
  WHILE i<max_c DO pos_x[i]:=0; INC(i) END;
  y:=0; i:=0;
  WHILE (y<bmd.h) & (i<max_l) DO pos_y[i]:=y; INC(i); INC(y,char_h) END;
  lines:=i;
  WHILE i<max_l DO pos_y[i]:=0; INC(i) END;
END mark_pos;

PROCEDURE init_scr;
  VAR l,c: INTEGER;
BEGIN
  c_on:=FALSE;
  i_on:=FALSE;
  u_on:=FALSE;
  wl:=0; wc:=0;
  set_mode(rep);
  car_on(TRUE);
END init_scr;

PROCEDURE init_bmd(tty_bmd: ADDRESS);
  VAR i: INTEGER; tbmd: POINTER TO BMD;
BEGIN
  tbmd:=tty_bmd;
  bmd.w:=tbmd^.w;           bmd.wpl     :=tbmd^.wpl;
  bmd.h:=tbmd^.h;           bmd.base    :=tbmd^.base+tbmd^.h*tbmd^.wpl;
  car_bmd.w:=tbmd^.w;       car_bmd.wpl :=tbmd^.wpl;
  car_bmd.h:=tbmd^.h;       car_bmd.base:=tbmd^.base;
  bmd.base^:=0;
  move(car_bmd.base+1,car_bmd.base,car_bmd.wpl*car_bmd.h*4-1);
  wnd.refresh(win);
END init_bmd;

VAR     ESC: BOOLEAN;
    esc_seq: ARRAY [0..15] OF CHAR;
    esc_len: INTEGER;

PROCEDURE take_int(VAR pos,res: INTEGER);
  VAR d: INTEGER;
BEGIN
  res:=0;
  IF pos>=esc_len THEN RETURN END;
  d:=ORD(esc_seq[pos])-ORD("0");
  WHILE d IN {0..9} DO
    IF res<999999 THEN res:=res*10+d END;
    INC(pos);
    IF pos>=esc_len THEN RETURN END;
    d:=ORD(esc_seq[pos])-ORD("0");
  END;
END take_int;

CONST end_chars = 'HJKABCDTS@PLMm=';

VAR ends: ARRAY CHAR OF CHAR;

PROCEDURE esc_action(ch: CHAR);
  VAR i,j,pos: INTEGER; on: BOOLEAN;
BEGIN
  ESC:=FALSE;
  IF (esc_len>0) & (esc_seq[0]='[') THEN
    pos:=1;
    CASE ch OF
    |'H': IF esc_len=1 THEN set_car(0,0); RETURN END;
          take_int(pos,i);
          IF (pos<esc_len) & (esc_seq[pos]=';') THEN
            INC(pos); take_int(pos,j);
          ELSE
            j:=0;
          END;
          set_car(i,j);
    |'J': take_int(pos,i);
          IF    i=0 THEN erase_eos
          ELSIF i=2 THEN i:=wl; j:=wc; set_car(0,0); erase_eos; set_car(i,j)
          ELSE
          END;
          IF (wl=0)&(wc=0) THEN init_bmd(SYSTEM.ADR(car_bmd)) END;
    |'K': take_int(pos,i);
          IF    i=0 THEN erase_eol
          ELSIF i=2 THEN j:=wc; set_car(wl,0); erase_eol; set_car(wl,j)
          ELSE
          END;
    |'A': take_int(pos,i);
          IF i=0 THEN i:=1 END;
          set_car(wl-i,wc);
    |'B': take_int(pos,i);
          IF i=0 THEN i:=1 END;
          set_car(wl+i,wc);
    |'C': take_int(pos,i);
          IF i=0 THEN i:=1 END;
          set_car(wl,wc+i);
    |'D': take_int(pos,i);
          IF i=0 THEN i:=1 END;
          set_car(wl,wc-i);
    |'T': take_int(pos,i);
          IF i=0 THEN i:=1 END;
          scroll_dw(i);
    |'S': take_int(pos,i);
          IF i=0 THEN i:=1 END;
          scroll_up(i);
    |'@': take_int(pos,i);
          IF i=0 THEN i:=1 END;
          ic(i);
    |'P': take_int(pos,i);
          IF i=0 THEN i:=1 END;
          dc(i);
    |'L': take_int(pos,i);
          IF i=0 THEN i:=1 END;
          il(i);
    |'M': take_int(pos,i);
          IF i=0 THEN i:=1 END;
          dl(i);
    |'m': take_int(pos,i);
          IF    i= 7 THEN i_on:=TRUE << 2;
          ELSIF i=27 THEN i_on:=FALSE;
          ELSIF i= 4 THEN u_on:=TRUE;
          ELSIF i=24 THEN u_on:=FALSE;
          END;
    ELSE
    END;
  ELSE
    CASE ch OF
    |'=': i:=ORD(esc_seq[0])-40b;
          j:=ORD(esc_seq[1])-40b;
          set_car(i,j);
    |'T': esc_seq[esc_len]:=0c;
          IF    esc_seq="?2;0" THEN c_on_save:=FALSE;
          ELSIF esc_seq="?2;1" THEN c_on_save:=TRUE
          ELSE
          END;
    ELSE
    END;
  END;
END esc_action;

CONST NL=ASCII.NL;

PROCEDURE write(VAL str: ARRAY OF CHAR; len: INTEGER);

  VAR i: INTEGER; c: BOOLEAN;

  PROCEDURE lf;
  BEGIN
    IF wl<max_l-1 THEN set_car(wl+1,wc)
    ELSE scroll_up(1); set_car(wl,wc)
    END;
  END lf;

  VAR ch: CHAR;

BEGIN
  c_on_save:=c_on;
  IF c_on THEN show_carret END;
  c_on:=FALSE;
  i:=0;
  IF len>HIGH(str) THEN len:=HIGH(str)+1 END;
  (*$T-$W$Z*)
  WHILE (i<len) & (str[i]#0c) DO ch:=str[i];
    CASE ch OF
    |33c: ESC:=TRUE; esc_len:=0;
    |15c: set_car(wl,0);
    |12c: lf;
    |NL : set_car(wl,0); lf
    ELSE
      IF ESC THEN
        IF (esc_len>HIGH(esc_seq)) OR (ends[ch]#0c) THEN
          ESC:=FALSE;
          IF ch='=' THEN
            IF (i+2) < len THEN
              esc_seq[0]:=str[i+1];
              esc_seq[1]:=str[i+2];
              INC(i,2);
              esc_len:=2;
              esc_action(ch);
            END;
          ELSE
            esc_action(ch)
          END;
        ELSE
          esc_seq[esc_len]:=ch; INC(esc_len)
        END;
      ELSE
        dch(INTEGER(i_on)+mode,SYSTEM.ADR(bmd),pos_x[wc],pos_y[wl],font,ch);
        IF u_on THEN
          bbltg(2,bmd.base+((pos_y[wl]+char_h-2)*bmd.wpl),pos_x[wc],
                   SYSTEM.ADR(lnFF),0,char_w);
        END;
        wc:=wc+1;
        IF wc>=max_c THEN wc:=max_c-1 END;
        --IF wc>=max_c THEN set_car(wl,0); lf END;
      END;
    END;
    i:=i+1;
  END;
  c_on:=c_on_save;
  IF c_on THEN show_carret END;
  wnd.ref_box(win,pos_y[wl],font^.h);
END write;

PROCEDURE init(tty_bmd: ADDRESS);
  VAR i: INTEGER; c: CHAR;
BEGIN
  win:=tty_bmd;
  init_bmd(tty_bmd);
  init_hw;
  init_font;
  max_l:=bmd.h DIV font^.h;
  max_c:=bmd.w DIV font^.w;
  mark_pos;
  init_scr;
  ESC:=FALSE; esc_len:=0;
  FOR c:=0c TO HIGH(ends) DO ends[c]:=0c END;
  i:=0;
  WHILE end_chars[i]#0c DO ends[end_chars[i]]:=1c; INC(i) END;
END init;

END SCRtty.
