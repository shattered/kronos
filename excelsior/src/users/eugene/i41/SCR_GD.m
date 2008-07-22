IMPLEMENTATION MODULE SCR_GD; (* 14-May-89. (c) KRONOS *)

IMPORT SYSTEM, mCodeMnem, ASCII;

FROM SYSTEM     IMPORT  ADDRESS, ADR, WORD;

CONST
  max_l=24; (* maximum screen sizes *)
  max_c=80;

VAR
  lines  : INTEGER;  (* screen sizes *)
  columns: INTEGER;
  text   : BOOLEAN;  (* text mode on *)
  char_w : INTEGER;  (* char place   *)
  char_h : INTEGER;  (* sizes        *)
  wl,wc  : INTEGER;  (* write position  *)
  c_on   : BOOLEAN;  (* carret  off/on  *)
  i_on   : BOOLEAN;  (* reverse off/on  *)
  u_on   : BOOLEAN;  (* underlined off/on *)
  mode   : INTEGER;

CONST
  rep=0; or=1; xor=2; bic=3;

  SYS_FNT_font = ARRAY OF INTEGER {
8, 12, 74299,
000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,
000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,
000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,
000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,
000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,
000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,
000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,
000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,
000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,
000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,
000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,
000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,
000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,
000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,
000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,
000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,
000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,
000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,
000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,
000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,
000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,
000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,
000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,
000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,
000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,
000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,
000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,
000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,
000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,
000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,
000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,
000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,
000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,
000h,008h,008h,008h,008h,008h,008h,008h,000h,008h,000h,000h,
000h,048h,024h,012h,000h,000h,000h,000h,000h,000h,000h,000h,
000h,000h,000h,014h,014h,07Fh,014h,07Fh,014h,014h,000h,000h,
000h,014h,03Eh,055h,015h,03Eh,054h,055h,03Eh,014h,000h,000h,
000h,002h,045h,025h,012h,008h,024h,052h,051h,020h,000h,000h,
000h,008h,014h,014h,008h,044h,02Ah,011h,029h,046h,000h,000h,
000h,010h,008h,004h,000h,000h,000h,000h,000h,000h,000h,000h,
000h,020h,010h,008h,008h,008h,008h,008h,010h,020h,000h,000h,
000h,002h,004h,008h,008h,008h,008h,008h,004h,002h,000h,000h,
000h,000h,000h,014h,008h,03Eh,008h,014h,000h,000h,000h,000h,
000h,000h,000h,008h,008h,03Eh,008h,008h,000h,000h,000h,000h,
000h,000h,000h,000h,000h,000h,000h,000h,000h,00Ch,008h,004h,
000h,000h,000h,000h,000h,000h,03Eh,000h,000h,000h,000h,000h,
000h,000h,000h,000h,000h,000h,000h,000h,000h,00Ch,000h,000h,
000h,000h,040h,020h,010h,008h,004h,002h,001h,000h,000h,000h,
000h,03Eh,041h,061h,051h,049h,045h,043h,041h,03Eh,000h,000h,
000h,008h,00Ch,008h,008h,008h,008h,008h,008h,01Ch,000h,000h,
000h,03Eh,041h,040h,040h,020h,018h,006h,041h,07Fh,000h,000h,
000h,07Fh,040h,020h,018h,020h,040h,040h,041h,03Eh,000h,000h,
000h,020h,030h,028h,024h,022h,021h,07Fh,020h,020h,000h,000h,
000h,07Fh,001h,001h,03Fh,040h,040h,040h,041h,03Eh,000h,000h,
000h,03Eh,041h,001h,001h,03Fh,041h,041h,041h,03Eh,000h,000h,
000h,07Fh,040h,040h,020h,010h,008h,008h,008h,008h,000h,000h,
000h,03Eh,041h,041h,041h,03Eh,041h,041h,041h,03Eh,000h,000h,
000h,03Eh,041h,041h,041h,07Eh,040h,040h,041h,03Eh,000h,000h,
000h,000h,000h,000h,000h,00Ch,000h,000h,000h,00Ch,000h,000h,
000h,000h,000h,000h,000h,00Ch,000h,000h,000h,00Ch,008h,004h,
000h,020h,010h,008h,004h,002h,004h,008h,010h,020h,000h,000h,
000h,000h,000h,000h,03Eh,000h,000h,03Eh,000h,000h,000h,000h,
000h,002h,004h,008h,010h,020h,010h,008h,004h,002h,000h,000h,
000h,01Ch,022h,020h,020h,010h,008h,008h,000h,008h,000h,000h,
000h,01Ch,022h,041h,041h,079h,049h,079h,002h,00Ch,000h,000h,
000h,008h,014h,022h,041h,041h,07Fh,041h,041h,041h,000h,000h,
000h,01Fh,021h,021h,021h,03Fh,041h,041h,041h,03Fh,000h,000h,
000h,01Ch,022h,041h,001h,001h,001h,041h,022h,01Ch,000h,000h,
000h,01Fh,021h,041h,041h,041h,041h,041h,021h,01Fh,000h,000h,
000h,07Fh,001h,001h,001h,01Fh,001h,001h,001h,07Fh,000h,000h,
000h,07Fh,001h,001h,001h,01Fh,001h,001h,001h,001h,000h,000h,
000h,01Ch,022h,041h,001h,001h,071h,041h,022h,01Ch,000h,000h,
000h,041h,041h,041h,041h,07Fh,041h,041h,041h,041h,000h,000h,
000h,01Ch,008h,008h,008h,008h,008h,008h,008h,01Ch,000h,000h,
000h,01Ch,008h,008h,008h,008h,008h,008h,009h,006h,000h,000h,
000h,041h,021h,011h,009h,005h,00Bh,011h,021h,041h,000h,000h,
000h,001h,001h,001h,001h,001h,001h,001h,041h,07Fh,000h,000h,
000h,041h,063h,055h,049h,049h,041h,041h,041h,041h,000h,000h,
000h,041h,043h,045h,049h,051h,061h,041h,041h,041h,000h,000h,
000h,01Ch,022h,041h,041h,041h,041h,041h,022h,01Ch,000h,000h,
000h,03Fh,041h,041h,041h,041h,03Fh,001h,001h,001h,000h,000h,
000h,01Ch,022h,041h,041h,041h,049h,051h,022h,05Ch,000h,000h,
000h,03Fh,041h,041h,041h,041h,03Fh,011h,021h,041h,000h,000h,
000h,03Eh,041h,001h,001h,03Eh,040h,040h,041h,03Eh,000h,000h,
000h,07Fh,008h,008h,008h,008h,008h,008h,008h,008h,000h,000h,
000h,041h,041h,041h,041h,041h,041h,041h,041h,03Eh,000h,000h,
000h,041h,041h,041h,041h,041h,041h,022h,014h,008h,000h,000h,
000h,041h,049h,049h,049h,049h,049h,049h,055h,022h,000h,000h,
000h,041h,022h,014h,008h,008h,008h,014h,022h,041h,000h,000h,
000h,041h,041h,041h,022h,014h,008h,008h,008h,008h,000h,000h,
000h,07Fh,040h,020h,010h,008h,004h,002h,001h,07Fh,000h,000h,
000h,038h,008h,008h,008h,008h,008h,008h,008h,038h,000h,000h,
000h,000h,001h,002h,004h,008h,010h,020h,040h,000h,000h,000h,
000h,00Eh,008h,008h,008h,008h,008h,008h,008h,00Eh,000h,000h,
000h,008h,014h,022h,000h,000h,000h,000h,000h,000h,000h,000h,
000h,000h,000h,000h,000h,000h,000h,000h,000h,07Fh,000h,000h,
000h,004h,008h,010h,000h,000h,000h,000h,000h,000h,000h,000h,
000h,000h,000h,00Ch,012h,012h,012h,012h,012h,06Ch,000h,000h,
000h,002h,002h,01Ah,026h,022h,022h,022h,026h,01Ah,000h,000h,
000h,000h,000h,01Ch,022h,002h,002h,002h,022h,01Ch,000h,000h,
000h,020h,020h,02Ch,032h,022h,022h,022h,032h,02Ch,000h,000h,
000h,000h,000h,01Ch,022h,022h,03Eh,002h,022h,01Ch,000h,000h,
000h,010h,028h,008h,01Ch,008h,008h,008h,008h,008h,000h,000h,
000h,000h,000h,01Dh,022h,022h,022h,022h,022h,03Ch,020h,01Ch,
000h,002h,002h,01Ah,026h,022h,022h,022h,022h,022h,000h,000h,
000h,004h,000h,004h,004h,004h,004h,004h,014h,008h,000h,000h,
000h,010h,000h,010h,010h,010h,010h,010h,010h,010h,014h,008h,
000h,002h,002h,022h,012h,00Ah,006h,00Ah,012h,022h,000h,000h,
000h,004h,004h,004h,004h,004h,004h,004h,014h,008h,000h,000h,
000h,000h,000h,025h,05Bh,049h,049h,049h,049h,049h,000h,000h,
000h,000h,000h,01Ah,026h,022h,022h,022h,022h,022h,000h,000h,
000h,000h,000h,01Ch,022h,022h,022h,022h,022h,01Ch,000h,000h,
000h,000h,000h,01Ah,026h,022h,022h,022h,026h,01Ah,002h,002h,
000h,000h,000h,02Ch,032h,022h,022h,022h,032h,02Ch,020h,020h,
000h,000h,000h,01Ah,026h,002h,002h,002h,002h,002h,000h,000h,
000h,000h,000h,01Ch,022h,002h,01Ch,020h,022h,01Ch,000h,000h,
000h,004h,00Eh,004h,004h,004h,004h,004h,024h,018h,000h,000h,
000h,000h,000h,022h,022h,022h,022h,022h,022h,01Ch,000h,000h,
000h,000h,000h,022h,022h,022h,022h,022h,014h,008h,000h,000h,
000h,000h,000h,02Ah,02Ah,02Ah,02Ah,02Ah,02Ah,014h,000h,000h,
000h,000h,000h,022h,014h,008h,008h,008h,014h,022h,000h,000h,
000h,000h,000h,022h,022h,022h,022h,022h,03Ch,020h,020h,01Ch,
000h,000h,000h,03Eh,020h,010h,008h,004h,002h,03Eh,000h,000h,
000h,010h,008h,008h,008h,004h,008h,008h,008h,010h,000h,000h,
000h,008h,008h,008h,008h,008h,008h,008h,008h,008h,000h,000h,
000h,004h,008h,008h,008h,010h,008h,008h,008h,004h,000h,000h,
000h,07Fh,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,
000h,07Fh,07Fh,07Fh,07Fh,07Fh,07Fh,07Fh,07Fh,07Fh,07Fh,07Fh,
000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,
000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,
000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,
018h,018h,018h,018h,018h,018h,018h,018h,018h,018h,018h,018h,
018h,018h,018h,018h,018h,01Fh,018h,018h,018h,018h,018h,018h,
018h,018h,018h,018h,01Fh,018h,01Fh,018h,018h,018h,018h,018h,
024h,024h,024h,024h,024h,027h,024h,024h,024h,024h,024h,024h,
000h,000h,000h,000h,000h,03Fh,024h,024h,024h,024h,024h,024h,
000h,000h,000h,000h,01Fh,018h,01Fh,018h,018h,018h,018h,018h,
024h,024h,024h,024h,027h,020h,027h,024h,024h,024h,024h,024h,
024h,024h,024h,024h,024h,024h,024h,024h,024h,024h,024h,024h,
000h,000h,000h,000h,03Fh,020h,027h,024h,024h,024h,024h,024h,
024h,024h,024h,024h,027h,020h,03Fh,000h,000h,000h,000h,000h,
024h,024h,024h,024h,024h,0FFh,000h,000h,000h,000h,000h,000h,
018h,018h,018h,018h,01Fh,018h,01Fh,000h,000h,000h,000h,000h,
000h,000h,000h,000h,000h,01Fh,018h,018h,018h,018h,018h,018h,
018h,018h,018h,018h,018h,0F8h,000h,000h,000h,000h,000h,000h,
018h,018h,018h,018h,018h,0FFh,000h,000h,000h,000h,000h,000h,
000h,000h,000h,000h,000h,0FFh,018h,018h,018h,018h,018h,018h,
018h,018h,018h,018h,018h,0F8h,018h,018h,018h,018h,018h,018h,
000h,000h,000h,000h,000h,0FFh,000h,000h,000h,000h,000h,000h,
018h,018h,018h,018h,018h,0FFh,018h,018h,018h,018h,018h,018h,
018h,018h,018h,018h,0F8h,018h,0F8h,018h,018h,018h,018h,018h,
024h,024h,024h,024h,024h,0E4h,024h,024h,024h,024h,024h,024h,
024h,024h,024h,024h,0E4h,004h,0FCh,000h,000h,000h,000h,000h,
000h,000h,000h,000h,0FCh,004h,0E4h,024h,024h,024h,024h,024h,
024h,024h,024h,024h,0E7h,000h,0FFh,000h,000h,000h,000h,000h,
000h,000h,000h,000h,0FFh,000h,0E7h,024h,024h,024h,024h,024h,
024h,024h,024h,024h,0E4h,004h,0E4h,024h,024h,024h,024h,024h,
000h,000h,000h,000h,0FFh,000h,0FFh,000h,000h,000h,000h,000h,
024h,024h,024h,024h,0E7h,000h,0E7h,024h,024h,024h,024h,024h,
018h,018h,018h,018h,0FFh,000h,0FFh,000h,000h,000h,000h,000h,
024h,024h,024h,024h,024h,0FFh,000h,000h,000h,000h,000h,000h,
024h,024h,024h,024h,024h,0FCh,000h,000h,000h,000h,000h,000h,
000h,000h,000h,000h,000h,0FFh,024h,024h,024h,024h,024h,024h,
000h,000h,000h,000h,0FFh,000h,0FFh,018h,018h,018h,018h,018h,
018h,018h,018h,018h,0F8h,018h,0F8h,000h,000h,000h,000h,000h,
000h,000h,000h,000h,0F8h,018h,0F8h,018h,018h,018h,018h,018h,
000h,000h,000h,000h,000h,0FCh,024h,024h,024h,024h,024h,024h,
024h,024h,024h,024h,024h,0FFh,024h,024h,024h,024h,024h,024h,
018h,018h,018h,018h,0FFh,018h,0FFh,018h,018h,018h,018h,018h,
018h,018h,018h,018h,018h,01Fh,000h,000h,000h,000h,000h,000h,
000h,000h,000h,000h,000h,0F8h,018h,018h,018h,018h,018h,018h,
0FFh,0FFh,0FFh,0FFh,0FFh,0FFh,0FFh,0FFh,0FFh,0FFh,0FFh,0FFh,
000h,000h,000h,000h,000h,000h,0FFh,0FFh,0FFh,0FFh,0FFh,0FFh,
00Fh,00Fh,00Fh,00Fh,00Fh,00Fh,00Fh,00Fh,00Fh,00Fh,00Fh,00Fh,
0F0h,0F0h,0F0h,0F0h,0F0h,0F0h,0F0h,0F0h,0F0h,0F0h,0F0h,0F0h,
0FFh,0FFh,0FFh,0FFh,0FFh,0FFh,000h,000h,000h,000h,000h,000h,
000h,03Eh,041h,05Dh,045h,045h,045h,05Dh,041h,03Eh,000h,000h,
000h,03Eh,041h,05Dh,055h,05Dh,04Dh,055h,041h,03Eh,000h,000h,
000h,000h,000h,000h,000h,000h,00Fh,00Fh,00Fh,00Fh,00Fh,00Fh,
0F0h,0F0h,0F0h,0F0h,0F0h,0F0h,000h,000h,000h,000h,000h,000h,
000h,000h,000h,000h,000h,000h,0F0h,0F0h,0F0h,0F0h,0F0h,0F0h,
00Fh,00Fh,00Fh,00Fh,00Fh,00Fh,000h,000h,000h,000h,000h,000h,
000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,
000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,
000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,
000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,
000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,
000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,
000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,
000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,
000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,
000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,
000h,000h,000h,012h,02Ah,02Ah,02Eh,02Ah,02Ah,012h,000h,000h,
000h,000h,000h,01Ch,022h,020h,03Ch,022h,022h,05Ch,000h,000h,
000h,03Ch,008h,010h,03Ch,022h,022h,022h,022h,01Ch,000h,000h,
000h,000h,000h,012h,012h,012h,012h,012h,012h,06Ch,040h,000h,
000h,01Ch,020h,020h,02Ch,032h,022h,022h,022h,01Ch,000h,000h,
000h,000h,000h,01Ch,022h,022h,03Eh,002h,022h,01Ch,000h,000h,
000h,000h,000h,008h,03Eh,049h,049h,049h,03Eh,008h,000h,000h,
000h,000h,000h,01Ch,022h,010h,008h,004h,022h,01Ch,000h,000h,
000h,000h,000h,022h,014h,008h,008h,008h,014h,022h,000h,000h,
000h,000h,000h,022h,022h,022h,022h,022h,032h,04Ch,000h,000h,
000h,01Ch,000h,022h,022h,022h,022h,022h,032h,04Ch,000h,000h,
000h,000h,000h,022h,012h,00Ah,006h,00Ah,012h,022h,000h,000h,
000h,000h,000h,038h,024h,024h,024h,024h,024h,022h,000h,000h,
000h,000h,000h,022h,036h,02Ah,022h,022h,022h,022h,000h,000h,
000h,000h,000h,022h,022h,022h,03Eh,022h,022h,022h,000h,000h,
000h,000h,000h,01Ch,022h,022h,022h,022h,022h,01Ch,000h,000h,
000h,000h,000h,03Eh,022h,022h,022h,022h,022h,022h,000h,000h,
000h,000h,000h,03Ch,022h,022h,03Ch,028h,024h,022h,000h,000h,
000h,000h,000h,01Eh,022h,022h,022h,01Eh,002h,002h,000h,000h,
000h,000h,000h,01Ch,022h,002h,002h,002h,022h,01Ch,000h,000h,
000h,000h,000h,03Eh,008h,008h,008h,008h,008h,008h,000h,000h,
000h,000h,000h,022h,022h,022h,022h,022h,03Ch,020h,020h,01Ch,
000h,000h,000h,049h,049h,02Ah,01Ch,02Ah,049h,049h,000h,000h,
000h,004h,00Ah,00Ah,00Ah,006h,01Eh,022h,022h,01Ch,000h,000h,
000h,000h,000h,002h,002h,01Eh,022h,022h,022h,01Eh,000h,000h,
000h,000h,000h,042h,042h,042h,04Eh,052h,052h,04Eh,000h,000h,
000h,000h,000h,01Ch,022h,020h,010h,020h,022h,01Ch,000h,000h,
000h,000h,000h,049h,049h,049h,049h,049h,049h,07Fh,000h,000h,
000h,000h,000h,01Ch,022h,020h,03Ch,020h,022h,01Ch,000h,000h,
000h,000h,000h,049h,049h,049h,049h,049h,049h,0FFh,080h,040h,
000h,000h,000h,022h,022h,022h,022h,03Ch,020h,020h,000h,000h,
000h,000h,000h,007h,004h,03Ch,044h,044h,044h,03Ch,000h,000h,
000h,031h,049h,049h,049h,04Fh,049h,049h,049h,031h,000h,000h,
000h,01Ch,022h,041h,041h,041h,07Fh,041h,041h,041h,000h,000h,
000h,07Fh,001h,001h,001h,03Fh,041h,041h,041h,03Fh,000h,000h,
000h,041h,041h,041h,041h,041h,041h,041h,041h,0FFh,080h,040h,
000h,038h,024h,022h,022h,022h,022h,022h,022h,07Fh,041h,000h,
000h,07Fh,001h,001h,001h,01Fh,001h,001h,001h,07Fh,000h,000h,
000h,008h,036h,049h,049h,049h,049h,049h,03Eh,008h,000h,000h,
000h,07Fh,041h,001h,001h,001h,001h,001h,001h,001h,000h,000h,
000h,041h,041h,022h,014h,008h,014h,022h,041h,041h,000h,000h,
000h,041h,041h,041h,061h,051h,049h,045h,043h,041h,000h,000h,
014h,049h,041h,041h,061h,051h,049h,045h,043h,041h,000h,000h,
000h,041h,021h,011h,009h,005h,00Bh,011h,021h,041h,000h,000h,
000h,07Ch,042h,042h,042h,042h,042h,042h,042h,041h,000h,000h,
000h,041h,063h,055h,049h,049h,041h,041h,041h,041h,000h,000h,
000h,041h,041h,041h,041h,07Fh,041h,041h,041h,041h,000h,000h,
000h,03Eh,041h,041h,041h,041h,041h,041h,041h,03Eh,000h,000h,
000h,07Fh,041h,041h,041h,041h,041h,041h,041h,041h,000h,000h,
000h,07Eh,041h,041h,041h,041h,07Eh,044h,042h,041h,000h,000h,
000h,03Fh,041h,041h,041h,03Fh,001h,001h,001h,001h,000h,000h,
000h,03Eh,041h,001h,001h,001h,001h,001h,041h,03Eh,000h,000h,
000h,07Fh,008h,008h,008h,008h,008h,008h,008h,008h,000h,000h,
000h,041h,041h,041h,041h,07Eh,040h,040h,041h,03Eh,000h,000h,
000h,049h,049h,02Ah,01Ch,008h,01Ch,02Ah,049h,049h,000h,000h,
000h,03Fh,041h,041h,041h,03Fh,041h,041h,041h,03Fh,000h,000h,
000h,001h,001h,001h,001h,03Fh,041h,041h,041h,03Fh,000h,000h,
000h,041h,041h,041h,041h,04Fh,051h,051h,051h,04Fh,000h,000h,
000h,03Eh,041h,040h,020h,010h,020h,040h,041h,03Eh,000h,000h,
000h,049h,049h,049h,049h,049h,049h,049h,049h,07Fh,000h,000h,
000h,03Eh,041h,040h,040h,07Ch,040h,040h,041h,03Eh,000h,000h,
000h,049h,049h,049h,049h,049h,049h,049h,049h,0FFh,080h,040h,
000h,041h,041h,041h,041h,07Eh,040h,040h,040h,040h,000h,000h,
000h,003h,002h,002h,002h,03Eh,042h,042h,042h,03Eh,000h,000h
};


TYPE
  Font = RECORD
    w,h : INTEGER;
    base: ADDRESS;
  END;
  BMD = RECORD
    w,h: INTEGER;
    wpl: INTEGER;
    base: ADDRESS;
    patt: BITSET;
  END;
  Line = ARRAY [0..19] OF BITSET;

VAR
  font: POINTER TO Font;
  bmd : BMD;
  ln00: Line;
  lnFF: Line;
  bump: Line;

---------------------  HARDWARE INTERFACE  ---------------------
                     ----------------------

PROCEDURE dch(mode: WORD; bmd: ADDRESS; x,y: INTEGER; font: ADDRESS; ch: WORD);
CODE 0F9h 003h END dch;

PROCEDURE bblt(to: ADDRESS;   to_ofs: INTEGER;
             from: ADDRESS; from_ofs: INTEGER; nobits: INTEGER);
CODE mCodeMnem.bblt END bblt;

PROCEDURE bbltg(mode: INTEGER;
                to: ADDRESS;   to_ofs: INTEGER;
              from: ADDRESS; from_ofs: INTEGER; nobits: INTEGER);
CODE 0F9h 02h END bbltg;

PROCEDURE move(dest,sou: ADDRESS; size: INTEGER);
CODE mCodeMnem.move END move;

PROCEDURE init_hw;
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO HIGH(ln00) DO ln00[i]:={} END;
  FOR i:=0 TO HIGH(lnFF) DO lnFF[i]:={0..31} END;
END init_hw;

-------------------------------------------------------------

PROCEDURE init_font;
  PROCEDURE adr(VAL s: ARRAY OF INTEGER): SYSTEM.ADDRESS;
  BEGIN
    RETURN SYSTEM.ADR(s);
  END adr;
BEGIN
  font:=adr(SYS_FNT_font);
  font^.base:=SYSTEM.ADDRESS(font)+SIZE(font^);
END init_font;

-------------------------------------------------------------

VAR
  pos_x: ARRAY [0..max_c-1] OF INTEGER;
  pos_y: ARRAY [0..max_l-1] OF INTEGER;

PROCEDURE show_carret;
BEGIN
  dch(xor,SYSTEM.ADR(bmd),pos_x[wc],pos_y[wl],font,253c);
END show_carret;

PROCEDURE set_car(l,c: INTEGER);
BEGIN
  IF l<0 THEN l:=0 ELSIF l>=max_l THEN l:=max_l-1 END;
  IF c<0 THEN c:=0 ELSIF c>=max_c THEN c:=max_c-1 END;
  IF (l=wl) & (c=wc) THEN RETURN END;
  wl:=l;  wc:=c;
END set_car;

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
  fill:=SYSTEM.ADR(ln00);
  bits:=(max_c-c-no)*char_w;
  v:=no*char_w;
  FOR i:=0 TO char_h-1 DO
    bblt(SYSTEM.ADR(bump),0,adr,x,bits);
    bblt(adr,x,fill,x,v);
    bblt(adr,x+v,SYSTEM.ADR(bump),0,bits);
    INC(adr,bmd.wpl);
  END;
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
  fill:=SYSTEM.ADR(ln00);
  bits:=(max_c-c-no)*char_w;
  v:=no*char_w;
  FOR i:=0 TO char_h-1 DO
    bblt(adr,x,adr,x+v,bits);
    bblt(adr,x+bits,fill,x,v);
    INC(adr,bmd.wpl);
  END;
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
  fill:=SYSTEM.ADR(ln00);
  move(adr0,adr1,v); INC(adr0,v);
  move(adr0,fill,n); adr1:=adr0+n;
  move(adr1,adr0,n*(no*char_h-1));
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
  fill:=SYSTEM.ADR(ln00);
  v:=char_h*(max_l-(l+no))-1;
  FOR i:=0 TO v DO move(adr0,adr1,n); DEC(adr0,n); DEC(adr1,n) END;
  v:=char_h*no-1;
  FOR i:=0 TO v DO move(adr0,fill,n); DEC(adr0,n) END;
END il;

PROCEDURE scroll_up(no: INTEGER);
  VAR sav_l,sav_c: INTEGER;
BEGIN
  sav_l:=wl; sav_c:=wc;
  set_car(0,0);
  dl(no);
  set_car(sav_l,sav_c);
END scroll_up;

PROCEDURE scroll_dw(no: INTEGER);
  VAR sav_l,sav_c: INTEGER;
BEGIN
  sav_l:=wl; sav_c:=wc;
  set_car(0,0);
  il(no);
  set_car(sav_l,sav_c);
END scroll_dw;

PROCEDURE erase_eos;
  VAR i: INTEGER;
      l: INTEGER;
    v,h: INTEGER;
      y: INTEGER;
    adr: ADDRESS;
   fill: ADDRESS;
  space: INTEGER;
BEGIN
  erase_eol;
  l:=wl+1;
  IF l>max_l-1 THEN RETURN END;
  fill:=SYSTEM.ADR(ln00);
  y:=pos_y[l]; h:=(max_l-l)*char_h;
  adr:=bmd.wpl*y+bmd.base;
  v:=(max_l-l)*char_h-1;
  FOR i:=0 TO v DO move(adr,fill,bmd.wpl); INC(adr,bmd.wpl) END;
END erase_eos;

PROCEDURE erase_eol;
  VAR
    i    : INTEGER;
    l,c  : INTEGER;
    x,y  : INTEGER;
    bits : INTEGER;
    adr  : ADDRESS;
    fill : ADDRESS;
    space: INTEGER;
BEGIN
  l:=wl; c:=wc;
  fill:=SYSTEM.ADR(ln00);
  x:=pos_x[c]; y:=pos_y[l];
  adr:=bmd.wpl*y+bmd.base;  bits:=pos_x[max_c-1]-x+char_w;
  FOR i:=0 TO char_h-1 DO bblt(adr,x,fill,0,bits); INC(adr,bmd.wpl) END;
END erase_eol;

----------------------------  INIT  ----------------------------
                            --------

PROCEDURE set_mode(m: INTEGER);
BEGIN
  mode:=m MOD 4;
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
  c_on:=TRUE;
  i_on:=FALSE;
  u_on:=FALSE;
  wl:=0; wc:=0;
  set_mode(rep);
END init_scr;

PROCEDURE init_bmd;
  VAR i: INTEGER; a: ADDRESS;
  CONST
    csr=46h<<30;
    chA=40h<<30;
    chB=42h<<30;
    chC=44h<<30;
    shft=-1;
  PROCEDURE out(n,v: INTEGER); CODE 93h END out;
BEGIN
  out(csr,90h);
  out(chB,shft);
  out(chC,shft DIV 256);
  bmd.w:=640;
  bmd.wpl :=20;
  bmd.h:=300;
  bmd.base:=0F00000h+0F0000h DIV 4;
  a:=bmd.base;
  FOR i:=0 TO bmd.h*bmd.wpl-1 DO a^:=0; INC(a) END;
END init_bmd;

VAR
  ESC    : BOOLEAN;
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

CONST
  end_chars = 'HJKABCDTS@PLMm=';

VAR
  ends: ARRAY CHAR OF CHAR;

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
          IF (wl=0)&(wc=0) THEN init_bmd END;
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
            IF    esc_seq="?2;0" THEN c_on:=FALSE;
            ELSIF esc_seq="?2;1" THEN c_on:=TRUE
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
  IF c_on THEN show_carret END;
  i:=0;
  IF len>HIGH(str) THEN len:=HIGH(str)+1 END;
  (*$T-$W$Z*)
  WHILE (i<len) & (str[i]#0c) DO
    ch:=str[i];
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
        IF wc>=max_c THEN set_car(wl,0); lf END;
      END;
    END;
    i:=i+1;
  END;
  IF c_on THEN show_carret END;
END write;

PROCEDURE init;
  VAR i: INTEGER; c: CHAR;
BEGIN
  init_bmd;
  init_hw;
  init_font;
  mark_pos;
  init_scr;
  ESC:=FALSE; esc_len:=0;
  FOR c:=0c TO HIGH(ends) DO ends[c]:=0c END;
  i:=0;
  WHILE end_chars[i]#0c DO ends[end_chars[i]]:=1c; INC(i) END;
END init;

BEGIN
  init;
END SCR_GD.
