        jump     @start
        nop
        nop

start:  movb    $0h     @0FFF914h
        bicpsrw $F02h
        lprd    SP      $200000h
        setcfg  IFM
        lmr     msr     $0h

        movd    $10000h R0
delay:  acbd    -1 R0 delay

wait_mem:
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        movqb   1       @0FFF90Ch
        cbitib  $0h     @0CF8000h
        movqb   0       @0FFF908h
        bfc     wait_mem

        movw    @0CF8004h  @0CF804Ch
        addw    $100h @0CF8004h
        movqw   3     @0CF804Eh
        movqb   1     @0CF8000h

        movzwd  @CF804Ch  R1   % CE 59 A8 C0 CF 80 4C .Y....L
F8089Dh: addr     00CF0000h(R1)  R0   % 27 48 C0 CF 00 00 'H....
F808A3h: movw     $7007h  R1   % 55 A0 70 07 U.p.
F808A7h: movw     R1  00h(R0)   % 15 0A 00 ...
F808AAh: movw     R1  02h(R0)   % 15 0A 02 ...
F808ADh: movqw    0  08h(R0)   % 5D 40 08 ]@.
F808B0h: movqw    0  0Ch(R0)   % 5D 40 0C ]@.
F808B3h: movqw    0  10h(R0)   % 5D 40 10 ]@.
F808B6h: movqw    0  14h(R0)   % 5D 40 14 ]@.
F808B9h: movqw    0  18h(R0)   % 5D 40 18 ]@.
F808BCh: movqw    0  1Ch(R0)   % 5D 40 1C ]@.
F808BFh: movqw    1  04h(R0)   % DD 40 04 .@.
F808C2h: addr     04h(R0)  push   % E7 45 04 .E.
F808C5h: movqb    1  @FFF90Ch   % DC A8 C0 FF F9 0C ......
F808CBh: cbitib   $0h  0(0(SP))   % 4E 4C A4 00 00 00 NL....
F808D1h: movqb    0  @FFF908h   % 5C A8 C0 FF F9 08 \.....
F808D7h: bfs      *+16   [F808E7h] % 8A 10 ..
F808D9h: nop       % A2 .
F808DAh: nop       % A2 .
F808DBh: nop       % A2 .
F808DCh: nop       % A2 .
F808DDh: nop       % A2 .
F808DEh: nop       % A2 .
F808DFh: nop       % A2 .
F808E0h: nop       % A2 .
F808E1h: nop       % A2 .
F808E2h: nop       % A2 .
F808E3h: nop       % A2 .
F808E4h: nop       % A2 .
F808E5h: br       *-32   [F808C5h] % EA 60 .`
F808E7h: addqw    1  08h(R0)   % 8D 40 08 .@.
F808EAh: movw     0Ch(R0)  R3   % D5 40 0C .@.
F808EDh: movqw    0  0Ch(R0)   % 5D 40 0C ]@.
F808F0h: cmpqw    2  R3   % 1D 19 ..
F808F2h: bne      *+51   [F80925h] % 1A 33 .3
F808F4h: movzwd   14h(R0)  R1   % CE 59 40 14 .Y@.
F808F8h: movzwd   16h(R0)  R2   % CE 99 40 16 ..@.
F808FCh: rotd     $10h  R2   % 4E 83 A0 10 N...
F80900h: ord      R2  R1   % 5B 10 [.
F80902h: movzwd   10h(R0)  R2   % CE 99 40 10 ..@.
F80906h: movzwd   12h(R0)  R3   % CE D9 40 12 ..@.
F8090Ah: rotd     $10h  R3   % 4E C3 A0 10 N...
F8090Eh: ord      R3  R2   % 9B 18 ..
F80910h: movzwd   18h(R0)  R5   % CE 59 41 18 .YA.
F80914h: movzwd   1Ah(R0)  R6   % CE 99 41 1A ..A.
F80918h: rotd     $10h  R6   % 4E 83 A1 10 N...
F8091Ch: ord      R6  R5   % 5B 31 [1
F8091Eh: movd     R5  R0   % 17 28 .(
F80920h: movsw     % 0E 01 00 ...
F80923h: br       *+22   [F80939h] % EA 16 ..
F80925h: cmpqw    1  R3   % 9D 18 ..
F80927h: bne      *+20   [F8093Bh] % 1A 14 ..
F80929h: movzwd   10h(R0)  R3   % CE D9 40 10 ..@.
F8092Dh: movzwd   12h(R0)  R4   % CE 19 41 12 ..A.
F80931h: rotd     $10h  R4   % 4E 03 A1 10 N...
F80935h: ord      R4  R3   % DB 20 .
F80937h: jsr      (R3)   % 7F 1E ..
F80939h: movd     R0  R7   % D7 01 ..
F8093Bh: movzwd   @CF804Ch  R1   % CE 59 A8 C0 CF 80 4C .Y....L
F80942h: addr     00CF0000h(R1)  R0   % 27 48 C0 CF 00 00 'H....
F80948h: movw     R7  1Ch(R0)   % 15 3A 1C .:.
F8094Bh: rotd     $-10h  R7   % 4E C3 A1 F0 N...
F8094Fh: movw     R7  1Eh(R0)   % 15 3A 1E .:.
F80952h: movqb    1  04h(R0)   % DC 40 04 .@.
F80955h: movd     $10000h  R7   % D7 A1 00 01 00 00 ......
F8095Bh: acbd     -1  R7  *+0   [F8095Bh] % CF 3F 00 .?.
F8095Eh: br       *-156   [F808C2h] % EA BF 64 ..d
F80961h: br       *+0   [F80961h] % EA 00 ..
