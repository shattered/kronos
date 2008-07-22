MODULE piano; (* 23-Apr-90. (c) KRONOS *)

FROM Terminal    IMPORT print;
FROM Time        IMPORT sys_time, tick;

TYPE
  notes=(do, do_, re, re_, mi, fa, fa_, sol, sol_, la, la_, si, pause, none);

PROCEDURE sound(freq,time: INTEGER);
BEGIN
  print('%c?42;%dT%c?41;%dT'7c,33c,time,33c,freq);
END sound;

PROCEDURE do_note(n: notes; oct,time: INTEGER);
  VAR s,t: INTEGER;
BEGIN
  CASE n OF
    |do  : s:=7643;
    |do_ : s:=7214;
    |re  : s:=6809;
    |re_ : s:=6427;
    |mi  : s:=6066;
    |fa  : s:=5726;
    |fa_ : s:=5404;
    |sol : s:=5101;
    |sol_: s:=4815;
    |la  : s:=4545;
    |la_ : s:=4289;
    |si  : s:=4049;
  ELSE
    t:=sys_time(tick)+time;
    REPEAT UNTIL sys_time(tick)>=t;
    RETURN;
  END;
  CASE oct OF
    |-1: s:=s*4;
    | 0: s:=s*2;
    | 1:
    | 2: s:=s DIV 2;
    | 3: s:=s DIV 4;
  END;
  IF time>2 THEN t:=(time-1) DIV 2 ELSE t:=1 END;
  print('%c?42;%dT%c?41;%dT'7c,33c,t,33c,s);
  t:=sys_time(tick)+time;
  REPEAT UNTIL sys_time(tick)>=t;
END do_note;

CONST
  q=24;
  h=6;

VAR
  i: notes;

BEGIN
  LOOP
    do_note(mi,2,q+2*h);
    do_note(re_,2,1*h);
    do_note(mi,2,1*h);
    do_note(fa,2,2*h);
    do_note(mi,2,2*h);
    do_note(do,2,2*h);
    do_note(do,2,1*h);
    do_note(si,1,1*h);
    do_note(la,1,q+2*h);
    do_note(do,2,2*h);
    do_note(mi,2,2*h);
    do_note(mi,2,2*h);
    do_note(la,2,q+2*h);
    do_note(mi,2,2*h);
    do_note(sol,2,2*h);
    do_note(fa_,2,2*h);
    do_note(fa,2,q+2*h);
    do_note(re,2,2*h);
    do_note(si,1,2);
    do_note(si,2,3*h-2);
    do_note(la,2,h);
    do_note(sol_,2,3*h);
    do_note(fa,2,h);
    do_note(mi,2,3*h);
    do_note(re,2,h);
    do_note(mi,2,3*h);
    do_note(re,2,h);
    do_note(do,2,q+2*h);
    do_note(la,1,2*h);

    do_note(si,1,h);
    do_note(do,2,h);
    do_note(si,1,h);
    do_note(la,1,h);
    do_note(si,1,h);
    do_note(re,2,h);
    do_note(fa,2,h);
    do_note(mi,2,h);
    do_note(re,2,h);
    do_note(do,2,h);
    do_note(si,1,h);
    do_note(do,2,h);
    do_note(la,1,h);
    do_note(do,2,h);
    do_note(mi,2,h);
    do_note(la,2,h);

    do_note(do,3,q+2*h);
    do_note(la,2,2*h);
    do_note(si,2,h);
    do_note(do,3,h);
    do_note(si,2,h);
    do_note(la,2,h);
    do_note(si,2,h);
    do_note(re,3,h);
    do_note(fa,3,h);
    do_note(mi,3,h);
    do_note(re,3,h);
    do_note(do,3,h);
    do_note(si,2,h);
    do_note(la,2,h);
    do_note(sol_,2,h);
    do_note(la,2,h);
    do_note(si,2,h);
    do_note(sol_,2,h);

    do_note(si,2,q);
    do_note(la,2,2*h);

    do_note(pause,0,100);


  END;
END piano.
