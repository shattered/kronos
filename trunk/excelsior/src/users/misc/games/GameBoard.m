IMPLEMENTATION MODULE GameBoard; (* Shu 09-Jul-87. (c) KRONOS *)

IMPORT Terminal, TTYs;
FROM SYSTEM    IMPORT   ADR;
FROM Scheduler IMPORT   ProcessId, Send, Wait, MakeProcess, Start
                      , Signal, InitSignal, Sleep, Sendings, Awaited, MyTask;
FROM Resource  IMPORT   Final;


TYPE Action = RECORD
       key: WORD;
        on: BOOLEAN;
      END;

MODULE Buffer;

IMPORT Action;

EXPORT Get, Put, Amount, Full?;

  VAR ring: ARRAY [0..127] OF Action;
    in,out: CARDINAL;

PROCEDURE Get(VAR e: Action);
BEGIN
  ASSERT(out#in);
  e:=ring[out]; INC(out);
  IF out>HIGH(ring) THEN out:=0 END;
END Get;

PROCEDURE Put(e: Action);
  VAR new: INTEGER;
BEGIN
  new:=in+1;
  IF new>HIGH(ring) THEN new:=0 END;
  ASSERT(new#out);
  ring[in]:=e; in:=new;
END Put;

PROCEDURE Amount(): CARDINAL;
BEGIN
  RETURN (in + HIGH(ring)+1 - out) MOD (HIGH(ring)+1)
END Amount;

PROCEDURE Full?(): BOOLEAN;
BEGIN RETURN Amount()>=HIGH(ring) END Full?;

BEGIN
  in:=0; out:=0;
END Buffer;

  CONST dummy = BOOLEAN(100);


  VAR
    board: ARRAY [0c..377c] OF Action;
    rawon: BOOLEAN;
     stop: BOOLEAN;
  pressco: INTEGER; (* pressed pushbuttons counter *)
    final: Signal;
   change: Signal;
    Break: CHAR;

      wsp: ARRAY [0..1023] OF INTEGER;

PROCEDURE Raw(on: BOOLEAN);
  VAR k: CHAR;
BEGIN
  WHILE Terminal.Pressed()>0 DO
    IF Terminal.BusyRead()=0c THEN END
  END;
  Terminal.TransparentIn(on);
  Terminal.SetMode(NOT on);
  Terminal.print("" 33c "?46;%dT",NOT NOT on);
  rawon:=on;
  WHILE Terminal.Pressed()>0 DO
    IF Terminal.BusyRead()=0c THEN RETURN END
  END;
END Raw;

PROCEDURE Reader;
  VAR c: CHAR;  i: CARDINAL; never: Signal;
BEGIN i:=0;
  REPEAT
    IF rawon & ODD(i) & (Terminal.Pressed()>0) THEN c:=Terminal.Read();
      IF c=Break THEN stop:=TRUE END;
      WITH board[c] DO
       IF (CARDINAL(key)>=0) &
         (NOT on OR (on & NOT press?[CARDINAL(key)])) & NOT Full?() THEN
         Put(board[c]); press?[CARDINAL(key)]:=on;
         IF on THEN INC(pressco) ELSE DEC(pressco) END;
         Send(change); Sleep;
       ELSIF Full?() THEN Terminal.Write(7c);
       END;
      END;
    END;
    Sleep; INC(i);
  UNTIL stop;
  WHILE Awaited(change) DO Send(change) END;
  Raw(FALSE);
  InitSignal(never); Send(final); Wait(never);
END Reader;

PROCEDURE ReadPair(VAR on,off: CHAR);
  VAR save: BOOLEAN;
BEGIN
  save:=rawon;
  IF NOT save THEN Raw(TRUE) END;
  on:=Terminal.Read(); off:=Terminal.Read();
  IF NOT save THEN Raw(FALSE) END;
  ASSERT(save=rawon);
END ReadPair;

PROCEDURE LetBe(on,off: CHAR; key: CARDINAL);
BEGIN
  ASSERT(press?[key]=dummy);
  board[on ].key:=key; board[on ].on:=TRUE;
  board[off].key:=key; board[off].on:=FALSE;
  press?[key]:=FALSE;
END LetBe;

PROCEDURE SetBreak(on,off: CHAR);
BEGIN
  Break:=off;
END SetBreak;

PROCEDURE WaitAny(VAR p: CARDINAL): BOOLEAN;
  VAR a: Action;
BEGIN
  Wait(change);
  IF stop THEN HALT END;
  Get(a); p:=a.key; RETURN a.on
END WaitAny;

PROCEDURE Flush;
  VAR k: CARDINAL;
BEGIN
  WHILE Sendings(change)>0 DO
    IF WaitAny(k) THEN END
  END;
END Flush;

PROCEDURE Pressed(): INTEGER;
BEGIN RETURN pressco END Pressed;

(*-----------------------------------------------------------------*)

PROCEDURE finish;
BEGIN
  stop:=TRUE; Wait(final); Terminal.print("\rGameBoard OFF\n");
END finish;

PROCEDURE start;
  VAR p: ProcessId; c: CHAR; i: CARDINAL;
BEGIN
  FOR i:=0 TO MAXKEY DO press?[i]:=dummy END;
  FOR c:=0c TO 377c DO
    board[c].on :=FALSE;
    board[c].key:= -1;
  END;
  InitSignal(final);    InitSignal(change);
  stop:=FALSE;          rawon:=FALSE;           pressco:=0;
  SetBreak(124c,324c);
  p:=MakeProcess(Reader,ADR(wsp),SIZE(wsp)); Start(p);
END start;

BEGIN
  IF TTYs.GetTermType(MyTask())#2 THEN
    Terminal.print("только для IBM-PC клавиатуры"); HALT
  END;
  Final(finish); start;
END GameBoard.
