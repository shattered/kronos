MODULE ex; (* Leo  28-Jun-87. (c) KRONOS *)
           (* Ned  06-Oct-89. (c) KRONOS *)


IMPORT  arg: tskArgs;
IMPORT main: exMain;
IMPORT  key: Keyboard;
IMPORT  cmd: exCmd;
IMPORT face: myEditor;
IMPORT       ASCII;

CONST empty = 0c;
  lpull   = CHAR( ORD('W') MOD 32);
  rpull   = CHAR( ORD('D') MOD 32);

PROCEDURE Gold;
  VAR k: CHAR;
BEGIN
  k:=main.GetKey();
  CASE k OF
    |key.pgup   : main.FileTop;
    |key.pgdw   : main.FileBottom;
    |key.left   : main.LineLeft;
    |key.right  : main.LineRight;
    |key.del    : main.DelWord1;
    |key.back   : main.RubWord1;
    |key.f7     : main.DupOver;
    |key.up     : main.WordLeft1;
    |key.dw     : main.WordRight1;
    |key.f4     : main.DelLn(key.f4,TRUE);
    |key.f3     : main.InsLn(key.f3,TRUE);
    |key.f1     : cmd.Command;
    |key.rep    : main.InsRep(-1);
    |key.f10    : main.SETUP
    |key.f2     : main.UnDel
  ELSE
    IF    k='{' THEN main.BeginMacro(TRUE)
    ELSIF k='}' THEN main.EndMacro(TRUE,2)
    ELSIF k='&' THEN main.EmitTime
    ELSE             main.PopMacro(TRUE,k)
    END;
  END;
END Gold;

PROCEDURE Bronze;
  VAR k: CHAR;
BEGIN k:=main.GetKey();
  IF    k=key.f1    THEN main.SwapOld;
  ELSIF k='{'       THEN main.BeginMacro(FALSE)
  ELSIF k='}'       THEN main.EndMacro(FALSE,2)
  ELSIF k=key.up    THEN main.WordLeft0
  ELSIF k=key.dw    THEN main.WordRight0
  ELSIF k=key.left  THEN main.WordLeft0
  ELSIF k=key.right THEN main.WordRight0
  ELSIF k=key.del   THEN main.DelWord0
  ELSIF k=key.back  THEN main.RubWord0
  ELSE main.PopMacro(FALSE,k)
  END;
END Bronze;

PROCEDURE Silver;
  VAR k: CHAR;
BEGIN k:=main.GetKey();
  CASE k OF
    |key.pgdw : cmd.Shell;
    |key.up   : main.JumpBegin
    |key.dw   : main.JumpEnd
    |'['      : main.MarkBegin
    |']'      : main.MarkEnd
    |'.'      : main.MarkLine
    |key.right: cmd.FindNext
    |'w','W'  : cmd.write_file;
    |'@','1'  : main.FmtPara
    |'!','2'  : main.Centre(FALSE)
    |'<','('  : main.setlmargin
    |'>',')'  : main.setrmargin
    |'|'      : main.setfmargin
    |'&'      : main.EmitTime
    |'+'      : cmd.show_mark(FALSE)
    |'-'      : cmd.show_mark(TRUE)
  ELSE
    cmd.Filter(k);
  END;
END Silver;

PROCEDURE Keypad;
  VAR k: CHAR;
BEGIN
  LOOP k:=main.GetKey();
    CASE k OF
      |key.up     : main.Up(k);
      |key.dw     : main.Dw(k);
      |key.left   : main.Left(k);
      |key.right  : main.Right(k);
      |key.bcktab : main.Ltab(k);
      |key.home   : main.NewLine(key.home); main.Up(key.home);
      |key.end    : main.LineRight;
      |key.tab    : main.Rtab(k);
      |key.f5     : main.SwapUp(k);
      |key.f6     : main.SwapDw(k);
      |key.f4     : main.DelLn(k,FALSE);
      |key.f3     : main.InsLn(k,FALSE);
      |key.f7     : main.DupLn(k);
      |key.ins    : main.InsCh(' ');
      |key.del    : main.DelCh;
      |key.pgup   : main.PageUp(k);
      |key.pgdw   : main.PageDw(k);
      |lpull      : main.PullLeft;
      |rpull      : main.PullRight;

      |key.rep    : main.InsRep(0);
      |key.f8     : main.ClearTail;
      |key.back   : main.Del(k);
      |key.newln  : main.NewLine(k);

      |key.f1     : Gold;
      |key.f10    : Silver;
      |key.f2     : Bronze;

      |key.exit   : IF cmd.write_and_exit() THEN EXIT END;

      |ASCII.CR   : main.CarriageReturn(k);
      |ASCII.LF   : main.LineFeed(k);
    ELSE
      main.ValidChar(k);
    END;
  END; (* LOOP *)
END Keypad;

BEGIN
  IF HIGH(arg.words)<0 THEN cmd.SetMain('')
  ELSE cmd.SetMain(arg.words[0])
  END;
  cmd.FirstRead;
  main.break_mode(TRUE);
  main.alarm_mode(TRUE);
  key.set_break(0);
  IF arg.flag('-','w') THEN cmd.SetMain("") END;
  cmd.fill_my_editor;
  face.start;
  cmd.show_mark(FALSE);
  Keypad;
  main.FINISH;
END ex.
