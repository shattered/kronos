MODULE T9P5D7;

  (* TECT ПPEДЛAГAET ЛИШHЮЮ METKУ *)

FROM InOut IMPORT WriteString;
  TYPE DAY=(MON,TUE,WED);
  VAR A:INTEGER;D:[MON..TUE];

BEGIN FOR D:=MON TO TUE DO
          CASE D OF
             MON:A:=1 |
             TUE:A:=2 |
             WED:A:=3    (*IMPOSSIBLE LABEL*)
          END
      END;
      WriteString('PERMITS IMPOSSIBLE CASE LABEL...9.5-7',38)
END T9P5D7.
