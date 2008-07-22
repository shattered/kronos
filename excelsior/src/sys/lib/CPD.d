DEFINITION MODULE CPD; (* Leo & Leg 21-Sep-89. (c) KRONOS *)

IMPORT  SYSTEM;

TYPE
  STATE  = POINTER TO STATUS;
  STATUS = RECORD
             type  : INTEGER;
             nokeys: INTEGER;
             rel   : BOOLEAN;
             x_max : INTEGER;
             y_max : INTEGER;
             x,y   : INTEGER; (* x,y & keys by was read by     *)
             keys  : BITSET;  (* last call of "read" procedure *)
           END;

VAL
  done : BOOLEAN;
  error: INTEGER;
  state: STATE;

PROCEDURE read(VAR x,y: INTEGER; VAR pressedkeys: BITSET);

PROCEDURE ready(): INTEGER;

PROCEDURE wait(timeout: INTEGER);

PROCEDURE ioctl(no: INTEGER; SEQ args: SYSTEM.WORD);

PROCEDURE attach(dev_name: ARRAY OF CHAR);

PROCEDURE reset; (* to initial state *)

PROCEDURE restore(status: STATUS);

PROCEDURE nop;

END CPD.


wait
====

  when timeout occured 'done=FALSE' 'error=time_out'

N O T E

     This  module  does  not  attach  driver  at  initialization.
Attaching  is  performed  when  first  calling  of driver occure.
Variable  -state- points to blank record before attaching. Hence,
if you want to use variable -state- before any calling to driver,
you have to call procedure -nop- to fulfil attaching.

                                              Leg, 15-Oct-90

N O T E

    x,y & keys in "state" update at every call of "read" procedure,
    not in driver!

                                             Leo, 27-Dec-90
