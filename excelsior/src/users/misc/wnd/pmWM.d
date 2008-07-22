DEFINITION MODULE pmWM; (* Leo 23-Apr-91. (c) KRONOS *)

IMPORT  pmWnd;

TYPE
  WINDOW = pmWnd.WINDOW;
  EVENT  = RECORD
             win : WINDOW;  (* located window    *)
             but : BITSET;  (* mouse buttons     *)
             cha : BITSET;  (* changed buttons   *)
             x,y : INTEGER; (* on screen  coord  *)
             key : CHAR;    (* keyboard pressed  *)
           END;

    MONITOR = PROCEDURE (EVENT);

PROCEDURE incl(w: WINDOW);
(* includes window in WM monitoring *)

PROCEDURE excl(w: WINDOW);
(* excludes window out of WM monitoring *)


PROCEDURE monitoring(w: WINDOW; p: MONITOR);
(*
   procedure "p" will be called when any of the next events happend
   when mouse cursor are onto the visible part of window "w":
   1. mouse cursor change it`s coordinates;
   2. any key pressed on the keyboard;
   3. any button pressed or released on the mouse
   4. timeout value (20 miliseconds) exceded
   Note: when cursor moves from one window to another event
   happend for both windows!
*)

PROCEDURE monitor(w: WINDOW): MONITOR;
(* return previouse tracker *)

PROCEDURE listen(VAR event: EVENT);
(* waits any event happend with tracking *)

PROCEDURE read(VAR event: EVENT);
(* waits any event happend without tracking *)

PROCEDURE back;
(* puts last "read" event back to the event queue *)

END pmWM.

(*

   Calls of "tracking" procedures happen
   only when application program is waiting
   events in procedures "monitor".

   Typical example of monitoring procedure is:

   PROCEDURE applicationmonitor(event: EVENT);
   BEGIN
     IF (* application not need pay attetion on this "event" *) THEN
       RETURN
     END;
     LOOP
       (* do usefull work according to "event" *)
       read(event);
       IF (* it's not my event *) THEN
         back; (* to put event back to the event queue *)
         RETURN
       END
     END
   END applicationmonitor;

   When application program are reading events through
   "read(event)" procedure, calls of tracking
   suppressed to prevent recursive call of
   tracking procedure.

   Application have to put event "back" to event queue
   after "read" if it decide to finish it`s tracking action,
   otherwise this event will be lost for other monitors.

*)
