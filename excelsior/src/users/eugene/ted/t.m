MODULE t; (* 28-Oct-89. (c) KRONOS *)

FROM tal        IMPORT  app, box, show, tile;
FROM VIDEO      IMPORT  start, fill, finish, trapez, color;
FROM Terminal   IMPORT  Read, print, Home;
FROM Random     IMPORT  Rand;

PROCEDURE prt(t: tile);
BEGIN
  print('(%4d,%4d)-(%4d,%4d) %d\n',t^.x,t^.y,t^.rt^.x,t^.up^.y,t^.dt);
END prt;

VAR x,y,sx,sy,i: INTEGER;

BEGIN
  start; fill(0);
  LOOP
    box(0,0,800,300,show);
    IF Read()=0c THEN END;
    FOR i:=0 TO 99 DO
      sx:=Rand() MOD 100 + 30;
      sy:=Rand() MOD 50 + 15;
      x:=Rand() MOD (699-sx-20)+10;
      y:=Rand() MOD (199-sy-5)+5;
      sx:=sx DIV 10 * 10;
      sy:=sy DIV 5 * 5;
      x:=x DIV 10 * 10;
      y:=y DIV 5 * 5;
      app(x,y,sx,sy,Rand() MOD 4);
    END;
  END;
END t.
