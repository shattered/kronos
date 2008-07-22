MODULE t; (*  29-Jan-91. (c) KRONOS *)

IMPORT  men : cdsMenu;
IMPORT  wnd : libWindows;
IMPORT  vg  : cdsGrafic;

VAR w: wnd.window;
  x,y: INTEGER;

BEGIN
  men.free_mem(0,0);
END t.
