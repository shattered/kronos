MODULE p; (*  10-Sep-91. (c) KRONOS *)
IMPORT  plt: Plotter;

BEGIN

 plt.reset;
 plt.origin(0,0);
 plt.line(0,0,200,200);

 plt.circ(100,100,80);
 LOOP  END

END p.
