IMPLEMENTATION MODULE Vectors; (* nick 02-Aug-90. (c) KRONOS *)

IMPORT  mat: realMath;

IMPORT  tty: Terminal;

CONST x=0; y=1; z=2;

PROCEDURE sub(VAR vr: VECTOR; v1,v2: VECTOR);  --- vr=v1-v2
BEGIN
  vr[0]:=v1[0]-v2[0];
  vr[1]:=v1[1]-v2[1];
  vr[2]:=v1[2]-v2[2]
END sub;

PROCEDURE add(VAR vr: VECTOR; v1,v2: VECTOR);  --- vr=v1+v2
BEGIN
  vr[0]:=v1[0]+v2[0];
  vr[1]:=v1[1]+v2[1];
  vr[2]:=v1[2]+v2[2]
END add;

PROCEDURE vml(VAR vr: VECTOR; v1,v2: VECTOR);  --- vr=[v1*v2]
BEGIN
  vr[x]:=v1[y]*v2[z]-v1[z]*v2[y];
  vr[y]:=v1[z]*v2[x]-v1[x]*v2[z];
  vr[z]:=v1[x]*v2[y]-v1[y]*v2[x];
END vml;

PROCEDURE sml(v1,v2: VECTOR): REAL;            --- RETURN [v1 v2]
BEGIN
  RETURN v1[x]*v2[x]+v1[y]*v2[y]+v1[z]*v2[z]
END sml;

PROCEDURE len(v: VECTOR): REAL;            --- lenght of vect
  VAR X,Y,Z: REAL;
BEGIN
  X:=v[x];  Y:=v[y];  Z:=v[z];
  RETURN mat.sqrt(X*X+Y*Y+Z*Z)
END len;

PROCEDURE normal(VAR v: VECTOR);
  VAR len: REAL;
BEGIN
  len:=mat.sqrt(v[x]*v[x]+v[y]*v[y]+v[z]*v[z]);
  v[0]:=v[0]/len;
  v[1]:=v[1]/len;
  v[2]:=v[2]/len;
END normal;

PROCEDURE equal(v1,v2: VECTOR): BOOLEAN;
BEGIN
  RETURN (v1[x]=v2[x]) & (v1[y]=v2[y]) & (v1[z]=v2[z])
END equal;

PROCEDURE VxM(VAR vr: VECTOR; v: VECTOR; m: MATRIX);
  VAR s,v0,v1,v2: REAL;
BEGIN
  v0:=v[0];  v1:=v[1]; v2:=v[2];
  s:=v0*m[0,0];  s:=v1*m[0,1]+s;  s:=v2*m[0,2]+s; vr[0]:=s;
  s:=v0*m[1,0];  s:=v1*m[1,1]+s;  s:=v2*m[1,2]+s; vr[1]:=s;
  s:=v0*m[2,0];  s:=v1*m[2,1]+s;  s:=v2*m[2,2]+s; vr[2]:=s;
END VxM;

PROCEDURE MxM(VAR mr: MATRIX; l,r: MATRIX);
  VAR i,j,k: INTEGER; s: REAL;
BEGIN
  FOR i:=0 TO 2 DO    -- str in mr
    FOR j:=0 TO 2 DO  -- col in mr
      s:=0.;
      FOR k:=0 TO 2 DO s:=s+l[k,i]*r[j,k] END;
      mr[j,i]:=s
    END
  END
END MxM;

VAR E: MATRIX;

BEGIN
  null[x]:=0.; E[0,0]:=1.; E[1,0]:=0.; E[2,0]:=0.;
  null[y]:=0.; E[0,1]:=0.; E[1,1]:=1.; E[2,1]:=0.;
  null[z]:=0.; E[0,2]:=0.; E[1,2]:=0.; E[2,2]:=1.
END Vectors.
