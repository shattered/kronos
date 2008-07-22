DEFINITION MODULE ModelMisc; (* Sem 13-Feb-87. (c) KRONOS *)

FROM Model     IMPORT   Object, Objects, Segment;

PROCEDURE PinLocation(o: Object; VAR X,Y: CARDINAL);

TYPE Seg=ARRAY [0..2] OF INTEGER;

VAR X1,X2,Y1,Y2,
    Size,ViasSize : CARDINAL;
    Layer         : BITSET;
    Fixed,Empty   : BOOLEAN;
    Undef         : BOOLEAN;
    Ident         : CARDINAL;
    Signal        : Object;

PROCEDURE StartConductor(c: Object);

PROCEDURE NextConductor;

PROCEDURE AppConductor;

PROCEDURE DelConductor;

PROCEDURE FindConductor(id: CARDINAL);

PROCEDURE CreateConductor(size: CARDINAL);

PROCEDURE TruncConductor;

PROCEDURE PackSeg(VAR s: Segment);

PROCEDURE UnPackSeg(VAL s: Segment);

END ModelMisc.
