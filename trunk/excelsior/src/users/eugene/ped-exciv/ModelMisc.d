DEFINITION MODULE ModelMisc; (* Sem 13-Feb-87. (c) KRONOS *)

FROM Model     IMPORT   Object, Segment;

VAR X1,X2,Y1,Y2,
    Size,ViasSize : INTEGER;
    Layer         : BITSET;
    Fixed,Empty   : BOOLEAN;
    Ident         : INTEGER;
    Signal        : Object;

PROCEDURE StartConductor(c: Object; ExtPin: BOOLEAN);

PROCEDURE NextConductor;

PROCEDURE AppConductor(ExtPin: BOOLEAN);

PROCEDURE DelConductor;

PROCEDURE FindConductor(id: INTEGER);

PROCEDURE CreateConductor(size: INTEGER; ExtPin: BOOLEAN);

PROCEDURE TruncConductor(ExtPin: BOOLEAN);

PROCEDURE PackSeg(VAR s: Segment);

PROCEDURE UnPackSeg(VAR s: Segment);

END ModelMisc.
