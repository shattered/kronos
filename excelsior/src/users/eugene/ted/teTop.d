DEFINITION MODULE teTop; (* 25-Oct-89. (c) KRONOS *)


TYPE
  signal=POINTER TO signal_rec;
  segment=POINTER TO segment_rec;
  seg_tree=POINTER TO seg_node;
  seg_node=RECORD
    up    : seg_tree;
    dw    : seg_tree;
    seg_fr: INTEGER;
    seg_no: INTEGER;
    seg   : segment;
  END;
  segment_rec=RECORD
    x,y,l : INTEGER;
    sx,sy : INTEGER;
    sig   : signal;
  END;
  signal_rec=RECORD
    name  : ARRAY [0..31] OF CHAR;
    seg   : POINTER TO ARRAY [0..0FFFFh] OF segment;
    seg_no: INTEGER;
  END;

PROCEDURE app_seg(VAR t: seg_tree; s: segment_rec);

END teTop.
