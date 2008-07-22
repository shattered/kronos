IMPLEMENTATION MODULE pcK; (* Ned 28-Mar-91. (c) KRONOS *)

BEGIN
  WITH null_struct DO
    mode:=invtype;
    mno :=0;
    obj :=NIL;
    next:=NIL;
    link:=NIL;
    locs:=NIL;
    base:=NIL;
    inx :=NIL;
    size:=-1;
    n   :=0;
    m   :=0;
    ref :=0;
    pos :=0;
    adr :=0;
    ext :=NIL;
  END;
  WITH null_object DO
    mode :=inv;
    name :='';
    l    :=NIL;
    r    :=NIL;
    next :=NIL;
    type :=NIL;
    tags :={};
    scope:=0;
    adr  :=0;
    head :=NIL;
    ext  :=NIL;
    pos  :=0;
  END;
  WITH null_node DO
    mode:=inv;
    sub :=0;
    next:=NIL;
    type:=NIL;
    obj :=NIL;
    ext :=NIL;
    pos :=0;
    l   :=NIL;
    r   :=NIL;
  END;
END pcK.
