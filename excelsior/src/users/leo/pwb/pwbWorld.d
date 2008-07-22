DEFINITION MODULE pwbWorld; (* Ilx 29-Mar-90. (c) KRONOS *)

TYPE
  SIGNAL;
  MODEL;

  SEGMENT = RECORD
              start,end,size: INTEGER;
            END;

CONST
  power=0; fantom=1; fixed=2;

  Clearens  =  8;  (* 200 mkm *)
  Overlay   =  2;  (*  50 mkm *)
  Grid      = 24;  (* 625 mkm = 1/40" *)

VAL done: BOOLEAN;
   error: INTEGER;

   snull: SIGNAL;
   mnull: MODEL;


PROCEDURE new_mdl (VAR mdl: MODEL;    name: ARRAY OF CHAR);
PROCEDURE ins_mdl(dest,sou: MODEL);
PROCEDURE rem_mdl     (VAR mdl: MODEL);
PROCEDURE mdl_changed (mdl: MODEL): BOOLEAN;
PROCEDURE mdl_rename  (mdl: MODEL;     name: ARRAY OF CHAR);
PROCEDURE mdl_name    (mdl: MODEL; VAR name: ARRAY OF CHAR);
PROCEDURE mdl_set_size(mdl: MODEL;     sx,sy: INTEGER);
PROCEDURE mdl_get_size(mdl: MODEL; VAR sx,sy: INTEGER);
PROCEDURE find_mdl(VAR mdl: MODEL; where: MODEL; name: ARRAY OF CHAR);
PROCEDURE open        (mdl: MODEL; internal: BOOLEAN);
PROCEDURE opencount   (mdl: MODEL): INTEGER;
PROCEDURE close       (mdl: MODEL; internal: BOOLEAN);
PROCEDURE read_mdl(VAR mdl: MODEL; name: ARRAY OF CHAR);
PROCEDURE write_mdl   (mdl: MODEL; name: ARRAY OF CHAR);

PROCEDURE new_chip (VAR chip: MODEL; type,mdl: MODEL;
                                l_name,e_name: ARRAY OF CHAR);
PROCEDURE rem_chip     (VAR chip: MODEL);
PROCEDURE chip_rename  (chip: MODEL;     l_name,e_name: ARRAY OF CHAR);
PROCEDURE chip_name    (chip: MODEL; VAR l_name,e_name: ARRAY OF CHAR);
PROCEDURE chip_type    (chip: MODEL; VAR type: MODEL);
PROCEDURE find_chip(VAR chip: MODEL; mdl: MODEL; l_name: ARRAY OF CHAR);
PROCEDURE move_chip    (chip: MODEL;     x,y,r: INTEGER);
PROCEDURE chip_pos     (chip: MODEL; VAR x,y,r: INTEGER);

PROCEDURE new_sig  (VAR sig: SIGNAL; mdl: MODEL; internal?: BOOLEAN;
                    type: BITSET; name: ARRAY OF CHAR);
PROCEDURE rem_sig      (VAR sig: SIGNAL);
PROCEDURE sig_set_type (sig: SIGNAL;     type: BITSET);
PROCEDURE sig_get_type (sig: SIGNAL; VAR type: BITSET);
PROCEDURE sig_rename   (sig: SIGNAL;     name: ARRAY OF CHAR);
PROCEDURE sig_name     (sig: SIGNAL; VAR name: ARRAY OF CHAR);

PROCEDURE find_sig (VAR sig: SIGNAL; mdl: MODEL; name: ARRAY OF CHAR);
PROCEDURE empty_sig(VAR sig: SIGNAL; mdl: MODEL);

PROCEDURE get_sig_gang (sig: SIGNAL; VAR gang: INTEGER);
PROCEDURE get_sig_hard (sig: SIGNAL; VAR hard: INTEGER);
PROCEDURE set_sig_gang (sig: SIGNAL;     gang: INTEGER);
PROCEDURE set_sig_hard (sig: SIGNAL;     hard: INTEGER);

PROCEDURE connect(    sig: SIGNAL; chip: MODEL; pin_name: ARRAY OF CHAR);

PROCEDURE tied_to(VAR sig: SIGNAL; chip: MODEL; pin_name: ARRAY OF CHAR);

TYPE
  ITERMODEL =
    RECORD
      mdl: MODEL;
      id : INTEGER;
    END;

   ITERSIG =
     RECORD
       sig: SIGNAL;
       id : INTEGER;
     END;

PROCEDURE first_tied(VAR pin_name: ARRAY OF CHAR; VAR chip: MODEL;
                     VAR    where: ITERSIG; sig: SIGNAL): BOOLEAN;

PROCEDURE next_tied(VAR pin_name: ARRAY OF CHAR; VAR chip: MODEL;
                       VAR where: ITERSIG): BOOLEAN;

PROCEDURE first_mdl(VAR mdl: MODEL; VAR where: ITERMODEL; host: MODEL): BOOLEAN;
PROCEDURE next_mdl (VAR mdl: MODEL; VAR where: ITERMODEL           ): BOOLEAN;

PROCEDURE first_sig(VAR sig: SIGNAL; VAR where: ITERMODEL; host: MODEL): BOOLEAN;
PROCEDURE next_sig (VAR sig: SIGNAL; VAR where: ITERMODEL           ): BOOLEAN;

PROCEDURE first_epin(VAR epin: SIGNAL; VAR where: ITERMODEL; host: MODEL): BOOLEAN;
PROCEDURE next_epin (VAR epin: SIGNAL; VAR where: ITERMODEL           ): BOOLEAN;

PROCEDURE first_chip(VAR chip: MODEL; VAR where: ITERMODEL; host: MODEL): BOOLEAN;
PROCEDURE next_chip (VAR chip: MODEL; VAR where: ITERMODEL           ): BOOLEAN;

TYPE DWS;

TYPE
  WIRE =
    RECORD
      start,end: INTEGER;
      ident    : INTEGER;
      sig      : SIGNAL;
      dws      : DWS;
    END;

 TYPE
   TOPOLOGY =
     RECORD
       x1,y1: INTEGER;
       x2,y2: INTEGER;
       size : INTEGER;
       vsize: INTEGER;
       layer: BITSET;
       fixed: BOOLEAN;
     END;

PROCEDURE on_line    (line0,line1: TOPOLOGY): BOOLEAN;
PROCEDURE side       (x1,y1,x2,y2,x,y: INTEGER): INTEGER;
PROCEDURE strong_side(x1,y1,x2,y2,x,y: INTEGER): INTEGER;
PROCEDURE len        (x1,y1,x2,y2,x,y: INTEGER): INTEGER;

PROCEDURE sig2wire(sig: SIGNAL; VAR wire: WIRE);

PROCEDURE stars     (signal: SIGNAL): INTEGER;
PROCEDURE first_star(VAR wire: WIRE): BOOLEAN;
PROCEDURE next_star (VAR wire: WIRE): BOOLEAN;

PROCEDURE first_seg(VAR seg: SEGMENT; box: SEGMENT; VAR w: WIRE): BOOLEAN;
PROCEDURE next_seg (VAR seg: SEGMENT; box: SEGMENT; VAR w: WIRE): BOOLEAN;

PROCEDURE pack  (VAR s: SEGMENT;     top: TOPOLOGY);
PROCEDURE unpack(    s: SEGMENT; VAR top: TOPOLOGY);

PROCEDURE reset_changes(mdl: MODEL);
PROCEDURE last_changes (mdl: MODEL; VAR clip: TOPOLOGY);

PROCEDURE app_seg(VAR w: WIRE; seg: SEGMENT);
PROCEDURE del_seg(VAR w: WIRE);


TYPE
  ITERBOX =
    RECORD
      dws            : DWS;
      x1,y1,x2,y2    : INTEGER;
      xid,yid,did,lid: INTEGER;
      sigs           : POINTER TO DYNARR OF SIGNAL;
    END;

PROCEDURE first_in_box(VAR seg: SEGMENT; box: SEGMENT; VAR  sig: SIGNAL;
                                                       VAR ibox: ITERBOX;
                           mdl: MODEL;   int: BOOLEAN): BOOLEAN;

PROCEDURE next_in_box(VAR seg: SEGMENT; box: SEGMENT;
                      VAR sig: SIGNAL; VAR ibox: ITERBOX): BOOLEAN;

PROCEDURE ins_range(sig: SIGNAL; top: TOPOLOGY; chk: BOOLEAN;
                             VAR short_circuit: BOOLEAN);

PROCEDURE del_range(sig: SIGNAL; top: TOPOLOGY);

PROCEDURE shorted  (top: TOPOLOGY; sig: SIGNAL;
                    VAR seg: SEGMENT; VAR short: SIGNAL): BOOLEAN;

END pwbWorld.
