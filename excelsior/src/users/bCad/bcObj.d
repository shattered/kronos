DEFINITION MODULE bcObj; (* brd 06-Dec-90. (c) KRONOS *)

IMPORT  def: bcDef;

TYPE lin_ptr = POINTER TO LINE;
     cir_ptr = POINTER TO CIRC;
     arc_ptr = POINTER TO ARC;
     ell_ptr = POINTER TO ELLIPS;
     pln_ptr = POINTER TO PLINE;
     txt_ptr = POINTER TO TEXT;
    surf_ptr = POINTER TO SURF;
     grp_ptr = POINTER TO GROUP;

     Clip   = RECORD x,y,X,Y: INTEGER END;

     LINE   = RECORD
                next: lin_ptr;
                prev: lin_ptr;
                line: def.Line;
                rect: Clip
              END;

     CIRC   = RECORD
                next: cir_ptr;
                prev: cir_ptr;
                circ: def.Circ;
                rect: Clip
              END;

        ARC = RECORD
                next: arc_ptr;
                prev: arc_ptr;
                arc : def.Arc;
                rect: Clip
              END;

     ELLIPS = RECORD
                  next: ell_ptr;
                  prev: ell_ptr;
                ellips: def.Ellips;
                rect: Clip
              END;

     PLINE  = RECORD
                 next: pln_ptr;
                 prev: pln_ptr;
                pline: def.Pline;
                 rect: Clip
              END;

     TEXT = RECORD
              next: txt_ptr;
              prev: txt_ptr;
              text: def.Text;
              rect: Clip
            END;

     SURF = RECORD
              next: surf_ptr;
              prev: surf_ptr;
              surf: def.Surf;
              rect: Clip
            END;

     GROUP  = RECORD
                next: grp_ptr;
                prev: grp_ptr;
               group: def.Group;
                rect: Clip
              END;

      LAYER = RECORD
                name : ARRAY [0..15] OF CHAR;
                lines: lin_ptr;
                circs: cir_ptr;
                arcs : arc_ptr;
                plins: pln_ptr;
                ellps: ell_ptr;
                texts: txt_ptr;
                surfs: surf_ptr;
                grps : grp_ptr
              END;

 Layer  =  POINTER TO LAYER;
 MODEL  =  DYNARR OF Layer;
 Model  =  POINTER TO MODEL;


VAR  mark_lin: DYNARR OF lin_ptr;   klin: DYNARR OF INTEGER;
     mark_cir: DYNARR OF cir_ptr;   kcir: DYNARR OF INTEGER;
     mark_arc: DYNARR OF arc_ptr;   karc: DYNARR OF INTEGER;
     mark_ell: DYNARR OF ell_ptr;   kell: DYNARR OF INTEGER;
     mark_pln: DYNARR OF pln_ptr;   kpln: DYNARR OF INTEGER;
     mark_grp: DYNARR OF grp_ptr;   kgrp: DYNARR OF INTEGER;
     mark_txt: DYNARR OF txt_ptr;   ktxt: DYNARR OF INTEGER;
     mark_srf: DYNARR OF surf_ptr;  ksrf: DYNARR OF INTEGER;

VAL null_clip: Clip;
        error: INTEGER;
         done: BOOLEAN;

--------------------------- INSERTING --------------------------

PROCEDURE copy_pict (VAR dst: def.PICTURE;  sou: def.PICTURE);
PROCEDURE copy_vpict(VAR dst: def.VPICTURE; sou: def.VPICTURE);

PROCEDURE new_line (model: Model; lin: def.Line;      lay: INTEGER);
PROCEDURE new_circ (model: Model; cir: def.Circ;      lay: INTEGER);
PROCEDURE new_arc  (model: Model; arc: def.Arc;       lay: INTEGER);

PROCEDURE new_pline(model: Model; pln: def.Pline;     lay: INTEGER);
PROCEDURE new_ell  (model: Model; ell: def.Ellips;    lay: INTEGER);

PROCEDURE new_text (model: Model; txt: def.Text;      lay: INTEGER);

PROCEDURE new_surf (model: Model; surf: def.Surf;     lay: INTEGER);

PROCEDURE new_group(model: Model; grp: def.Group;     lay: INTEGER);

PROCEDURE new_layer(model: Model);

--------------------------- DELETING ---------------------------

PROCEDURE pdispose(VAR pict: def.PICTURE );
PROCEDURE vdispose(VAR pict: def.VPICTURE);

PROCEDURE dispose_line(VAR l: def.Line);
PROCEDURE dispose_circ(VAR c: def.Circ);
PROCEDURE dispose_arc (VAR a: def.Arc);

PROCEDURE dispose_ell (VAR e: def.Ellips);
PROCEDURE dispose_pln (VAR p: def.Pline);

PROCEDURE dispose_txt (VAR t: def.Text);
PROCEDURE dispose_surf(VAR s: def.Surf);
PROCEDURE dispose_grp (VAR g: def.Group);

PROCEDURE dispose_mrk;

PROCEDURE rem_line (model: Model;  del_lin: lin_ptr; lay:INTEGER);
PROCEDURE rem_circ (model: Model;  del_cir: cir_ptr; lay:INTEGER);
PROCEDURE rem_arc  (model: Model;  del_arc: arc_ptr; lay:INTEGER);

PROCEDURE rem_ellip(model: Model;  del_ell: ell_ptr; lay:INTEGER);
PROCEDURE rem_pline(model: Model;  del_pln: pln_ptr; lay:INTEGER);
PROCEDURE rem_group(model: Model;  del_grp: grp_ptr; lay:INTEGER);

PROCEDURE rem_text (model: Model;  del_txt: txt_ptr; lay:INTEGER);

PROCEDURE rem_surf (model: Model;  del_surf: surf_ptr; lay: INTEGER);

PROCEDURE rem_lay(model: Model; lay: INTEGER);
PROCEDURE rem_all(model: Model);

----------------------------- Clip -----------------------------

PROCEDURE clp_vct(vect: def.VECTOR;   VAR clip: Clip);
PROCEDURE clp_vpc(vpic: def.VPICTURE; VAR clip: Clip);
PROCEDURE clp_pic(pict: def.PICTURE;  VAR clip: Clip);

PROCEDURE clp_lin(line: def.Line;      VAR clip: Clip);
PROCEDURE clp_cir(circ: def.Circ;      VAR clip: Clip);
PROCEDURE clp_arc(arc: def.Arc;        VAR clip: Clip);

PROCEDURE clp_ell(ell: def.Ellips;     VAR clip: Clip);
PROCEDURE clp_pln(pln: def.Pline;      VAR clip: Clip);

PROCEDURE clp_txt(txt: def.Text;       VAR clip: Clip);

PROCEDURE clp_surf(surf: def.Surf;     VAR clip: Clip);

PROCEDURE clp_grp(grp: def.Group;      VAR clip: Clip);

PROCEDURE fuse_clp(clip0: Clip;  VAR clip1: Clip);

END bcObj.
