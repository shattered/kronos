IMPLEMENTATION MODULE Statistics; (* Leo 14-May-90. (c) KRONOS *)

IMPORT      SYSTEM;
IMPORT  os: osKernel;
IMPORT  fs: osFiles;

PROCEDURE get(no: INTEGER; VAR val: SYSTEM.WORD);
  VAR i,j,k,l: INTEGER;
BEGIN
  val:=0;
  CASE no DIV 100h OF
  |01h: (* os *)
        CASE no OF
        |os_vers   : val:=os.version
        |os_runtime: val:=os.timer DIV 1000 * os.tick
        ELSE
        END
  |02h: (* mem *)
        CASE no OF
        |mem_top  : val:=os.memtop
        |mem_core : val:=os.core
        |mem_total: val:=os.total
        |mem_free : val:=os.free
        ELSE
        END
  |03h: (* files *)
        fs.statistic(i,j,k,l);
        CASE no OF
        |fs_chsize : val:=i
        |fs_dkwrite: val:=j
        |fs_dkread : val:=k
        |fs_chread : val:=l
        ELSE
        END
  ELSE
  END
END get;

END Statistics.
