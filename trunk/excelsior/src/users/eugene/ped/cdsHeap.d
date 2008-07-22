DEFINITION MODULE cdsHeap;

IMPORT  SYSTEM;

PROCEDURE   Allocate(VAR a: SYSTEM.ADDRESS;     size: INTEGER);
PROCEDURE Deallocate(VAR a: SYSTEM.ADDRESS;     size: INTEGER);
PROCEDURE Reallocate(VAR a: SYSTEM.ADDRESS; VAR high: INTEGER;
                                            len,el_byte_size: INTEGER);

END cdsHeap.
