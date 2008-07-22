IMPLEMENTATION MODULE cdsHeap;

IMPORT  SYSTEM;
IMPORT  defErrors;
IMPORT  Heap;
IMPORT  ModelPbl;
IMPORT  Lexicon;

CONST Sorry ='Переполнена динамическая память, очень жаль...';

PROCEDURE   Allocate(VAR a: SYSTEM.ADDRESS;     size: INTEGER);
BEGIN
  Heap.allocate(a,size);
  IF Heap.done THEN RETURN END;
  IF Heap.error=defErrors.no_memory THEN
    ModelPbl.Message:=Sorry;
    ModelPbl.RaiseInMe(ModelPbl.MemoryOverflow)
  ELSE
    Lexicon.perror(ModelPbl.Message,Heap.error,"cdsHeap: %%s");
    ModelPbl.RaiseInMe(ModelPbl.Failure)
  END;
END Allocate;

PROCEDURE Deallocate(VAR a: SYSTEM.ADDRESS;     size: INTEGER);
BEGIN
  Heap.deallocate(a,size);
  IF Heap.done THEN RETURN END;
  Lexicon.perror(ModelPbl.Message,Heap.error,"cdsHeap: %%s");
  ModelPbl.RaiseInMe(ModelPbl.Failure)
END Deallocate;

PROCEDURE Reallocate(VAR a: SYSTEM.ADDRESS; VAR high: INTEGER;
                                            len,el_byte_size: INTEGER);
BEGIN
  Heap.reallocate(a,high,len,el_byte_size);
  IF Heap.done THEN RETURN END;
  IF Heap.error=defErrors.no_memory THEN
    ModelPbl.Message:=Sorry;
    ModelPbl.RaiseInMe(ModelPbl.MemoryOverflow)
  ELSE
    Lexicon.perror(ModelPbl.Message,Heap.error,"cdsHeap: %%s");
    ModelPbl.RaiseInMe(ModelPbl.Failure)
  END;
END Reallocate;

END cdsHeap.
