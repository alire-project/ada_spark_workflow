with Ada.Containers.Formal_Vectors;

generic
   with package Vector_Pck is new Ada.Containers.Formal_Vectors (<>);
procedure Ada_SPARK_Workflow.Word_Search.Shuffle
  (Vect : in out Vector_Pck.Vector);
