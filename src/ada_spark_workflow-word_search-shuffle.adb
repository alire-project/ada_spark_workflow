with Ada.Numerics.Discrete_Random;

procedure Ada_SPARK_Workflow.Word_Search.Shuffle
  (Vect : in out Vector_Pck.Vector)
is
   use Vector_Pck;

   package Rand_Index is new Ada.Numerics.Discrete_Random (Index_Type);

   Gen : Rand_Index.Generator;

   First : constant Index_Type := First_Index (Vect);
   Last  : constant Index_Type := Last_Index (Vect);

begin

   Rand_Index.Reset (Gen);

   for X in First .. Last loop
      Vector_Pck.Swap (Vect,
                       X,
                       Rand_Index.Random (Gen, X, Last));
   end loop;

end Ada_SPARK_Workflow.Word_Search.Shuffle;
