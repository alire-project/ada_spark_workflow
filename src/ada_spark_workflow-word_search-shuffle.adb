with Ada_SPARK_Workflow.Word_Search.RNG;

procedure Ada_SPARK_Workflow.Word_Search.Shuffle
  (Vect : in out Vector_Pck.Vector)
is
   use Vector_Pck;

   package Index_RNG is new Word_Search.RNG (Index_Type);

   Gen : constant Index_RNG.Instance := Index_RNG.Create;

   First : constant Index_Type := First_Index (Vect);
   Last  : constant Extended_Index := Last_Index (Vect);

begin

   if Last not in Index_Type then
      return;
   end if;

   for X in First .. Index_Type (Last) loop
      pragma Loop_Invariant (First_Index (Vect) = First);
      pragma Loop_Invariant (Last_Index (Vect) = Last);

      Vector_Pck.Swap (Vect,
                       X,
                       Gen.Random (X, Last));
   end loop;

end Ada_SPARK_Workflow.Word_Search.Shuffle;
