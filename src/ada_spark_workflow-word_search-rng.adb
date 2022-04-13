package body Ada_SPARK_Workflow.Word_Search.RNG
with SPARK_Mode => Off
is

   ------------
   -- Create --
   ------------

   function Create return Instance is
   begin
      return This : Instance do
         Disc_Rand.Reset (This.Gen);
      end return;
   end Create;

   ------------
   -- Random --
   ------------

   function Random (This : Instance) return Result_Subtype is
   begin
      return Disc_Rand.Random (This.Gen);
   end Random;

   ------------
   -- Random --
   ------------

   function Random (This  : Instance;
                    First : Result_Subtype;
                    Last  : Result_Subtype)
                    return Result_Subtype
   is
   begin
      return Disc_Rand.Random (This.Gen, First, Last);
   end Random;

end Ada_SPARK_Workflow.Word_Search.RNG;
