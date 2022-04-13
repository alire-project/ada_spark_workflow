private with Ada.Numerics.Discrete_Random;

generic
   type Result_Subtype is (<>);
package Ada_SPARK_Workflow.Word_Search.RNG
  with SPARK_Mode
is
   type Instance (<>)
   is tagged limited
   private;

   function Create return Instance;

   function Random (This : Instance) return Result_Subtype;

   function Random (This  : Instance;
                    First : Result_Subtype;
                    Last  : Result_Subtype)
                    return Result_Subtype
     with Post => Random'Result in First .. Last;

private

   pragma SPARK_Mode (Off);

   package Disc_Rand is new Ada.Numerics.Discrete_Random (Result_Subtype);

   type Instance
   is tagged limited
      record
         Gen : Disc_Rand.Generator;
      end record;

end Ada_SPARK_Workflow.Word_Search.RNG;
