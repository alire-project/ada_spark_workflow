with Ada_SPARK_Workflow.Word_Search.Word;

with Ada.Containers;

private with Ada.Containers.Formal_Vectors;

package Ada_SPARK_Workflow.Word_Search.Solution
is
   type Instance (Max_Words : Ada.Containers.Count_Type)
   is tagged
   private;

   procedure Add_Word (This           : in out Instance;
                       W              :        Word.Instance;
                       XS, YS, XE, YE :        Positive);

   procedure Print (This : Instance);

private

   type Word_Placement is record
      W              : Word.Instance;
      XS, YS, XE, YE : Positive;
   end record;

   package Word_Placement_Vector
   is new Ada.Containers.Formal_Vectors (Index_Type   => Positive,
                                         Element_Type => Word_Placement);

   type Instance (Max_Words : Ada.Containers.Count_Type)
   is tagged record
      Placements : Word_Placement_Vector.Vector (Max_Words);
   end record;

end Ada_SPARK_Workflow.Word_Search.Solution;
