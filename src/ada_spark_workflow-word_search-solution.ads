with Ada_SPARK_Workflow.Word_Search.Word;

with Ada.Containers; use Ada.Containers;

private with Ada.Containers.Formal_Vectors;

package Ada_SPARK_Workflow.Word_Search.Solution
with SPARK_Mode
is
   type Instance (Max_Words : Ada.Containers.Count_Type)
   is
   private;

   function Word_Count (This : Instance) return Ada.Containers.Count_Type;

   procedure Add_Word (This           : in out Instance;
                       W              :        Word.Instance;
                       XS, YS, XE, YE :        Positive)
     with Pre => Word_Count (This) < This.Max_Words;

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
   is record
      Placements : Word_Placement_Vector.Vector (Max_Words);
   end record;

   function Word_Count (This : Instance) return Ada.Containers.Count_Type
   is (Word_Placement_Vector.Length (This.Placements));

end Ada_SPARK_Workflow.Word_Search.Solution;
