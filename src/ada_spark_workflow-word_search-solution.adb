with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Strings.Fixed;

package body Ada_SPARK_Workflow.Word_Search.Solution is

   --------------
   -- Add_Word --
   --------------

   procedure Add_Word (This           : in out Instance;
                       W              :        Word.Instance;
                       XS, YS, XE, YE :        Positive)
   is
   begin
      Word_Placement_Vector.Append (This.Placements, (W, XS, YS, XE, YE));
   end Add_Word;

   -----------
   -- Print --
   -----------

   procedure Print (This : Instance) is
      use Ada.Text_IO;
      use Ada.Integer_Text_IO;
      use Ada.Strings.Fixed;

      Int_Width : constant := 3;

      Max_Word_Len : Natural := 0;

   begin

      for P of This.Placements loop
         Max_Word_Len := Natural'Max (Max_Word_Len, P.W.To_Str'Length);
      end loop;

      declare
         Title_Len : constant Positive := Max_Word_Len + 4 * Int_Width + 10;
      begin
         Put_Line (Head ("--- Solution ", Title_Len, '-'));
      end;

      for P of This.Placements loop
         Put (Head (P.W.To_Str, Max_Word_Len, ' ') & " (");
         Put (P.XS, Int_Width);
         Put (", ");
         Put (P.YS, Int_Width);
         Put (") (");
         Put (P.XE, Int_Width);
         Put (", ");
         Put (P.YE, Int_Width);
         Put (")");
         New_Line;
      end loop;
   end Print;

end Ada_SPARK_Workflow.Word_Search.Solution;
