with Ada.Text_IO;

with Ada_SPARK_Workflow.Resources;
with Ada_SPARK_Workflow.Word_Search.Shuffle;

package body Ada_SPARK_Workflow.Word_Search.Dictionary
with SPARK_Mode
is

   ----------
   -- Load --
   ----------

   procedure Load (This : in out Instance;
                   Min_Word_Len, Max_Word_Len : Positive)
   is
      use Word_Vector;
      use Ada.Text_IO;
      use type Ada.Containers.Count_Type;

      ----------------
      -- Valid_Word --
      ----------------

      function Valid_Word (Str : String) return Boolean
      is (Str'Length in Min_Word_Len .. Max_Word_Len
          and then
          Str'Length <= Word.Max_Length
          and then
            (for all C of Str => C in 'a' .. 'z'));

      Res_Path : constant String := Resources.Resource_Path;
      Filename : constant String := "/unixdict.txt";

      File : File_Type;

      Last : Natural;
      Line : String (1 .. Word.Max_Length);
   begin

      --  Make sure string concat will not overflow (happy SPARK is happy)
      if Res_Path'Last >= Positive'Last - Filename'Length then
         return;
      end if;

      Open (File, In_File, Res_Path & Filename);
      if not Is_Open (File) then
         return;
      end if;

      while not End_Of_File (File) loop
         Get_Line (File, Line, Last);

         --  Dict full?
         exit when Length (This.Words) = Capacity (This.Words);

         declare
            W : constant String := Line (Line'First .. Last);
         begin
            if Valid_Word (W) then
               Word_Vector.Append (This.Words, Word.Create (W));
            end if;
         end;
      end loop;

      Close (File);
      pragma Unreferenced (File);
   end Load;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (This : Instance) return Boolean
   is (Word_Vector.Is_Empty (This.Words));

   --------------
   -- Pop_Last --
   --------------

   procedure Pop_Last (This : in out Instance; W : out Word.Instance) is
   begin
      W := Word_Vector.Last_Element (This.Words);
      Word_Vector.Delete_Last (This.Words);
   end Pop_Last;

   --------------------
   -- Random_Shuffle --
   --------------------

   procedure Random_Shuffle (This : in out Instance) is
      procedure Word_Vect_Shuffle
      is new Ada_SPARK_Workflow.Word_Search.Shuffle (Word_Vector);
   begin
      Word_Vect_Shuffle (This.Words);
   end Random_Shuffle;

   procedure Print (This : Instance) is
   begin
      for Elt of This.Words loop
         Ada.Text_IO.Put_Line (Elt.To_Str);
      end loop;
   end Print;

end Ada_SPARK_Workflow.Word_Search.Dictionary;
