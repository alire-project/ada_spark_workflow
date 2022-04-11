with Ada.Text_IO;

with Ada_SPARK_Workflow.Resources;
with Ada_SPARK_Workflow.Word_Search.Shuffle;

package body Ada_SPARK_Workflow.Word_Search.Dictionary is

   ------------
   -- Create --
   ------------

   function Create (Min_Word_Len, Max_Word_Len : Positive) return Instance is
      use Ada.Text_IO;
      use Ada.Containers;

      ----------------
      -- Valid_Word --
      ----------------

      function Valid_Word (Str : String) return Boolean
      is (Str'Length in Min_Word_Len .. Max_Word_Len
          and then
          Str'Length <= Word.Max_Word_Length
          and then
            (for all C of Str => C in 'a' .. 'z'));

      -----------------------
      -- Count_Valid_Words --
      -----------------------

      function Count_Valid_Words (File : File_Type) return Count_Type is
         Result : Count_Type := 0;
      begin
         while not End_Of_File (File) loop
            if Valid_Word (Get_Line (File)) then
               Result := Result + 1;
            end if;
         end loop;
         return Result;
      end Count_Valid_Words;

      Filename : constant String := Resources.Resource_Path & "/unixdict.txt";
      File : File_Type;

      Valid_Words : Count_Type;
   begin

      Open (File, In_File, Filename);
      if not Is_Open (File) then
         return Instance'(Capacity => 0, others => <>);
      end if;

      Valid_Words := Count_Valid_Words (File);
      Reset (File);

      return This : Instance := Instance'(Capacity => Valid_Words,
                                          others   => <>)
      do
         while not End_Of_File (File) loop
            declare
               Line : constant String := Get_Line (File);
            begin
               if Valid_Word (Line) then
                  Word_Vector.Append (This.Words, Word.Create (Line));
               end if;
            end;
         end loop;
         Close (File);
      end return;
   end Create;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (This : Instance) return Boolean
   is (Word_Vector.Is_Empty (This.Words));

   --------------
   -- Pop_Last --
   --------------

   function Pop_Last (This : in out Instance) return Word.Instance is
      Result : constant Word.Instance := Word_Vector.Last_Element (This.Words);
   begin
      Word_Vector.Delete_Last (This.Words);
      return Result;
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
