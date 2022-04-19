with Ada.Containers;

with Ada_SPARK_Workflow.Word_Search.Word;

private with Ada_SPARK_Workflow.Word_Search.Word_Vector;

package Ada_SPARK_Workflow.Word_Search.Dictionary
with SPARK_Mode
is

   type Instance(Capacity : Ada.Containers.Count_Type)
   is tagged limited
   private;

   Builtin_Dict_Words : constant := 25103;

   procedure Load (This : in out Instance;
                   Min_Word_Len, Max_Word_Len : Positive);
   --  Load words from built-in dictionary

   function Is_Empty (This : Instance) return Boolean;
   --  Return true if there is no more word in the dictionary

   procedure Pop_Last (This : in out Instance; W : out Word.Instance)
     with Pre'Class => not This.Is_Empty;
   --  Remove the last Word from the dictionary and return it

   procedure Random_Shuffle (This : in out Instance);
   --  Shuffle words of the dictionary in random order

   procedure Print (This : Instance);
   --  Print all words on stdout

private

   type Instance (Capacity : Ada.Containers.Count_Type)
   is tagged limited
      record
         Words : Word_Vector.Vector (Capacity);
      end record;

end Ada_SPARK_Workflow.Word_Search.Dictionary;
