with Ada_SPARK_Workflow.Word_Search.Word;

private with Ada_SPARK_Workflow.Word_Search.Word_Vector;
private with Ada.Containers;

package Ada_SPARK_Workflow.Word_Search.Dictionary is

   type Instance (<>)
   is tagged limited
   private;

   function Create (Min_Word_Len, Max_Word_Len : Positive) return Instance;

   function Is_Empty (This : Instance) return Boolean;
   --  Return true if there is no more word in the dictionary

   function Pop_Last (This : in out Instance) return Word.Instance;
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
