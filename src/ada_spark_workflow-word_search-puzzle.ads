with Ada_SPARK_Workflow.Word_Search.Word;
with Ada_SPARK_Workflow.Word_Search.Solution;
with Ada_SPARK_Workflow.Word_Search.Dictionary;

with Ada.Containers;

package Ada_SPARK_Workflow.Word_Search.Puzzle
with SPARK_Mode
is

   subtype Grid_Size is Positive range Positive'First .. 10_000;

   type Instance (Width, Height : Grid_Size;
                  Max_Words : Ada.Containers.Count_Type)
   is tagged
   private;

   procedure Create (This : in out Instance;
                     Dict : in out Dictionary.Instance);
   --  Create a Word Search Puzzle from provided dictionary

   procedure Print (This : Instance);

   function Solution (This : Instance) return Word_Search.Solution.Instance;

private

   use type Ada.Containers.Count_Type;

   Empty_Cell : constant Character := ' ';

   type Puzzle_Grid is array (Grid_Size range <>,
                              Grid_Size range <>)
     of Character;

   type Instance (Width, Height : Grid_Size;
                  Max_Words : Ada.Containers.Count_Type)
   is tagged
      record
         Grid : Puzzle_Grid (1 .. Width, 1 .. Height) :=
           (others => (others => Empty_Cell));

         Sol :  Word_Search.Solution.Instance (Max_Words);
      end record;

   function Used_Count (This : Instance) return Ada.Containers.Count_Type
   is (Word_Search.Solution.Word_Count (This.Sol));

   function Complete (This : Instance) return Boolean
   is (This.Max_Words = This.Used_Count);
   --  Return True if the puzzle is complete, i.e. Max_Words are inserted in
   --  the puzzle.

   function Empty_Cells (This : Instance) return Boolean;
   --  Return True if at least one of the puzzel cell is empty

   procedure Add_Word (This    : in out Instance;
                       W       :        Word.Instance;
                       Success :    out Boolean);
   --  Add a Word in the grid

   type Direction is (North, North_East,
                      East, South_East,
                      South, South_West,
                      West, North_West);

   procedure Try_Set_Word (This    : in out Instance;
                           W       :        Word.Instance;
                           XS, YS  :        Positive;
                           Dir     :        Direction;
                           Success :    out Boolean)
     with Pre'Class => This.Used_Count < This.Max_Words
                       and then XS <= This.Grid'Last (1)
                       and then YS <= This.Grid'Last (2);

   procedure Fill_Empty (This : in out Instance);
   --  Fill empty cells with random characters

end Ada_SPARK_Workflow.Word_Search.Puzzle;
