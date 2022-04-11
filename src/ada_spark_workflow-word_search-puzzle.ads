with Ada_SPARK_Workflow.Word_Search.Word;
with Ada_SPARK_Workflow.Word_Search.Solution;
with Ada_SPARK_Workflow.Word_Search.Dictionary;

private with Ada.Containers;
private with Ada_SPARK_Workflow.Word_Search.Word_Vector;

package Ada_SPARK_Workflow.Word_Search.Puzzle is

   type Instance (<>)
   is tagged
   private;

   function Create (Width, Height              : Positive;
                    Max_Words                  : Positive;
                    Min_Word_Len, Max_Word_Len : Positive)
                    return Instance;
   --  Create a Word Search Puzzle from internal dictionary

   function Create (Width, Height, Max_Words : Positive;
                    Dict : in out Dictionary.Instance)
                    return Instance;
   --  Create a Word Search Puzzle from provided dictionary

   procedure Print (This : Instance);

   function Solution (This : Instance) return Word_Search.Solution.Instance;

private

   Empty_Cell : constant Character := ' ';

   type Puzzle_Grid is array (Positive range <>,
                              Positive range <>)
     of Character;

   type Instance (Width, Height : Positive;
                  Max_Words : Ada.Containers.Count_Type)
   is tagged
      record
         Grid : Puzzle_Grid (1 .. Width, 1 .. Height) :=
           (others => (others => Empty_Cell));

         Used       : Word_Search.Word_Vector.Vector (Max_Words);
         Used_Count : Ada.Containers.Count_Type := 0;

         Sol :  Word_Search.Solution.Instance (Max_Words);
      end record;

   function Complete (This : Instance) return Boolean;
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
     with Pre => XS in This.Grid'Range (1) and then YS in This.Grid'Range (2);

   procedure Fill_Empty (This : in out Instance);
   --  Fill empty cells with random characters

end Ada_SPARK_Workflow.Word_Search.Puzzle;
