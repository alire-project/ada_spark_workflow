with Ada_SPARK_Workflow.Word_Search.Puzzle;
use Ada_SPARK_Workflow.Word_Search;

procedure Tests is
   Puz  : constant Puzzle.Instance :=
     Puzzle.Create (Width        => 10,
                    Height       => 10,
                    Max_Words    => 25,
                    Min_Word_Len => 4,
                    Max_Word_Len => 12);
begin
   Puz.Print;
   Puz.Solution.Print;
end Tests;
