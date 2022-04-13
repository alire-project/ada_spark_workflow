with Ada_SPARK_Workflow.Word_Search.Puzzle;
with Ada_SPARK_Workflow.Word_Search.Dictionary;
with Ada_SPARK_Workflow.Word_Search.Solution;
use Ada_SPARK_Workflow.Word_Search;

procedure Tests is

   type Dict_Access is access Dictionary.Instance;

   Dict : constant not null Dict_Access :=
     new Dictionary.Instance (Dictionary.Builtin_Dict_Words);

   Puz  : Puzzle.Instance (Width     => 10,
                           Height    => 10,
                           Max_Words => 25);

begin
   Dict.Load (Min_Word_Len => 4,
              Max_Word_Len => 12);

   Puz.Create (Dict.all);

   Puz.Print;

   Solution.Print (Puz.Solution);
end Tests;
