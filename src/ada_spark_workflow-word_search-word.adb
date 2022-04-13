package body Ada_SPARK_Workflow.Word_Search.Word
with SPARK_Mode
is

   ------------
   -- Create --
   ------------

   function Create (Str : String) return Instance is
   begin
      return This : Instance do
         This.Len := Str'Length;
         This.Str := (others => ASCII.NUL);
         This.Str (1 .. This.Len) := Str;
      end return;
   end Create;

   ---------
   -- "=" --
   ---------

   overriding
   function "=" (A, B : Instance) return Boolean
   is (A.Len = B.Len
       and then
       A.Str (1 .. A.Len) = B.Str (1 .. B.Len));

   ------------
   -- To_Str --
   ------------

   function To_Str (This : Instance) return String
   is (This.Str (1 .. This.Len));

end Ada_SPARK_Workflow.Word_Search.Word;
