package Ada_SPARK_Workflow.Word_Search.Word
with Preelaborate
is

   Max_Word_Length : constant := 512;

   type Instance
   is tagged
   private;

   function Create (Str : String) return Instance
     with Pre => Str'Length <= Max_Word_Length;

   function "=" (A, B : Instance) return Boolean;

   function To_Str (This : Instance) return String;

private

   type Instance
   is tagged record
      Str : String (1 .. Max_Word_Length);
      Len : Natural := 0;
   end record;

end Ada_SPARK_Workflow.Word_Search.Word;
