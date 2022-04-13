package Ada_SPARK_Workflow.Word_Search.Word
with Preelaborate,
     SPARK_Mode
is

   Max_Length : constant := 512;

   type Instance
   is tagged
   private;

   function Create (Str : String) return Instance
     with Pre'Class => Str'Length in 1 .. Max_Length;

   overriding
   function "=" (A, B : Instance) return Boolean;

   function To_Str (This : Instance) return String
     with Post'Class => To_Str'Result'Length <= Max_Length;

private

   subtype Word_Length is Positive range 1 .. Max_Length;

   type Instance
   is tagged record
      Str : String (1 .. Max_Length);
      Len : Word_Length := Word_Length'First;
   end record;

end Ada_SPARK_Workflow.Word_Search.Word;
