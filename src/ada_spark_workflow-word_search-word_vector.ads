with Ada.Containers.Formal_Vectors;

with Ada_SPARK_Workflow.Word_Search.Word;

package Ada_SPARK_Workflow.Word_Search.Word_Vector
is new Ada.Containers.Formal_Vectors (Natural,
                                      Word.Instance,
                                      Word."=");
