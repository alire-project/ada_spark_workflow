with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Strings.Fixed;
with Ada.Numerics.Discrete_Random;

package body Ada_SPARK_Workflow.Word_Search.Puzzle is

   ------------
   -- Create --
   ------------

   function Create (Width, Height              : Positive;
                    Max_Words                  : Positive;
                    Min_Word_Len, Max_Word_Len : Positive)
                    return Instance
   is
      Dict : Dictionary.Instance := Dictionary.Create (Min_Word_Len,
                                                       Max_Word_Len);
   begin
      return Create (Width, Height, Max_Words, Dict);
   end Create;

   ------------
   -- Create --
   ------------

   function Create (Width, Height, Max_Words : Positive;
                    Dict : in out Dictionary.Instance)
                    return Instance
   is
      Success : Boolean;
   begin
      return This : Instance (Width, Height,
                              Ada.Containers.Count_Type (Max_Words))
      do
         Dict.Random_Shuffle;

         while not Dict.Is_Empty loop

            This.Add_Word (Dict.Pop_Last, Success);

            exit when
              Success
              and then
                not This.Empty_Cells
                and then
                  This.Complete;

         end loop;

         This.Fill_Empty;
      end return;
   end Create;

   --------------
   -- Complete --
   --------------

   function Complete (This : Instance) return Boolean is
      use type Ada.Containers.Count_Type;
   begin
      return This.Used_Count >= This.Max_Words;
   end Complete;

   -----------------
   -- Empty_Cells --
   -----------------

   function Empty_Cells (This : Instance) return Boolean is
   begin
      return (for some Cell of This.Grid => Cell = ' ');
   end Empty_Cells;

   --------------
   -- Add_Word --
   --------------

   procedure Add_Word (This    : in out Instance;
                       W       :        Word.Instance;
                       Success :    out Boolean)
   is
      generic
         type T is (<>);
      function Next_Gen (Val : T) return T;

      function Next_Gen (Val : T) return T
      is (if Val = T'Last
          then T'First
          else T'Succ (Val));

      subtype Coord_X is Positive range This.Grid'Range (1);
      subtype Coord_Y is Positive range This.Grid'Range (2);

      package Rand_X_Pck is new Ada.Numerics.Discrete_Random (Coord_X);
      package Rand_Y_Pck is new Ada.Numerics.Discrete_Random (Coord_Y);
      package Rand_Dir_Pck is new Ada.Numerics.Discrete_Random (Direction);

      function Next_X is new Next_Gen (Coord_X);
      function Next_Y is new Next_Gen (Coord_Y);
      function Next_Dir is new Next_Gen (Direction);

      Gen_X      : Rand_X_Pck.Generator;
      First_X, X : Coord_X;

      Gen_Y      : Rand_Y_Pck.Generator;
      First_Y, Y : Coord_Y;

      Gen_Dir        : Rand_Dir_Pck.Generator;
      First_Dir, Dir : Direction;
   begin

      if This.Complete then
         --  No more room
         Success := False;
         return;
      end if;

      Rand_X_Pck.Reset (Gen_X);
      First_X := Rand_X_Pck.Random (Gen_X);
      X := First_X;

      Rand_Y_Pck.Reset (Gen_Y);
      First_Y := Rand_Y_Pck.Random (Gen_Y);
      Y := First_Y;

      Rand_Dir_Pck.Reset (Gen_Dir);
      First_Dir := Rand_Dir_Pck.Random (Gen_Dir);
      Dir := First_Dir;

      --  Try all direction and positions, starting from random direction and
      --  position.

      loop
         loop
            loop
               This.Try_Set_Word (W, X, Y, Dir, Success);

               if Success then
                  return;
               end if;

               X := Next_X (X);
               exit when X = First_X;
            end loop;

            Y := Next_Y (Y);
            exit when Y = First_Y;
         end loop;

         Dir := Next_Dir (Dir);
         exit when Dir = First_Dir;
      end loop;

   end Add_Word;

   -----------
   -- Print --
   -----------

   procedure Print (This : Instance) is
      use Ada.Text_IO;
      use Ada.Integer_Text_IO;
      use Ada.Strings.Fixed;

      Int_Width : constant := 3;

      Title_Len : constant Positive := 3 + Int_Width * This.Grid'Length (1);

   begin
      Put_Line (Head ("--- Puzzle ", Title_Len, '-'));

      Put ("   ");
      for X in This.Grid'Range (1) loop
         Put (X, Int_Width);
      end loop;
      New_Line;

      for Y in This.Grid'Range (2) loop
         Put (Y, Int_Width);
         Put ("  ");

         for X in This.Grid'Range (1) loop
            Put (This.Grid (X, Y) & "  ");
         end loop;

         New_Line;
      end loop;
   end Print;

   --------------
   -- Solution --
   --------------

   function Solution (This : Instance) return Word_Search.Solution.Instance
   is (This.Sol);

   ------------------
   -- Try_Set_Word --
   ------------------

   procedure Try_Set_Word (This    : in out Instance;
                           W       :        Word.Instance;
                           XS, YS  :        Positive;
                           Dir     :        Direction;
                           Success :    out Boolean)
   is
      use type Ada.Containers.Count_Type;

      DX : constant Integer := (case Dir is
                                   when North | South => 0,
                                   when North_East | East | South_East  => 1,
                                   when South_West | West | North_West => -1);

      DY : constant Integer := (case Dir is
                                   when East | West => 0,
                                   when North | North_East | North_West => -1,
                                   when South | South_East | South_West => 1);

      Str : constant String := W.To_Str;

      XE : constant Integer := XS + Str'Length * DX;
      YE : constant Integer := YS + Str'Length * DY;

      X : Positive := XS;
      Y : Positive := YS;
   begin

      --  Check bounds
      if XE not in This.Grid'Range (1)
        or else
         YE not in This.Grid'Range (2)
      then

         --  Word doesn't fit in grid
         Success := False;
         return;
      end if;

      for C of Str loop

         if This.Grid (X, Y) not in C | Empty_Cell then
            --  Conflict with word already in the grid
            Success := False;
            return;
         end if;

         X := X + DX;
         Y := Y + DY;
      end loop;

      --  If we reach this point, the word fits in grid

      --  Place the word
      X := XS;
      Y := YS;
      for C of Str loop
         This.Grid (X, Y) := C;
         X := X + DX;
         Y := Y + DY;
      end loop;

      This.Used_Count := This.Used_Count + 1;
      This.Sol.Add_Word (W, XS, YS, XE, YE);
      Success := True;
   end Try_Set_Word;

   ----------------
   -- Fill_Empty --
   ----------------

   procedure Fill_Empty (This : in out Instance) is
      subtype Valid_Char is Character range 'a' .. 'z';

      package Rand_Char_Pck is new Ada.Numerics.Discrete_Random (Valid_Char);

      Gen : Rand_Char_Pck.Generator;
   begin
      Rand_Char_Pck.Reset (Gen);

      for C of This.Grid loop
         if C = Empty_Cell then
            C := Rand_Char_Pck.Random (Gen);
         end if;
      end loop;
   end Fill_Empty;

end Ada_SPARK_Workflow.Word_Search.Puzzle;
