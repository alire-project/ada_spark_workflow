with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Strings.Fixed;

with Ada_SPARK_Workflow.Word_Search.RNG;

package body Ada_SPARK_Workflow.Word_Search.Puzzle
with SPARK_Mode
is

   ------------
   -- Create --
   ------------

   procedure Create (This : in out Instance;
                     Dict : in out Dictionary.Instance)
   is
      Success : Boolean;
   begin
      Dict.Random_Shuffle;

      while not Dict.Is_Empty loop

         declare
            W : Word.Instance;
         begin
            Dict.Pop_Last (W);
            This.Add_Word (W, Success);
         end;

         exit when Success
           and then not This.Empty_Cells
           and then This.Complete;

      end loop;

      This.Fill_Empty;
   end Create;

   -----------------
   -- Empty_Cells --
   -----------------

   function Empty_Cells (This : Instance) return Boolean is
   begin
      for X in This.Grid'Range (1) loop
         for Y in This.Grid'Range (2) loop
            if This.Grid (X, Y) = Empty_Cell then
               return True;
            end if;
         end loop;
      end loop;

      return False;
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

      package Rand_X_Pck is new RNG (Coord_X);
      package Rand_Y_Pck is new RNG (Coord_Y);
      package Rand_Dir_Pck is new RNG (Direction);

      function Next_X is new Next_Gen (Coord_X);
      function Next_Y is new Next_Gen (Coord_Y);
      function Next_Dir is new Next_Gen (Direction);

      Gen_X      : constant Rand_X_Pck.Instance := Rand_X_Pck.Create;
      First_X    : constant Coord_X := Gen_X.Random;
      X          : Coord_X := First_X;

      Gen_Y      : constant Rand_Y_Pck.Instance := Rand_Y_Pck.Create;
      First_Y    : constant Coord_Y := Gen_Y.Random;
      Y          : Coord_Y := First_Y;

      Gen_Dir    : constant Rand_Dir_Pck.Instance  := Rand_Dir_Pck.Create;
      First_Dir  : constant Direction := Gen_Dir.Random;
      Dir        : Direction := First_Dir;
   begin

      if This.Complete then
         --  No more room
         Success := False;
         return;
      end if;

      --  Try all direction and positions, starting from random direction and
      --  position.

      loop
         pragma Loop_Invariant (This.Used_Count < This.Max_Words);
         loop
            pragma Loop_Invariant (This.Used_Count < This.Max_Words);
            loop
               pragma Loop_Invariant (This.Used_Count < This.Max_Words);

               This.Try_Set_Word (W, X, Y, Dir, Success);

               if Success or else This.Complete then
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
      if Str'Length = 0
        or else
         XE not in This.Grid'Range (1)
        or else
         YE not in This.Grid'Range (2)
      then
         --  Word doesn't fit in grid
         Success := False;
         return;
      end if;

      --  Check if word can be placed in the grid at given position and
      --  direction.
      for Count in 0 .. Str'Length - 1 loop
         pragma Loop_Invariant (X = XS + Count * DX);
         pragma Loop_Invariant (Y = YS + Count * DY);

         if This.Grid (X, Y) not in Str (Str'First + Count) | Empty_Cell
         then
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
      for Count in 0 .. Str'Length - 1 loop
         pragma Loop_Invariant (X = XS + Count * DX);
         pragma Loop_Invariant (Y = YS + Count * DY);

         This.Grid (X, Y) := Str (Str'First + Count);
         X := X + DX;
         Y := Y + DY;
      end loop;

      Word_Search.Solution.Add_Word (This.Sol, W, XS, YS, XE, YE);
      Success := True;
   end Try_Set_Word;

   ----------------
   -- Fill_Empty --
   ----------------

   procedure Fill_Empty (This : in out Instance) is
      subtype Valid_Char is Character range 'a' .. 'z';

      package Rand_Char_Pck is new RNG (Valid_Char);

      Gen : constant Rand_Char_Pck.Instance := Rand_Char_Pck.Create;
   begin

      for C of This.Grid loop
         if C = Empty_Cell then
            C := Gen.Random;
         end if;
      end loop;
   end Fill_Empty;

end Ada_SPARK_Workflow.Word_Search.Puzzle;
