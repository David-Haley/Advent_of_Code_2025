with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Doubly_Linked_Lists;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_01 is

   type Dials is mod 100;
   Origin : constant Dials := 50;

   subtype Rotations is Integer;

   package Combination_Stores is new
     Ada.Containers.Doubly_Linked_Lists (Rotations);
   use Combination_Stores;

   procedure Read_Input (Combination_Store : out Combination_Stores.List) is

      Input_File : File_Type;
      Text : Unbounded_String;

   begin -- Read_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_01.txt");
      else
         Open (Input_File, In_File, Argument (1));
      end if; -- Argument_Count = 0
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         if Element (Text, 1) = 'L' then
            Append (Combination_Store,
                    -Natural'Value (Slice (Text, 2, Length (Text))));
         elsif Element (Text, 1) = 'R' then
            Append (Combination_Store,
                    Natural'Value (Slice (Text, 2, Length (Text))));
         else
            raise Program_Error with "Unexpevted direction '" &
                         Element (Text, 1) & "'";
         end if; -- Element (Text, 1) = 'L'
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_Input;

   function Count_Zeros (Combination_Store : Combination_Stores.List)
                         return Natural is

      Count : Natural := 0;
      Dial : Dials := Origin;

   begin -- Count_Zeros
      for C in Iterate (Combination_Store) loop
         Dial := Dials'Mod (Rotations (Dial) + Element (C));
         if Dial = 0 then
            Count := @ + 1;
         end if; -- Dial = 0
      end loop; -- C in Iterate (Combination_Store)
      return Count;
   end Count_Zeros;

   function Count_Zeros_2 (Combination_Store : Combination_Stores.List)
                           return Natural is

      --  Test every step and count steps that end at 0.

      Count : Natural := 0;
      Dial : Dials := Origin;

   begin -- Count_Zeros_2
      for C in Iterate (Combination_Store) loop
         for N in Rotations range 1 .. abs (Element (C)) loop
            if Element (C) < 0 then
               Dial := @ - 1;
            else
               Dial := @ + 1;
            end if; -- Element (C) < 0
            if Dial = 0 then
               Count := @ + 1;
            end if; -- Dial = 0
         end loop; -- N in Rotations range 1 .. abs (Element (C))
      end loop; -- C in Iterate (Combination_Store)
      return Count;
   end Count_Zeros_2;

   function Count_Zeros_2_Alt (Combination_Store : Combination_Stores.List)
                               return Natural is

      --  Analytical sloution, every full rotation where the L or R Number is
      --  greater than 100 must pass through 0 Number / 100 times. There may be
      --  additional zero crossings dependent on the magnitude and sign of the
      --  Remainder after Number is divided by 100. Ending on zero must be
      --  counted once only. This solution was produced to demonstrate an
      --  analytical s0lution which would be faster for very large L or R
      --  Numbers than executing every single step and testing for 0.

      procedure Reduce (Rotation : Rotations;
                        Circles : out Natural;
                        Remainder : out Rotations) is

         Dividend : Integer;

      begin -- Reduce
         Dividend := Rotation / Dials'Modulus;
         Remainder := Rotation - Dividend * Dials'Modulus;
         Circles := abs (Dividend);
      end Reduce;

      Count : Natural := 0;
      Dial : Dials := Origin;
      Next_Dial : Dials;
      Circles : Natural;
      Remainder :  Rotations;

   begin -- Count_Zeros_2_Alt
      for C in Iterate (Combination_Store) loop
         Reduce (Element (C), Circles, Remainder);
         Count := @ + Circles;
         Next_Dial := Dials'Mod (Rotations (Dial) + Remainder);
         if Remainder > 0 and then Dial /= 0 and then
           Next_Dial < Dial
         then
            Count := @ + 1;
         elsif Remainder < 0 and then Dial /= 0 and then
           (Next_Dial > Dial or else Next_Dial = 0)
         then
            Count := @ + 1;
         end if; -- Remainder > 0 and then
         Dial := Next_Dial;
      end loop; -- C in Iterate (Combination_Store)
      return Count;
   end Count_Zeros_2_Alt;

   Combination_Store : Combination_Stores.List;

begin -- December_01
   Read_Input (Combination_Store);
   Put_Line ("Part one:" & Count_Zeros (Combination_Store)'Img);
   Put_CPU_Time;
   Put_Line ("Part two:" & Count_Zeros_2 (Combination_Store)'Img);
   Put_CPU_Time;
   Put_Line ("Part two:" & Count_Zeros_2_Alt (Combination_Store)'Img);
   Put_CPU_Time;
end December_01;
