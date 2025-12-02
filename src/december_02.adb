with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Containers.Doubly_Linked_Lists;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_02 is

   subtype Big_Natural is Long_Long_Integer range 0 .. Long_Long_Integer'Last;
   type Number_Ranges is record
      Low, High : Big_Natural;
   end record; -- Number_Ranges

   package Range_Stores is new
     Ada.Containers.Doubly_Linked_Lists (Number_Ranges);
   use Range_Stores;

   procedure Read_Input (Range_Store : out Range_Stores.List) is

      Input_File : File_Type;
      Text : Unbounded_String;
      Start_At, First : Positive;
      Last : Natural;
      Number_Range : Number_Ranges;

   begin -- Read_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_02.txt");
      else
         Open (Input_File, In_File, Argument (1));
      end if; -- Argument_Count = 0
      Get_Line (Input_File, Text);
      Clear (Range_Store);
      Start_At := 1;
      while Start_At < Length (Text) loop
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
         Number_Range.Low := Big_Natural'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         if Element (Text, Start_At) /= '-' then
            raise Program_Error with "Found '" & Element (Text, Start_At) &
              "' expected '-'";
         end if; -- Element (Text, Start_At) /= '-'
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
         Number_Range.High := Big_Natural'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Append (Range_Store, Number_Range);
      end loop;
      Close (Input_File);
   end Read_Input;

   function Invalid (Id : Big_Natural) return Boolean is

      Id_String : constant String := Trim (Id'Img, Both);
      Mid : constant Positive := (1 + Id_String'Last) / 2;

   begin -- Invalid
      if Id_String'Length mod 2 = 0 then
         return Id_String (1 .. Mid) = Id_String (Mid + 1 .. Id_String'Last);
      else
         return False;
      end if; -- Id_String'Length mod 2 = 0
   end Invalid;

   function Invalid_2 (Id : Big_Natural) return Boolean is

      Id_String : constant String := Trim (Id'Img, Both);
      Result : Boolean := False;
      N : Positive := 1;

   begin -- Invalid_2
      while not Result and then N <= Id_String'Length / 2 loop
         if Id_String'Length mod N = 0 then
            declare -- Substring Comparison block
               First_SS : constant String := Id_String (1 .. N);
               First : Positive := N + 1;
               Last : Positive := First + N - 1;
               Equal : Boolean := True;
            begin -- Substring Comparison block
               while Last <= Id_String'Length loop
                  Equal := @ and then First_SS = Id_String (First .. Last);
                  First := Last + 1;
                  Last := First + N - 1;
               end loop; -- Last <= Id_String'Length
               Result := @ or else Equal;
            end; -- Substring Comparison block
         end if; -- Id_String'Length mod N = 0
         N := @ + 1;
      end loop; -- not Result and then N <= Id_String'Length / 2
      return Result;
   end Invalid_2;

   Range_Store : Range_Stores.List := Range_Stores.Empty_List;
   Sum : Big_Natural := 0;

begin -- December_02
   Read_Input (Range_Store);
   for R in Iterate (Range_Store) loop
      for Id in Big_Natural range Element (R).Low .. Element (R).High loop
         if Invalid (Id) then
            Sum := @ + Id;
         end if; -- Invalid (Id)
      end loop; -- Id in Big_Natural range Element (R).Low .. Element (R).High
   end loop; -- R in Iterate (range_Store)
   Put_Line ("Part one:" & Sum'Img);
   Put_CPU_Time;
   Sum := 0;
   for R in Iterate (Range_Store) loop
      for Id in Big_Natural range Element (R).Low .. Element (R).High loop
         if Invalid_2 (Id) then
            Sum := @ + Id;
         end if; -- Invalid_2 (Id)
      end loop; -- Id in Big_Natural range Element (R).Low .. Element (R).High
   end loop; -- R in Iterate (range_Store)
   Put_Line ("Part two:" & Sum'Img);
   Put_CPU_Time;
end December_02;
