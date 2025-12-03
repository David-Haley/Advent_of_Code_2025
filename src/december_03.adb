with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Containers.Doubly_Linked_Lists;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_03 is

   subtype Joltages is Long_Long_Integer range 0 .. Long_Long_Integer'Last;
   subtype Battery_Joltages is Joltages range 1 .. 9;
   subtype Battery_Indices is Positive;

   package Banks is new
     Ada.Containers.Vectors (Battery_Indices, Battery_Joltages);
   use Banks;

   package Bank_Stores is new
     Ada.Containers.Doubly_Linked_Lists (Banks.Vector);
   use Bank_Stores;

   procedure Read_Input (Bank_Store : out Bank_Stores.List) is

      Input_File : File_Type;
      Text : Unbounded_String;
      Bank : Banks.Vector;

   begin -- Read_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_03.txt");
      else
         Open (Input_File, In_File, Argument (1));
      end if; -- Argument_Count = 0
      Clear (Bank_Store);
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         Clear (Bank);
         for B in Battery_Indices range 1 .. Length (Text) loop
            Append (Bank, Battery_Joltages'Value (Slice (Text, B, B)));
         end loop; -- B in Battery_Indices range 1 .. Length (Text)
         Append (Bank_Store, Bank);
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_Input;

   function Find_Joltage (Bank : Banks.Vector) return Joltages is

      Max_Index : Battery_Indices := 1;
      Next_Index : Battery_Indices;

   begin -- Find_Joltage
      for B in Iterate (Bank) loop
         if Element (B) > Bank (Max_Index) then
            Max_Index := To_Index (B);
         end if; -- Element (B) > Max_Array (1)
      end loop; -- B in Iterate (Bank)
      if Max_Index < Last_Index (Bank) then
         Next_Index := Max_Index + 1;
         for B in Battery_Indices range Max_Index + 1 .. Last_Index (Bank) loop
            if Bank (B) > Bank (Next_Index) then
               Next_Index := B;
            end if; -- Bank (B) > Bank (Next_Index)
         end loop; -- B in Battery_Indices range Max_Index + 1 ...
         return Bank (Max_Index) * 10 + Bank (Next_Index);
      else
         Next_Index := Max_Index - 1;
         for B in Battery_Indices range 1 .. Max_Index - 1 loop
            if Bank (B) > Bank (Next_Index) then
               Next_Index := B;
            end if; -- Bank (B) > Bank (Next_Index)
         end loop; -- B in Battery_Indices range 1 .. Max_Index - 1
         return Bank (Next_Index) * 10 + Bank (Max_Index);
      end if; -- Max_Index < Last_Index (Bank
   end Find_Joltage;

   function Find_Joltage_2 (Bank : Banks.Vector) return Joltages is

      subtype Digit_Indices is Natural range 0 .. 11;
      Best : array (Digit_Indices) of Battery_Indices;
      Next : Battery_Indices := 1;
      Result : Joltages := 0;

   begin -- Find_Joltage_2
      for D in reverse Digit_Indices loop
         Best (D) := Next;
         Result := @ * 10;
         for B in Battery_Indices range Next .. Last_Index (Bank) - D loop
            if Bank (B) > Bank (Best (D)) then
               Best (D) := B;
            end if; -- Bank (B) > Bank (Best (D))
         end loop; -- D in reverse Digit_Indices
         Next := Best (D) + 1;
         Result := @ + Bank (Best (D));
      end loop; -- D in reverse Digit_Indices
      return Result;
   end Find_Joltage_2;

   Bank_Store : Bank_Stores.List := Bank_Stores.Empty_List;
   Total_Joltage : Joltages := 0;

begin -- December_03
   Read_Input (Bank_Store);
   for Bs in Iterate (Bank_Store) loop
      Total_Joltage := @ + Find_Joltage (Element (Bs));
   end loop; -- Bs in Iterate (Bank_Store)
   Put_Line ("Part one:" & Total_Joltage'Img);
   Put_CPU_Time;
   Total_Joltage := 0;
   for Bs in Iterate (Bank_Store) loop
      Total_Joltage := @ + Find_Joltage_2 (Element (Bs));
   end loop; -- Bs in Iterate (Bank_Store)
   Put_Line ("Part two:" & Total_Joltage'Img);
   Put_CPU_Time;
end December_03;
