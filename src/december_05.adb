with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Doubly_Linked_Lists;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_05 is

   subtype Ingredients is Long_Long_Integer range 1 .. Long_Long_Integer'Last;

   package Fresh_Maps is new
     Ada.Containers.Ordered_Maps (Ingredients, Ingredients);
   use Fresh_Maps;

   package Ingredient_Lists is new
     Ada.Containers.Doubly_Linked_Lists (Ingredients);
   use Ingredient_Lists;

   procedure Read_Input (Fresh_Map : out Fresh_Maps.Map;
                         Ingredient_List : out Ingredient_Lists.List) is

      Input_File : File_Type;
      Text : Unbounded_String;
      Start_At, First : Positive;
      Last : Natural;
      Low, High : Ingredients;

   begin -- Read_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_05.txt");
      else
         Open (Input_File, In_File, Argument (1));
      end if; -- Argument_Count = 0
      Clear (Fresh_Map);
      Clear (Ingredient_List);
      loop -- Get one line of fresh range
         Get_Line (Input_File, Text);
         exit when Length (Text) = 0;
         Start_At := 1;
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
         Low := Ingredients'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
         High := Ingredients'Value (Slice (Text, First, Last));
         if Contains (Fresh_Map, Low) then
            if High > Fresh_Map (Low) then
               Fresh_Map (Low) := High;
            end if; -- High > Fresh_Map (Low)
         else
            Include (Fresh_Map, Low, High);
         end if; -- Contains (Fresh_Map, Low)
      end loop; -- Get one line of fresh range
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         Append (Ingredient_List, Ingredients'Value (To_String (Text)));
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_Input;

   function Is_Fresh (Fresh_Map : Fresh_Maps.Map;
                      Ingredient : Ingredients) return Boolean is

      Fc : Fresh_Maps.Cursor := First (Fresh_Map);
      Result : Boolean := False;

   begin -- Is_Fresh
      while Fc /= Fresh_Maps.No_Element and then Key (Fc) <= Ingredient
        and then not Result loop
         Result := Ingredient in Key (Fc) .. Element (Fc);
         Next (Fc);
      end loop; -- Fc /= Fresh_Maps.No_Element and then ...
      return Result;
   end Is_Fresh;

   procedure Consolidate (Fresh_Map : in out Fresh_Maps.Map) is

      --  Combines overlaping ranges into a single range.

      Fc : Fresh_Maps.Cursor := First (Fresh_Map);
      Fn :  Fresh_Maps.Cursor := Next (Fc);

   begin -- Consolidate
      while Fc /= Fresh_Maps.No_Element and then Fn /= Fresh_Maps.No_Element
      loop
         --  Because of the map ordering Key (Fc) must be less than Key (Fn)
         --  thus, if Key (Fn) is in the range defined by Fc, there must be an
         --  overlap.
         if Key (Fn) <= Element (Fc) then
            --  The test below will fail if the range defined by Fn is totally
            --  contained within the range defined by Fc. If the range is
            --  totally contained then it can be deleted.
            if Element (Fn) > Element (Fc) then
               Fresh_Map (Fc) := Element (Fn);
            end if;
            Delete (Fresh_Map, Fn);
            Fn := Next (Fc);
         else
            Next (Fc);
            if Fc /= Fresh_Maps.No_Element then
               Fn := Next (Fc);
            else
               Fn := Fresh_Maps.No_Element;
            end if; -- Fc /= Fresh_Maps.No_Element
         end if; -- Key (Fn) <= Element (Fc)
      end loop; -- Fc /= Fresh_Maps.No_Element
   end Consolidate;

   Fresh_Map : Fresh_Maps.Map := Fresh_Maps.Empty_Map;
   Ingredient_List : Ingredient_Lists.List := Ingredient_Lists.Empty_List;
   Fresh_Count : Natural := 0;
   Total_Fresh : Long_Long_Integer range 0 .. Long_Long_Integer'Last := 0;

begin -- December_05
   Read_Input (Fresh_Map, Ingredient_List);
   --  Consolidate is only essential to part 2 but will speed up execution of
   --  part 2.
   Consolidate (Fresh_Map);
   for I in Iterate (Ingredient_List) loop
      if Is_Fresh (Fresh_Map, Element (I)) then
         Fresh_Count := @ + 1;
      end if; -- Is_Fresh (Fresh_Map, Element (I))
   end loop; -- I in Iterate (Ingredient_List)
   Put_Line ("Part one:" & Fresh_Count'Img);
   Put_CPU_Time;
   for F in Iterate (Fresh_Map) loop
      Total_Fresh := @ + Element (F) - Key (F) + 1;
   end loop; -- F in Iterate (Fresh_Map)
   Put_Line ("Part two:" & Total_Fresh'Img);
   Put_CPU_Time;
end December_05;
