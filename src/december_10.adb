with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Doubly_Linked_Lists;
with Interfaces; use Interfaces;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_10 is

   type Controls is new Natural;

   package Control_States is new Ada.Containers.Ordered_Sets (Controls);
   use Control_States;

   type Button_Indices is new Natural;

   package Button_Maps is new
     Ada.Containers.Ordered_Maps (Button_Indices, Control_States.Set);
   use Button_Maps;

   package Button_Sets is new Ada.Containers.Ordered_Sets (Button_Indices);
   use Button_Sets;

   package Button_Subsets is new
     Ada.Containers.Doubly_Linked_Lists (Button_Sets.Set);
   use Button_Subsets;

   subtype Products is Long_Long_Integer range 0 .. Long_Long_Integer'Last;

   type Button_Ranks is record
      Button_Index : Button_Indices;
      Product : Products;
   end record; -- Button_Ranks;

   package Button_Lists is new
     Ada.Comtainers.Doubly_Linked_Lists (Button_Ranks);
   use Button_Lists;

   function "<" (Left, Right : Button_Ranks) return Boolean is
     (Left.Product < Right.Product);

   package Button_List_Sort is new Button_Lists.Generic_Sort;
   use Button_List_Sort;

   subtype Joltages is Natural;

   package Joltage_Stores is new
     Ada.Containers.Ordered_Maps (Controls, Joltages);
   use Joltage_Stores;

   function "<=" (Left, Right : Joltage_Stores.Map) return Boolean is

      Result : Boolean := True;
      Lc : Joltage_Stores.Cursor := First (Left);

   begin -- "<="
      if Length (Left) = Length (Right) then
         while Result and then Lc /= Joltage_Stores.No_Element loop
            Result := @ and then Element (Lc) <= Right (Key (Lc));
            Next (Lc);
         end loop; -- Result and then Lc /= Joltage_Stores.No_element
      else
         raise Program_Error with "Arguments of ""<"" are not equal length";
      end if; -- Length (Left) = Length (Right)
      return Result;
   end "<=";

   function "-" (Left, Right : Joltage_Stores.Map)
                 return Joltage_Stores.Map is

      Result : Joltage_Stores.Map := Joltage_Stores.Empty_Map;

   begin -- "-"
      if Length (Left) = Length (Right) then
         for L in Iterate (Left) loop
            Insert (Result, Key (L), Element (L) - Right (Key (L)));
         end loop; -- L in Iterate (Left)
      else
         raise Program_Error with "Arguments of ""-"" are not equal length";
      end if; -- Length (Left) = Length (Right)
      return Result;
   end "-";

   type Machines is record
      Light_State : Control_States.Set := Control_States.Empty_Set;
      Button_Map : Button_Maps.Map := Button_Maps.Empty_Map;
      Joltage_Store : Joltage_Stores.Map := Joltage_Stores.Empty_Map;
   end record; -- Machines

   subtype Presses is Natural;

   package Machine_Stores is new Ada.Containers.Doubly_Linked_Lists (Machines);
   use Machine_Stores;

   type Actions is record
      Pressed : Presses;
      Jolts : Joltage_Stores.Map;
   end record; -- Actions

   package Action_Lists is new Ada.Containers.Doubly_Linked_Lists (Actions);
   use Action_Lists;

   procedure Read_Input (Machine_Store : out  Machine_Stores.List) is

      Input_File : File_Type;
      Text : Unbounded_String;

   begin -- Read_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_10.txt");
      else
         Open (Input_File, In_File, Argument (1));
      end if; -- Argument_Count = 0
      Clear (Machine_Store);
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         declare -- Machine declaration block

            Start_At : Positive := 1;
            First : Positive;
            Last : Natural;
            Machine : Machines;
            Button_Index : Button_Indices := Button_Indices'First;
            Joltage_Index : Controls := Controls'First;
            Light_String : Unbounded_String;
            Square_Set : constant Character_Set := To_Set ("[]");
            Round_Start : constant Character_Set := To_Set ("(");
            Curly_Start : constant Character_Set := To_Set ("{");
            Curly_End : constant Character_Set := To_Set ("}");

         begin -- Machine declaration block
            --  Read lights
            Find_Token (Text, Square_Set, Start_At, Outside, First, Last);
            Light_String := Unbounded_Slice (Text, First, Last);
            Start_At := Last + 1;
            for Light in Controls range 0 ..
            Controls (Length (Light_String) - 1) loop
               case Element (Light_String, Positive (Light + 1)) is
                  when '.' =>
                     null;
                  when '#' =>
                     Include (Machine.Light_State, Light);
                  when others =>
                     raise Data_Error with
                       "Expected '.' or '#' and found " &
                       Element (Light_String, Positive (Light + 1)) & "'";
               end case; -- Element (Light_String, Light + 1)
            end loop; -- Light in Controls range 0 ...
            loop -- Read buttons
               Find_Token (Text, Round_Start, Start_At, Inside, First, Last);
               exit when Last = 0;
               declare -- Buttons
                  Control_State : Control_States.Set :=
                    Control_States.Empty_Set;
               begin -- Buttons
                  Start_At := Last + 1;
                  loop -- Read one button
                     Find_Token (Text, Decimal_Digit_Set, Start_At, Inside,
                                 First, Last);
                     Start_At := Last + 1;
                     Include (Control_State,
                              Controls'Value (Slice (Text, First, Last)));
                     case Element (Text, Start_At) is
                     when ',' =>
                        null;
                     when ')' =>
                        Insert (Machine.Button_Map, Button_Index,
                                Control_State);
                        Button_Index := @ + 1;
                        exit;
                     when others =>
                        raise Data_Error with "Expected ')' and found '" &
                          Element (Text, Start_At) & "'";
                     end case; -- Element (Text, Start_At)
                  end loop; -- Read one button
               end; -- Read one button
            end loop; -- Read buttons
            --  Read Joltages
            Find_Token (Text, Curly_Start, Start_At, Inside, First, Last);
            if Element (Text, First) = '{' then
               Start_At := Last + 1;
            else
               raise Data_Error with "Expected '{' and found '" &
                 Element (Text, First) & "'";
            end if; -- Element (Text, First) /= '{'
            loop -- Read one Joltage
               Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First,
                           Last);
               exit when Last = 0;
               Start_At := Last + 1;
               Insert (Machine.Joltage_Store, Joltage_Index,
                       Joltages'Value (Slice (Text, First, Last)));
               Joltage_Index := @ + 1;
            end loop; -- Read one Joltage
            Find_Token (Text, Curly_End, Start_At, Inside, First, Last);
            if Element (Text, First) = '}' then
               Append (Machine_Store, Machine);
            else
               raise Data_Error with "Expected '}' and found '" &
                 Element (Text, First) & "'";
            end if; -- Element (Text, First) = '}'
         end; -- Machine declaration block
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_Input;

   procedure Generate_Subsets (Button_Set : Button_Sets.Set;
                               Button_Subset : out Button_Subsets.List) is

      subtype Generators is Unsigned_32
        range 1 .. 2 ** Natural (Length (Button_Set)) - 1;
      Mask : constant Generators := 1;
      subtype Element_Indices is Natural range
        0 .. Natural (Length (Button_Set) - 1);
      Element_Array : array (Element_Indices) of Button_Indices;
      Subset : Button_Sets.Set;
      Bc : Button_Sets.Cursor := First (Button_Set);

   begin -- Generate_Subsets
      Clear (Button_Subset);
      for B in Element_Indices loop
         Element_Array (B) := Element (Bc);
         Next (Bc);
      end loop; -- B in Element_Indices
      for G in Generators loop
         Clear (Subset);
         for B in Element_Indices loop
            if (G and Shift_Left (Mask, B)) > 0 then
               Include (Subset, Element_Array (B));
            end if; -- (G and Shift_Left (Mask, B)) > 0
         end loop; -- B in Element_Indices
         Append (Button_Subset, Subset);
      end loop; -- G in Generators
   end Generate_Subsets;

   function Count_Presses (Machine : Machines) return Presses is

      Button_Set : Button_Sets.Set := Button_Sets.Empty_Set;
      Button_Subset : Button_Subsets.List;
      Best : Presses := Presses'Last;
      Test : Control_States.Set;

   begin -- Count_Presses
      for B in Iterate (Machine.Button_Map) loop
         Include (Button_Set, Key (B));
      end loop; -- B in Iterate (Machine.Button_Map)
      Generate_Subsets (Button_Set, Button_Subset);
      for S in Iterate (Button_Subset) loop
         Clear (Test);
         for B in Iterate (Element (S)) loop
            Symmetric_Difference (Test, Machine.Button_Map (Element (B)));
         end loop; -- B in Iterate (Element (S))
         if Test = Machine.Light_State and then
           Presses (Length (Element (S))) < Best
         then
            Best := Presses (Length (Element (S)));
         end if; -- Test = Machine.Light_State and then ...
      end loop; -- S in Iterate (Button_Subset)
      return Best;
   end Count_Presses;

   function Count_Presses_2 (Machine : Machines) return Presses is

      procedure Find_Actions (Machine : Machines;
                              Button_Subset : Button_Subsets.List;
                              Reduced_State : Joltage_Stores.Map;
                              Action_List : out Action_Lists.List) is

         Target_Odd_State : Control_States.Set := Control_States.Empty_Set;
         Test : Control_States.Set;
         Action : Actions := (0, Joltage_Stores.Empty_Map);

      begin -- Find_Actions
         for R in Iterate (Reduced_State) loop
            if Element (R) mod 2 /= 0 then
               Insert (Target_Odd_State, Key (R));
            end if; -- Element (R) mod 2 /= 0
         end loop; -- R in Iterate (Reduced_State)
         for J in Iterate (Machine.Joltage_Store) loop
            Insert (Action.Jolts, Key (J), 0);
         end loop; -- J in Iterate (Machine.Joltage_Store)
         Clear (Action_List);
         for S in Iterate (Button_Subset) loop
            Clear (Test);
            for B in Iterate (Element (S)) loop
               Symmetric_Difference (Test, Machine.Button_Map (Element (B)));
            end loop; -- B in Iterate (Element (S))
            if Test = Target_Odd_State then
               Action.Pressed := Presses (Length (Element (S)));
               for J in Iterate (Action.Jolts) loop
                  Action.Jolts (J) := 0;
               end loop; -- Test = Target_Odd_State
               for B in Iterate (Element (S)) loop
                  for J in Iterate (Machine.Button_Map (Element (B))) loop
                     Action.Jolts (Element (J)) :=
                       Action.Jolts (Element (J)) + 1;
                  end loop; -- J in Iterate (Machine.Button_Map (Element (B)))
               end loop; -- B in Iterate (Element (S))
               Append (Action_List, Action);
            end if; -- Test = Target_Odd_State
         end loop; -- S in Iterate (Button_Subset)
      end Find_Actions;

      function Half (Current : Joltage_Stores.Map) return Joltage_Stores.Map is

         Result : Joltage_Stores.Map := Joltage_Stores.Empty_Map;

      begin -- Half
         for C in Iterate (Current) loop
            Insert (Result, Key (C), Element (C) / 2);
         end loop; -- C in Iterate (Current)
         return Result;
      end Half;

      function Search (Machine : Machines;
                       Button_Subset : Button_Subsets.List;
                       Current : Joltage_Stores.Map) return Presses is

         Best : Presses := Presses'Last;

      begin -- Search
         if (for all J of Current => J = 0) then
            --  Solved
            Best := 0;
         elsif (for all J of Current => J mod 2 = 0) then
            declare -- Continue Search, all even
               Local : Presses;
            begin -- Continue Search, all even
               Local := Search (Machine, Button_Subset, Half (Current));
               if Local < Presses'Last and then 2 * Local < Best then
                  Best := 2 * Local;
               end if; -- Local < Presses'Last and then 2 * Local < Best
            end;  -- Continue Search, all even
         else
            declare -- Continue Search, some odd values
               Action_List : Action_Lists.List := Action_Lists.Empty_List;
               Next : Joltage_Stores.Map;
               Local : Presses;
            begin -- Continue Search, some odd values
               Find_Actions (Machine, Button_Subset, Current, Action_List);
               for A in Iterate (Action_List) loop
                  if Element (A).Jolts <= Current then
                     Next := Half (Current - Element (A).Jolts);
                     Local := Search (Machine, Button_Subset, Next);
                     if Local < Presses'Last and then
                       2 * Local + Element (A).Pressed < Best
                     then
                        Best := 2 * Local + Element (A).Pressed;
                     end if; -- Local < Presses'Last and then ...
                  end if; -- Element (A).Jolts < Current
               end loop; -- A in Iterate (Action_List)
            end; -- Continue Search, some odd values
         end if; -- (for all J of Current => J = 0)
         return Best;
      end Search;

      function Search (Button_List_In : Button_Lists.List;
                       Button_Map : Button_Maps.Map;
                       Current : Joltage_Store.Map) return Presses is

         procedure Update (Button_List : in out Button_Lists;
                           Button_Map : Button_Maps.Map;
                           Current : Joltage_Store.Map) is

         begin -- Update
            for B in Iterate (Button_List) loop
               Button_List (B).Product := 1;
               for J in Iterate (Button_Map (Element (B).Button_Index)) loop
                  Button_List (B).Product :=
                    Button_List (B).Product * Current (J);
               end loop; -- J in Iterate (Button_Map (Element (B) ...
            end loop; -- B in Iterate (Button_List)
            Sort (Button_List);
            while First_Element (Button_List) = 0 loop
               Delete_First (Button_List);
            end loop; -- First_Element (Button_List) = 0
         end Update;

         function Possible (Button_List : Button_Lists;
                            Button_Map : Button_Maps.Map;
                            Current : Joltage_Store.Map) return Boolean is

            Can_Change, Non_Zero : Control_States := Control_States.Empty_Set;

         begin -- Possible
            for B in Iterate (Button_List) loop
               Union (Can_Change, Button_Map (Element (B).Button_Index));
            end loop; -- B in Iterate (Button_List)
            for J in Iterate (Current) loop
               if Element (J) > 0 then
                  Include (Non_Zero, Key (J));
               end if; -- Element (J) > 0
            end loop; -- J in Iterate (Current)
            --  Subset allows for a button in the list being pressed zero
            --  times.
            return Is_Subset (Non_Zero, Can_Change);
         end Possible;

         procedure Limit (Button_Index : Button_indices;
                          Next_Button_List : Button_Lists.List;
                          Button_Map : Button_Maps.Map;
                          Current : Joltage_Store.Map;
                          Lower, Upper : out Presses) is

            Can_Change, Non_Zero : Control_States := Control_States.Empty_Set;

         begin -- Limit
            Upper := Presses'Last;
            Lower := 0;
            for J in Iterate (Button_Map (Button_Index)) loop
               if Current (Element(J)) < Upper then
                  Upper := Current (Element(J));
               end if; -- Current (Element(J)) < Upper
            end loop; -- J in Iterate (Button_Map (Button_Index))
            for B in Iterate (Next_Button_List) loop
               Union (Can_Change, Button_Map (Element (B).Button_Index));
            end loop; -- B in Iterate (Button_List)
            for J in Iterate (Current) loop
               if Element (J) > 0 then
                  Include (Non_Zero, Key (J));
               end if; -- Element (J) > 0
            end loop; -- J in Iterate (Current)
            Can_Change := Intersection (Button_Map (Button_Index),
                                        Non_Zero - Can_Change);
            if Length (Can_Change) = 1 then
               -- Only one button can increase a particular Joltage
               Lower :=
                 Current (First_Element (Button_Map (Button_Index)));
               --  Potentially Upper could be less than Lower if the
               --  button increases another Joltage that is less than this
               --  Joltage.
            end if; -- Length (Can_Change) = 1
         end Limit;

         function Next (P : Presses;
                        Control_State: Control_States.Set;
                        Current : Joltage_Store.Map)
                        return Joltage_Stores.Map is

            Result : Joltage_Store.Map := Copy (Current);

         begin -- Next
            for J in Iterate (Control_State) loop
               Next (Element (J)) := Current (Element (J)) - Joltages (P);
            end loop; -- J in Iterate (Control_State)
         end Next;

         Button_list : Button_Lists.List := Copy (Button_List_In);
         Next_Button_List : Button_Lists.List;
         Best : Presses := Presses'Last;
         Button_Effect : Control_States;
         Upper, Lower, Local : Presses;

      begin -- Search
         if (for all J of Current => J = 0) then
            --  Solved
            Best := 0;
         else
            --  Continue Search, testing one button at this level
            Update (Button_list);
            if Possible (Button_List, Button_Map, Current) then
               Next_Button_List := Copy (Button_list);
               Delete_First (Next_Button_List);
               Button_Effect :
                 Button_Map (First_Element (Button_list).Button_Index;
               Limit (First_Element (Button_list).Button_Index,
                      Next_Button_List, Button_Map, Current, Lower, Upper);
               for P in Presses range Lower .. Upper loop
                  Local := Search (Next_Button_List, Button_Map,
                                   Next (P, Button_Effect, Current)));
                  if Local < Presses'Last and then Local + P < Best then
                     Best := Local + P;
                  end if; -- Local + P < Best
               end loop; -- P in Presses range Lower .. Upper
            end if; -- Possible (Button_List, Button_Map, Current)
         end if; -- (for all J of Current => J = 0)
         return Best;
      end Search;

      Button_Set : Button_Sets.Set := Button_Sets.Empty_Set;
      Button_Subset : Button_Subsets.List;
      Button_List : Button_Lists.List := Button_Lists.Empty_List;
      Best : Presses;

   begin -- Count_Presses_2
      for B in Iterate (Machine.Button_Map) loop
         Include (Button_Set, Key (B));
      end loop; -- B in Iterate (Machine.Button_Map)
      Generate_Subsets (Button_Set, Button_Subset);
      Best := Search (Machine, Button_Subset, Machine.Joltage_Store);
      if Best < Presses'Last then
         return Best;
      else
         for B in Iterate (Machine.Button_Map) loop
            Append (Button_List, (Key (B)), 0);
         end loop; -- in Iterate (Machine.Button_Map)
         Best := Search (Button_List, Machine.Button_Map, Machine.Joltage_Store);
      end if; -- Best < Presses'Last
   end Count_Presses_2;

   Machine_Store : Machine_Stores.List := Machine_Stores.Empty_List;
   Sum : Presses := 0;

begin -- December_10
   Read_Input (Machine_Store);
   for M in Iterate (Machine_Store) loop
      Sum := @ + Count_Presses (Element (M));
   end loop; -- M in Iterate (Machine_Store)
   Put_Line ("Part one:" & Sum'Img);
   Put_CPU_Time;
   Sum := 0;
   for M in Iterate (Machine_Store) loop
      Sum := @ + Count_Presses_2 (Element (M));
   end loop; -- M in Iterate (Machine_Store)
   Put_Line ("Part two:" & Sum'Img);
   Put_CPU_Time;
end December_10;
