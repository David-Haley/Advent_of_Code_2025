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
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
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

   subtype Joltages is Natural;

   package Joltage_Stores is new
     Ada.Containers.Ordered_Maps (Controls, Joltages);
   use Joltage_Stores;

   type Machines is record
      Light_State : Control_States.Set := Control_States.Empty_Set;
      Button_Map : Button_Maps.Map := Button_Maps.Empty_Map;
      Joltage_Store : Joltage_Stores.Map := Joltage_Stores.Empty_Map;
   end record; -- Machines

   subtype Presses is Natural;

   package Machine_Stores is new Ada.Containers.Doubly_Linked_Lists (Machines);
   use Machine_Stores;

   type Buttons is record
      Button_Index : Button_Indices;
      Max_Presses : Presses;
      Exact : Boolean;
   end record; -- Buttons

   package To_Press_Lists is new Ada.Containers.Doubly_Linked_Lists (Buttons);
   use To_Press_Lists;

   function "<" (Left, Right : Buttons) return Boolean is
     (Left.Exact > Right.Exact or else
     (Left.Exact > Right.Exact and then Left.Max_Presses < Right.Max_Presses));
   --  The sorting order is to allow buttons with a known number of presses to
   --  be processed first, followed by buttons with the smallest range of
   --  presses.

   package To_Press_Sorting is new To_Press_Lists.Generic_Sorting;
   use To_Press_Sorting;

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

   function Count_Presses (Machine : Machines) return Presses is

      --  A key clue from Redit is that any button only needs to be pressed
      --  once.

      type Queue_Elements is record
         Light_State : Control_States.Set;
         To_Press : Button_Indices;
         Can_Press : Button_Sets.Set;
         Presses : Natural;
      end record; -- Queue_Elements

      package Q_Int is new
        Ada.Containers.Synchronized_Queue_Interfaces (Queue_Elements);

      package Queues is new
        Ada.Containers.Unbounded_Synchronized_Queues (Q_Int);

      Queue : Queues.Queue;
      Current, Next : Queue_Elements;

   begin -- Count_Presses
      Current := (Control_States.Empty_Set, Button_Indices'First,
                  Button_Sets.Empty_Set, 0);
      for B in Iterate (Machine.Button_Map) loop
         Current.To_Press := Key (B);
         Clear (Current.Can_Press);
         for C in Iterate (Machine.Button_Map) loop
            if B /= C then
               Include (Current.Can_Press, Key (C));
            end if; -- C in Iterate (Machine.Button_Map)
         end loop; -- C in Iterate (Machine.Button_Map)
         Queue.Enqueue (Current);
      end loop; -- B in Iterate (Machine.Button_Map)
      loop -- until solved
         Queue.Dequeue (Current);
         Symmetric_Difference (Current.Light_State,
                               Machine.Button_Map (Current.To_Press));
         Current.Presses := @ + 1;
         --  Press button toggle lights
         exit when Current.Light_State = Machine.Light_State;
         Next.Presses := Current.Presses;
         Next.Light_State := Copy (Current.Light_State);
         for B in Iterate (Current.Can_Press) loop
            Next.To_Press := Element (B);
            Next.Can_Press := Copy (Current.Can_Press);
            Exclude (Next.Can_Press, Element (B));
            Queue.Enqueue (Next);
         end loop; -- B in Iterate (Machine.Button_Map)
      end loop; -- until solved
      return Current.Presses;
   end Count_Presses;

   procedure Press_Limits (Machine : Machines;
                           Can_Press : Button_Sets.Set;
                           Current : Joltage_Stores.Map;
                           To_Press_List : out To_Press_Lists.List) is

      subtype Jolt_Indices is Controls range Controls'First ..
        Last_Key (Machine.Joltage_Store);

      type Button_References is record
         Count : Natural := 0;
         Tc : To_Press_Lists.Cursor := To_Press_Lists.No_Element;
      end record; -- Button_References

      Button_Reference : array (Jolt_Indices) of Button_References;

   begin -- Press_Limits
      --  An upper bound on the number of times a button can be pressed is set
      --  by the lowest Joltage that is to be increment. In some cases this may
      --  be 0.
      Clear (To_Press_List);
      for B in Iterate (Can_Press) loop
         Append (To_Press_List, (Element (B), Presses'Last, False));
         for J in Iterate (Machine.Button_Map (Element (B))) loop
            if Machine.Joltage_Store (Element (J)) - Current (Element (J))
            < Last_Element (To_Press_List).Max_Presses
            then
               To_Press_List (Last (To_Press_List)).Max_Presses :=
                 Machine.Joltage_Store (Element (J)) - Current (Element (J));
            end if; -- Machine.Joltage_Store (Element (J)) < ...
            Button_Reference (Element (J)).Count := @ + 1;
            Button_Reference (Element (J)).Tc := Last (To_Press_List);
         end loop; -- J in Iterate (Machine.Button_Map (Element (B)))
      end loop; -- B in Iterate (Can_Press)
      --  If only one button can set a particular Joltage, then the exact
      --  number of presses reqired is known.
      for J in Jolt_Indices loop
         if Button_Reference (J).Count = 1 then
            To_Press_List (Button_Reference (J).Tc).Exact := True;
         end if; -- Button_Reference (J).Count = 1
      end loop; -- J in Jolt_Indices
      Sort (To_Press_List);
   end Press_Limits;

   procedure Press (Machine : Machines;
                    Button : Button_Indices;
                    Count : Presses;
                    Current : Joltage_Stores.Map;
                    Next : out Joltage_Stores.Map) is

   begin -- Press
      Next := Copy (Current);
      for J in Iterate (Machine.Button_Map (Button)) loop
         Next (Element (J)) := Next (Element (J)) + Count;
      end loop;
   end Press;

   function Count_Presses_2 (Machine : Machines) return Presses is

      procedure Search (Machine : Machines;
                        Current : Joltage_Stores.Map;
                        Pressed : Presses;
                        Can_Press : Button_Sets.Set;
                        Best : in out Presses) is

         Next : Joltage_Stores.Map;
         Next_To_Press : Button_Sets.Set := Copy (Can_Press);
         To_Press_List : To_Press_Lists.List :=
           To_Press_Lists.Empty_List;
         Tc : To_Press_Lists.Cursor;

      begin -- Search
         --  Put_Line (Current'Img & Pressed'Img & Can_Press'Img);
         if Current = Machine.Joltage_Store and then Pressed < Best then
            --  A better solution found
            Best := Pressed;
         elsif Pressed < Best then
            --  Continue search, solution not found and more presses are
            --  available without exceeing Best.
            Press_Limits (Machine, Can_Press, Current, To_Press_List);
            Tc := First (To_Press_List);
            if Tc /= To_Press_Lists.No_Element then
               Exclude (Next_To_Press, Element (Tc).Button_Index);
               if Element (Tc).Exact then
                  Press (Machine, Element (Tc).Button_Index,
                        Element (Tc).Max_Presses, Current, Next);
                  Search (Machine, Next, Pressed + Element (Tc).Max_Presses,
                        Next_To_Press, Best);
               else
                  for P in Presses range 0 .. Element (Tc).Max_Presses loop
                     Press (Machine, Element (Tc).Button_Index, P, Current,
                           Next);
                     Search (Machine, Next, Pressed + P, Next_To_Press, Best);
                  end loop; -- P in Presses range 0 .. Element (Tc).Max_Presses
               end if; -- Element (Tc).Exact
            end if; -- Tc /= To_Press_Lists.No_Element then
         end if; -- Current = Machine.Joltage_Store and then Pressed < Best
      end Search;

      Best : Presses := Presses'Last;
      Current : Joltage_Stores.Map := Joltage_Stores.Empty_Map;
      Can_Press : Button_Sets.Set := Button_Sets.Empty_Set;

   begin -- Count_Presses_2
      for J in Iterate (Machine.Joltage_Store) loop
         Insert (Current, Key (J), 0);
      end loop; -- J in Iterate (Machine.Joltage_Store)
      for B in Iterate (Machine.Button_Map) loop
         Include (Can_Press, Key (B));
      end loop; -- B in Iterate (Machine.Button_Map)
      Search (Machine, Current, 0, Can_Press, Best);
      Put_Line (Best'Img);
      return Best;
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
