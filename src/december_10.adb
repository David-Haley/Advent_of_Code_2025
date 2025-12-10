with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_10 is

   subtype Controls is Natural;

   package Control_States is new Ada.Containers.Ordered_Sets (Controls);
   use Control_States;

   package Button_Lists is new
     Ada.Containers.Doubly_Linked_Lists (Control_States.Set);
   use Button_Lists;

   subtype Joltage_Indices is Positive;
   subtype Joltages is Natural;

   package Joltage_Stores is new
     Ada.Containers.Vectors (Joltage_Indices, Joltages);
   use Joltage_Stores;

   type Machines is record
      Light_State : Control_States.Set := Control_States.Empty_Set;
      Button_List : Button_Lists.List := Button_Lists.Empty_List;
      Joltage_Store : Joltage_Stores.Vector := Joltage_Stores.Empty_Vector;
   end record; -- Machines

   package Machine_Stores is new Ada.Containers.Doubly_Linked_Lists (Machines);
   use Machine_Stores;

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
            Light_String : Unbounded_String;
            Button_Set : Control_States.Set := Control_States.Empty_Set;
            Square_Set : constant Character_Set := To_Set ("[]");
            Round_Start : constant Character_Set := To_Set ("(");
            Curly_Start : constant Character_Set := To_Set ("{");
            Curly_End : constant Character_Set := To_Set ("}");

         begin -- Machine declaration block
            --  Read lights
            Find_Token (Text, Square_Set, Start_At, Outside, First, Last);
            Light_String := Unbounded_Slice (Text, First, Last);
            Start_At := Last + 1;
            for Light in Controls range 0 .. Length (Light_String) - 1 loop
               case Element (Light_String, Light + 1) is
                  when '.' =>
                     null;
                  when '#' =>
                     Include (Machine.Light_State, Light);
                  when others =>
                     raise Data_Error with
                       "Expected '.' or '#' and found " &
                       Element (Light_String, Light + 1) & "'";
               end case; -- Element (Light_String, Light + 1)
            end loop; -- Light in Controls range 0 .. Length (Light_String) ...
            loop -- Read buttons
               Find_Token (Text, Round_Start, Start_At, Inside, First, Last);
               exit when Last = 0;
               Clear (Button_Set);
               Start_At := Last + 1;
               loop -- Read one button
                  Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First,
                              Last);
                  Start_At := Last + 1;
                  Include (Button_Set,
                           Controls'Value (Slice (Text, First, Last)));
                  case Element (Text, Start_At) is
                     when ',' =>
                        null;
                     when ')' =>
                        Append (Machine.Button_List, Button_Set);
                        exit;
                        when others =>
                        raise Data_Error with "Expected ')' and found '" &
                          Element (Text, Start_At) & "'";
                  end case; -- Element (Text, Start_At)
               end loop; -- Read one button
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
               Append (Machine.Joltage_Store,
                       Joltages'Value (Slice (Text, First, Last)));
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

   function Count_Presses (Machine : Machines) return Natural is

      type Queue_Elements is record
         Light_State : Control_States.Set;
         To_Press : Control_States.Set;
         Presses : Natural;
      end record; -- Queue_Elements

      package Q_Int is new
        Ada.Containers.Synchronized_Queue_Interfaces (Queue_Elements);

      package Queues is new
        Ada.Containers.Unbounded_Synchronized_Queues (Q_Int);

      Queue : Queues.Queue;
      Current, Next : Queue_Elements;

   begin -- Count_Presses
      Current := (Control_States.Empty_Set, Control_States.Empty_Set, 0);
      for B in Iterate (Machine.Button_List) loop
         Current.To_Press := Copy (Element (B));
         Queue.Enqueue (Current);
      end loop; -- B in Iterate (Machine.Button_List)
      loop -- until solved
         Queue.Dequeue (Current);
         Symmetric_Difference (Current.Light_State, Current.To_Press);
         Current.Presses := @ + 1;
         --  Press button toggle lights
         exit when Current.Light_State = Machine.Light_State;
         Next.Presses := Current.Presses;
         for B in Iterate (Machine.Button_List) loop
            if Element (B) /= Current.To_Press then
               Next.Light_State := Copy (Current.Light_State);
               Next.To_Press := Copy (Element (B));
               Queue.Enqueue (Next);
            end if; -- Element (B) /= Current.To_Press
         end loop; -- B in Iterate (Machine.Button_List)
      end loop;
      return Current.Presses;
   end Count_Presses;

   Machine_Store : Machine_Stores.List := Machine_Stores.Empty_List;
   Sum : Natural := 0;

begin -- December_10
   Read_Input (Machine_Store);
   for M in Iterate (Machine_Store) loop
      Sum := @ + Count_Presses (Element (M));
   end loop; -- M in Iterate (Machine_Store)
   Put_Line ("Part one:" & Sum'Img);
   Put_CPU_Time;
   Put_Line ("Part two:");
   Put_CPU_Time;
end December_10;
