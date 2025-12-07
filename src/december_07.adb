with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_07 is

   subtype Ordinates is Natural;
   type Coordinates is record
      X, Y : Ordinates;
   end record; -- Coordinates

   function "<" (Left, Right : Coordinates) return Boolean is
     (Left.X < Right.X or else (Left.X = Right.X and then Left.Y < Right.Y));

   package Splitter_Stores is new Ada.Containers.Ordered_Sets (Coordinates);
   use Splitter_Stores;

   subtype Big_Natural is Long_Long_Integer range 0 .. Long_Long_Integer'Last;

   procedure Read_Input (Splitter_Store : out Splitter_Stores.Set;
                         Start : out Coordinates;
                         Last_Y : out Ordinates) is

      Input_File : File_Type;
      Text : Unbounded_String;

   begin -- Read_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_07.txt");
      else
         Open (Input_File, In_File, Argument (1));
      end if; -- Argument_Count = 0
      Clear (Splitter_Store);
      Last_Y := 1;
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         for X in Ordinates range 1 .. Length (Text) loop
            case Element (Text, X) is
               when '.' =>
                  null;
               when 'S' =>
                  Start := (X, Last_Y);
               when '^' =>
                  Include (Splitter_Store, (X, Last_Y));
               when others =>
                  raise Program_Error with "Unexpected Character '" &
                    Element (Text, X) & "at (" & X'Img & ',' &
                    Last_Y'Img & ')';
            end case; -- Element (Text, X)
         end loop; -- X in Ordinates range 1 .. Length (Text)
         Last_Y := @ + 1;
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_Input;

   function Split_Count (Splitter_Store : Splitter_Stores.Set;
                         Start : Coordinates;
                         Last_Y : Ordinates) return Count_Type is

      package Q_Int is new
        Ada.Containers.Synchronized_Queue_Interfaces (Coordinates);

      package Beam_Queues is new
        Ada.Containers.Unbounded_Synchronized_Queues (Q_Int);

      Beam_Queue : Beam_Queues.Queue;
      Beam_Set : Splitter_Stores.Set := Splitter_Stores.Empty_Set;
      Beam : Coordinates;

   begin -- Split_Count
      Beam_Queue.Enqueue (Start);
      while Beam_Queue.Current_Use > 0 loop
         Beam_Queue.Dequeue (Beam);
         if Contains (Splitter_Store, Beam) and then
           not Contains (Beam_Set, Beam)
         then
            Beam_Queue.Enqueue ((Beam.X - 1, Beam.Y));
            Beam_Queue.Enqueue ((Beam.X + 1, Beam.Y));
         elsif Beam.Y >= Last_Y then
            null; -- beam exited map
         elsif not Contains (Beam_Set, Beam) then
            Beam_Queue.Enqueue ((Beam.X, Beam.Y + 1));
         end if; -- Contains (Splitter_Store, Beam) and then
         Include (Beam_Set, Beam);
      end loop; -- Beam_Queue.Current_Use > 0
      return Length (Intersection (Splitter_Store, Beam_Set));
   end Split_Count;

   function Timeline_Count (Splitter_Store : Splitter_Stores.Set;
                            Start : Coordinates;
                            Last_Y : Ordinates) return Big_Natural is

      package Caches is new
        Ada.Containers.Ordered_Maps (Coordinates, Big_Natural);
      use Caches;

      function Search (Splitter_Store : Splitter_Stores.Set;
                       Beam : Coordinates;
                       Last_Y : Ordinates;
                       Cache : in out Caches.Map) return Big_Natural is

         Timeline : Big_Natural := 0;

      begin -- Search
         if Contains (Cache, Beam) then
            Timeline := Cache (Beam);
         elsif Beam.Y >= Last_Y then
            Timeline := 1; -- Exited map
         elsif Contains (Splitter_Store, Beam) then
            --  Beam splits
            Timeline := Search (Splitter_Store, (Beam.X - 1, Beam.Y), Last_Y,
                                Cache) +
              Search (Splitter_Store, (Beam.X + 1, Beam.Y), Last_Y, Cache);
         else
            --  Beam continues straight
            Timeline := Search (Splitter_Store, (Beam.X, Beam.Y + 1), Last_Y,
                                Cache);
         end if; -- Contains (Cache, Beam)
         Include (Cache, Beam, Timeline);
         return Timeline;
      end Search;

      Cache : Caches.Map := Caches.Empty_Map;

   begin -- Timeline_Count
      Clear (Cache);
      return Search (Splitter_Store, Start, Last_Y, Cache);
   end Timeline_Count;

   Splitter_Store : Splitter_Stores.Set;
   Start : Coordinates;
   Last_Y : Ordinates;

begin -- December_07
   Read_Input (Splitter_Store, Start, Last_Y);
   Put_Line ("Part one:" & Split_Count (Splitter_Store, Start, Last_Y)'Img);
   Put_CPU_Time;
   Put_Line ("Part two:" & Timeline_Count (Splitter_Store, Start, Last_Y)'Img);
   Put_CPU_Time;
end December_07;
