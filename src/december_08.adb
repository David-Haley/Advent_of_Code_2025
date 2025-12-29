with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Doubly_Linked_Lists;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_08 is

   subtype Ordinates is Long_Long_Integer;

   type Coordinates is record
      X, Y, Z : Ordinates;
   end record; -- Coordinates

   subtype Distances is Long_Long_Integer range 0 .. Long_Long_Integer'Last;

   subtype Box_Indices is Positive;

   subtype Circuit_Indices is Positive;

   package Box_Sets is new Ada.Containers.Ordered_Sets (Box_Indices);
   use Box_Sets;

   type Junction_Boxes is record
      Position : Coordinates;
      Connected : Box_Sets.Set := Box_Sets.Empty_Set;
   end record; -- Junction_Boxes

   package Box_Stores is new
     Ada.Containers.Vectors (Box_Indices, Junction_Boxes);
   use Box_Stores;

   package Circuit_Stores is new
     Ada.Containers.Vectors (Circuit_Indices, Box_Sets.Set);
   use Circuit_Stores;

   function "<" (Left, Right : Box_Sets.Set) return Boolean is
     (Length (Left) > Length (Right));
   --  This looks wrong but the sort required is greatest to least!

   package Circuit_Sorting is new Circuit_Stores.Generic_Sorting;

   type Pairs is record
      B1, B2 : Box_Indices;
      Distance : Distances;
   end record; -- Pairs;

   package Pair_Stores is new Ada.Containers.Doubly_Linked_Lists (Pairs);
   use Pair_Stores;

   function "<" (Left, Right : Pairs) return Boolean is
     (Left.Distance < Right.Distance);

   package Pair_Sorting is new Pair_Stores.Generic_Sorting;

   procedure Read_Input (Box_Store : out Box_Stores.Vector;
                         Max_Pairs : out Positive) is

      Input_File : File_Type;
      Text : Unbounded_String;
      Box : Junction_Boxes;
      Start_At, First : Positive;
      Last : Natural;

   begin -- Read_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_08.txt");
      else
         Open (Input_File, In_File, Argument (1));
      end if; -- Argument_Count = 0
      if Argument_Count = 2 then
         Max_Pairs := Positive'Value (Argument (2));
      else
         Max_Pairs := 1000;
      end if; -- Argument_Count = 2
      Clear (Box_Store);
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         Start_At := 1;
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
         Box.Position.X := Ordinates'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
         Box.Position.Y := Ordinates'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
         Box.Position.Z := Ordinates'Value (Slice (Text, First, Last));
         Append (Box_Store, Box);
         Include (Box_Store (Last_Index (Box_Store)).Connected,
                  Last_Index (Box_Store));
         --  In a circui with itself
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_Input;

   function Distance (P1, P2 : Coordinates) return Distances is
     ((P1.X - P2.X) ** 2 + (P1.Y - P2.Y) ** 2 + (P1.Z - P2.Z) ** 2);
   --  More correctly this returns the square of the distsnce!

   procedure Build_Pair_Store (Box_Store : Box_Stores.Vector;
                               Pair_Store : out Pair_Stores.List) is

   begin -- Build_Pair_Store
      Clear (Pair_Store);
      for B1 in Box_Indices range Box_Indices'First ..
        Last_Index (Box_Store) - 1
      loop
         for B2 in Box_Indices range B1 + 1 .. Last_Index (Box_Store) loop
            Append (Pair_Store, (B1, B2,
                    Distance (Box_Store (B1).Position,
                      Box_Store (B2).Position)));
         end loop; -- B2 in Box_Indices range B1 + 1 .. Last_Index (Box_Store)
      end loop; -- B1 in Box_Indices range Box_Indices'First ...
      Pair_Sorting.Sort (Pair_Store);
   end Build_Pair_Store;

   procedure Build_Circuits (Box_Store : in out Box_Stores.Vector;
                             Pair_Store : Pair_Stores.List;
                             Circuit_Store : out Circuit_Stores.Vector;
                             Max_Pairs : Positive;
                             P : out Pair_Stores.Cursor) is

      Pair_Count : Natural := 0;
      Common : Box_Sets.Set;

   begin -- Build_Circuits
      P := First (Pair_Store);
      Clear (Circuit_Store);
      loop -- Until Max_Pairs have been processed
         Pair_Count := @ + 1;
         if Is_Empty (Intersection (Box_Store (Element (P).B1).Connected,
                      Box_Store (Element (P).B2).Connected))
         then
            --  Can be connected, no common junction box
            Common := Union (Box_Store (Element (P).B1).Connected,
                             Box_Store (Element (P).B2).Connected);
            Include (Common, Element (P).B1);
            Include (Common, Element (P).B2);
            for C in Iterate (Common) loop
               Box_Store (Element (C)).Connected := Copy (Common);
            end loop; -- C in Iterate (Common)
         end if; --  Is_Empty (Intersection (Box_Store (Element (P).B1) ...
         exit when Pair_Count >= Max_Pairs;
         Next (P);
         --  it is assumed that Max_Pairs will be less that the total number of
         --  pairs, an exception will be raised if this is not the case.
      end loop; -- Until Max_Pairs have been processed
      for B in Iterate (Box_Store) loop
         if Find (Circuit_Store, Element (B).Connected) =
           Circuit_Stores.No_Element
         then
            Append (Circuit_Store, Copy (Element (B).Connected));
         end if; -- Find (Circuit_Store, Element (B).Connected) = ...
      end loop; -- B in Iterate (Box_Store)
      Circuit_Sorting.Sort (Circuit_Store);
   end Build_Circuits;

   function Build_Circuits (Box_Store : in out Box_Stores.Vector;
                            Last_Pair : Pair_Stores.Cursor) return Distances is

      --  This function finishes the job of connection all the junction boxes,
      --  resuming where the procedure Build_Circuits (part 1 solution)
      --  finished.

      P : Pair_Stores.Cursor := Next (Last_Pair);
      Common : Box_Sets.Set := Box_Sets.Empty_Set;
      Common_Length : Count_Type := Length (Common);

   begin -- Build_Circuits
      loop -- Untill all junction boxes are connected
         if Is_Empty (Intersection (Box_Store (Element (P).B1).Connected,
                      Box_Store (Element (P).B2).Connected))
         then
            --  Can be connected, no common junction box
            Common := Union (Box_Store (Element (P).B1).Connected,
                             Box_Store (Element (P).B2).Connected);
            Include (Common, Element (P).B1);
            Include (Common, Element (P).B2);
            Common_Length := Length (Common);
            for C in Iterate (Common) loop
               Box_Store (Element (C)).Connected := Copy (Common);
            end loop; -- C in Iterate (Common)
         end if; --  Is_Empty (Intersection (Box_Store (Element (P).B1) ...
         exit when Common_Length >= Length (Box_Store);
         --  The loop should terminate whilst there are more pairs to processs.
         Next (P);
      end loop; -- P /= Pair_Stores.No_Element and then ...
      --  P should access the last pair to be connected.
      return Box_Store (Element (P).B1).Position.X *
        Box_Store (Element (P).B2).Position.X;
   end Build_Circuits;

   Box_Store : Box_Stores.Vector := Box_Stores.Empty_Vector;
   Pair_Store : Pair_Stores.List := Pair_Stores.Empty_List;
   Circuit_Store : Circuit_Stores.Vector := Circuit_Stores.Empty_Vector;
   Max_Pairs : Positive;
   Last_Pair : Pair_Stores.Cursor;

begin -- December_08
   Read_Input (Box_Store, Max_Pairs);
   Build_Pair_Store (Box_Store, Pair_Store);
   Build_Circuits (Box_Store, Pair_Store, Circuit_Store, Max_Pairs, Last_Pair);
   Put_Line ("Part one:" & Count_Type'Image (Length (Circuit_Store (1)) *
               Length (Circuit_Store (2)) * Length (Circuit_Store (3))));
   Put_CPU_Time;
   Put_Line ("Part two:" & Build_Circuits (Box_Store, Last_Pair)'Img);
   Put_CPU_Time;
end December_08;
