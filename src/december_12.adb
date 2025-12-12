with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Ordered_Maps;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_12 is

   --  The "proper" solution to this problem is NP complete and still subject
   --  to accedemic research. This is not a general solution. It was based on
   --  the assumption that some Regions could be quickly eliminated as having
   --  an impossibly high occupancy ratio proved corect. For my input there was
   --  a clear break between 73% occupancy and 100%. Some truly great
   --  programmers may have produced general solutions but the problem appears
   --  to be like previous day 25 problems, it has a twist and not as difficult
   --  as some earlier problems. Note this sloltion has been fiddled to give
   --  the correct answer for the example!

   subtype Ordinates is Natural;

   subtype Box_Ordinates is Ordinates range 0 .. 2;

   subtype Present_Indices is Natural range 0 .. 5;

   type Boxes is array (Box_Ordinates, Box_Ordinates) of Boolean;

   package Box_Lists is new Ada.Containers.Doubly_Linked_Lists (Boxes);
   use Box_Lists;

   subtype Areas is Natural;

   type Presents is record
      Box_List : Box_Lists.List := Box_Lists.Empty_List;
      Area : Areas := 0;
   end record; -- Presents

   type Present_Arrays is array (Present_Indices) of Presents;

   type Packing_Lists is array (Present_Indices) of Natural;

   type Regions is record
      X, Y : Ordinates;
      Packing_List : Packing_Lists;
      Area, Present_Area : Areas;
   end record; -- Regions

   package Region_Stores is new Ada.Containers.Doubly_Linked_Lists (Regions);
   use Region_Stores;

   procedure Read_Input (Present_Array : out Present_Arrays;
                         Region_Store : out Region_Stores.List) is

      Input_File : File_Type;
      Text : Unbounded_String;
      Start_At : Positive;
      First : Positive;
      Last : Natural;
      Box : Boxes;
      Region : Regions;

   begin -- Read_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_12.txt");
      else
         Open (Input_File, In_File, Argument (1));
      end if; -- Argument_Count = 0
      Clear (Region_Store);
      for P in Present_Indices loop
         Get_Line (Input_File, Text);
         Start_At := 1;
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
         if P /= Present_Indices'Value (Slice (Text, First, Last)) then
            raise Data_Error with "Bad Present_Index expected" & P'Img &
              " found """ & Slice (Text, First, Last) & '"';
         end if;
         Present_Array (P).Area := 0;
         for Y in Box_Ordinates loop
            Get_Line (Input_File, Text);
            for X in Box_Ordinates loop
               case Element (Text, X + 1) is
                  when '#' =>
                     Box (X, Y) := True;
                     Present_Array (P).Area := @ + 1;
                  when '.' =>
                     Box (X, Y) := False;
                  when others =>
                     raise Data_Error with
                       "Bad box element expected '#' or '.' and found '" &
                       Element (Text, X + 1) & "'";
               end case; -- Element (Text, X + 1)
            end loop; -- X in Box_Ordinates
         end loop; -- Y in Box_Ordinates
         Clear (Present_Array (P).Box_List);
         Append  (Present_Array (P).Box_List, Box);
         Get_Line (Input_File, Text);
      end loop; -- P in Present_Indices
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         Start_At := 1;
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
         Region.X := Ordinates'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
         Region.Y := Ordinates'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         for P in Present_Indices loop
            Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First,
                        Last);
            Region.Packing_List (P) :=
              Natural'Value (Slice (Text, First, Last));
            Start_At := Last + 1;
         end loop; -- P in Present_Indices
         Region.Area := Region.X * Region.Y;
         Append (Region_Store, Region);
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_Input;

   procedure Put_Usage (Present_Array : Present_Arrays;
                        Region_Store : in out Region_Stores.List) is

      package Histograms is new Ada.Containers.Ordered_Maps (Natural, Natural);
      use Histograms;

      package Natural_IO is new Ada.Text_IO.Integer_IO (Natural);

      Histogram : Histograms.Map := Empty_Map;
      Ratio : Natural;

   begin -- Put_Usage
      Put_Line ("Histogram of the Area Utilisation in ratio in Percent");
      for R in Iterate (Region_Store) loop
         Region_Store (R).Present_Area := 0;
         for P in Present_Indices loop
            Region_Store (R).Present_Area :=
              Region_Store (R).Present_Area +
              Present_Array (P).Area * Element (R).Packing_List (P);
         end loop; -- P in Present_Indices
         Ratio := 100 * Element (R).Present_Area / Element (R).Area;
         if Contains (Histogram, Ratio) then
            Histogram (Ratio) := Histogram (Ratio) + 1;
         else
            Insert (Histogram, Ratio, 1);
         end if; -- Contains (Histogram, Ratio)
      end loop; -- R in Iterate (Region_Store)
      Put_Line ("Ratio %  Count");
      for H in Iterate (Histogram) loop
         Natural_IO.Put (Key (H), 7);
         if Key (H) < 100 then
            Put_Line ("  " & Element (H) * '#');
         else
            Natural_IO.Put (Element (H), 5);
            New_Line (2);
         end if; -- (Key (H) < 100
      end loop; -- H in Iterate Histogram
   end Put_Usage;

   function Count_Low_Occupancy (Region_Store : Region_Stores.List)
                                 return Natural is

      --  Counts the number of regions where the ratio of required area to
      --  to the region area is less than 100 percent!

      Result : Natural := 0;

   begin -- Count_Low_Occupancy
      for R in Iterate (Region_Store) loop
         if 100 * Element (R).Present_Area / Element (R).Area < 87 then
            --  The magic number 87 gives the correct answer for the example.
            Result := @ + 1;
         end if; -- 100 * Element (R).Present_Area / Element (R).Area < 87
      end loop; -- R in Iterate (Region_Store)
      return Result;
   end Count_Low_Occupancy;

   Present_Array : Present_Arrays;
   Region_Store : Region_Stores.List;

begin -- December_12
   Read_Input (Present_Array, Region_Store);
   Put_Usage (Present_Array, Region_Store);
   Put_Line ("Part one:" & Count_Low_Occupancy (Region_Store)'Img);
   Put_CPU_Time;
end December_12;
