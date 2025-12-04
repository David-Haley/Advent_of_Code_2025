with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Sets;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_04 is

   subtype Ordinates is Natural;
   type Coordinates is record
      X, Y : Ordinates;
   end record; -- Coordinates

   function "<" (Left, Right : Coordinates) return Boolean is
     (Left.X < Right.X or else (Left.X = Right.X and then Left.Y < Right.Y));

   --  Only recorde where the paper rolls are
   package Paper_Stores is new
     Ada.Containers.Ordered_Sets (Coordinates);
   use Paper_Stores;

   procedure Read_Input (Paper_Store : out Paper_Stores.Set) is

      --  The origin of the Paper_Store is (1, 1) to allow 1 ro be subtracted
      --  from both X and Y and remain within the range of Ordinates. This
      --  allows for a simple contains test without having to validate
      --  the Ordinate first.

      Input_File : File_Type;
      Text : Unbounded_String;
      Y : Ordinates := 1;

   begin -- Read_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_04.txt");
      else
         Open (Input_File, In_File, Argument (1));
      end if; -- Argument_Count = 0
      Clear (Paper_Store);
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         for X in Ordinates range 1 .. Length (Text) loop
            if Element (Text, X) = '@' then
               Include (Paper_Store, (X, Y));
            end if; -- Element (Text, X) = '@'
         end loop; -- X in Ordinates range 1 .. Length (Text)
         Y := @ + 1;
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_Input;

   function Count_Rolls (Paper_Store : Paper_Stores.Set;
                         Position : Paper_Stores.Cursor) return Natural is

      subtype Offsets is Integer range -1 .. 1;

      function Test_Coordinate (Coordinate : Coordinates;
                                Xo, Yo : Offsets) return Coordinates is
        (Ordinates (Integer (Coordinate.X) + Xo),
         Ordinates (Integer (Coordinate.Y) + Yo));

      Result : Natural := 0;

   begin --  Count_Rolls
      for Xo in Offsets loop
         for Yo in Offsets loop
            if  (Xo /= 0 or else Yo /= 0) and then
              Contains (Paper_Store,
                        Test_Coordinate (Element (Position), Xo, Yo))
            then
               Result := @ + 1;
            end if; --  (X0 /= 0 or else Yo /= 0) and then
         end loop; -- Yo in Offsets
      end loop; -- Xo In Offsets
      return Result;
   end Count_Rolls;

   function Count_Moved (Paper_Store : in out Paper_Stores.Set)
                         return Count_Type is

      Initial_Roll_Count : constant Count_Type := Length (Paper_Store);
      Moveable : Paper_Stores.Set := Paper_Stores.Empty_Set;

   begin -- Count_Moved
      loop -- remove a batch of rolls
         Clear (Moveable);
         for P in Iterate (Paper_Store) loop
            if Count_Rolls (Paper_Store, P) < 4 then
               Include (Moveable, Element (P));
            end if; -- Count_Rolls (Paper_Store, P) < 4
         end loop; -- P in Iterate (Paper_Store)
         exit when Is_Empty (Moveable);
         for M in Iterate (Moveable) loop
            Exclude (Paper_Store, Element (M));
         end loop; -- M in Iterate (Moveable)
      end loop; -- remove a batch of rolls
      return Initial_Roll_Count - Length (Paper_Store);
   end Count_Moved;

   Paper_Store : Paper_Stores.Set := Paper_Stores.Empty_Set;
   Roll_Count : Natural := 0;

begin -- December_04
   Read_Input (Paper_Store);
   for P in Iterate (Paper_Store) loop
      if Count_Rolls (Paper_Store, P) < 4 then
         Roll_Count := @ + 1;
      end if; -- Count_Rolls (Paper_Store, P) < 4
   end loop; -- P in Iterate (Paper_Store)
   Put_Line ("Part one:" & Roll_Count'Img);
   Put_CPU_Time;
   Put_Line ("Part two:" & Count_Moved (Paper_Store)'Img);
   Put_CPU_Time;
end December_04;
