with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Doubly_Linked_Lists;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_09 is

   subtype Ordinates is Positive;
   type Coordinates is record
      X, Y : Ordinates;
   end record; -- Coordinates

   function "<" (Left, Right : Coordinates) return Boolean is
     (Left.X < Right.X or else (Left.X = Right.X and then Left.Y < Right.Y));

   package Red_Tile_Stores is new
     Ada.Containers.Ordered_Sets (Coordinates);
   use Red_Tile_Stores;

   type Corner_Indices is (Top_Left, Top_Right, Bottom_Right, Bottom_Left);

   type Corners is array (Corner_Indices) of Coordinates;

   subtype Areas is Long_Long_Integer range 1 .. Long_Long_Integer'Last;

   type Rectangles is record
      Corner : Corners;
      Area : Areas;
   end record;

   package Rectangle_Stores is new
     Ada.Containers.Doubly_Linked_Lists (Rectangles);
   use Rectangle_Stores;

   function "<" (Left, Right : Rectangles) return Boolean is
     (Left.Area < Right.Area);

   package Rectangle_Sorting is new Rectangle_Stores.Generic_Sorting;

   procedure Read_Input (Red_Tile_Store : out Red_Tile_Stores.Set) is

      Input_File : File_Type;
      Text : Unbounded_String;
      Start_At, First : Positive;
      Last : Natural;
      Coordinate : Coordinates;

   begin -- Read_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_09.txt");
      else
         Open (Input_File, In_File, Argument (1));
      end if; -- Argument_Count = 0
      Clear (Red_Tile_Store);
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         Start_At := 1;
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
         Coordinate.X := Ordinates'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
         Coordinate.Y := Ordinates'Value (Slice (Text, First, Last));
         Include (Red_Tile_Store, Coordinate);
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_Input;

   procedure Largest (Red_Tile_Store : Red_Tile_Stores.Set;
                      Rectangle_Store : out Rectangle_Stores.List) is

      --  Note the sorting order of tile coordinates ensures that for all T,
      --  T (N).X < T (N + K).X for all positive K. Rectangle corners are
      --  arranged such that Corner (3).Y >= Corner (1).Y.

      function Area (T1, T2 : Coordinates) return Areas is
        (Areas (T2.X - T1.X + 1) * Areas (T2.Y - T1.Y + 1));

      Rc1 : Red_Tile_Stores.Cursor := First (Red_Tile_Store);
      Rc2 : Red_Tile_Stores.Cursor;
      Rectangle : Rectangles;
      Y_Low, Y_High : Ordinates;

   begin -- Area
      Clear (Rectangle_Store);
      loop -- Rc1
         Rc2 := Next (Rc1);
         exit when Rc2 = Red_Tile_Stores.No_Element;
         loop -- Rc2
            if Element (Rc1).Y < Element (Rc2).Y then
               Y_Low := Element (Rc1).Y;
               Y_High := Element (Rc2).Y;
            else
               Y_Low := Element (Rc2).Y;
               Y_High := Element (Rc1).Y;
            end if; -- Element (Rc1).Y < Element (Rc2).Y
            Rectangle.Corner (Top_Left) := (Element (Rc1).X, Y_Low);
            Rectangle.Corner (Top_Right) := (Element (Rc2).X, Y_Low);
            Rectangle.Corner (Bottom_Right) := (Element (Rc2).X, Y_High);
            Rectangle.Corner (Bottom_Left) := (Element (Rc1).X, Y_High);
            Rectangle.Area := Area (Rectangle.Corner (Top_Left),
                                    Rectangle.Corner (Bottom_Right));
            Append (Rectangle_Store, Rectangle);
            Next (Rc2);
            exit when Rc2 = Red_Tile_Stores.No_Element;
         end loop; -- Rc2
         Next (Rc1);
      end loop; -- Rc1
      Rectangle_Sorting.Sort (Rectangle_Store);
   end Largest;

   function Empty_Rectangle (Red_Tile_Store : Red_Tile_Stores.Set;
                             Rectangle_Store : Rectangle_Stores.List)
                             return Areas is

      subtype Range_Indices is Natural range 0 .. 1;
      type Range_Elements is array (Range_Indices) of Ordinates;

      package Ranges is new
        Ada.Containers.Doubly_Linked_Lists (Range_Elements);
      use Ranges;

      package Breaks is new
        Ada.Containers.Ordered_Maps (Ordinates, Ranges.List);
      use Breaks;

      procedure Find_Breaks (Red_Tile_Store : Red_Tile_Stores.Set;
                             X_Break, Y_Break : out Breaks.Map) is

         Edges : Natural := 0;
         Inside : Range_Elements;

      begin -- Find_Breaks
         for T in Iterate (Red_Tile_Store) loop
            Include (X_Break, Element (T).X, Ranges.Empty_List);
            Include (Y_Break, Element (T).Y, Ranges.Empty_List);
         end loop; -- T in Iterate (Red_Tile_Store)
         for X in Iterate (X_Break) loop
            Edges := 0;
            for Y in Iterate (Y_Break) loop
               if Contains (Red_Tile_Store, (Key (X), Key (Y))) then
                  Inside (Edges mod 2) := Key (Y);
                  if Edges mod 2 = 1 then
                     Append (X_Break (Key (X)), Inside);
                  end if; -- Edges mod 2 = 1
                  Edges := @ + 1;
               end if; -- Contains (Red_Tile_Store, (Key (X), Key(Y)))
            end loop; -- Y in Iterate (Y_Break)
         end loop; -- X in Iterate (X_Break)
         for Y in Iterate (Y_Break) loop
            Edges := 0;
            for X in Iterate (X_Break) loop
               if Contains (Red_Tile_Store, (Key (X), Key (Y))) then
                  Inside (Edges mod 2) := Key (X);
                  if Edges mod 2 = 1 then
                     Append (Y_Break (Key (Y)), Inside);
                  end if; -- Edges mod 2 = 1
                  Edges := @ + 1;
               end if; -- Contains (Red_Tile_Store, (Key (X), Key(Y)))
            end loop; -- X in Iterate (X_Break)
         end loop; -- Y in Iterate (Y_Break)
      end Find_Breaks;

      Empty : constant Boolean := False;
      Rc : Rectangle_Stores.Cursor := Last (Rectangle_Store);
      X_Break, Y_Break : Breaks.Map := Breaks.Empty_Map;

   begin -- Empty_Rectangle
      Find_Breaks (Red_Tile_Store, X_Break, Y_Break);
      if Empty then
         return Rectangle_Store (Rc).Area;
      else
         return Areas'First;
      end if; -- Empty
   end Empty_Rectangle;

   Red_Tile_Store : Red_Tile_Stores.Set := Red_Tile_Stores.Empty_Set;
   Rectangle_Store : Rectangle_Stores.List := Rectangle_Stores.Empty_List;

begin -- December_09
   Read_Input (Red_Tile_Store);
   Largest (Red_Tile_Store, Rectangle_Store);
   Put_Line ("Part one:" & Last_Element (Rectangle_Store).Area'Img);
   Put_CPU_Time;
   Put_Line ("Part two:" &
               Empty_Rectangle (Red_Tile_Store, Rectangle_Store)'Img);
   Put_CPU_Time;
end December_09;
