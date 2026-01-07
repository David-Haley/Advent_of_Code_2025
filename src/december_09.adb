with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Doubly_Linked_Lists;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_09 is

   subtype Ordinates is Natural;
   type Coordinates is record
      X, Y : Ordinates;
   end record; -- Coordinates

   package Red_Tile_Stores is new
     Ada.Containers.Doubly_Linked_Lists (Coordinates);
   use Red_Tile_Stores;

   type Corner_Indices is (Top_Left, Top_Right, Bottom_Right, Bottom_Left);

   type Corners is array (Corner_Indices) of Coordinates;

   subtype Areas is Long_Long_Integer range 0 .. Long_Long_Integer'Last;

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

   type Lines is record
      L1, L2 : Ordinates;
   end record; -- Lines

   package Line_Lists is new Ada.Containers.Doubly_Linked_Lists (Lines);
   use Line_Lists;

   package Line_Stores is new
     Ada.Containers.Ordered_Maps (Ordinates, Line_Lists.List);
   use Line_Stores;

   procedure Read_Input (Red_Tile_Store : out Red_Tile_Stores.List) is

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
         Append (Red_Tile_Store, Coordinate);
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_Input;

   procedure Largest (Red_Tile_Store : Red_Tile_Stores.List;
                      Rectangle_Store : out Rectangle_Stores.List) is

      function Area (T1, T2 : Coordinates) return Areas is
        (Areas (T2.X - T1.X + 1) * Areas (T2.Y - T1.Y + 1));

      Rc1 : Red_Tile_Stores.Cursor := First (Red_Tile_Store);
      Rc2 : Red_Tile_Stores.Cursor;
      Rectangle : Rectangles;
      X_Low, X_High, Y_Low, Y_High : Ordinates;

   begin -- Area
      Clear (Rectangle_Store);
      loop -- Rc1
         Rc2 := Next (Rc1);
         exit when Rc2 = Red_Tile_Stores.No_Element;
         loop -- Rc2
            if Element (Rc1).X < Element (Rc2).X then
               X_Low := Element (Rc1).X;
               X_High := Element (Rc2).X;
            else
               X_Low := Element (Rc2).X;
               X_High := Element (Rc1).X;
            end if; -- Element (Rc1).X < Element (Rc2).X
            if Element (Rc1).Y < Element (Rc2).Y then
               Y_Low := Element (Rc1).Y;
               Y_High := Element (Rc2).Y;
            else
               Y_Low := Element (Rc2).Y;
               Y_High := Element (Rc1).Y;
            end if; -- Element (Rc1).Y < Element (Rc2).Y
            Rectangle.Corner (Top_Left) := (X_Low, Y_Low);
            Rectangle.Corner (Top_Right) := (X_High, Y_Low);
            Rectangle.Corner (Bottom_Right) := (X_High, Y_High);
            Rectangle.Corner (Bottom_Left) := (X_Low, Y_High);
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

   procedure Find_Lines (Red_Tile_Store : Red_Tile_Stores.List;
                         Horizontal, Vertical : out Line_Stores.Map) is

      Previous : Coordinates := Last_Element (Red_Tile_Store);

   begin -- Find_Lines
      Clear (Horizontal);
      Clear (Vertical);
      for R in Iterate (Red_Tile_Store) loop
         if Previous.X = Element (R).X then
            --  Vertical line
            if not Contains (Vertical, Previous.X) then
               Insert (Vertical, Previous.X, Line_Lists.Empty_List);
            end if; -- not Contains (Vertical, Previous.X)
            if Previous.Y < Element (R).Y then
               Append (Vertical (Previous.X), (Previous.Y, Element (R).Y));
            else
               Append (Vertical (Previous.X), (Element (R).Y, Previous.Y));
            end if; -- Previous.Y < Element (R).Y
         elsif Previous.Y = Element (R).Y then
            --  Horizontal Line
            if not Contains (Horizontal, Previous.Y) then
               Insert (Horizontal, Previous.Y, Line_Lists.Empty_List);
            end if; -- not Contains (Horizontal, Previous.Y)
            if Previous.X < Element (R).X then
               Append (Horizontal (Previous.Y), (Previous.X, Element (R).X));
            else
               Append (Horizontal (Previous.Y), (Element (R).X, Previous.X));
            end if; -- Previous.X < Element (R).X
         else
            raise Data_Error with "Not orthoginal line " & Previous'Img &
              " to " & Element (R)'Img;
         end if; -- Previous.X = Element (R).X
         Previous := Element (R);
      end loop; -- in Iterate (Red_Tile_Store)
   end Find_Lines;

   function Largest (Rectangle_Store : Rectangle_Stores.List;
                     Horizontal, Vertical : Line_Stores.Map) return Areas is

      --  Check that no retangle side crosses the outside perometer line.

      function Is_Inside (Line_Store : Line_Stores.Map;
                          Ordinate : Ordinates;
                          L1, L2 : Ordinates) return Boolean is

         Lc : Line_Stores.Cursor := Floor (Line_Store, L1);
         Result : Boolean := True;

      begin -- Is_Inside
         while Lc /= Line_Stores.No_Element and then Key (Lc) <= L2 and then
           Result loop
            for L in Iterate (Line_Store (Lc)) loop
               Result := @ and then
                 (((L1 = Key (Lc) or else L2 = Key (Lc)) and then
                   Ordinate in Element (L).L1 .. Element (L).L2) or else
               --  An end of the line represented by Ordinate, L1 and L2
               --  is included in the line represented by L.
                 not (Key (Lc) in L1 + 1 .. L2 - 1 and then
                      Ordinate in Element (L).L1 .. Element (L).L2));
               --  Does not have an intersection other than at the end
            end loop; -- L in Iterate (Line_Store (Lc))
            Next (Lc);
         end loop; -- Lc /= Line_Stores.No_Element and then Key (Lc) <= L2 ...
         return Result;
      end Is_Inside;

      R : Rectangle_Stores.Cursor := Last (Rectangle_Store);
      All_Sides : Boolean;

   begin -- Largest
      loop -- Check one rectangle
         All_Sides :=
           Is_Inside (Vertical,
                      Element (R).Corner (Top_Left).Y,
                      Element (R).Corner (Top_Left).X,
                      Element (R).Corner (Top_Right).X) and then -- Top
           Is_Inside (Vertical,
                      Element (R).Corner (Bottom_Left).Y,
                      Element (R).Corner (Bottom_Left).X,
                      Element (R).Corner (Bottom_Right).X) and then -- Bottom
           Is_Inside (Horizontal,
                      Element (R).Corner (Top_Left).X,
                      Element (R).Corner (Top_Left).Y,
                      Element (R).Corner (Bottom_Left).Y) and then -- Left
           Is_Inside (Horizontal,
                      Element (R).Corner (Top_Right).X,
                      Element (R).Corner (Top_Right).Y,
                      Element (R).Corner (Bottom_Right).Y); -- Right
         exit when All_Sides or else
           Previous (R) = Rectangle_Stores.No_Element;
         Previous (R);
      end loop; -- Check one rectangle
      if All_Sides then
         return Element (R).Area;
      else
         return 0;
      end if; -- All_Corners
   end Largest;

   Red_Tile_Store : Red_Tile_Stores.List := Red_Tile_Stores.Empty_List;
   Rectangle_Store : Rectangle_Stores.List := Rectangle_Stores.Empty_List;
   Horizontal, Vertical : Line_Stores.Map := Line_Stores.Empty_Map;

begin -- December_09
   Read_Input (Red_Tile_Store);
   Largest (Red_Tile_Store, Rectangle_Store);
   Put_Line ("Part one:" & Last_Element (Rectangle_Store).Area'Img);
   Put_CPU_Time;
   Find_Lines (Red_Tile_Store, Horizontal, Vertical);
   Put_Line ("Part two:" &
               Largest (Rectangle_Store, Horizontal, Vertical)'Img);
   Put_CPU_Time;
end December_09;
