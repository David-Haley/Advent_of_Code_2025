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

   function Enclosed_Rectangle (Red_Tile_Store : Red_Tile_Stores.Set;
                             Rectangle_Store : Rectangle_Stores.List)
                             return Areas is

      subtype Range_Indices is Natural range 0 .. 1;
      type Range_Elements is array (Range_Indices) of Ordinates;

      package Ranges is new
        Ada.Containers.Doubly_Linked_Lists (Range_Elements);
      use Ranges;

      package Line_Lists is new
        Ada.Containers.Ordered_Maps (Ordinates, Range_Element);
      use Line_Lists;

      procedure Find_Breaks (Red_Tile_Store : Red_Tile_Stores.Set;
                             X_Break, Y_Break : out Breaks.Map) is

         --  Find intersections with horizontal and vertical lines.

         function Is_In_Line (Ordinate : Ordinates;
                              Limit : Range_Elements) return Boolean is
           (Ordinate in Limit (0) .. Limit (1));

         package Lines is new
           Ada.Containers.Ordered_Maps (Ordinates, Line_Lists.List);
         use Lines;

         Ends : Natural := 0;
         Line, Limit : Range_Elements;
         V_Line, H_Line : Lines.Map := Lines.Empty_Map;

      begin -- Find_Breaks
         for T in Iterate (Red_Tile_Store) loop
            Include (V_Line, Element (T).X, Line_Lists.Empty_List);
            Include (H_Line, Element (T).Y, Line_Lists.Empty_List);
            Include (X_Break, Element (T).X,
                     (Ordinates'Last, Ordinates'First));
            Include (Y_Break, Element (T).Y,
                     (Ordinates'Last, Ordinates'First));
         end loop; -- T in Iterate (Red_Tile_Store)
         for X in Iterate (V_Line) loop
            Ends := 0;
            for Y in Iterate (H_Line) loop
               if Contains (Red_Tile_Store, (Key (X), Key (Y))) then
                  Line (Ends mod 2) := Key (Y);
                  if Ends mod 2 = 1 then
                     Append (V_Line (Key (X)), Line);
                  end if; -- Ends mod 2 = 1
                  Ends := @ + 1;
               end if; -- Contains (Red_Tile_Store, (Key (X), Key(Y)))
            end loop; -- Y in Iterate (H_Line)
         end loop; -- X in Iterate (V_Line)
         for Y in Iterate (H_Line) loop
            Ends := 0;
            for X in Iterate (V_Line) loop
               if Contains (Red_Tile_Store, (Key (X), Key (Y))) then
                  Line (Ends mod 2) := Key (X);
                  if Ends mod 2 = 1 then
                     Append (H_Line (Key (Y)), Line);
                  end if; -- Ends mod 2 = 1
                  Ends := @ + 1;
               end if; -- Contains (Red_Tile_Store, (Key (X), Key(Y)))
            end loop; -- X in Iterate (V_Line)
         end loop; -- Y in Iterate (H_Line)
         for X in Iterate (X_Break) loop
             --  Find Top limit
            for Y in Iterate (H_Lines) loop
               Limit (0) := Ordinates'Last;
               for L in Iterate (Element (Y)) loop
                  if Limit (0) = Ordinates'Last and then
                    Is_In_Line (Key (X), Element (L)) then
                     Limit (0) := Key (Y);
                  end if; -- Limit (0) = Ordinates'Last and then ...
               end loop; -- L in Iterate (Element (Y))
            end loop; -- Y in Iterate (H_Lines)
            ****
            --  Find bottom limit
         end loop; -- X in Iterate (X_Break)
         --  Find left limit
         --  Find right limit
      end Find_Breaks;

      function Is_Inside (Rectangle : Rectangles;
                          X_Break, Y_Break : Breaks.Map) return Boolean is

         Result : Boolean := True;
         Line : Boolean;

      begin -- Is_Inside
         Put_Line ("Rectangle => " & Rectangle'Img);
         Line := False;
         for R in Iterate (X_Break (Rectangle.Corner (Top_Left).X)) loop
            --  Check left
            Line := @ or else
              (Element (R) (0) <= Rectangle.Corner (Top_Left).Y
               and then
               Rectangle.Corner (Bottom_Left).Y <= Element (R) (1));
            Put_Line ("Left" & Element (R)'Img & Line'Img);
         end loop; -- R in Iterate (X_Break (Rectangle.Corner (Top_Left).X))
         Result := @ and then Line;
         Line := False;
         for R in Iterate (X_Break (Rectangle.Corner (Top_Right).X)) loop
            --  Check right
            Line := @ or else
              (Element (R) (0) <= Rectangle.Corner (Top_Right).Y
               and then
               Rectangle.Corner (Bottom_Right).Y <= Element (R) (1));
            Put_Line ("Right" & Element (R)'Img & Line'Img);
         end loop; -- R in Iterate (X_Break (Rectangle.Corner (Top_Right).X))
         Result := @ and then Line;
         Line := False;
         for R in Iterate (Y_Break (Rectangle.Corner (Top_Left).Y)) loop
            --  Check top
            Line := @ or else
              (Element (R) (0) <= Rectangle.Corner (Top_Left).X
               and then
               Rectangle.Corner (Top_Right).X <= Element (R) (1));
            Put_Line ("Top" & Element (R)'Img & Line'Img);
         end loop; -- R in Iterate (Y_Break (Rectangle.Corner (Top_Left).Y))
         Result := @ and then Line;
         Line := False;
         for R in Iterate (Y_Break (Rectangle.Corner (Bottom_Left).Y)) loop
            --  Check bottom
            Line := @ or else
              (Element (R) (0) <= Rectangle.Corner (Bottom_Left).X
               and then
               Rectangle.Corner (Bottom_Right).X <= Element (R) (1));
            Put_Line ("Bottom" & Element (R)'Img & Line'Img);
         end loop; -- R in Iterate (Y_Break (Rectangle.Corner (Bottom_Left).Y))
         Result := @ and then Line;
         return Result;
      end Is_Inside;

      Rc : Rectangle_Stores.Cursor := Last (Rectangle_Store);
      X_Break, Y_Break : Breaks.Map := Breaks.Empty_Map;

   begin -- Enclosed_Rectangle
      Find_Breaks (Red_Tile_Store, X_Break, Y_Break);
      Put_Line ("X_Break => " & X_Break'Img);
      Put_Line ("Y_Break => " & Y_Break'Img);
      while Rc /= Rectangle_Stores.No_Element and then
       not Is_Inside (Element (Rc), X_Break, Y_Break) loop
         Previous (Rc);
      end loop; -- Rc /= Rectangle_Stores.No_Element and then
      if Rc /= Rectangle_Stores.No_Element then
         return Rectangle_Store (Rc).Area;
      else
         return Areas'First;
      end if; -- Rc /= Rectangle_Stores.No_Element
   end Enclosed_Rectangle;

   Red_Tile_Store : Red_Tile_Stores.Set := Red_Tile_Stores.Empty_Set;
   Rectangle_Store : Rectangle_Stores.List := Rectangle_Stores.Empty_List;

begin -- December_09
   Read_Input (Red_Tile_Store);
   Largest (Red_Tile_Store, Rectangle_Store);
   Put_Line ("Part one:" & Last_Element (Rectangle_Store).Area'Img);
   Put_CPU_Time;
   Put_Line ("Part two:" &
               Enclosed_Rectangle (Red_Tile_Store, Rectangle_Store)'Img);
   Put_CPU_Time;
end December_09;
