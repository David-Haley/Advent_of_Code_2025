with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_07_Claude is

   -- Dynamic 2D array types for grid and memoization table
   type Grid_Type is array (Positive range <>, Positive range <>) of Character;
   type Memo_Type is array (Positive range <>, Positive range <>) of Long_Long_Integer;

   -- Grid data and starting position
   type Grid_Data is record
      Grid : access Grid_Type;
      Start_Row : Positive;
      Start_Col : Positive;
      Num_Rows : Positive;
      Num_Cols : Positive;
   end record;

   procedure Read_Input (Data : out Grid_Data) is
      Input_File : File_Type;
      Lines : array (1 .. 1000) of Unbounded_String;
      Line_Count : Natural := 0;
      Max_Length : Natural := 0;
      S_Row : Positive;
      S_Col : Positive;
      Found_S : Boolean := False;
   begin
      -- Open input file
      if Argument_Count = 0 then
         Open (Input_File, In_File, "bin/december_07.txt");
      else
         Open (Input_File, In_File, Argument (1));
      end if;

      -- First pass: read all lines and determine dimensions
      while not End_Of_File (Input_File) loop
         Line_Count := Line_Count + 1;
         Lines (Line_Count) := Get_Line (Input_File);
         if Length (Lines (Line_Count)) > Max_Length then
            Max_Length := Length (Lines (Line_Count));
         end if;
      end loop;
      Close (Input_File);

      -- Allocate grid
      Data.Grid := new Grid_Type (1 .. Line_Count, 1 .. Max_Length);
      Data.Num_Rows := Line_Count;
      Data.Num_Cols := Max_Length;

      -- Second pass: populate grid and find S
      for R in 1 .. Line_Count loop
         declare
            Line : constant String := To_String (Lines (R));
         begin
            for C in 1 .. Max_Length loop
               if C <= Line'Length then
                  Data.Grid (R, C) := Line (C);
                  if Line (C) = 'S' then
                     S_Row := R;
                     S_Col := C;
                     Found_S := True;
                  end if;
               else
                  Data.Grid (R, C) := '.';  -- Pad with empty space
               end if;
            end loop;
         end;
      end loop;

      if not Found_S then
         raise Constraint_Error with "Starting position 'S' not found in input";
      end if;

      Data.Start_Row := S_Row;
      Data.Start_Col := S_Col;
   end Read_Input;

   function Count_Timelines
     (Row : Integer;
      Col : Integer;
      Grid : Grid_Type;
      Memo : in out Memo_Type) return Long_Long_Integer
   is
      Result : Long_Long_Integer;
   begin
      -- Base case: out of bounds means particle exited (1 timeline completed)
      if Row > Grid'Last (1) or Col < Grid'First (2) or Col > Grid'Last (2) then
         return 1;
      end if;

      -- Check memoization table (only for valid grid positions)
      if Memo (Row, Col) /= -1 then
         return Memo (Row, Col);
      end if;

      -- Compute based on cell type
      if Grid (Row, Col) = '^' then
         -- Splitter: timeline splits into left and right paths
         Result := Count_Timelines (Row + 1, Col - 1, Grid, Memo) +
                   Count_Timelines (Row + 1, Col + 1, Grid, Memo);
      else
         -- Empty space or S: continue downward
         Result := Count_Timelines (Row + 1, Col, Grid, Memo);
      end if;

      -- Store in memoization table
      Memo (Row, Col) := Result;
      return Result;
   end Count_Timelines;

   Data : Grid_Data;
   Memo : access Memo_Type;
   Part_Two_Result : Long_Long_Integer;

begin
   -- Read input and find starting position
   Read_Input (Data);

   -- Initialize memoization table to -1 (not computed)
   Memo := new Memo_Type (1 .. Data.Num_Rows, 1 .. Data.Num_Cols);
   for R in 1 .. Data.Num_Rows loop
      for C in 1 .. Data.Num_Cols loop
         Memo (R, C) := -1;
      end loop;
   end loop;

   -- Count timelines starting from S
   Part_Two_Result := Count_Timelines (Data.Start_Row, Data.Start_Col, Data.Grid.all, Memo.all);

   -- Output result
   Put_Line ("Part Two:" & Part_Two_Result'Img);
   Put_CPU_Time;

end December_07_Claude;
