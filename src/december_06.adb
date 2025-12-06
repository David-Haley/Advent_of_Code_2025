with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Containers.Ordered_Maps;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_06 is

   subtype Ordinates is Positive;
   type Coordinates is record
      X, Y : Ordinates;
   end record; -- Coordinates

   function "<" (Left, Right : Coordinates) return Boolean is
     (Left.X < Right.X or else (Left.X = Right.X and then Left.Y < Right.Y));

   subtype Numbers is Long_Long_Integer range 0 .. Long_Long_Integer'Last;

   package Number_Stores is new
     Ada.Containers.Ordered_Maps (Coordinates, Numbers);
   use Number_Stores;

   subtype Operators is Character with
     Static_Predicate => Operators in '+' | '*';

   package Operator_Stores is new
     Ada.Containers.Ordered_Maps (Ordinates, Operators);
   use Operator_Stores;

   subtype Sum_Digits is Character with
     Static_Predicate => Sum_Digits in '0' .. '9' | ' ';

   package Digit_Stores is new
     Ada.Containers.Ordered_Maps (Coordinates, Sum_Digits);
   use Digit_Stores;

   procedure Read_Input (Number_Store : out Number_Stores.Map;
                         Operator_Store : out Operator_Stores.Map;
                         Digit_Store : out Digit_Stores.Map) is

      Input_File : File_Type;
      Text : Unbounded_String;
      X, Y : Ordinates;
      Start_At, First : Positive;
      Last : Natural;

   begin -- Read_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_06.txt");
      else
         Open (Input_File, In_File, Argument (1));
      end if; -- Argument_Count = 0
      Clear (Number_Store);
      Clear (Operator_Store);
      Y := 1;
      loop -- Read row of Numbers
         Get_Line (Input_File, Text);
         Start_At := 1;
         X := 1;
         while Start_At < Length (Text) loop
            Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First,
                        Last);
            exit when Last = 0;
            Insert (Number_Store, (X, Y),
                    Numbers'Value (Slice (Text, First, Last)));
            X := @ + 1;
            Start_At := Last + 1;
         end loop; -- Start_At < Length (Text)
         exit when Last = 0;
         Y := @ + 1;
      end loop; -- Read row of Numbers
      Start_At := 1;
      X := 1;
      while Start_At < Length (Text) loop
         if Element (Text, Start_At) in Operators then
            Insert (Operator_Store, X, Element (Text, Start_At));
            X := @ + 1;
         end if; -- Element (Text, Start_At) in Operators
         Start_At := @ + 1;
      end loop; -- Start_At < Length (Text)
      Reset (Input_File);
      for Yd in Ordinates range 1 .. Last_Key (Number_Store).Y loop
         Get_Line (Input_File, Text);
         for Xd in Ordinates range 1 .. Length (Text) loop
            Insert (Digit_Store, (Xd, Yd), Element (Text, Xd));
         end loop; -- Xd in Ordinates range 1 .. Length (Text)
      end loop; -- Yd in Ordinates range 1 .. Last_Key (Number_Store).Y
      Close (Input_File);
   end Read_Input;

   function Do_Column (Number_Store : Number_Stores.Map;
                       Operator_Store : Operator_Stores.Map;
                       X : Ordinates;
                       Y_Max : Ordinates)
                       return Numbers is

      --  Note the "Contains" test is only necessary in part two where not all
      --  the sums have the same number of operands.

      Result : Numbers;

   begin -- Do_Column
      case Operator_Store (X) is
         when '+' =>
            Result := 0;
            for Y in Ordinates range 1 .. Y_Max loop
               if Contains (Number_Store, (X, Y)) then
                  Result := @ + Number_Store ((X, Y));
               end if; -- Contains (Number_Store, (X, Y))
            end loop; -- Y in Ordinate range 1 .. Y_Max
         when '*' =>
            Result := 1;
            for Y in Ordinates range 1 .. Y_Max loop
               if Contains (Number_Store, (X, Y)) then
                  Result := @ * Number_Store ((X, Y));
               end if; -- Contains (Number_Store, (X, Y))
            end loop; -- Y in Ordinate range 1 .. Y_Max
      end case; -- Operator_Store (Xo)
      return Result;
   end Do_Column;

   procedure Build_Number_Store (Digit_Store : Digit_Stores.Map;
                                 Number_Store : out Number_Stores.Map;
                                 Y_Max : out Ordinates) is

      --  Reading Right to Left appears to be a bit of a "red herring", the key
      --  difference is that numbers are now formed by reading digits
      --  vertically. The Y coordinate now represents the position of the
      --  number in each column.

      N_String : String (1 .. Last_Key (Digit_Store).Y);
      X, Y : Ordinates;

   begin -- Build_Number_Store
      X := 1; -- First column
      Y := 1; -- First number in sum
      Y_Max := 1;
      Clear (Number_Store);
      for Xd in Ordinates range 1 .. Last_Key (Digit_Store).X loop
         for Yd in Ordinates range 1 .. Last_Key (Digit_Store).Y loop
            N_String (Yd) := Digit_Store ((Xd, Yd));
         end loop; -- Yd in Ordinates range 1 .. Last_Key (Digit_Store).Y
         if (for all I in N_String'Range => N_String (I) = ' ') then
            X := @ + 1;
            Y := 1;
         else
            Insert (Number_Store, (X, Y), Numbers'Value (N_String));
            if Y_Max < Y then
               Y_Max := Y;
            end if;
            Y := @ + 1;
         end if; -- (for all I in N_String'Range => N_String (I) = ' ')
      end loop; -- Xd in Ordinates range 1 .. Last_Key (Digit_Store).X
   end Build_Number_Store;

   Number_Store, Number_Store_2 : Number_Stores.Map := Number_Stores.Empty_Map;
   Operator_Store : Operator_Stores.Map := Operator_Stores.Empty_Map;
   Digit_Store : Digit_Stores.Map := Digit_Stores.Empty_Map;
   Y_Max : Ordinates;
   Total : Numbers := 0;

begin -- December_06
   Read_Input (Number_Store, Operator_Store, Digit_Store);
   for X in Ordinates range 1 .. Last_Key (Number_Store).X loop
      Total := @ + Do_Column (Number_Store, Operator_Store, X,
                              Last_Key (Number_Store).Y);
   end loop; -- X in Ordinates range 1 .. Last_Key (Number_Store).X
   Put_Line ("Part one:" & Total'Img);
   Put_CPU_Time;
   Total := 0;
   Build_Number_Store (Digit_Store, Number_Store_2, Y_Max);
   for X in Ordinates range 1 .. Last_Key (Number_Store_2).X loop
      Total := @ + Do_Column (Number_Store_2, Operator_Store, X, Y_Max);
   end loop; -- X in Ordinates range 1 .. Last_Key (Number_Store_2).X
   Put_Line ("Part two:" & Total'Img);
   Put_CPU_Time;
end December_06;
