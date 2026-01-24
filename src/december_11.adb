with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Doubly_Linked_Lists;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_11 is

   subtype Devices is String (1 .. 3);

   package Device_Lists is new Ada.Containers.Doubly_Linked_Lists (Devices);
   use Device_Lists;

   package Device_Maps is new
     Ada.Containers.Ordered_Maps (Devices, Device_Lists.List);
   use Device_Maps;

   subtype Big_Natural is Long_Long_Integer range 0 .. Long_Long_Integer'Last;

   procedure Read_Input (Device_Map : out Device_Maps.Map) is

      Input_File : File_Type;
      Text : Unbounded_String;
      Delimiters : constant Character_Set := To_Set (": ");

   begin -- Read_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_11.txt");
      else
         Open (Input_File, In_File, Argument (1));
      end if; -- Argument_Count = 0
      Clear (Device_Map);
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         declare -- Device declaration block

            Start_At : Positive := 1;
            First : Positive;
            Last : Natural;
            Device : Devices;
            Device_List : Device_Lists.List := Device_Lists.Empty_List;

         begin -- Device declaration block
            Find_Token (Text, Delimiters, Start_At, Outside, First, Last);
            Device := Slice (Text, First, Last);
            Start_At := Last + 1;
            while Start_At < Length (Text) loop
               Find_Token (Text, Delimiters, Start_At, Outside, First, Last);
               Append (Device_List, Slice (Text, First, Last));
               Start_At := Last + 1;
            end loop; -- Start_At < Length (Text)
            Insert (Device_Map, Device, Device_List);
         end; -- Device declaration block
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_Input;

   function Count_Paths (Device_Map : Device_Maps.Map)
                         return Big_Natural is

      Search_Start : constant Devices := "you";
      Search_End : constant Devices := "out";

      procedure Search (Device_Map : Device_Maps.Map;
                        Current : Devices;
                        Result : in out Big_Natural) is

      begin -- Search
         if Current = Search_End then
            Result := @ + 1;
         elsif Contains (Device_Map, Current) then
            for N in Iterate (Device_Map (Current)) loop
               Search (Device_Map, Element (N), Result);
            end loop; -- N in Iterate (Device_Map (Current))
         end if; -- Current = Search_End
      end Search;

      Result : Big_Natural := 0;

   begin -- Count_Paths
      Search (Device_Map, Search_Start, Result);
      return Result;
   end Count_Paths;

   function Count_Paths_2 (Device_Map : Device_Maps.Map) return Big_Natural is

      subtype Cache_Keys is String (1 .. 4);

      package Caches is new
        Ada.Containers.Ordered_Maps (Cache_Keys, Big_Natural);
      use Caches;

      function Make_Key (Device : Devices;
                         FFT_Found, DAC_Found : Boolean) return Cache_Keys is

         Result : Cache_Keys;

      begin -- Make_Key
         Result (1 .. 3) := Device;
         if FFT_Found then
            if DAC_Found then
               Result (4) := '3';
            else
               Result (4) := '2';
            end if; -- DAC_Found
         else
            if DAC_Found then
               Result (4) := '1';
            else
               Result (4) := '0';
            end if; -- DAC_Found
         end if; -- FFT_Found
         return Result;
      end Make_Key;

      Search_Start : constant Devices := "svr";
      FFT : constant Devices := "fft";
      DAC : constant Devices := "dac";
      Search_End : constant Devices := "out";

      function Search (Device_Map : Device_Maps.Map;
                       Current : Devices;
                       FFT_Found, DAC_Found : Boolean;
                       Cache : in out Caches.Map)
                       return Big_Natural is

         Key : constant Cache_Keys := Make_Key (Current, FFT_Found, DAC_Found);
         FFT_Found_Out, DAC_Found_Out : Boolean;
         Result : Big_Natural := 0;

      begin -- Search
         if Contains (Cache, Key) then
            Result := Cache (Key);
         elsif Current = Search_End then
            if FFT_Found and then DAC_Found then
               Result := 1;
            end if; -- FFT_Found and then DAC_Found
         elsif Contains (Device_Map, Current) then
            FFT_Found_Out := FFT_Found or else Current = FFT;
            DAC_Found_Out := DAC_Found or else Current = DAC;
            for N in Iterate (Device_Map (Current)) loop
               Result := @ + Search (Device_Map, Element (N), FFT_Found_Out,
                                     DAC_Found_Out, Cache);
            end loop; -- N in Iterate (Device_Map (Current))
         end if; -- Contains (Cache, Key)
         Include (Cache, Key, Result);
         return Result;
      end Search;

      Cache : Caches.Map := Caches.Empty_Map;

   begin -- Count_Paths_2
      return Search (Device_Map, Search_Start, False, False, Cache);
   end Count_Paths_2;

   Device_Map : Device_Maps.Map := Device_Maps.Empty_Map;

begin -- December_11
   Read_Input (Device_Map);
   Put_Line ("Part one:" & Count_Paths (Device_Map)'Img);
   Put_CPU_Time;
   Put_Line ("Part two:" & Count_Paths_2 (Device_Map)'Img);
   Put_CPU_Time;
end December_11;
