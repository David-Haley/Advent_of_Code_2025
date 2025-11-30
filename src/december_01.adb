with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_01 is

   procedure Read_Input  is

      Input_File : File_Type;
      Text : Unbounded_String;

   begin -- Read_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_01.txt");
      else
         Open (Input_File, In_File, Argument (1));
      end if; -- Argument_Count = 0
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_Input;

begin -- December_01
   Read_Input;
   Put_Line ("Part one:");
   Put_CPU_Time;
   Put_Line ("Part two:");
   Put_CPU_Time;
end December_01;
