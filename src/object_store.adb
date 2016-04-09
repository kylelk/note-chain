with GNAT.Directory_Operations;
with Ada.Directories;
with File_Operations;
with Config;
with Ada.Integer_Text_IO;
with Ada.Strings.Fixed;
with Ada.Characters.Handling;

package body Object_Store is
   package DIR_OPS renames GNAT.Directory_Operations;

   procedure Write
     (Object_Type :     String;
      Content     :     String;
      Hash        : out SHA256_Value)
   is
      Data_File : Ada.Text_IO.File_Type;
   begin
      TIO.Open (Data_File, TIO.Out_File, Config.Temp_Object_File);
      TIO.Put (Object_Type & ' ' & Content'Length'Img & ASCII.LF & Content);
      TIO.Close (Data_File);
      Hash := File_Operations.Get_File_Sha256 (Config.Temp_Object_File);
   end Write;

   function Read (Hash : SHA256_Value) return String is
      function Get_Content (File_Path : String) return String is
      begin
         if Ada.Directories.Exists (File_Path) then
            return File_Operations.Load_File (File_Path);
         else
            raise Object_Not_Found;
         end if;
      end Get_Content;

      function Char_Index (Data : String; Char : Character) return Integer is
      begin
         for I in Data'Range loop
            if Data (I) = Char then
               return I;
            end if;
         end loop;
         return -1;
      end Char_Index;

      Path          : constant String := Object_Path (Hash);
      File_Content  : constant String := Get_Content (Path);
      Newline_Index : Integer;
   begin
      Newline_Index := Char_Index (File_Content, ASCII.LF);
      return File_Content (Newline_Index .. File_Content'Last);
   end Read;

   procedure Init is
      function Integer2Hexa
        (Hex_Int : Integer;
         Width   : Positive := 2) return String
      is

         Hex_Prefix_Length : constant := 3;
         Hexa              : String (1 .. Hex_Prefix_Length + Width + 1);
         Result            : String (1 .. Width);
         Start             : Natural;
      begin
         Ada.Integer_Text_IO.Put (Hexa, Hex_Int, 16);
         Start := Ada.Strings.Fixed.Index (Source => Hexa, Pattern => "#");
         Ada.Strings.Fixed.Move
           (Source  => Hexa (Start + 1 .. Hexa'Last - 1),
            Target  => Result,
            Justify => Ada.Strings.Right,
            Pad     => '0');
         Result := Ada.Characters.Handling.To_Lower (Result);
         return Result;
      end Integer2Hexa;

      procedure Create_New_Dir (Name : String) is
      begin
         if not Ada.Directories.Exists (Name) then
            Ada.Directories.Create_Directory (Name);
         end if;
      end Create_New_Dir;

      Prefix : String (1 .. 2);
   begin
      for I in 0 .. 255 loop
         Prefix := Integer2Hexa (I);
         Create_New_Dir
           (DIR_OPS.Format_Pathname (Config.Object_Dir & "/" & Prefix));
      end loop;
   end Init;

   function Object_Path (Hash : SHA256_Value) return String is
   begin
      return DIR_OPS.Format_Pathname
          (Config.Object_Dir & '/' & Hash (1 .. 2) & '/' & Hash);
   end Object_Path;
end Object_Store;
