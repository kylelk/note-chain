with GNAT.Directory_Operations;
with Ada.Directories;
with File_Operations;
with Config;
with Ada.Strings.Fixed;

package body Object_Store is
   package DIR_OPS renames GNAT.Directory_Operations;

   procedure Write
     (Object_Type :     String;
      Content     :     String;
      Hash        : out SHA256_Value)
   is
      Data_File  : Ada.Text_IO.File_Type;
      Length_Str : constant String :=
        Ada.Strings.Fixed.Trim (Content'Length'Img, Ada.Strings.Left);
   begin
      TIO.Create (Data_File, TIO.Out_File, Config.Temp_Object_File);
      TIO.Put (Data_File, Object_Type & ' ' & Length_Str & ASCII.LF & Content);
      TIO.Close (Data_File);
      Hash := File_Operations.Get_File_Sha256 (Config.Temp_Object_File);
      Ada.Directories.Rename (Config.Temp_Object_File, Object_Path (Hash));
   end Write;

   function Read (Hash : SHA256_Value) return String is
      Path          : constant String := Object_Path (Hash);
      File_Content  : constant String := Get_Content (Path);
      Newline_Index : Integer;
   begin
      Newline_Index := Char_Index (File_Content, ASCII.LF) + 1;
      return File_Content (Newline_Index .. File_Content'Last);
   end Read;

   function Read_Data(Hash : SHA256_Value) return String is
      Path          : constant String := Object_Path (Hash);
   begin
      return Get_Content (Path);
   end Read_Data;

   function Object_Type (Hash : SHA256_Value) return String is
      Path          : constant String := Object_Path (Hash);
      File_Content  : constant String := Get_Content (Path);
      Newline_Index : Integer;
      Last_Space    : Integer;
   begin
      Newline_Index := Char_Index (File_Content, ASCII.LF);
      Last_Space    := Last_Index (File_Content (1 .. Newline_Index), ' ');
      return File_Content (1 .. Last_Space);
   end Object_Type;

   function Exists (Hash : SHA256_Value) return Boolean is
   begin
      return Ada.Directories.Exists (Object_Path (Hash));
   end Exists;

   function Object_Path (Hash : SHA256_Value) return String is
      Dir : constant String :=
        DIR_OPS.Format_Pathname (Config.Object_Dir & '/' & Hash (1 .. 2));
   begin
      Check_SHA256(Hash);
      if not Ada.Directories.Exists (Dir) then
         Ada.Directories.Create_Directory (Dir);
      end if;
      return DIR_OPS.Format_Pathname (Dir & '/' & Hash);
   end Object_Path;

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

   function Last_Index (Data : String; Char : Character) return Integer is
   begin
      for I in reverse Data'Range loop
         if Data (I) = Char then
            return I;
         end if;
      end loop;
      return -1;
   end Last_Index;

   procedure Check_SHA256(Hash : SHA256_Value) is
   begin
      for C of Hash loop
         if C not in '0'..'9' | 'a'..'f' | 'A'..'F' then
            raise Invalid_Hash_Format;
         end if;
      end loop;
   end Check_SHA256;
end Object_Store;
