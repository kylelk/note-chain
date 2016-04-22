with Config;
with Ada.Directories;
with File_Operations;

package body File_Object_Store is
   procedure Setup (Self : in out Data) is
      pragma Unreferenced(Self);
   begin
      if not Ada.Directories.Exists (Config.Object_Dir) then
         Ada.Directories.Create_Directory (Config.Object_Dir);
      end if;
   end Setup;

   procedure Cleanup (self : in out Data) is
   begin
      null;
   end Cleanup;

   procedure Set (Self : in out Data; Key : SHA256_Value ; Value : String) is
   begin
      null;
   end Set;

   function Get (Self : in out Data; Key : SHA256_Value) return String is
   begin
      return "";
   end Get;

   function Contains (Self : Data; Key : SHA256_Value) return Boolean is
   begin
      return False;
   end Contains;

   procedure Remove (Self : in out Data; Key : SHA256_Value) is
   begin
      null;
   end Remove;

   procedure Commit (Self : in out Data) is
   begin
      null;
   end Commit;

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
end File_Object_Store;
