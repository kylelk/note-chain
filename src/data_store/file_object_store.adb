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
      pragma Unreferenced(Self);
   begin
      return File_Operations.Load_File(Object_Path(Key));
   end Get;

   function Exists (Self : Data; Key : SHA256_Value) return Boolean is
      pragma Unreferenced(Self);
   begin
      return Ada.Directories.Exists(Object_Path(Key));
   end Exists;

   procedure Remove (Self : in out Data; Key : SHA256_Value) is
      pragma Unreferenced(Self);
   begin

   end Remove;

   procedure Commit (Self : in out Data) is
   begin
      null;
   end Commit;

    function Object_Path (Hash : SHA256_Value) return String is
      Dir : constant String :=
        DIR_OPS.Format_Pathname (Config.Object_Dir & '/' & Hash (1 .. 2));
   begin
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
end File_Object_Store;
