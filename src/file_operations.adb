with Ada.Directories;
with Ada.Direct_IO;
with GNAT.SHA256;
with Ada.Text_IO;

package body File_Operations is
   function Load_File (Filename : in String) return String is
      use Ada.Directories;

      File_Size : constant Natural := Natural (Size (Filename));

      subtype Test_JSON_Str is String (1 .. File_Size);
      package File_IO is new Ada.Direct_IO (Test_JSON_Str);

      File           : File_IO.File_Type;
      String_Content : Test_JSON_Str;
   begin
      File_IO.Open (File => File, Mode => File_IO.In_File, Name => Filename);
      File_IO.Read (File => File, Item => String_Content);
      File_IO.Close (File => File);

      return String_Content;
   end Load_File;

   procedure Remake_Directory (Path : String) is
      use Ada.Directories;
   begin
      if Exists (Path) then
         Delete_Tree (Path);
         Create_Directory (Path);
      end if;
   end Remake_Directory;

   function Get_File_Sha256 (File_Name : String) return SHA256_Value is
      File_Size : constant Natural :=
        Natural (Ada.Directories.Size (File_Name));
      C         : GNAT.SHA256.Context;
      Seek_Size : constant Positive := 4096;
      subtype File_String is String (1 .. Seek_Size);
      subtype End_String is String (1 .. 1);
      package File_String_IO is new Ada.Direct_IO (File_String);
      package End_File_IO is new Ada.Direct_IO (End_String);

      File        : File_String_IO.File_Type;
      Contents    : File_String;
      End_Content : End_String;
      End_File    : End_File_IO.File_Type;
      Last_Index  : Integer := 1;
   begin

      File_String_IO.Open
        (File,
         Mode => File_String_IO.In_File,
         Name => File_Name);

      while not File_String_IO.End_Of_File (File) loop
         Last_Index := Last_Index + Seek_Size;
         File_String_IO.Read (File => File, Item => Contents);
         GNAT.SHA256.Update (C, Contents);
      end loop;
      File_String_IO.Close (File);

      if (File_Size mod Seek_Size) > 0 then
         End_File_IO.Open (End_File, End_File_IO.In_File, File_Name);
         End_File_IO.Set_Index (End_File, End_File_IO.Count (Last_Index));
         while not End_File_IO.End_Of_File (End_File) loop
            End_File_IO.Read (End_File, End_Content);
            GNAT.SHA256.Update (C, End_Content);
         end loop;
         End_File_IO.Close (End_File);
      end if;

      return GNAT.SHA256.Digest (C);
   end Get_File_Sha256;

   function String_Hash (Data : String) return SHA256_Value is
      C : GNAT.SHA256.Context;
   begin
      GNAT.SHA256.Update (C, Data);
      return GNAT.SHA256.Digest (C);
   end String_Hash;

   procedure create_empty_file(path : String) is
      File_Item : Ada.Text_IO.File_Type;
      begin
      if not Ada.Directories.Exists(path) then
         Ada.Text_IO.Create(File_Item, Ada.Text_IO.Out_File, path);
         Ada.Text_IO.Close(File_Item);
      end if;
   end create_empty_file;
end File_Operations;
