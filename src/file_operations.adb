with Ada.Directories;
with Ada.Direct_IO;

package body File_Operations is
    function Load_File(Filename : in String) return String
   is
      use Ada.Directories;

      File_Size    : constant Natural := Natural (Size (Filename));

      subtype Test_JSON_Str is String (1 .. File_Size);
      package File_IO is new Ada.Direct_IO (Test_JSON_Str);

      File           : File_IO.File_Type;
      String_Content : Test_JSON_Str;
   begin
      File_IO.Open (File => File,
                    Mode => File_IO.In_File,
                    Name => Filename);
      File_IO.Read (File => File,
                    Item => String_Content);
      File_IO.Close (File => File);

      return String_Content;
   end Load_File;
end File_Operations;
