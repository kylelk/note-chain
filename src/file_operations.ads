package File_Operations is
   subtype SHA256_Value is String (1 .. 64);

   function Load_File (Filename : String) return String;

   procedure Remake_Directory (Path : String);

   -- @description
   -- computes the SHA-256 hash of a file by reading it in 4096 byte blocks
   function Get_File_Sha256 (File_Name : String) return SHA256_Value;

   function String_Hash (Data : String) return SHA256_Value;

   procedure create_empty_file(path : String);

   procedure Write_String(Path : String ; Content : String);

   procedure Execute_System_Cmd (Command : String);
end File_Operations;
