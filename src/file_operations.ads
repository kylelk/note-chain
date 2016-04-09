package File_Operations is
   subtype SHA256_Value is String (1 .. 64);

   function Load_File (Filename : String) return String;

   procedure Remake_Directory (Path : String);

   function Get_File_Sha256 (File_Name : String) return SHA256_Value;

   function String_Hash (Data : String) return SHA256_Value;
end File_Operations;
