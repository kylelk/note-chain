with GNAT.Directory_Operations;
package Config is
   function Join(Path : String) return String is (GNAT.Directory_Operations.Format_Pathname(Path));

   Data_Dir : constant String := ".note_chain_data/";

   Object_Dir : constant String := Join(Data_Dir & "objects");

   Temp_Dir : constant String := Join(Data_Dir & "temp");

   Branch_JSON_File : constant String := Join(Data_Dir & "branches.json");
end Config;