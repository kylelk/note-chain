with GNAT.Directory_Operations;
with Ada.Strings.Unbounded;

package Config is
   package UBS renames Ada.Strings.Unbounded;

   function Join(Path : String) return String is (GNAT.Directory_Operations.Format_Pathname(Path));

   -- directory used to store project data and folders
   Data_Dir : constant String := ".note_chain_data/";

   -- place to store all of the file objects used for storing data
   Object_Dir : constant String := Join(Data_Dir & "objects");

   -- place to store temporary files and folders while the app is running
   Temp_Dir : constant String := Join(Data_Dir & "temp/");

   -- JSON file to store the current branches of the project
   Branch_JSON_File : constant String := Join(Data_Dir & "branches.json");

   -- when the app starts for the first time a default branch is created
   Default_Branch_Name : constant UBS.Unbounded_String := UBS.To_Unbounded_String("master");
end Config;
