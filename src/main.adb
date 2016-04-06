with Ada.Text_IO;
with Ada.Directories;
with Config;

procedure Main is
   package TIO renames Ada.Text_IO;

   procedure Setup_Project is
      procedure Create_Dir(Path : String) is
      begin
         if not Ada.Directories.Exists (Path) then
            Ada.Directories.Create_Directory (Path);
         end if;
      end Create_Dir;
   begin
      Create_Dir(Config.Data_Dir);
      Create_Dir(Config.Object_Dir);
      Create_Dir(Config.Temp_Dir);
   end Setup_Project;

begin
   Setup_Project;
   TIO.Put_Line("hello world");
end Main;
