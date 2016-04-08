with Ada.Directories;
with Ada.IO_Exceptions;
with GNAT.Command_Line;
with Config;
with Client;

procedure Main is
   package CMD renames GNAT.Command_Line;

   procedure Setup_Project is
      procedure Create_Dir (Path : String) is
      begin
         if not Ada.Directories.Exists (Path) then
            Ada.Directories.Create_Directory (Path);
         end if;
      end Create_Dir;
   begin
      Create_Dir (Config.Data_Dir);
      Create_Dir (Config.Object_Dir);
      Create_Dir (Config.Temp_Dir);
   end Setup_Project;

   Default_Branch : Client.Branch;
   First_Load : Boolean := False;
   Note_Client : Client.Client_Status;
begin
   Setup_Project;

   begin
      Note_Client.Load_Branches;
   exception when Ada.IO_Exceptions.Name_Error => First_Load := True;
   end;
   
   if First_Load then
      Default_Branch.Name := Config.Default_Branch_Name;
      Note_Client.Set_Branch(Default_Branch);
      Note_Client.Set_Head(Config.Default_Branch_Name);
   end if;

   Note_Client.Save_Branches;
end Main;
