with Ada.Directories;
with Ada.IO_Exceptions;
with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Strings.Unbounded;

with Config;
with Client;

procedure Main is
   package CLI renames Ada.Command_Line;
   package TIO renames Ada.Text_IO;
   package UBS renames Ada.Strings.Unbounded;

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

   procedure Cmd_Branch (Info : in out Client.Client_Status) is
   begin
      if CLI.Argument_Count > 1 then
         if CLI.Argument_Count > 2 then
            if CLI.Argument (2) = "new" then
               Info.Copy_Branch
               (Info.Branch_Status
                .Head, UBS.To_Unbounded_String(CLI.Argument (3)));
               TIO.Put_Line("created branch: " & CLI.Argument(3));
            elsif CLI.Argument(2) = "checkout" then
               TIO.Put_Line("TODO: checkout branch");
            elsif CLI.Argument(2) = "merge"  then
               TIO.Put_Line("TODO: merge branch");
            elsif CLI.Argument(2) = "remove" then
              TIO.Put_Line("TODO: remove");
            end if;
         end if;
         if CLI.Argument(2) = "head" then
            TIO.Put_Line("head: " & UBS.To_String(Info.Branch_Status.Head));
         end if;
      else
         TIO.Put_Line ("TODO: branch list");
      end if;
   end Cmd_Branch;

   procedure Cmd_Note (Info : in out Client.Client_Status) is
   begin
      null;
   end Cmd_Note;

   Default_Branch : Client.Branch;
   First_Load     : Boolean := False;
   Note_Client    : Client.Client_Status;
begin
   Setup_Project;

   begin
      Note_Client.Load_Branches;
   exception
      when Ada.IO_Exceptions.Name_Error =>
         First_Load := True;
   end;

   if First_Load then
      Default_Branch.Name := Config.Default_Branch_Name;
      Note_Client.Set_Branch (Default_Branch);
      Note_Client.Set_Head (Config.Default_Branch_Name);
   end if;

   if CLI.Argument_Count >= 1 then
      if CLI.Argument (1) = "branch" then
         Cmd_Branch (Note_Client);
      elsif CLI.Argument (1) = "help" then
         TIO.Put_Line("TODO: help");
      elsif CLI.Argument (1) = "note" then
         Cmd_Note (Note_Client);
      end if;
   else
      TIO.Put_Line ("usage infomation");
   end if;

   Note_Client.Save_Branches;
end Main;
