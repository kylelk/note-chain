with Ada.Directories;
with Ada.IO_Exceptions;
with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with GNAT.OS_Lib;

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

   procedure Execute_System (Command : String) is
      use GNAT.OS_Lib;

      List      : String_List_Access := Argument_String_To_List (Command);
      Exec_Path : String_Access      := Locate_Exec_On_Path (List (1).all);
      Success   : Boolean;

   begin
      if Exec_Path /= null then
         Spawn
           (Program_Name => Exec_Path.all,
            Args         => List (2 .. List'Last),
            Success      => Success);
         Free (Exec_Path);
      else
         Ada.Text_IO.Put_Line ("Command not found");
      end if;

      Free (List);
   end Execute_System;

   procedure Edit_Note_Content is
   begin
      Execute_System ("vim " & Config.Temp_Note_File);
   end Edit_Note_Content;

   procedure Cmd_Branch (Info : in out Client.Client_Status) is
   begin
      if CLI.Argument_Count > 1 then
         if CLI.Argument_Count > 2 then
            if CLI.Argument (2) = "new" then
               Info.Copy_Branch
               (Info.Branch_Status
                  .Head, UBS.To_Unbounded_String
                  (CLI.Argument (3)));
               TIO.Put_Line ("created branch: " & CLI.Argument (3));
            elsif CLI.Argument (2) = "checkout" then
               TIO.Put_Line ("TODO: checkout branch");
            elsif CLI.Argument (2) = "merge" then
               TIO.Put_Line ("TODO: merge branch");
            elsif CLI.Argument (2) = "remove" then
               TIO.Put_Line ("TODO: remove");
            end if;
         end if;
         if CLI.Argument (2) = "head" then
            TIO.Put_Line ("head: " & UBS.To_String (Info.Branch_Status.Head));
         end if;
      else
         TIO.Put_Line ("TODO: branch list");
      end if;
   end Cmd_Branch;

   procedure Cmd_Note (Status : in out Client.Client_Status) is
      Note_Item : Client.Note;
   begin
      if CLI.Argument_Count > 1 then
         if CLI.Argument (2) = "new" then
            Edit_Note_Content;
            Status.Create_Note(Note_Item);
            Status.Save(Note_Item);
         end if;
      end if;
   end Cmd_Note;

   Default_Branch : Client.Branch;
   First_Load     : Boolean := False;
   Note_Client    : Client.Client_Status;
begin
   Setup_Project;
   Note_Client.Init;

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
         TIO.Put_Line ("TODO: help");
      elsif CLI.Argument (1) = "note" then
         Cmd_Note (Note_Client);
      end if;
   else
      TIO.Put_Line ("usage infomation");
   end if;

   TIO.Put_Line(UBS.To_String(Client.Get_Note("f90aab610ad199e64c2e9b97f6755b0cafefae20b3e24413e01646d38314a358").Note_Text));

   Note_Client.Save_Branches;
end Main;
