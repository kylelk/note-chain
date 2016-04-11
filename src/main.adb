with Ada.Directories;
with Ada.IO_Exceptions;
with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with GNAT.OS_Lib;
with Ada.Calendar.Formatting;

with Config;
with Client;
with Object_Store;
with Ada.Strings.Fixed;

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

   procedure List_Branches(Status : Client.Client_Status) is
      Cursor : Client.Branch_Map.Cursor;
      Branch_Result : Client.Branch;
      use Client.Branch_Map;
      use UBS;
   begin
      Cursor := Status.Branch_Status.Branches.First;
      while Cursor /= Client.Branch_Map.No_Element loop
         Branch_Result := Client.Branch_Map.Element(Cursor);
         if Branch_Result.Name = Status.Branch_Status.Head then
            TIO.Put ("* ");
         else
            TIO.Put("  ");
         end if;
         TIO.Put_Line(UBS.To_String(Branch_Result.Name));
         Cursor := Client.Branch_Map.Next(Cursor);
      end loop;
   end List_Branches;

   procedure Display_Commit(Item : Client.Commit) is
   begin
      TIO.Put_Line(Item.Object_Ref & " " &
                     Ada.Calendar.Formatting.Image(Item.Created_At));
   end Display_Commit;
   
   procedure List_Notes(Status : Client.Client_Status) is
      Next_Ref : Client.SHA256_Value;
      Tree_Result : Client.Tree_Entry;
      Note_Result : Client.Note;
      use client;
   begin
      if Status.Head_Commit_Ref = Client.Empty_Hash_Ref then
         TIO.Put_Line("tree is null");
         return;
      end if;
      Next_Ref := Status.Head_Commit.Tree_Ref;
      while next_ref /= Client.Empty_Hash_Ref loop
         Tree_Result := Client.Get_Tree_Entry(next_ref);
         if Tree_Result.Entry_Type = Client.Type_Note then
            Note_Result := Client.Get_Note(Tree_Result.Child_Ref);
            TIO.Put_Line("note: " & UBS.To_String(Note_Result.Note_Text));
         end if;
         Next_Ref := Tree_Result.Next_Ref;
      end loop;
   end List_Notes;

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
               Info.Checkout_Branch(UBS.To_Unbounded_String(CLI.Argument(3)));
               TIO.Put_Line("changed to branch: " & CLI.Argument(3));
               
            elsif CLI.Argument (2) = "merge" then
               TIO.Put_Line ("TODO: merge branch");
               
            elsif CLI.Argument (2) = "remove" then
               TIO.Put_Line ("TODO: remove");
            end if;
         end if;
         if CLI.Argument (2) = "head" then
            TIO.Put_Line ("head: " & UBS.To_String (Info.Branch_Status.Head));
         elsif CLI.Argument (2) = "list" then
            List_Branches(Info);
         end if;
      else
         List_Branches(Info);
      end if;
   end Cmd_Branch;

   procedure Cmd_Note (Status : in out Client.Client_Status) is
      Note_Item : Client.Note;
      Note_Entry : Client.Tree_Entry;
      New_Commit : Client.Commit;
   begin
      Note_Entry.Entry_Type := Client.Type_Note;

      if Status.Head_Commit_Ref /= Client.Empty_Hash_Ref then
         Note_Entry.Next_Ref := Status.Head_Commit.Tree_Ref;
         New_Commit.Parent_Ref := Status.Head_Commit_Ref;
      end if;

      if CLI.Argument_Count > 1 then
         if CLI.Argument (2) = "new" then
            Edit_Note_Content;
            Status.Create_Note(Note_Item);
            Status.Save(Note_Item);

            -- save tree entry for note
            Note_Entry.Child_Ref := Note_Item.Object_Ref;
            Status.Save(Note_Entry);

            -- create new commit for changes
            New_Commit.Tree_Ref := Note_Entry.Object_Ref;
            Status.Save(New_Commit);

            -- update the head commit to point to the newest tree
            Status.Set_Head(New_Commit);

            TIO.Put_Line("created new note");
         end if;

         if CLI.Argument_Count > 2 then
            if CLI.Argument(2) = "view" then
               null;
            end if;
         end if;
         
         if CLI.Argument(2) = "list" then
            List_Notes(Status);
         end if;
      end if;
   end Cmd_Note;

   procedure Cmd_Log(Status : in out Client.Client_Status) is
      Next_Commit_Ref : Client.SHA256_Value;
      Next_Commit : Client.Commit;
   begin
      if CLI.Argument_Count > 1 then
         null;
      else
         Next_Commit_Ref := Status.Head_Commit_Ref;
         while Next_Commit_Ref /= Client.Empty_Hash_Ref loop
            Next_Commit := Client.Get_Commit(Next_Commit_Ref);
            Display_Commit(Next_Commit);
            Next_Commit_Ref := Next_Commit.Parent_Ref;
         end loop;
      end if;
   end Cmd_Log;

   procedure Cmd_Object(Status : Client.Client_Status) is
      pragma Unreferenced (Status);
   begin
      if CLI.Argument_Count > 1 then
         if CLI.Argument_Count > 2 then
            if CLI.Argument(2) = "type" then
               TIO.Put_Line(Object_Store.Object_Type(CLI.Argument(3)));
            elsif CLI.Argument(2) = "print" then
               TIO.Put_Line(Object_Store.Read(CLI.Argument(3)));
            end if;
         end if;
      end if;
   end Cmd_Object;

   procedure Display_Help is
      longest_name : Integer := 0;
      procedure P(Run, Indent : Integer; Name : String; Help : String := "") is
         use Ada.Strings.Fixed;
      begin
         if Run = 1 then
            if (Indent + Name'Length) > longest_name then
               longest_name := Indent + Name'Length;
            end if;
         elsif Run = 2 then
            if Indent = 0 then
               TIO.New_Line;
            end if;
            TIO.Put_Line(
            Indent*" " & 
            Name & ((longest_name+4)-Name'Length-Indent)*" " & Help);
         end if;
      end P;
   begin
      TIO.Put_Line("usage:");
      
      for R in 1..2 loop
         P(R, 0, "branch");
         P(R, 2, "checkout <name>", "change into another branch");
         P(R, 2, "head", "get the name of the curent branch");
         P(R, 2, "list", "list all branches");
         P(R, 2, "merge <name>", "pull the commits of a another branch info the current");
         P(R, 2, "new <name>", "create a new branch from the current");
         P(R, 2, "remove <name>", "delete a branch");
         
         P(R, 0, "help", "displays the usage options");
         
         P(R, 0, "log", "displays a list of commits in the current branch");

         P(R, 0, "object");
         P(R, 2, "print <sha256>", "show the content of a data object");
         P(R, 2, "type <sha256>", "get the type of a data object");
         
         P(R, 0, "note");
         P(R, 2, "list", "list notes in the current branch");
         P(R, 2, "new", "create a new note");
         P(R, 2, "view <sha256>", "view the content of a note in the editor");
         
         P(R, 0, "version", "displays the current version number");
      end loop;
   end Display_Help;

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
      Note_Client.Checkout_Branch (Config.Default_Branch_Name);
   end if;

   if CLI.Argument_Count >= 1 then
      if CLI.Argument (1) = "branch" then
         Cmd_Branch (Note_Client);
      elsif CLI.Argument (1) = "help" then
         Display_Help;
      elsif CLI.Argument (1) = "--help" then
         Display_Help;
      elsif CLI.Argument (1) = "note" then
         Cmd_Note (Note_Client);
      elsif CLI.Argument(1) = "version" then
         TIO.Put_Line(Config.Version);
      elsif CLI.Argument(1) = "log" then
         Cmd_Log(Note_Client);
      elsif CLI.Argument(1) = "object" then
         Cmd_Object(Note_Client);
      end if;
   else
      TIO.Put_Line ("usage infomation");
   end if;

   Note_Client.Cleanup;
end Main;
