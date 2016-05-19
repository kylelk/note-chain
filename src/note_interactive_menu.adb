with Config;
with File_Operations;
with File_Assets;
with Ada.Calendar;
with Ada.Containers.Generic_Array_Sort;
with String_Operations;

package body Note_Interactive_Menu is
   procedure Show_Select_Menu
     (Status : in out Client.Client_Status;
      Db     : in out KV_Store.KV_Container'Class)
   is
      Result_Ref  : Client.SHA256_Value;
      Note_Count  : Integer;
      Output_File : Ada.Text_IO.File_Type;
      Vim_Cmd     : constant String :=
        "vim -S " &
        Config.Vim_Selected_Line_Script &
        " " &
        Config.Note_List_File &
        " " &
        Config.Selected_Line_File;
      use File_Operations;
   begin
      Write_String
        (Config.Vim_Selected_Line_Script,
         File_Assets.Vim_Select_Line_Script);

      TIO.Create (Output_File, TIO.Out_File, Config.Note_List_File);
      List_Notes (Status, Db, Output_File, Note_Count);
      TIO.Close (Output_File);

      if Note_Count > 0 then
         Execute_System_Cmd (Vim_Cmd);
         Result_Ref :=
           Load_File (Config.Selected_Line_File)
           (1 .. Client.SHA256_Value'Length);
         View_Note (Db, Result_Ref);
      else
         TIO.Put_Line ("no notes to show");
      end if;
   end Show_Select_Menu;

   procedure View_Note
     (Db  : in out KV_Store.KV_Container'Class;
      Ref :        Client.SHA256_Value)
   is

      Note_Result : constant Client.Note := Client.Get (Db, Ref);
   begin
      File_Operations.Write_String
        (Config.Temp_Note_File,
         Note_Result.Format_Note);
      File_Operations.Execute_System_Cmd
        ("vim -c 'set nospell' " & Config.Temp_Note_File);
   end View_Note;

   procedure List_Notes
     (Status       : in out Client.Client_Status;
      Db           : in out KV_Store.KV_Container'Class;
      Output       : in out TIO.File_Type;
      Result_Count :    out Integer)
   is
      Tree_Ref    : Client.SHA256_Value;
      Tree_Result : Client.Tree;

      function Format_Line (Item : Client.Note) return String is
      begin
         return Item.Object_Ref &
           " " &
           String_Operations.To_ISO_8601 (Item.Created_At);
      end Format_Line;
      use Client;
   begin
      Result_Count := 0;
      if Status.Head_Commit_Ref = Client.Empty_Hash_Ref then
         -- Ada.Text_IO.Put_Line ("tree is null");
         return;
      end if;
      Tree_Ref := Status.Head_Commit (Db).Tree_Ref;
      if Tree_Ref = Empty_Hash_Ref then
         return;
      end if;

      Tree_Result := Client.Get (Db, Tree_Ref);
      declare
         type Note_Array is array (Integer range <>) of Note;
         Note_Count : constant Integer := Integer (Tree_Result.Entries.Length);
         Notes      : Note_Array (1 .. Note_Count);
         I          : Integer          := 1;
         function "<" (L, R : Note) return Boolean is
            use Ada.Calendar;
         begin
            return L.Created_At > R.Created_At;
         end "<";
         procedure Sort is new Ada.Containers.Generic_Array_Sort
           (Integer,
            Note,
            Note_Array);
      begin
         for Item of Tree_Result.Entries loop
            if Item.Entry_Type = Type_Note then
               Notes (I) := Get (Db, Item.Child_Ref);
            end if;
            I := I + 1;
         end loop;

         Result_Count := I - 1;
         Sort (Notes);

         for Item of Notes loop
            TIO.Put_Line (File => Output, Item => Format_Line (Item));
         end loop;
      end;
   end List_Notes;
end Note_Interactive_Menu;
