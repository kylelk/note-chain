with Config;
with File_Operations;
with File_Assets;
with Ada.Strings.Fixed;
with Ada.Calendar;
with Ada.Containers.Generic_Array_Sort;

package body Note_Interactive_Menu is
   procedure Show_Select_Menu(Status : in out Client.Client_Status;
                              Db : in out KV_Store.KV_Container'Class) is
      use File_Operations;
   begin
      Write_String(Config.Vim_Selected_Line_Script,
                   File_Assets.Vim_Select_Line_Script);

   end Show_Select_Menu;

    procedure List_Notes (Status : in out Client.Client_Status;
               Db : in out KV_Store.KV_Container'Class;
               Output : out Ada.Text_IO.File_Type) is
      Tree_Ref    : Client.SHA256_Value;
      Tree_Result : Client.Tree;

      function Format_Line(Item : Note) return String is
      begin
         Result.Set_Header ("Created_At", To_ISO_8601 (Item.Created_At));
      end Format_Line;

      use Client;
      use Ada.Strings.Fixed;
   begin
      if Status.Head_Commit_Ref = Client.Empty_Hash_Ref then
         -- Ada.Text_IO.Put_Line ("tree is null");
         return;
      end if;
      Tree_Ref := Status.Head_Commit(Db).Tree_Ref;
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
         Sort (Notes);
         for Item of Notes loop
            Ada.Text_IO.Put_Line(File => Output,
                                 Item => Item.Object_Ref & "");
         end loop;
      end;
   end List_Notes;
end Note_Interactive_Menu;
