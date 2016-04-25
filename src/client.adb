with Ada.Streams.Stream_IO;
with Ada.Containers;
with Ada.Directories;
with Ada.IO_Exceptions;
with Ada.Strings.Fixed;

with Config;
with File_Operations;
with Message_Format;

package body Client is
   procedure Init (Status : in out Client_Status) is
      procedure Create_Dir (Path : String) is
      begin
         if not Ada.Directories.Exists (Path) then
            Ada.Directories.Create_Directory (Path);
         end if;
      end Create_Dir;

      Default_Branch : Client.Branch;
      First_Load     : Boolean := False;
   begin
      Create_Dir (Config.Data_Dir);
      Create_Dir (Config.Temp_Dir);

      Status.Data.Setup;

      begin
         Status.Load_Branches;
      exception
         when Ada.IO_Exceptions.Name_Error =>
            First_Load := True;
      end;

      if First_Load then
         Default_Branch.Name := Config.Default_Branch_Name;
         Status.Set_Branch (Default_Branch);
         Status.Checkout_Branch (Config.Default_Branch_Name);
      end if;

      Status.Settings_Status.Load;
   end Init;

   function Tree_Entry_Hash
     (Item : Tree_Entry) return Ada.Containers.Hash_Type
   is
   begin
      return Ada.Strings.Hash (Item.Child_Ref & UBS.To_String (Item.Name));
   end Tree_Entry_Hash;

   procedure Cleanup (Status : in out Client_Status) is
   begin
      Status.Save_Branches;
      Status.Settings_Status.Save;
      Status.Data.Cleanup;

      -- clear the temp directory
      File_Operations.Remake_Directory (Config.Temp_Dir);
   end Cleanup;

   procedure Load_Branches (Status : in out Client_Status) is
      procedure Handler (Name : JSON.UTF8_String; Value : JSON.JSON_Value) is
         Branch_Result : Branch;
         use JSON;
      begin
         Branch_Result.Name := UBS.To_Unbounded_String (Name);
         if JSON.Kind (Value.Get ("commit_ref")) = JSON.JSON_String_Type then
            Branch_Result.Commit_Ref := Value.Get ("commit_ref");
            Branch_Result.Name       := Value.Get ("name");
         end if;
         Status.Set_Branch (Branch_Result);
      end Handler;
      Branch_Json : JSON.JSON_Value;
   begin
      Branch_Json :=
        JSON.Read (File_Operations.Load_File (Config.Branch_JSON_File), "");
      JSON.Map_JSON_Object
        (Val => Branch_Json.Get ("branches"),
         CB  => Handler'Access);
      Status.Checkout_Branch (Branch_Json.Get ("head"));
   end Load_Branches;

   procedure Checkout_Branch
     (Status      : in out Client_Status;
      Branch_Name :        UBS.Unbounded_String)
   is
   begin
      if not STR_OPS.Valid_Branch_Name (UBS.To_String (Branch_Name)) then
         raise Branch_Name_Format_Error;
      end if;
      if Status.Branch_Exists (Branch_Name) then
         Status.Branch_Status.Head := Branch_Name;
      else
         raise No_Branch_Error;
      end if;
   end Checkout_Branch;

   procedure Set_Branch (Status : in out Client_Status; Item : Branch) is
      use Branch_Map;
      Result_Cursor : Branch_Map.Cursor;
   begin
      Result_Cursor := Status.Branch_Status.Branches.Find (Item.Name);
      if Result_Cursor = Branch_Map.No_Element then
         Status.Branch_Status.Branches.Insert (Item.Name, Item);
      else
         Status.Branch_Status.Branches.Replace_Element (Result_Cursor, Item);
      end if;
   end Set_Branch;

   procedure Copy_Branch
     (Status   : in out Client_Status;
      From, To :        UBS.Unbounded_String)
   is
      use Branch_Map;
      New_Branch    : Branch;
      Result_Cursor : Branch_Map.Cursor;
   begin
      if not STR_OPS.Valid_Branch_Name (UBS.To_String (To)) then
         raise Branch_Name_Format_Error;
      end if;
      Result_Cursor := Status.Branch_Status.Branches.Find (From);
      if Result_Cursor /= Branch_Map.No_Element then
         New_Branch      := Branch_Map.Element (Result_Cursor);
         New_Branch.Name := To;
         Status.Branch_Status.Branches.Insert (To, New_Branch);
      else
         raise No_Branch_Error
           with "could not find branch with name: " & UBS.To_String (From);
      end if;
   end Copy_Branch;

   procedure Create_Note
     (Status       : in out Client_Status;
      Item         :    out Note'Class;
      Note_Content :        String)
   is
   begin
      Item.Note_Text  := UBS.To_Unbounded_String (Note_Content);
      Item.Encoding   := UBS.To_Unbounded_String ("UTF-8");
      Item.Uniq_UUID  := STR_OPS.Random_SHA256;
      Item.Created_At := Ada.Calendar.Clock;
      Item.Updated_At := Ada.Calendar.Clock;

      if Status.Settings_Status.Exists ("author") then
         Item.Author :=
           UBS.To_Unbounded_String (Status.Settings_Status.Get ("author"));
      end if;
   end Create_Note;

   procedure Add_Note (T : in out Tree; Note_Entry : Note'Class) is
      New_Entry : Tree_Entry;
   begin
      New_Entry.Entry_Type := Type_Note;
      New_Entry.Child_Ref  := Note_Entry.Object_Ref;
      T.Entries.Insert (New_Entry);
   end Add_Note;

   procedure Save_Branches (Status : in out Client_Status) is
      Result_JSON, Branch_JSON, Branch_Entry_JSON : JSON.JSON_Value;
      Branch_Cursor                               : Branch_Map.Cursor;
      Temp_Branch                                 : Branch;
      Data_File                                   : Ada.Text_IO.File_Type;
   begin
      Branch_JSON   := JSON.Create_Object;
      Branch_Cursor := Status.Branch_Status.Branches.First;
      while Branch_Map.Has_Element (Branch_Cursor) loop
         Temp_Branch       := Branch_Map.Element (Branch_Cursor);
         Branch_Entry_JSON := JSON.Create_Object;
         Branch_Entry_JSON.Set_Field
         ("name", UBS.To_String (Temp_Branch.Name));
         if Temp_Branch.Commit_Ref /= Empty_Hash_Ref then
            Branch_Entry_JSON.Set_Field ("commit_ref", Temp_Branch.Commit_Ref);
         else
            Branch_Entry_JSON.Set_Field ("commit_ref", JSON.JSON_Null);
         end if;
         Branch_JSON.Set_Field
         (UBS.To_String (Temp_Branch.Name), Branch_Entry_JSON);
         Branch_Map.Next (Branch_Cursor);
      end loop;
      Result_JSON := JSON.Create_Object;
      Result_JSON.Set_Field ("branches", Branch_JSON);
      Result_JSON.Set_Field ("head", Status.Branch_Status.Head);

      Ada.Text_IO.Create
        (Data_File,
         Ada.Text_IO.Out_File,
         Config.Branch_JSON_File);
      Ada.Text_IO.Put (Data_File, Result_JSON.Write (Compact => False));
      Ada.Text_IO.Close (Data_File);
   end Save_Branches;

   procedure Save (Status : in out Client_Status; Item : in out Tree'Class) is
      Result_JSON : JSON.JSON_Value;
      Result_Hash : SHA256_Value;
   begin
      To_JSON (Item, Result_JSON);
      Object_Store.Write (Status, "tree", Result_JSON.Write, Result_Hash);
      Item.Object_Ref := Result_Hash;
      Item.Saved      := True;
   end Save;

   procedure To_JSON (Item : in Note; Result : out JSON.JSON_Value) is
      use UBS;
      use STR_OPS;
   begin
      Result := JSON.Create_Object;
      Result.Set_Field ("note_text", Item.Note_Text);
      Result.Set_Field ("encoding", Item.Encoding);
      Result.Set_Field ("created_at", To_ISO_8601 (Item.Created_At));
      Result.Set_Field ("updated_at", To_ISO_8601 (Item.Updated_At));
      Result.Set_Field ("uniq_uuid", Item.Uniq_UUID);

      if Item.Next_Ref /= Empty_Hash_Ref then
         Result.Set_Field ("next_ref", Item.Next_Ref);
      else
         Result.Set_Field ("next_ref", JSON.JSON_Null);
      end if;

      if Item.Author /= UBS.Null_Unbounded_String then
         Result.Set_Field ("author", Item.Author);
      else
         Result.Set_Field ("author", JSON.JSON_Null);
      end if;
   end To_JSON;

   procedure To_JSON (Item : in Commit; Result : out JSON.JSON_Value) is
      Parents_Array : JSON.JSON_Array;
      use UBS;
      use STR_OPS;
   begin
      Result := JSON.Create_Object;
      for Ref of Item.Parents loop
         JSON.Append (Parents_Array, JSON.Create (Ref));
      end loop;
      Result.Set_Field ("parents", Parents_Array);
      Result.Set_Field ("tree_ref", Item.Tree_Ref);
      Result.Set_Field ("created_at", To_ISO_8601 (Item.Created_At));
      if Item.Message /= UBS.Null_Unbounded_String then
         Result.Set_Field ("message", Item.Message);
      else
         Result.Set_Field ("message", JSON.JSON_Null);
      end if;
   end To_JSON;

   procedure To_JSON (Item : in Tree; Result : out JSON.JSON_Value) is
      Entry_JSON    : JSON.JSON_Value;
      Entries_Array : JSON.JSON_Array;
      use UBS;
   begin
      Result := JSON.Create_Object;
      for Entry_Item of Item.Entries loop
         Entry_JSON := JSON.Create_Object;
         Entry_JSON.Set_Field ("entry_type", Entry_Item.Entry_Type'Img);
         Entry_JSON.Set_Field ("child_ref", Entry_Item.Child_Ref);
         if Entry_Item.Name /= UBS.Null_Unbounded_String then
            Entry_JSON.Set_Field ("name", Entry_Item.Name);
         else
            Entry_JSON.Set_Field ("name", JSON.JSON_Null);
         end if;
         JSON.Append (Entries_Array, Entry_JSON);
      end loop;
      Result.Set_Field ("entries", Entries_Array);
   end To_JSON;

   procedure From_JSON (Item : in out Note; Data : String) is
      Item_JSON : JSON.JSON_Value := JSON.Create_Object;
      use JSON;
      use STR_OPS;
   begin
      Item_JSON       := JSON.Read (Data, "");
      Item.Note_Text  := Item_JSON.Get ("note_text");
      Item.Encoding   := Item_JSON.Get ("encoding");
      Item.Uniq_UUID  := Item_JSON.Get ("uniq_uuid");
      Item.Created_At := From_ISO_8601 (Item_JSON.Get ("created_at"));
      Item.Updated_At := From_ISO_8601 (Item_JSON.Get ("updated_at"));
      if JSON.Kind (Item_JSON.Get ("next_ref")) = JSON.JSON_String_Type then
         Item.Next_Ref := Item_JSON.Get ("next_ref");
      end if;
      if JSON.Kind (Item_JSON.Get ("author")) = JSON.JSON_String_Type then
         Item.Author := Item_JSON.Get ("author");
      end if;
      Item.Saved := True;
   end From_JSON;

   procedure From_JSON (Item : in out Tree; Data : String) is
      Item_JSON     : JSON.JSON_Value;
      Entry_Item    : Tree_Entry;
      Entries_Array : JSON.JSON_Array;
      Entry_JSON    : JSON.JSON_Value;
      use JSON;
   begin
      Item_JSON     := JSON.Read (Data, "");
      Entries_Array := JSON.Get (Item_JSON, "entries");

      for I in 1 .. (JSON.Length (Entries_Array)) loop
         Entry_JSON            := JSON.Get (Entries_Array, I);
         Entry_Item.Entry_Type :=
           Object_Type'Value (Entry_JSON.Get ("entry_type"));
         Entry_Item.Child_Ref := Entry_JSON.Get ("child_ref");
         if JSON.Kind (Item_JSON.Get ("name")) = JSON.JSON_String_Type then
            Entry_Item.Name := Item_JSON.Get ("name");
         end if;
         Item.Entries.Insert (Entry_Item);
      end loop;
   end From_JSON;

   procedure From_JSON (Item : in out Commit; Data : String) is
      Item_JSON     : JSON.JSON_Value;
      Parents_Array : JSON.JSON_Array;
      Parent_Ref    : SHA256_Value;
      use JSON;
      use STR_OPS;
   begin
      Item_JSON     := JSON.Read (Data, "");
      Parents_Array := Item_JSON.Get ("parents");
      for I in 1 .. Length (Parents_Array) loop
         Parent_Ref := JSON.Get (Val => JSON.Get (Parents_Array, I));
         Item.Parents.Insert (Parent_Ref);
      end loop;
      if JSON.Kind (Item_JSON.Get ("message")) = JSON.JSON_String_Type then
         Item.Message := Item_JSON.Get ("message");
      end if;
      Item.Tree_Ref   := Item_JSON.Get ("tree_ref");
      Item.Created_At := From_ISO_8601 (Item_JSON.Get ("created_at"));
      Item.Saved      := True;
   end From_JSON;

   procedure Save
     (Status : in out Client_Status;
      Item   : in out Commit'Class)
   is
      Result_Hash : SHA256_Value;
      Result_JSON : JSON.JSON_Value;
   begin
      Item.Created_At := Ada.Calendar.Clock;
      To_JSON (Item, Result_JSON);
      Object_Store.Write (Status, "commit", Result_JSON.Write, Result_Hash);
      Item.Object_Ref := Result_Hash;
      Item.Saved      := True;
   end Save;

   procedure Save (Status : in out Client_Status; Item : in out Note'Class) is
      Result_Hash : SHA256_Value;
      Result_JSON : JSON.JSON_Value;
   begin
      Item.Created_At := Ada.Calendar.Clock;
      To_JSON (Item, Result_JSON);
      Object_Store.Write (Status, "note", Result_JSON.Write, Result_Hash);
      Item.Object_Ref := Result_Hash;
      Item.Saved      := True;
   end Save;

   function Head_Commit_Ref (Status : Client_Status) return SHA256_Value is
   begin
      return Status.Branch_Status.Branches.Element (Status.Branch_Status.Head)
          .Commit_Ref;
   end Head_Commit_Ref;

   function Get_Commit
     (Status : in out Client_Status'Class;
      Ref    :        SHA256_Value) return Commit
   is
      Result : Commit;
   begin
      From_JSON (Result, Object_Store.Read (Status, Ref));
      Result.Object_Ref := Ref;
      Result.Saved      := True;
      return Result;
   end Get_Commit;

   function Head_Commit (Status : in out Client_Status) return Commit'Class is
   begin
      return Get_Commit (Status, Status.Head_Commit_Ref);
   end Head_Commit;

   function Head (Status : Client_Status) return Branch is
   begin
      return Status.Branch_Status.Branches.Element (Status.Branch_Status.Head);
   end Head;

   function Get_Tree
     (Status : in out Client_Status'Class;
      Ref    :        SHA256_Value) return Tree
   is
      Result : Tree;
   begin
      From_JSON (Result, Object_Store.Read (Status, Ref));
      Result.Object_Ref := Ref;
      Result.Saved      := True;
      return Result;
   end Get_Tree;

   function Get_Note
     (Status : in out Client_Status'Class;
      Ref    :        SHA256_Value) return Note
   is
      Result : Note;
   begin
      From_JSON (Result, Object_Store.Read (Status, Ref));
      Result.Object_Ref := Ref;
      Result.Saved      := True;
      return Result;
   end Get_Note;

   procedure Set_Head_Ref
     (Status : in out Client_Status;
      Ref    :        SHA256_Value)
   is
      Updated_Branch : Branch;
   begin
      Updated_Branch            := Status.Head;
      Updated_Branch.Commit_Ref := Ref;
      Status.Branch_Status.Branches.Replace
      (Updated_Branch.Name, Updated_Branch);
   end Set_Head_Ref;

   function Branch_Exists
     (Status      : Client_Status;
      Branch_Name : UBS.Unbounded_String) return Boolean
   is
   begin
      return Status.Branch_Status.Branches.Contains (Branch_Name);
   end Branch_Exists;

   function Get_Branch
     (Status : Client_Status;
      Name   : UBS.Unbounded_String) return Branch
   is
   begin
      if Status.Branch_Exists (Name) then
         return Status.Branch_Status.Branches.Element (Name);
      else
         raise No_Branch_Error;
      end if;
   end Get_Branch;

   procedure Set_Head (Status : in out Client_Status; Item : Commit'Class) is
   begin
      Status.Set_Head_Ref (Item.Object_Ref);
   end Set_Head;

   procedure Tree_Refs
     (Status     : in out Client_Status;
      Start_Ref  :        SHA256_Value;
      References : in out Reference_Set.Set)
   is
      Tree_Result : Client.Tree;
   begin
      Tree_Result := Get_Tree (Status, Start_Ref);
      if not References.Contains (Start_Ref) then
         References.Insert (Start_Ref);
      end if;

      for Item of Tree_Result.Entries loop
         begin
            if Item.Entry_Type = Type_Note then
               References.Insert (Item.Child_Ref);
            elsif Item.Entry_Type = Type_Tree then
               Tree_Refs (Status, Item.Child_Ref, References);
            end if;
         exception
            when Constraint_Error =>
               null;
         end;
      end loop;
   end Tree_Refs;

   procedure Branch_Refs
     (Status     : in out Client_Status;
      Item       :        Branch;
      References : in out Reference_Set.Set)
   is
      procedure Commit_Refs (Item : Commit) is
      begin
         if not References.Contains (Item.Object_Ref) then
            References.Insert (Item.Object_Ref);
            Tree_Refs (Status, Item.Tree_Ref, References);
         end if;
      end Commit_Refs;
   begin
      if Item.Commit_Ref /= Client.Empty_Hash_Ref then
         Client.Traverse_Commits (Status, Item.Commit_Ref, Commit_Refs'Access);
      end if;
   end Branch_Refs;

   procedure Export (Status : in out Client_Status; Filename : String) is
      References : Reference_Set.Set;
   begin
      for Branch_Result of Status.Branch_Status.Branches loop
         Branch_Refs (Status, Branch_Result, References);
      end loop;
      Export_Refs (Status, References, Filename);
   end Export;

   procedure Export_Refs
     (Status   : in out Client_Status;
      Items    :        Reference_Set.Set;
      Filename :        String)
   is
      Filetype_Str  : constant String := "note chain export" & ASCII.LF;
      Output_File   : Ada.Streams.Stream_IO.File_Type;
      Output_Stream : Ada.Streams.Stream_IO.Stream_Access;
   begin
      Ada.Streams.Stream_IO.Create
        (File => Output_File,
         Mode => Ada.Streams.Stream_IO.Out_File,
         Name => Filename);
      Output_Stream := Ada.Streams.Stream_IO.Stream (Output_File);
      String'Write (Output_Stream, Filetype_Str);
      for Ref of Items loop
         declare
            Content : constant String :=
              Object_Store.Read_Object (Status, Ref);
         begin
            Integer'Write (Output_Stream, Content'Length);
            String'Write (Output_Stream, Content);
         end;
      end loop;
      Ada.Streams.Stream_IO.Close (Output_File);
   end Export_Refs;

   function Format_Note (Item : Note) return String is
      Result : Message_Format.Message;
      use STR_OPS;
   begin
      Result.Set_Header ("SHA-256", Item.Object_Ref);
      Result.Set_Header ("Uniq_UUID", Item.Uniq_UUID);
      Result.Set_Header ("Created_At", To_ISO_8601 (Item.Created_At));
      Result.Set_Header ("Updated_At", To_ISO_8601 (Item.Updated_At));

      Result.Set_Content (Item.Note_Text);
      return Result.To_String;
   end Format_Note;

   procedure Traverse_Commits
     (Status : in out Client_Status;
      Ref    :        SHA256_Value;
      Proc   :        access procedure (Item : Commit))
   is
      Next_Ref    : Client.SHA256_Value := Ref;
      Next_Commit : Client.Commit;
      Root        : Boolean             := False;
      use Ada.Containers;
   begin
      while not Root loop
         Next_Commit := Get_Commit (Status, Next_Ref);
         Proc.all (Next_Commit);
         if Next_Commit.Parents.Length = 1 then
            Next_Ref := Reference_Set.Element (Next_Commit.Parents.First);
         elsif Next_Commit.Parents.Length > 1 then
            for Parent_Ref of Next_Commit.Parents loop
               Traverse_Commits (Status, Parent_Ref, Proc);
            end loop;
            Root := True;
         else
            Root := True;
         end if;
      end loop;
   end Traverse_Commits;

   function Join_Trees (Left, Right : Tree) return Tree is
      Result : Tree;
   begin
      Result.Entries := Tree_Entry_Set.Union (Left.Entries, Right.Entries);
      return Result;
   end Join_Trees;

   procedure Merge_Branches
     (Status : in out Client_Status;
      A      : in out Branch;
      B      :        Branch)
   is
      Commit_A, Commit_B : Commit;

      Merged_Tree : Tree;
      New_Commit  : Client.Commit;
   begin
      -- exit if the commits are null
      if A.Commit_Ref = Empty_Hash_Ref or B.Commit_Ref = Empty_Hash_Ref then
         return;
      end if;

      -- exit if the branches are equal
      if A.Commit_Ref = B.Commit_Ref then
         Ada.Text_IO.Put_Line ("Error: branches are equal");
         return;
      end if;

      Commit_A    := Get_Commit (Status, A.Commit_Ref);
      Commit_B    := Get_Commit (Status, B.Commit_Ref);
      Merged_Tree :=
        Join_Trees
          (Left  => Get_Tree (Status, Commit_A.Tree_Ref),
           Right => Get_Tree (Status, Commit_B.Tree_Ref));
      Ada.Text_IO.Put_Line ("saving tree");
      Status.Save (Merged_Tree);

      New_Commit.Tree_Ref := Merged_Tree.Object_Ref;
      New_Commit.Parents.Insert (Commit_A.Object_Ref);
      New_Commit.Parents.Insert (Commit_B.Object_Ref);
      Ada.Text_IO.Put_Line ("saving commit");
      Status.Save (New_Commit);

      A.Commit_Ref := New_Commit.Object_Ref;
   end Merge_Branches;

   package body Object_Store is
      procedure Write
        (Status      : in out Client_Status'Class;
         Object_Type :        String;
         Content     :        String;
         Hash        :    out SHA256_Value)
      is
         Length_Str : constant String :=
           Ada.Strings.Fixed.Trim (Content'Length'Img, Ada.Strings.Left);
      begin
         declare
            Object_Content : constant String :=
              Object_Type & ' ' & Length_Str & ASCII.LF & Content;
         begin
            Hash := File_Operations.String_Hash (Object_Content);
            Status.Data.Set (Hash, Object_Content);
         end;
      end Write;

      function Read
        (Status : in out Client_Status'Class;
         Hash   :        SHA256_Value) return String
      is
         File_Content  : constant String := Status.Data.Get (Hash);
         Newline_Index : Integer;
      begin
         Newline_Index := STR_OPS.Char_Index (File_Content, ASCII.LF) + 1;
         return File_Content (Newline_Index .. File_Content'Last);
      end Read;

      function Read_Object
        (Status : Client_Status'Class;
         Hash   : SHA256_Value) return String
      is
         Info : Client_Status'Class := Status;
      begin
         STR_OPS.Check_SHA256 (Hash);
         return Info.Data.Get (Hash);
      end Read_Object;

      function Object_Type
        (Status : in out Client_Status'Class;
         Hash   :        SHA256_Value) return String
      is
         File_Content  : constant String := Read_Object (Status, Hash);
         Newline_Index : Integer;
         Last_Space    : Integer;
         use STR_OPS;
      begin
         Newline_Index := Char_Index (File_Content, ASCII.LF);
         Last_Space    := Last_Index (File_Content (1 .. Newline_Index), ' ');
         return File_Content (1 .. Last_Space - 1);
      end Object_Type;

      function Exists
        (Status : in out Client_Status'Class;
         Hash   :        SHA256_Value) return Boolean
      is
      begin
         STR_OPS.Check_SHA256 (Hash);
         return Status.Data.Exists (Hash);
      end Exists;
   end Object_Store;
end Client;
