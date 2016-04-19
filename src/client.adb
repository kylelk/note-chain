with Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with GNAT.Calendar.Time_IO;
with Ada.Calendar.Formatting;
with Ada.Streams.Stream_IO;
with GNAT.Regpat;
with Ada.Containers;

with Config;
with File_Operations;
with Object_Store;
with Message_Format;

package body Client is
   procedure Init (Status : in out Client_Status) is
      pragma Unreferenced (Status);
   begin
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
      if not Valid_Branch_Name (UBS.To_String (Branch_Name)) then
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
      if not Valid_Branch_Name (UBS.To_String (To)) then
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

   procedure Create_Note (Status : in out Client_Status; Item : out Note'Class) is
      Note_Content : constant String :=
        File_Operations.Load_File (Config.Temp_Note_File);
   begin
      Item.Note_Text  := UBS.To_Unbounded_String (Note_Content);
      Item.Encoding   := UBS.To_Unbounded_String ("UTF-8");
      Item.Uniq_UUID  := Random_SHA256;
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
      Ada.Text_IO.Put (Data_File, Result_JSON.Write(Compact => False));
      Ada.Text_IO.Close (Data_File);
   end Save_Branches;

   procedure Save (Status : in out Client_Status; Item : in out Tree'Class) is
      pragma Unreferenced (Status);
      Result_JSON   : constant JSON.JSON_Value := JSON.Create_Object;
      Result_Hash   : SHA256_Value;
      Entry_JSON    : JSON.JSON_Value;
      Entries_Array : JSON.JSON_Array;
      use UBS;
   begin
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
      Result_JSON.Set_Field ("entries", Entries_Array);
      Object_Store.Write ("tree", Result_JSON.Write, Result_Hash);
      Item.Object_Ref := Result_Hash;
   end Save;

   procedure Save (Status : in out Client_Status; Item : in out Commit'Class) is
      pragma Unreferenced (Status);
      Result_JSON : constant JSON.JSON_Value := JSON.Create_Object;
      Result_Hash : SHA256_Value;
      Parents_Array : JSON.JSON_Array;
      use Ada.Strings.Unbounded;
   begin
      Item.Created_At := Ada.Calendar.Clock;
      for Ref of Item.Parents loop
         JSON.Append(Parents_Array, JSON.Create(Ref));
      end loop;
      Result_JSON.Set_Field ("parents", Parents_Array);
      Result_JSON.Set_Field ("tree_ref", Item.Tree_Ref);
      Result_JSON.Set_Field ("created_at", To_ISO_8601 (Item.Created_At));
      if Item.Message /= UBS.Null_Unbounded_String then
         Result_JSON.Set_Field ("message", Item.Message);
      else
         Result_JSON.Set_Field ("message", JSON.JSON_Null);
      end if;
      Object_Store.Write ("commit", Result_JSON.Write, Result_Hash);
      Item.Object_Ref := Result_Hash;
      Item.Saved      := True;
   end Save;

   procedure Save (Status : in out Client_Status; Item : in out Note'Class) is
      pragma Unreferenced (Status);
      Result_JSON : constant JSON.JSON_Value := JSON.Create_Object;
      Result_Hash : SHA256_Value;
      use UBS;
   begin
      Result_JSON.Set_Field ("note_text", Item.Note_Text);
      Result_JSON.Set_Field ("encoding", Item.Encoding);
      Result_JSON.Set_Field ("created_at", To_ISO_8601 (Item.Created_At));
      Result_JSON.Set_Field ("updated_at", To_ISO_8601 (Item.Updated_At));
      Result_JSON.Set_Field ("uniq_uuid", Item.Uniq_UUID);
      Result_JSON.Set_Field ("version", Item.Version);

      if Item.Next_Ref /= Empty_Hash_Ref then
         Result_JSON.Set_Field ("next_ref", Item.Next_Ref);
      else
         Result_JSON.Set_Field ("next_ref", JSON.JSON_Null);
      end if;

      if Item.Author /= UBS.Null_Unbounded_String then
         Result_JSON.Set_Field ("author", Item.Author);
      else
         Result_JSON.Set_Field ("author", JSON.JSON_Null);
      end if;
      Object_Store.Write ("note", Result_JSON.Write, Result_Hash);
      Item.Object_Ref := Result_Hash;
      Item.Saved      := True;
   end Save;

   function Head_Commit_Ref (Status : Client_Status) return SHA256_Value is
   begin
      return Status.Branch_Status.Branches.Element (Status.Branch_Status.Head)
          .Commit_Ref;
   end Head_Commit_Ref;

   function Get_Commit (Ref : SHA256_Value) return Commit is
      Item_JSON : JSON.JSON_Value;
      Result    : Commit;
      Parents_Array : JSON.JSON_Array;
      Parent_Ref : SHA256_Value;
      use JSON;
   begin
      Item_JSON         := JSON.Read (Object_Store.Read (Ref), "");
      Result.Object_Ref := Ref;
      Parents_Array := Item_JSON.Get("parents");
      for I in 1 .. Length(Parents_Array) loop
         Parent_Ref := JSON.Get(Val => JSON.Get(Parents_Array, I));
         Result.Parents.Insert(Parent_Ref);
      end loop;
      if JSON.Kind (Item_JSON.Get ("message")) = JSON.JSON_String_Type then
         Result.Message := Item_JSON.Get ("message");
      end if;
      Result.Tree_Ref   := Item_JSON.Get ("tree_ref");
      Result.Created_At := From_ISO_8601 (Item_JSON.Get ("created_at"));
      Result.Saved      := True;
      return Result;
   end Get_Commit;

   function Head_Commit (Status : Client_Status) return Commit'Class is
   begin
      return Get_Commit (Status.Head_Commit_Ref);
   end Head_Commit;

   function Head (Status : Client_Status) return Branch is
   begin
      return Status.Branch_Status.Branches.Element (Status.Branch_Status.Head);
   end Head;

   function Get_Tree (Ref : SHA256_Value) return Tree is
      Result        : Tree;
      Item_JSON     : JSON.JSON_Value;
      Entry_Item    : Tree_Entry;
      Entries_Array : JSON.JSON_Array;
      Entry_JSON    : JSON.JSON_Value;
      use JSON;
   begin
      Item_JSON := JSON.Read (Object_Store.Read (Ref), "");
      Entries_Array := JSON.Get(Item_JSON, "entries");

      for I in 1 .. (JSON.Length (Entries_Array)) loop
         Entry_JSON            := JSON.Get (Entries_Array, I);
         Entry_Item.Entry_Type :=
           Object_Type'Value (Entry_JSON.Get ("entry_type"));
         Entry_Item.Child_Ref := Entry_JSON.Get ("child_ref");
         if JSON.Kind (Item_JSON.Get ("name")) = JSON.JSON_String_Type then
            Entry_Item.Name := Item_JSON.Get ("name");
         end if;
         Result.Entries.Insert (Entry_Item);
      end loop;
      Result.Saved := True;
      return Result;
   end Get_Tree;

   function Get_Note (Ref : SHA256_Value) return Note is
      Result    : Note;
      Item_JSON : JSON.JSON_Value;
      use JSON;
   begin
      Item_JSON         := JSON.Read (Object_Store.Read (Ref), "");
      Result.Note_Text  := Item_JSON.Get ("note_text");
      Result.Encoding   := Item_JSON.Get ("encoding");
      Result.Uniq_UUID  := Item_JSON.Get ("uniq_uuid");
      Result.Created_At := From_ISO_8601 (Item_JSON.Get ("created_at"));
      Result.Updated_At := From_ISO_8601 (Item_JSON.Get ("updated_at"));
      Result.Version    := Item_JSON.Get ("version");
      if JSON.Kind (Item_JSON.Get ("next_ref")) = JSON.JSON_String_Type then
         Result.Next_Ref := Item_JSON.Get ("next_ref");
      end if;
      if JSON.Kind (Item_JSON.Get ("author")) = JSON.JSON_String_Type then
         Result.Author := Item_JSON.Get ("author");
      end if;
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

   procedure Set_Head (Status : in out Client_Status; Item : Commit'Class) is
   begin
      Status.Set_Head_Ref (Item.Object_Ref);
   end Set_Head;

   procedure Tree_Refs
     (Start_Ref  :        SHA256_Value;
      References : in out Reference_Set.Set)
   is
      Tree_Result : Client.Tree;
   begin
      Tree_Result := Get_Tree (Start_Ref);
      if not References.Contains(Start_Ref) then
         References.Insert(Start_Ref);
      end if;

      for Item of Tree_Result.Entries loop
         begin
            if Item.Entry_Type = Type_Note then
               References.Insert (Item.Child_Ref);
            elsif Item.Entry_Type = Type_Tree then
               Tree_Refs (Item.Child_Ref, References);
            end if;
         exception
            when Constraint_Error => null;
         end;
      end loop;
   end Tree_Refs;

   procedure Branch_Refs
     (Item       :        Branch;
      References : in out Reference_Set.Set)
   is
      procedure Commit_Refs(Item : Commit) is
      begin
         if not References.Contains(Item.Object_Ref) then
            References.Insert(Item.Object_Ref);
            Tree_Refs(Item.Tree_Ref, References);
         end if;
      end Commit_Refs;
   begin
      if Item.Commit_Ref /= Client.Empty_Hash_Ref then
         Client.Traverse_Commits(Item.Commit_Ref, Commit_Refs'Access);
      end if;
   end Branch_Refs;

   procedure Export (Status : Client_Status; Filename : String) is
      References : Reference_Set.Set;
   begin
      for Branch_Result of Status.Branch_Status.Branches loop
         Branch_Refs (Branch_Result, References);
      end loop;
      Export_Refs (References, Filename);
   end Export;

   procedure Export_Refs (Items : Reference_Set.Set; Filename : String) is
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
            Content : constant String := Object_Store.Read_Object (Ref);
         begin
            Integer'Write (Output_Stream, Content'Length);
            String'Write (Output_Stream, Content);
         end;
      end loop;
      Ada.Streams.Stream_IO.Close (Output_File);
   end Export_Refs;

   function Format_Note (Item : Note) return String is
      Result : Message_Format.Message;
   begin
      Result.Set_Header ("SHA-256", Item.Object_Ref);
      Result.Set_Header ("Version", Item.Version'Img);
      Result.Set_Header ("Uniq_UUID", Item.Uniq_UUID);
      Result.Set_Header ("Created_At", To_ISO_8601 (Item.Created_At));
      Result.Set_Header ("Updated_At", To_ISO_8601 (Item.Updated_At));

      Result.Set_Content (Item.Note_Text);
      return Result.To_String;
   end Format_Note;

   function Valid_Branch_Name (Name : String) return Boolean is
      package Pat renames GNAT.Regpat;
      Valid_Pattern : constant String := "^([A-Za-z0-9_\-\+\(\)]+)$";
   begin
      if Name'Length = 40 then
         return False;
      end if;

      if Pat.Match ("\.\.", Name) then
         return False;
      end if;

      return Pat.Match (Valid_Pattern, Name);
   end Valid_Branch_Name;

   procedure Traverse_Commits (Ref : SHA256_Value; Proc : access procedure(Item : Commit)) is
      Next_Ref : Client.SHA256_Value := Ref;
      Next_Commit     : Client.Commit;
      Root : Boolean := False;
      use Ada.Containers;
   begin
       while not Root loop
         Next_Commit := Get_Commit(Next_Ref);
         Proc.all(Next_Commit);
            if Next_Commit.Parents.Length = 1 then
               Next_Ref := Reference_Set.Element(Next_Commit.Parents.First);
            elsif Next_Commit.Parents.Length > 1 then
               for Parent_Ref of Next_Commit.Parents loop
                  Traverse_Commits(Parent_Ref, Proc);
               end loop;
            else
               Root := True;
            end if;
         end loop;
   end Traverse_Commits;

   function Random_SHA256 return SHA256_Value is
      package Guess_Generator is new Ada.Numerics.Discrete_Random (Character);
      Gen  : Guess_Generator.Generator;
      Data : SHA256_Value;
   begin
      for I in Data'Range loop
         Guess_Generator.Reset (Gen);
         Data (I) := Guess_Generator.Random (Gen);
      end loop;
      return File_Operations.String_Hash (Data);
   end Random_SHA256;

   function To_ISO_8601 (Date : in Ada.Calendar.Time) return String is
   begin
      return GNAT.Calendar.Time_IO.Image (Date, "%Y-%m-%dT%H:%M:%S");
   end To_ISO_8601;

   function From_ISO_8601 (Date_Str : String) return Ada.Calendar.Time is
      Year   : Integer;
      Month  : Integer range 1 .. 12;
      Day    : Integer range 1 .. 31;
      Hour   : Integer range 1 .. 23;
      Minute : Integer range 1 .. 59;
      Second : Integer range 1 .. 59;
   begin
      Year  := Integer'Value (Date_Str (Date_Str'First .. Date_Str'First + 3));
      Month :=
        Integer'Value (Date_Str (Date_Str'First + 5 .. Date_Str'First + 6));
      Day :=
        Integer'Value (Date_Str (Date_Str'First + 8 .. Date_Str'First + 9));
      Hour :=
        Integer'Value (Date_Str (Date_Str'First + 11 .. Date_Str'First + 12));
      Minute :=
        Integer'Value (Date_Str (Date_Str'First + 14 .. Date_Str'First + 15));
      Second :=
        Integer'Value (Date_Str (Date_Str'First + 17 .. Date_Str'First + 18));
      return Ada.Calendar.Formatting.Time_Of
          (Year       => Year,
           Month      => Month,
           Day        => Day,
           Hour       => Hour,
           Minute     => Minute,
           Second     => Second,
           Sub_Second => 0.0);
   end From_ISO_8601;
end Client;
