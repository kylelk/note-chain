with Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with GNAT.Calendar.Time_IO;
with Ada.Calendar.Formatting;
with Ada.Streams.Stream_IO;
with GNAT.Regpat;

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
      if not Valid_Branch_Name(UBS.To_String(Branch_Name)) then
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
      if not Valid_Branch_Name(UBS.To_String(To)) then
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

   procedure Create_Note (Status : in out Client_Status; Item : out Note) is
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
      Ada.Text_IO.Put (Data_File, Result_JSON.Write);
      Ada.Text_IO.Close (Data_File);
   end Save_Branches;

   procedure Save (Status : in out Client_Status; Item : in out Tree_Entry) is
      pragma Unreferenced (Status);
      Result_JSON : constant JSON.JSON_Value := JSON.Create_Object;
      Result_Hash : SHA256_Value;
      use Ada.Strings.Unbounded;
   begin
      Result_JSON.Set_Field
      ("entry_type", Object_Type'Image (Item.Entry_Type));
      Result_JSON.Set_Field ("child_ref", Item.Child_Ref);

      if Item.Next_Ref /= Empty_Hash_Ref then
         Result_JSON.Set_Field ("next_ref", Item.Next_Ref);
      else
         Result_JSON.Set_Field ("next_ref", JSON.JSON_Null);
      end if;

      if Item.Name /= UBS.Null_Unbounded_String then
         Result_JSON.Set_Field ("name", Item.Name);
      else
         Result_JSON.Set_Field ("name", JSON.JSON_Null);
      end if;
      Object_Store.Write ("tree_entry", Result_JSON.Write, Result_Hash);
      Item.Object_Ref := Result_Hash;
   end Save;

   procedure Save (Status : in out Client_Status; Item : in out Commit) is
      pragma Unreferenced (Status);
      Result_JSON : constant JSON.JSON_Value := JSON.Create_Object;
      Result_Hash : SHA256_Value;
      use Ada.Strings.Unbounded;
   begin
      Item.Created_At := Ada.Calendar.Clock;
      if Item.Parent_Ref /= Empty_Hash_Ref then
         Result_JSON.Set_Field ("parent_ref", Item.Parent_Ref);
      else
         Result_JSON.Set_Field ("parent_ref", JSON.JSON_Null);
      end if;
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

   procedure Save (Status : in out Client_Status; Item : in out Note) is
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
      use JSON;
   begin
      Item_JSON         := JSON.Read (Object_Store.Read (Ref), "");
      Result.Object_Ref := Ref;
      if JSON.Kind (Item_JSON.Get ("parent_ref")) = JSON.JSON_String_Type then
         Result.Parent_Ref := Item_JSON.Get ("parent_ref");
      end if;
      if JSON.Kind (Item_JSON.Get ("message")) = JSON.JSON_String_Type then
         Result.Message := Item_JSON.Get ("message");
      end if;
      Result.Tree_Ref   := Item_JSON.Get ("tree_ref");
      Result.Created_At := From_ISO_8601 (Item_JSON.Get ("created_at"));
      Result.Saved      := True;
      return Result;
   end Get_Commit;

   function Head_Commit (Status : Client_Status) return Commit is
   begin
      return Get_Commit (Status.Head_Commit_Ref);
   end Head_Commit;

   function Head (Status : Client_Status) return Branch is
   begin
      return Status.Branch_Status.Branches.Element (Status.Branch_Status.Head);
   end Head;

   function Get_Tree_Entry (Ref : SHA256_Value) return Tree_Entry is
      Result    : Tree_Entry;
      Item_JSON : JSON.JSON_Value;
      use JSON;
   begin
      Item_JSON         := JSON.Read (Object_Store.Read (Ref), "");
      Result.Entry_Type := Object_Type'Value (Item_JSON.Get ("entry_type"));
      Result.Child_Ref  := Item_JSON.Get ("child_ref");
      if JSON.Kind (Item_JSON.Get ("next_ref")) = JSON.JSON_String_Type then
         Result.Next_Ref := Item_JSON.Get ("next_ref");
      end if;
      if JSON.Kind (Item_JSON.Get ("name")) = JSON.JSON_String_Type then
         Result.Name := Item_JSON.Get ("name");
      end if;
      Result.Object_Ref := Ref;
      return Result;
   end Get_Tree_Entry;

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

   procedure Set_Head (Status : in out Client_Status; Item : Commit) is
   begin
      Status.Set_Head_Ref (Item.Object_Ref);
   end Set_Head;

   procedure Tree_Refs
     (Start_Ref  :        SHA256_Value;
      References : in out Reference_Set.Set)
   is
      Next_Ref    : Client.SHA256_Value;
      Tree_Result : Client.Tree_Entry;
      Note_Result : Client.Note;
   begin
      Next_Ref := Start_Ref;
      while Next_Ref /= Empty_Hash_Ref loop
         --if not References.Contains(Next_Ref) then
         --   References.Insert(Next_Ref);
         --end if;
         exit when References.Contains (Next_Ref);
         References.Insert (Next_Ref);

         Tree_Result := Get_Tree_Entry (Next_Ref);
         if Tree_Result.Entry_Type = Type_Note then
            Note_Result := Get_Note (Tree_Result.Child_Ref);
            if not References.Contains (Note_Result.Object_Ref) then
               References.Insert (Note_Result.Object_Ref);
            end if;
         elsif Tree_Result.Entry_Type = Type_Tree then
            Tree_Refs (Tree_Result.Child_Ref, References);
         end if;
         Next_Ref := Tree_Result.Next_Ref;
      end loop;
   end Tree_Refs;

   procedure Branch_Refs(Item : Branch; References : in out Reference_Set.Set)
   is
      Next_Commit_Ref : Client.SHA256_Value;
      Next_Commit     : Client.Commit;
   begin
      Next_Commit_Ref := Item.Commit_Ref;
      while Next_Commit_Ref /= Client.Empty_Hash_Ref loop
         exit when References.Contains (Next_Commit_Ref);
         Next_Commit := Client.Get_Commit (Next_Commit_Ref);
         References.Insert (Next_Commit_Ref);
         Tree_Refs (Next_Commit.Tree_Ref, References);
         Next_Commit_Ref := Next_Commit.Parent_Ref;
      end loop;
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

   function Format_Note(Item : Note) return String is
      Result : Message_Format.Message;
   begin
      Result.Set_Header("SHA-256", Item.Object_Ref);
      Result.Set_Header("Version", Item.Version'Img);
      Result.Set_Header("Uniq_UUID", Item.Uniq_UUID);
      Result.Set_Header("Created_At", To_ISO_8601(Item.Created_At));
      Result.Set_Header("Updated_At", To_ISO_8601(Item.Updated_At));

      Result.Set_Content(Item.Note_Text);
      return Result.To_String;
   end Format_Note;

   function Valid_Branch_Name(Name : String) return Boolean is
      package Pat renames GNAT.Regpat;
      Valid_Pattern : constant String := "([A-Za-z0-9_\-\+\(\)]+|\.\.)\&";
   begin
      if Name'Length = 40 then
         return False;
      else
         return Pat.Match(Valid_Pattern, Name);
      end if;
   end Valid_Branch_Name;


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
