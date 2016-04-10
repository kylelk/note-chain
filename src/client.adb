with Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with GNAT.Calendar.Time_IO;

with Config;
with File_Operations;
with Object_Store;
with Ada.Calendar.Formatting;

package body Client is
   procedure Init(Status : in out Client_Status) is
      pragma Unreferenced (Status);
   begin
      Object_Store.Init;
   end Init;

   procedure Load_Branches (Status : in out Client_Status) is
      procedure Handler (Name : JSON.UTF8_String; Value : JSON.JSON_Value) is
         Branch_Result : Branch;
         use JSON;
      begin
         Branch_Result.Name := UBS.To_Unbounded_String (Name);
         if JSON.Kind (Value.Get ("commit_ref")) = JSON.JSON_String_Type then
            Branch_Result.Commit_Ref := Value.Get ("commit_ref");
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
      Status.Set_Head (Branch_Json.Get ("head"));
   end Load_Branches;

   procedure Set_Head
     (Status      : in out Client_Status;
      Branch_Name :        UBS.Unbounded_String)
   is
   begin
      Status.Branch_Status.Head := Branch_Name;
   end Set_Head;

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
      -- clear the temp directory
      -- File_Operations.Remake_Directory (Config.Temp_Dir);
   end Save_Branches;

   procedure Save(Status : in out Client_Status; Item : in out Tree_Entry) is
   begin
      null;
   end Save;

   procedure Save(Status : in out Client_Status; Item : in out Commit) is
   begin
      null;
   end Save;


   procedure Create_Note(Status : in out Client_Status; Item : out Note) is
      pragma Unreferenced (Status);
      Note_Content : constant String :=
        File_Operations.Load_File(Config.Temp_Note_File);
   begin
      Item.Note_Text := UBS.To_Unbounded_String(Note_Content);
      Item.Encoding := UBS.To_Unbounded_String("UTF-8");
      Item.Uniq_UUID := Random_SHA256;
      Item.Created_At := Ada.Calendar.Clock;
   end Create_Note;

   procedure Save(Status : in out Client_Status; Item : in out Note) is
      pragma Unreferenced (Status);
      Note_JSON : JSON.JSON_Value;
      Note_Hash : SHA256_Value;
   begin
      Note_JSON := JSON.Create_Object;
      Note_JSON.Set_Field("note_text", Item.Note_Text);
      Note_JSON.Set_Field("encoding", Item.Encoding);
      Note_JSON.Set_Field("created_at", To_ISO_8601(Item.Created_At));
      Note_JSON.Set_Field("uniq_uuid", Item.Uniq_UUID);
      Object_Store.Write("note", Note_JSON.Write, Note_Hash);
      Item.Object_Ref := Note_Hash;
      Item.Saved := True;
   end Save;

   function Head_Commit_Ref(Status : Client_Status) return SHA256_Value is
   begin
      return Status.Branch_Status.Branches.Element(Status.Branch_Status.Head).Commit_Ref;
   end Head_Commit_Ref;

   function Get_Commit(Ref : SHA256_Value) return Commit is
      Item_JSON : JSON.JSON_Value;
      Result : Commit;
   begin
      Item_JSON := JSON.Read(Object_Store.Read(Ref), "");
      Result.Object_Ref := Ref;
      Result.Parent_Ref := Item_JSON.Get("parent_ref");
      Result.Tree_Ref := Item_JSON.Get("tree_ref");
      Result.Created_At := From_ISO_8601(Item_JSON.Get("Created_At"));
      return Result;
   end Get_Commit;

   function Get_Tree_Entry(Ref : SHA256_Value) return Tree_Entry is
      Result : Tree_Entry;
      Item_JSON : JSON.JSON_Value;
   begin
      Item_JSON := JSON.Read(Object_Store.Read(Ref), "");
      Result.Entry_Type := Object_Type'Value(Item_JSON.Get("entry_type"));
      Result.Child_Ref := Item_JSON.Get("child_ref");
      Result.Next_Ref := Item_JSON.Get("next_ref");
      Result.Object_Ref := Ref;
      return Result;
   end Get_Tree_Entry;

   function Get_Note(Ref : SHA256_Value) return Note is
      Result : Note;
      Item_JSON : JSON.JSON_Value;
   begin
      Item_JSON := JSON.Read(Object_Store.Read(Ref), "");
      Result.Note_Text := Item_JSON.Get("note_text");
      Result.Encoding := Item_JSON.Get("encoding");
      Result.Uniq_UUID := Item_JSON.Get("uniq_uuid");
      Result.Created_At := From_ISO_8601(Item_JSON.Get("created_at"));
      Result.Saved := True;
      return Result;
   end Get_Note;

   function Random_SHA256 return SHA256_Value is
      package Guess_Generator is new Ada.Numerics.Discrete_Random(Character);
      Gen : Guess_Generator.Generator;
      Data : SHA256_Value;
   begin
      for I in Data'Range loop
         Guess_Generator.Reset(Gen);
         Data(I) := Guess_Generator.Random(Gen);
      end loop;
      return File_Operations.String_Hash(Data);
   end Random_SHA256;

   function To_ISO_8601 (Date : in Ada.Calendar.Time) return String is

   begin
      return GNAT.Calendar.Time_IO.Image(Date, "%Y-%m-%dT%H:%M:%S");
   end To_ISO_8601;

   function From_ISO_8601 (Date_Str : String) return Ada.Calendar.Time is
      Year : Integer;
      Month : Integer range 1..12;
      Day : Integer range 1..31;
      Hour : Integer range 1..23;
      Minute : Integer range 1..59;
      Second : Integer range 1..59;
   begin
      -- 2016-04-09T15:35:16
      Year := Integer'Value(Date_Str(Date_Str'First..Date_Str'First + 3));
      Month := Integer'Value(Date_Str(Date_Str'First + 5..Date_Str'First + 6));
      Day := Integer'Value(Date_Str(Date_Str'First + 8..Date_Str'First + 9));
      Hour := Integer'Value(Date_Str(Date_Str'First + 11..Date_Str'First + 12));
      Minute := Integer'Value(Date_Str(Date_Str'First + 14..Date_Str'First + 15));
      Second := Integer'Value(Date_Str(Date_Str'First + 17..Date_Str'First + 18));
      return Ada.Calendar.Formatting.Time_Of (Year        => Year,
                                              Month       => Month,
                                              Day         => Day,
                                              Hour        => Hour,
                                              Minute      => Minute,
                                              Second      => Second,
                                              Sub_Second  => 0.0);
   end From_ISO_8601;
end Client;
