with Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with GNAT.Calendar.Time_IO;

with Config;
with File_Operations;
with Object_Store;

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
   end Save;

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
end Client;
