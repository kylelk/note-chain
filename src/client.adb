with Ada.Text_IO;

with Config;
with File_Operations;

package body Client is

   -------------------
   -- Load_Branches --
   -------------------

   procedure Load_Branches(Status : in out Client_Status) is
      procedure Handler(Name : JSON.UTF8_String; Value : JSON.JSON_Value) is
         Branch_Result : Branch;
         use JSON;
      begin
         Branch_Result.Name := UBS.To_Unbounded_String(Name);
         if JSON.Kind(Value.Get("tree_ref")) = JSON.JSON_String_Type then
            Branch_Result.Tree_Ref := Value.Get("tree_ref");
         end if;
         Status.Set_Branch(Branch_Result);
      end Handler;
      Branch_Json : JSON.JSON_Value;
   begin
      Branch_Json := JSON.Read(File_Operations.Load_File (Config.Branch_JSON_File), "");
      JSON.Map_JSON_Object (Val => Branch_Json.Get("branches"), CB => Handler'Access);
   end Load_Branches;

   --------------
   -- Set_Head --
   --------------

   procedure Set_Head (Status : in out Client_Status; Branch_Name : UBS.Unbounded_String ) is
   begin
      Status.Branch_Status.Head := Branch_Name;
   end Set_Head;

   ----------------
   -- Set_Branch --
   ----------------

   procedure Set_Branch(Status : in out Client_Status; Item : Branch) is
      use Branch_Map;
      Result_Cursor : Branch_Map.Cursor;
   begin
      Result_Cursor := Status.Branch_Status.Branches.Find(Item.Name);
      if Result_Cursor = Branch_Map.No_Element then
         Status.Branch_Status.Branches.Insert(Item.Name, Item);
      else
         Status.Branch_Status.Branches.Replace_Element(Result_Cursor, Item);
      end if;
   end Set_Branch;

   -------------------
   -- Save_Branches --
   -------------------

   procedure Save_Branches (Status : in out Client_Status) is
      Result_JSON, Branch_JSON, Branch_Entry_JSON : JSON.JSON_Value;
      Branch_Cursor : Branch_Map.Cursor;
      Temp_Branch : Branch;
      Data_File : Ada.Text_IO.File_Type;
   begin
      Branch_JSON := JSON.Create_Object;
      Branch_Cursor := Status.Branch_Status.Branches.First;
      while Branch_Map.Has_Element(Branch_Cursor) loop
         Temp_Branch := Branch_Map.Element(Branch_Cursor);
         Branch_Entry_JSON := JSON.Create_Object;
         Branch_Entry_JSON.Set_Field("name", UBS.To_String(Temp_Branch.Name));
         if Temp_Branch.Tree_Ref /= Empty_Tree_Ref then
            Branch_Entry_JSON.Set_Field("tree_ref", Temp_Branch.Tree_Ref);
         else
            Branch_Entry_JSON.Set_Field("tree_ref", JSON.JSON_Null);
         end if;
         Branch_JSON.Set_Field(UBS.To_String(Temp_Branch.Name), Branch_Entry_JSON);
         Branch_Map.Next(Branch_Cursor);
      end loop;
      Result_JSON := JSON.Create_Object;
      Result_JSON.Set_Field("branches", Branch_JSON);
      Result_JSON.Set_Field("head", UBS.To_String(Status.Branch_Status.Head));

      Ada.Text_IO.Create(Data_File, Ada.Text_IO.Out_File, Config.Branch_JSON_File);
      Ada.Text_IO.Put(Data_File, Result_JSON.Write);
      Ada.Text_IO.Close(Data_File);
   end Save_Branches;
end Client;
