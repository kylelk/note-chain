with Ada.Text_IO;
with Ada.Directories;
with GNATCOLL_JSON;
with Ada.Strings.Unbounded;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded.Hash;
with Ada.IO_Exceptions;

with Config;
with File_Operations;

procedure Main is
   package TIO renames Ada.Text_IO;
   package JSON renames GNATCOLL_JSON;
   package UBS renames Ada.Strings.Unbounded;


   subtype SHA256_Value is String(1..64);
   Empty_Tree_Ref : constant SHA256_Value := (others=>' ');
   type Branch is record
      Name : UBS.Unbounded_String;
      Tree_Ref : SHA256_Value := Empty_Tree_Ref;
   end record;

   function Hash (Key : UBS.Unbounded_String) return Ada.Containers.Hash_Type is
    (UBS.Hash(Key));

   package Branch_Map is new Ada.Containers.Hashed_Maps
     (Key_Type     => UBS.Unbounded_String,
      Element_Type => Branch,
      Hash => Hash,
      Equivalent_Keys => UBS."=");

   type Branch_Info is record
      Head : UBS.Unbounded_String := UBS.Null_Unbounded_String;
      Branches : Branch_Map.Map;
   end record;

   procedure Set_Branch(Info : in out Branch_Info; Item : Branch) is
      use Branch_Map;
      Result_Cursor : Branch_Map.Cursor;
   begin
      Result_Cursor := Info.Branches.Find(Item.Name);
      if Result_Cursor = Branch_Map.No_Element then
         Info.Branches.Insert(Item.Name, Item);
      else
         Info.Branches.Replace_Element(Result_Cursor, Item);
      end if;
   end Set_Branch;

   procedure Set_Head(Info : in out Branch_Info; Branch_Name : UBS.Unbounded_String) is
   begin
      Info.Head := Branch_Name;
   end Set_Head;

   procedure Load_Branches(Result : out Branch_Info) is
      procedure Handler(Name : JSON.UTF8_String; Value : JSON.JSON_Value) is
         Branch_Result : Branch;
         use JSON;
      begin
         Branch_Result.Name := UBS.To_Unbounded_String(Name);
         if JSON.Kind(Value.Get("tree_ref")) = JSON.JSON_String_Type then
            Branch_Result.Tree_Ref := Value.Get("tree_ref");
         end if;
         Set_Branch(Result, Branch_Result);
      end Handler;

      Branch_Json : JSON.JSON_Value;
   begin
      Branch_Json := JSON.Read(File_Operations.Load_File (Config.Branch_JSON_File), "");
      JSON.Map_JSON_Object (Val => Branch_Json.Get("branches"), CB => Handler'Access);
   end Load_Branches;

   procedure Save_Branches(Info : Branch_Info) is
      Result_JSON, Branch_JSON, Branch_Entry_JSON : JSON.JSON_Value;
      Branch_Cursor : Branch_Map.Cursor;
      Temp_Branch : Branch;
      Data_File : TIO.File_Type;
   begin
      Branch_JSON := JSON.Create_Object;
      Branch_Cursor := Info.Branches.First;
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
      Result_JSON.Set_Field("head", Info.Head);

      TIO.Create(Data_File, TIO.Out_File, Config.Branch_JSON_File);
      Ada.Text_IO.Put(Data_File, Result_JSON.Write);
      TIO.Close(Data_File);
   end Save_Branches;

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

   Branch_Status : Branch_Info;
   Default_Branch : Branch;
   First_Load : Boolean := False;
begin
   Setup_Project;

   begin
      Load_Branches(Branch_Status);
   exception when Ada.IO_Exceptions.Name_Error => First_Load := True;
   end;

   if First_Load then
      Default_Branch.Name := Config.Default_Branch_Name;
      Set_Branch(Branch_Status, Default_Branch);
      Set_Head(Branch_Status, Config.Default_Branch_Name);
   end if;

   Save_Branches(Branch_Status);
end Main;
