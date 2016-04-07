with Ada.Text_IO;
with Ada.Directories;
with GNATCOLL_JSON;
with Ada.Strings.Unbounded;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded.Hash;

with Config;
with File_Operations;

procedure Main is
   package TIO renames Ada.Text_IO;
   package JSON renames GNATCOLL_JSON;
   package UBS renames Ada.Strings.Unbounded;


   subtype SHA256_Value is String(1..64);
   type Branch is record
      Name : UBS.Unbounded_String;
      Tree_Ref : SHA256_Value;
   end record;

   function Hash (Key : UBS.Unbounded_String) return Ada.Containers.Hash_Type is
    (UBS.Hash(Key));

   package Branch_Map is new Ada.Containers.Hashed_Maps
     (Key_Type     => UBS.Unbounded_String,
      Element_Type => Branch,
      Hash => Hash,
      Equivalent_Keys => UBS."=");

   type Branch_Info is record
      Head : Branch;
      Branches : Branch_Map.Map;
   end record;

   procedure Load_Branches is
      Result : Branch_Info;

      procedure Handler(Name : JSON.UTF8_String; Value : JSON.JSON_Value) is
         Branch_Result : Branch;
      begin
         Branch_Result.Name := UBS.To_Unbounded_String(Name);
         Branch_Result.Tree_Ref := Value.Get("tree_ref");
         Result.Branches.Insert(UBS.To_Unbounded_String(Name), Branch_Result);
      end Handler;

      Branch_Json : JSON.JSON_Value;
      Branch_Cursor : Branch_Map.Cursor;
   begin
      Branch_Json := JSON.Read(File_Operations.Load_File (Config.Branch_JSON_File), "");
      JSON.Map_JSON_Object (Val => Branch_Json.Get("branches"), CB => Handler'Access);
      Branch_Cursor := Result.Branches.First;
      while Branch_Map.Has_Element(Branch_Cursor) loop
         TIO.Put_Line(UBS.To_String(Branch_Map.Element(Branch_Cursor).Name));
         Branch_Map.Next(Branch_Cursor);
      end loop;

   end Load_Branches;

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

begin
   Setup_Project;
   Load_Branches;
end Main;
