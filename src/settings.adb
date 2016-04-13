with GNATCOLL_JSON;
with Config;
with Ada.Text_IO;
with Ada.IO_Exceptions;

with File_Operations;

package body Settings is
   package JSON renames GNATCOLL_JSON;
   package TIO renames Ada.Text_IO;

   procedure Load (Data : out Settings_Data) is
      JSON_Data : JSON.JSON_Value;

      procedure Handler (Name : JSON.UTF8_String; Value : JSON.JSON_Value) is
         pragma Unreferenced (Value);
      begin
         Data.Values.Insert
         (UBS.To_Unbounded_String (Name), JSON_Data.Get (Name));
      end Handler;
   begin
      begin
      JSON_Data :=
        JSON.Read (File_Operations.Load_File (Config.Settings_JSON_File), "");
         JSON.Map_JSON_Object (Val => JSON_Data, CB => Handler'Access);
      exception
         when Ada.IO_Exceptions.Name_Error => null;
      end;
   end Load;

   function Exists
     (Data : in out Settings_Data;
      Key  :        String) return Boolean
   is
   begin
      return Data.Values.Contains (UBS.To_Unbounded_String (Key));
   end Exists;

   function Get (Data : in out Settings_Data; Key : String) return String is
   begin
      if not Data.Exists (Key) then
         raise No_Key_Error;
      end if;

      return UBS.To_String
          (Data.Values.Element (UBS.To_Unbounded_String (Key)));
   end Get;

   procedure Set (Data : in out Settings_Data; Key : String; Value : String) is
      Result_Cursor : KV_Map.Cursor;
      Inserted      : Boolean;
   begin
      Data.Modified := True;
      Data.Values.Insert
      (Key                                       =>
         UBS.To_Unbounded_String (Key), New_Item =>
         UBS.To_Unbounded_String (Key), Position =>
         Result_Cursor, Inserted                 =>
         Inserted);
      if not Inserted then
         Data.Values.Replace_Element
         (Position => Result_Cursor, New_Item => To_Unbounded_String (Value));
      end if;
   end Set;

   procedure Save (Data : Settings_Data) is
      Data_File   : TIO.File_Type;
      Result_JSON : JSON.JSON_Value;
   begin
      if Data.Modified then
         Result_JSON := JSON.Create_Object;
         TIO.Create (Data_File, TIO.Out_File, Config.Settings_JSON_File);
         TIO.Put (Data_File, Result_JSON.Write (Compact => False));
         TIO.Close (Data_File);
      end if;
   end Save;
end Settings;
