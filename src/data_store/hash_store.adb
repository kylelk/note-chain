package body Hash_Store is
   procedure Setup (self : in out Data) is
   begin
      null;
   end Setup;

   procedure Cleanup (self : in out Data) is
   begin
      null;
   end Cleanup;

   procedure Set (Self : in out Data; Key : SHA256_Value; Value : String) is
      Result_Cursor : KV_Map.Cursor;
      Inserted      : Boolean;
   begin
      Self.Modified := True;
      Self.Values.Insert
      (To_Unbounded_String
         (Key), To_Unbounded_String
         (Value), Result_Cursor, Inserted);
      if not Inserted then
         Self.Values.Replace_Element
         (Position => Result_Cursor, New_Item => To_Unbounded_String (Value));
      end if;
   end Set;

   function Get (Self : in out Data; Key : SHA256_Value) return String is
   begin
      if Self.Exists (Key) then
         return To_String (Self.Values.Element (To_Unbounded_String (Key)));
      else
         raise KV_Store.No_Key_Error;
      end if;
   end Get;

   function Exists (Self : Data; Key : SHA256_Value) return Boolean is
   begin
      return Self.Values.Contains (To_Unbounded_String (Key));
   end Exists;

   procedure Remove (Self : in out Data; Key : SHA256_Value) is
   begin
      if Self.Exists (Key) then
         Self.Values.Delete (To_Unbounded_String (Key));
      else
         raise KV_Store.No_Key_Error;
      end if;
   end Remove;

   procedure Commit (Self : in out Data) is
   begin
      null;
   end Commit;
end Hash_Store;
