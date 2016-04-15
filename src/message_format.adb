with Ada.Strings.Fixed;

package body Message_Format is
   procedure Set_Header (Mesg : in out Message; Key, Value : String) is
   begin
      Mesg.Set_Header
      (Key                                    =>
         UBS.To_Unbounded_String (Key), Value =>
         UBS.To_Unbounded_String (Value));
   end Set_Header;

   procedure Set_Header
     (Mesg       : in out Message;
      Key, Value :        UBS.Unbounded_String)
   is
      Result_Cursor : KV_Map.Cursor;
      Inserted      : Boolean;
      Trimed_Value  : UBS.Unbounded_String := Value;
   begin
      Trimed_Value :=
        To_Unbounded_String
          (Ada.Strings.Fixed.Trim (UBS.To_String (Value), Ada.Strings.Left));
      Mesg.Headers.Insert (Key, Trimed_Value, Result_Cursor, Inserted);
      if not Inserted then
         Mesg.Headers.Replace_Element (Result_Cursor, Trimed_Value);
      end if;
   end Set_Header;

   procedure Set_Content
     (Mesg    : in out Message;
      Content :        UBS.Unbounded_String)
   is
   begin
      Mesg.Content := Content;
   end Set_Content;

   procedure Set_Content (Mesg : in out Message; Content : String) is
   begin
      Mesg.Content := UBS.To_Unbounded_String (Content);
   end Set_Content;

   function To_String (Mesg : in out Message) return String is
      Result : UBS.Unbounded_String;

      procedure Process_Header (Position : KV_Map.Cursor) is
         Key   : constant UBS.Unbounded_String := KV_Map.Key (Position);
         Value : constant UBS.Unbounded_String := KV_Map.Element (Position);
      begin
         Result :=
           Result &
           Key &
           To_Unbounded_String (": ") &
           Value &
           ASCII.CR &
           ASCII.LF;
      end Process_Header;
   begin
      Mesg.Headers.Iterate (Process_Header'Access);
      Result := Result & ASCII.CR & ASCII.LF;
      Result := Result & Mesg.Content;
      return UBS.To_String (Result);
   end To_String;
end Message_Format;
