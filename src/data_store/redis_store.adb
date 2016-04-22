with Ada.Text_IO;

package body Redis_Store is
   procedure Set_Server
     (Self      : in out Data;
      Host      :        String;
      Port      :        Integer;
      Namespace :        String)
   is

      Host_Entry : GNAT.Sockets.Host_Entry_Type :=
        GNAT.Sockets.Get_Host_By_Name (Host);
   begin
      Self.Address.Addr   := GNAT.Sockets.Addresses (Host_Entry);
      Self.Address.Port   := GNAT.Sockets.Port_Type (Port);
      Self.Namespace      := UBS.To_Unbounded_String (Namespace);
      Self.Defined_Server := True;
   end Set_Server;

   procedure Setup (self : in out Data) is
   begin
      if not self.Defined_Server then
         raise Server_Not_Set;
      end if;
      GNAT.Sockets.Create_Socket (self.Socket);
      GNAT.Sockets.Connect_Socket (self.Socket, self.Address);
   end Setup;

   procedure Cleanup (self : in out Data) is
   begin
      GNAT.Sockets.Close_Socket (self.Socket);
   end Cleanup;

   procedure Set (Self : in out Data; Key, Value : String) is
      JSON_String : JSON.JSON_Value;
      Result      : UBS.Unbounded_String;
   begin
      JSON_String := JSON.Create (Value);
      Result      :=
        UBS.To_Unbounded_String
          (Redis_Call
             (Self.Socket,
              "SET " &
              UBS.To_String (Self.Namespace) &
              Key &
              " " &
              JSON_String.Write));
   end Set;

   function Get (Self : in out Data; Key : String) return String is
   begin
      return "";
   end Get;

   function Contains (Self : Data; Key : String) return Boolean is
   begin
      return False;
   end Contains;

   procedure Remove (Self : in out Data; Key : String) is
   begin
      null;
   end Remove;

   procedure Commit (Self : in out Data) is
   begin
      null;
   end Commit;

   function Redis_Call
     (Conn : in out GNAT.Sockets.Socket_Type;
      Cmd  :        String) return String
   is
      Channel : GNAT.Sockets.Stream_Access;
      Data    : Ada.Streams.Stream_Element_Array (1 .. 1024);
      Size    : Ada.Streams.Stream_Element_Offset;
      Ret     : Ada.Strings.Unbounded.Unbounded_String;
      CRLF    : constant String := ASCII.CR & ASCII.LF;
      use Ada.Strings.Unbounded;
   begin
      Channel := GNAT.Sockets.Stream (Conn);
      String'Write (Channel, Cmd & CRLF);
      loop
         GNAT.Sockets.Receive_Socket (Conn, Data, Size);
         for I in 1 .. Size loop
            Ret := Ret & Character'Val (Data (I));
         end loop;
         if Size < Data'Length then
            exit;
         end if;
      end loop;
      return UBS.To_String (Ret);
   end Redis_Call;
end Redis_Store;
