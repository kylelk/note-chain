with Ada.Strings;
with Ada.Strings.Unbounded;
with GNAT.Sockets;

with KV_Store;
with GNATCOLL_JSON;
with Ada.Strings.Fixed;

package Redis_Store is
   type Data is new KV_Store.KV_Container with record
      Address : GNAT.Sockets.Sock_Addr_Type;
      Socket  : GNAT.Sockets.Socket_Type;
      Defined_Server : Boolean := False;
      Namespace : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   use KV_Store;

   Server_Not_Set : exception;
   Redis_Error : exception;

   function Name (Self : Data) return String is ("redis_store");

   procedure Set_Server(Self : in out Data; Host : String; Port : Integer; Namespace : String);

   procedure Setup (self : in out Data);

   procedure Cleanup (self : in out Data);

   procedure Set (Self : in out Data; Key : SHA256_Value; Value : String);

   function Get (Self : in out Data; Key : SHA256_Value) return String;

   function Exists (Self : in out Data; Key : SHA256_Value) return Boolean;

   procedure Remove (Self : in out Data; Key : SHA256_Value);

   procedure Commit (Self : in out Data);

private
     function Redis_Call
     (Conn : in out GNAT.Sockets.Socket_Type;
      Cmd  :        String) return String;
   package UBS renames Ada.Strings.Unbounded;
   package JSON renames GNATCOLL_JSON;
   package STR_FIX renames Ada.Strings.Fixed;
   CRLF    : constant String := ASCII.CR & ASCII.LF;
end Redis_Store;
