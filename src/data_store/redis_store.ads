with Ada.Containers.Hashed_Maps;
with Ada.Strings;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with GNAT.Sockets;
with Ada.Streams; use Ada.Streams;
with KV_Store;
with GNATCOLL_JSON;

package Redis_Store is


   type Data is new KV_Store.KV_Container with record
      Address : GNAT.Sockets.Sock_Addr_Type;
      Socket  : GNAT.Sockets.Socket_Type;
      Defined_Server : Boolean := False;
      Namespace : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   Server_Not_Set : exception;

   procedure Set_Server(Self : in out Data; Host : String; Port : Integer; Namespace : String);

   procedure Setup (self : in out Data);

   procedure Cleanup (self : in out Data);

   procedure Set (Self : in out Data; Key, Value : String);

   function Get (Self : in out Data; Key : String) return String;

   function Contains (Self : Data; Key : String) return Boolean;

   procedure Remove (Self : in out Data; Key : String);

   procedure Commit (Self : in out Data);

private
     function Redis_Call
     (Conn : in out GNAT.Sockets.Socket_Type;
      Cmd  :        String) return String;
   package UBS renames Ada.Strings.Unbounded;
   package JSON renames GNATCOLL_JSON;
end Redis_Store;
