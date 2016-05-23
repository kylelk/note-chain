with Client;
with KV_Store;
with Ada.Text_IO;

package Note_Interactive_Menu is
   No_Notes_Error : exception;

   function Note_Select_Menu
     (Status : in out Client.Client_Status;
      Db     : in out KV_Store.KV_Container'Class) return Client.Note;

   procedure View_Note
     (Db  : in out KV_Store.KV_Container'Class;
      Ref :        Client.SHA256_Value);

private
   package TIO renames Ada.Text_IO;

   procedure List_Notes
     (Status       : in out Client.Client_Status;
      Db           : in out KV_Store.KV_Container'Class;
      Output       : in out TIO.File_Type;
      Result_Count : out Integer);
end Note_Interactive_Menu;
