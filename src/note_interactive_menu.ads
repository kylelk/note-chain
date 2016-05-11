with Client;
with KV_Store;
with Ada.Text_IO;

package Note_Interactive_Menu is
   procedure Show_Select_Menu(Status : in out Client.Client_Status;
                              Db : in out KV_Store.KV_Container'Class);

private
   procedure List_Notes (Status : in out Client.Client_Status;
               Db : in out KV_Store.KV_Container'Class;
                         Output : out Ada.Text_IO.File_Type);
end Note_Interactive_Menu;
