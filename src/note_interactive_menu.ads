with Client;
with KV_Store;

package Note_Interactive_Menu is
   procedure Show_Menu(Status : in out Client.Client_Status;
                       Db : in out KV_Store.KV_Container'Class);
end Note_Interactive_Menu;
