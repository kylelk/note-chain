with AUnit.Assertions; use AUnit.Assertions;
with Client;
with AUnit.Test_Cases; use AUnit.Test_Cases;

package body Client_Test_Case is

   ----------
   -- Name --
   ----------

   overriding function Name
     (Test : Client_Test_Case)
      return Message_String
   is
   begin
      return Format("Client test");
   end Name;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (Test : in out Client_Test_Case) is
   begin
      null;
   end Register_Tests;

end Client_Test_Case;
