with AUnit.Assertions; use AUnit.Assertions;
with Ada.Text_IO;

with Client;
with Config;

package body Client_Test_Case is

   Test_Client : Client.Client_Status;

   overriding function Name
     (Test : Client_Test_Case)
      return Message_String
   is
      pragma Unreferenced (Test);
   begin
      return Format("Client test");
   end Name;

   procedure Test_Create_Branch(T : in out Test_Case'Class) is
   begin
      Set_Up(T);
      Test_Client.Copy_Branch(Test_Client.Branch_Status.Head, +"test");
   end Test_Create_Branch;

   procedure Set_Up_Case(Test : in out Test_Case'Class) is
      pragma Unreferenced (Test);
      Default_Value : Client.Client_Status;
      Default_Branch : Client.Branch;
   begin
      Test_Client := Default_Value;
      Default_Branch.Name := Config.Default_Branch_Name;
 --     Test_Client.Set_Branch (Default_Branch);
      Test_Client.Checkout_Branch (Config.Default_Branch_Name);
      Ada.Text_IO.Put_Line("hello world");
   end Set_Up_Case;

   procedure Tear_Down_Case(Test : in out Test_Case'Class) is
   begin
      null;
   end Tear_Down_Case;

   procedure Register_Tests (Test : in out Client_Test_Case) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine(Test, Test_Create_Branch'Access, "Test create branch");
   end Register_Tests;

end Client_Test_Case;
