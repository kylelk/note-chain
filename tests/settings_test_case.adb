with AUnit.Assertions; use AUnit.Assertions;
with Settings;
with AUnit.Test_Cases; use AUnit.Test_Cases;

package body Settings_Test_Case is

   ----------
   -- Name --
   ----------

   overriding function Name
     (Test : Settings_Test_Case)
      return Message_String
   is
      pragma Unreferenced (Test);
   begin
      return Format ("Settings test");
   end Name;

   --------------
   -- Run_Test --
   --------------

   procedure Test_Set_Path
     (T : in out Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin

      -- test the changing of the settings file location
      declare
         Data : Settings.Settings_Data;
         Path : constant String := "test_settings.json";
      begin
         Data.Set_Path(Path);
         Assert(Data.Get_Path = Path, "seting path failed");
      end;
   end Test_Set_Path;

   procedure Set_Up(Test : in out Settings_Test_Case) is
   begin
      null;
   end Set_Up;

   procedure Tear_Down(Test : in out Settings_Test_Case) is
   begin
      null;
   end Tear_Down;


   procedure Register_Tests( Test : in out Settings_Test_Case ) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine(Test, Test_Set_Path'Access, "Test setting path");
   end Register_Tests;

end Settings_Test_Case;
