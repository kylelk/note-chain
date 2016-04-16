with AUnit.Assertions; use AUnit.Assertions;
with Settings;

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

   overriding procedure Run_Test
     (Test : in out Settings_Test_Case)
   is
      pragma Unreferenced (Test);
   begin

      -- test the changing of the settings file location
      declare
         Data : Settings.Settings_Data;
         Path : constant String := "test_settings.json";
      begin
         Data.Set_Path(Path);
         Assert(Data.Get_Path = Path, "seting path failed");
      end;
   end Run_Test;

end Settings_Test_Case;
