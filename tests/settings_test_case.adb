with AUnit.Assertions; use AUnit.Assertions;
with Settings;
with AUnit.Test_Cases; use AUnit.Test_Cases;

package body Settings_Test_Case is

   ----------
   -- Name --
   ----------

   overriding function Name
     (Test : Settings_Test_Case) return Message_String
   is
      pragma Unreferenced (Test);
   begin
      return Format ("Settings test");
   end Name;

   --------------
   -- Run_Test --
   --------------

   procedure Test_Set_Path (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
   begin

      -- test the changing of the settings file location
      declare
         Data : Settings.Settings_Data;
         Path : constant String := "test_settings.json";
      begin
         Data.Set_Path (Path);
         Assert (Data.Get_Path = Path, "seting path failed");
      end;
   end Test_Set_Path;

   procedure Test_Set_Value (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
      Data : Settings.Settings_Data;
      Path : constant String := "test_settings.json";
   begin
      Data.Set_Path (Path);
      Data.Set ("message", "hello world");
      Assert (Data.Exists ("message"), "Error setting value");
   end Test_Set_Value;

   procedure Test_Get_Value (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
      Data : Settings.Settings_Data;
   begin
      Data.Set ("message", "hello world");
      Assert (Data.Get ("message") = "hello world", "Error getting value");
   end Test_Get_Value;

   procedure Test_Remove (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
      Data : Settings.Settings_Data;
   begin
      Data.Set ("message", "hello world");
      Data.Remove ("message");
      Assert (not Data.Exists ("message"), "Error removing item");
   end Test_Remove;

   procedure Test_Raising_Exception_Get is
      Data   : Settings.Settings_Data;
      Result : String (1 .. 11);
      pragma Unreferenced (Result);
   begin
      Result := Data.Get ("message");
   end Test_Raising_Exception_Get;

   procedure Test_No_Key_Exception_Get
     (T : in out Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      Assert_Exception
        (Test_Raising_Exception_Get'Access,
         "No_Key_Error was not raised");
   end Test_No_Key_Exception_Get;

   procedure Test_Raising_Exception_Remove is
      Data : Settings.Settings_Data;
   begin
      Data.Remove ("message");
   end Test_Raising_Exception_Remove;

   procedure Test_No_Key_Exception_Remove
     (T : in out Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      Assert_Exception
        (Test_Raising_Exception_Remove'Access,
         "No_Key_Error was not raised");
   end Test_No_Key_Exception_Remove;

   procedure Set_Up (Test : in out Settings_Test_Case) is
   begin
      null;
   end Set_Up;

   procedure Tear_Down (Test : in out Settings_Test_Case) is
   begin
      null;
   end Tear_Down;

   procedure Register_Tests (Test : in out Settings_Test_Case) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (Test, Test_Set_Path'Access, "Test setting path");
      Register_Routine (Test, Test_Set_Value'Access, "Test setting value");
      Register_Routine (Test, Test_Get_Value'Access, "Test getting value");
      Register_Routine (Test, Test_Remove'Access, "Test removing item");
      Register_Routine
        (Test,
         Test_No_Key_Exception_Get'Access,
         "Test exception get item");
      Register_Routine
        (Test,
         Test_No_Key_Exception_Remove'Access,
         "Test exception remove item");
   end Register_Tests;

end Settings_Test_Case;
