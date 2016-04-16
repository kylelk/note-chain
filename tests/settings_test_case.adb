with AUnit.Assertions; use AUnit.Assertions;

package body Settings_Test_Case is

   ----------
   -- Name --
   ----------

   overriding function Name
     (Test : Settings_Test_Case)
      return Message_String
   is
   begin
      return Format ("Settings test");
   end Name;

   --------------
   -- Run_Test --
   --------------

   overriding procedure Run_Test
     (Test : in out Settings_Test_Case)
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Run_Test unimplemented");
      raise Program_Error with "Unimplemented procedure Run_Test";
   end Run_Test;

end Settings_Test_Case;
