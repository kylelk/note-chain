with AUnit.Assertions; use AUnit.Assertions;
with Client;
with Object_Store;

package body Client_Test_Case is
   overriding function Name (Test : Client_Test_Case) return Message_String is
      pragma Unreferenced (Test);
   begin
      return Format ("Client test");
   end Name;

   procedure Test_Create_Branch (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Client_Status : Client.Client_Status;

      Test_Branch : constant Client.Branch :=
        (+"test", Client.Empty_Hash_Ref);
   begin
      Client_Status.Set_Branch (Test_Branch);
      Assert (Client_Status.Branch_Exists (+"test"), "Branch creation failed");
   end Test_Create_Branch;

   procedure Test_Valid_Branch_Name (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Assert (STR_OPS.Valid_Branch_Name ("test"), "failed branch name");
      Assert
        (STR_OPS.Valid_Branch_Name ("example.com"),
         "failed branch name");
      Assert
        (not STR_OPS.Valid_Branch_Name ("test..master"),
         "failed branch name");
      Assert
        (not STR_OPS.Valid_Branch_Name
           ("2aae6c35c94fcfb415dbe95f408b9ce91ee846ed"),
         "failed branch name");
      Assert
        (not STR_OPS.Valid_Branch_Name ("Contains Space"),
         "failed branch name");
   end Test_Valid_Branch_Name;

   procedure Test_Create_Note (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Client_Status : Client.Client_Status;
      New_Note      : Client.Note;
      use Ada.Strings.Unbounded;
   begin
      Client_Status.Create_Note (New_Note, "hello world");
      Assert (New_Note.Note_Text = +"hello world", "failed setting note text");
      Client.Save (Db, New_Note);
      Assert (New_Note.Saved, "failed saving note");
   end Test_Create_Note;

   procedure Test_Get_Note (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Client_Status : Client.Client_Status;
      New_Note      : Client.Note;
      Note_Result   : Client.Note;

      use Ada.Strings.Unbounded;
   begin
      -- create note
      Client_Status.Create_Note (New_Note, "test get note");
      Client.Save (Db, New_Note);
      -- get created note
      Note_Result := Client.Get (Db, New_Note.Object_Ref);
      Assert (Note_Result.Note_Text = "test get note", "failed geting note");
   end Test_Get_Note;

   procedure Test_Create_Tree (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Client_Status : Client.Client_Status;
      New_Note      : Client.Note;
      New_Tree      : Client.Tree;
      use Ada.Strings.Unbounded;
   begin
      Client_Status.Create_Note (New_Note, "creating a tree");
      Client.Save (Db, New_Note);
      Client.Add_Note (New_Tree, New_Note);
      Client.Save (Db, New_Tree);
      Assert
        (Object_Store.Exists (Db, New_Tree.Object_Ref),
         "failed creating tree");
      declare
         Object_Type : constant String :=
           Object_Store.Object_Type
             (Db,
              New_Tree.Object_Ref);
      begin
         Assert (Object_Type = "tree", "wrong object type " & Object_Type);
      end;
   end Test_Create_Tree;

   procedure Register_Tests (Test : in out Client_Test_Case) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (Test, Test_Create_Branch'Access, "create branch");
      Register_Routine
        (Test,
         Test_Valid_Branch_Name'Access,
         "branch name validation");
      Register_Routine (Test, Test_Create_Note'Access, "create note");
      Register_Routine (Test, Test_Get_Note'Access, "get note");
      Register_Routine (Test, Test_Create_Tree'Access, "create tree");
   end Register_Tests;

end Client_Test_Case;
