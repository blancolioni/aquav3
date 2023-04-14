with Ada.Directories;
with WL.Command_Line;

with Aqua.Addressable;
with Aqua.CPU;
with Aqua.Loader;

with Aqua.Paths;
with Aqua.Tests;

procedure Aqua.Driver is
begin
   if not Ada.Directories.Exists (".aqua-options") then
      Ada.Directories.Copy_File
        (Source_Name => Aqua.Paths.Config_File ("default-options.txt"),
         Target_Name => ".aqua-options");
   end if;

   WL.Command_Line.Load_Defaults (".aqua-options");

   if WL.Command_Line.Argument_Count = 1
     and then Ada.Directories.Exists (WL.Command_Line.Argument (1))
   then
      declare
         Argc : constant Word_32 := 16#A000#;
         Argv : constant Word_32 := 16#A004#;
         Arg_Count : constant Natural := WL.Command_Line.Argument_Count;
         Next : Word_32 :=
                       Argv + 4 * Word_32 (WL.Command_Line.Argument_Count) + 4;

         CPU       : constant Aqua.CPU.Reference :=
                       Aqua.Loader.Create_CPU;
      begin
         CPU.Addressable.Set_Word_32 (Argc, Word_32 (Arg_Count));
         for I in 1 .. WL.Command_Line.Argument_Count loop
            declare
               Arg : constant String :=
                       (if I = 1
                        then Ada.Directories.Base_Name
                          (WL.Command_Line.Argument (I))
                        else WL.Command_Line.Argument (I));
            begin
               CPU.Addressable.Set_Word_32
                 (Argv + Word_32 (4 * (I - 1)), Next);
               for Ch of Arg loop
                  CPU.Addressable.Set_Word_8 (Next, Character'Pos (Ch));
                  Next := Next + 1;
               end loop;
               CPU.Addressable.Set_Word_8 (Next, 0);
               Next := Next + 1;
               while Next mod 4 /= 0 loop
                  CPU.Addressable.Set_Word_8 (Next, 0);
                  Next := Next + 1;
               end loop;
            end;
         end loop;
         CPU.Addressable.Set_Word_32
           (Argv + 4 * Word_32 (WL.Command_Line.Argument_Count), 0);

         Aqua.Tests.Run (CPU.all, Argc, Argv, WL.Command_Line.Argument (1));
      end;
   else
      declare
         M      : constant Aqua.Addressable.Reference :=
                    Aqua.Tests.Create_Test_Memory (16#1_0000#);
         CPU    : Aqua.CPU.Instance (M);
      begin
         Aqua.Tests.Test (CPU);
      end;
   end if;
end Aqua.Driver;
